use std::{mem, slice};

use num_enum::TryFromPrimitive;
use smallvec::{smallvec, SmallVec};
use thiserror::Error;

use crate::{value::ValueUntyped, vm::Vm, Addr, Func, Trap, Value};

type CallFn = fn(
    *const Vm,
    Addr<Func>,
    *const ValueUntyped,
    usize,
    *mut *const ValueUntyped,
) -> (u8, *const ValueUntyped);

#[derive(Debug)]
pub struct Executable {
    /// Function pointer to the executable code. The first arg is a pointer to the locals, and the
    /// second arg is a pointer to the outputs.
    code: extern "C" fn(*const ValueUntyped, *const ValueUntyped, *const CallFn, *const Vm) -> u8,
    /// The size of the executable memory region.
    size: usize,
    num_results: usize,
}

#[derive(Debug, Error)]
pub enum ExecMapError {
    /// mmap failed with the given errno.
    #[error("mmap failed with errno {0}")]
    MapFailed(i32),
    /// mprotect failed with the given errno.
    #[error("mprotect failed with errno {0}")]
    MprotectFailed(i32),
    /// The code buffer was empty.
    #[error("empty code buffer")]
    EmtpyCodeBuffer,
}

impl Executable {
    /// Creates new executable memory from the given code.
    ///
    /// # Safety
    /// `code` must be valid executable code for the targetted system. It can to pretty much
    /// anything when called, so it is very important that it does not cause any undefined behavior
    /// or do malicious stuff.
    pub unsafe fn map(code: &[u8], num_results: usize) -> Result<Self, ExecMapError> {
        let size = code.len();
        // mmap will fail with EINVAL when size == 0
        if size == 0 {
            return Err(ExecMapError::EmtpyCodeBuffer);
        }
        let executable_memory_ptr = unsafe {
            libc::mmap(
                std::ptr::null_mut(),
                size,
                libc::PROT_READ | libc::PROT_WRITE,
                libc::MAP_PRIVATE | libc::MAP_ANON,
                0,
                0,
            )
        };
        if executable_memory_ptr == libc::MAP_FAILED {
            // SAFETY: __error must be set when mmap returns MAP_FAILED, so this should be safe.
            let errno = unsafe { *libc::__error() };
            return Err(ExecMapError::MapFailed(errno));
        }
        unsafe {
            // SAFETY: Since executable_memory_ptr didn't fail, it is a valid pointer. code is
            // also valid, since it comes from a slice. Lastly, we know that these two memory
            // regions must not overlap, since we just allocated `executable_memory_ptr` in freshly
            // mapped memory.
            libc::memcpy(
                executable_memory_ptr,
                code.as_ptr().cast::<libc::c_void>(),
                size,
            );
            // SAFETY: executable_memory_ptr is a valid pointer. It should not conflict with the
            // address permissions, since we just mapped it with PROT_READ | PROT_WRITE. The
            // combination of options should also be valid.
            //
            // From the man pages, mprotect errors when:
            // - EACCES (13)   the permissions conflict
            // - EINVAL (22)   the address is not a multiple of the page size
            // - EINVAL (22)   the address is out of range of the process's address space
            // - ENOTSUP (45)  the combination of flags is not supported
            let status = libc::mprotect(
                executable_memory_ptr,
                size,
                libc::PROT_READ | libc::PROT_EXEC,
            );
            if status != 0 {
                let errno = *libc::__error();
                return Err(ExecMapError::MprotectFailed(errno));
            }
        }
        // SAFETY: executable_memory_ptr points to an executable memory region that contains our
        // code. It should also be the same size as the originally passed in code, as a result of
        // the memcpy above.
        let executable_memory =
            unsafe { slice::from_raw_parts(executable_memory_ptr.cast::<u8>(), size) };
        // This is the function that we will call to execute the code. We transmute it to a
        // function pointer, which means that the underlying `code` must be proper binary for the
        // targetted system. This is the safety invariant that makes this function unsafe. `code`
        // can be pretty much anything, so we must trust that it is valid.
        let executable_function = unsafe { mem::transmute(executable_memory.as_ptr()) };

        Ok(Self {
            code: executable_function,
            size,
            num_results,
        })
    }

    /// Runs the executable code using the vm. It requires the base pointer to the stack frame and
    /// the number of results that the executable should write to the stack.
    pub unsafe fn run(&self, vm: &mut Vm) -> Result<(), Trap> {
        let out: SmallVec<[ValueUntyped; 3]> = smallvec![Value::I32(0).untyped(); self.num_results];
        let bp = vm.frame.bp;
        let locals_ptr = vm.stack[bp..].as_ptr();
        let out_ptr = out.as_ptr();
        let call_ptr = call as *const CallFn;
        // We pass in the locals and the output arrays as pointers to the executable function.
        // This will allow the machine code to interact with out VM.
        // We also pass a pointer to the call function, which the machine code can use to call VM
        // functions.
        let result = (self.code)(locals_ptr, out_ptr, call_ptr, vm);
        vm.stack.extend_from_slice(&out);

        match result {
            0 => Ok(()),
            code => Err(Trap::try_from_primitive(code).expect("BUG: should be a valid trap code")),
        }
    }

    /// Returns a slice of the executable memory region.
    pub fn as_bytes(&self) -> &[u8] {
        // SAFETY: self.code is a function pointer to the executable memory region. It should be
        // safe to cast it to a pointer to u8, since it is a pointer to memory.
        unsafe { slice::from_raw_parts(self.code as *const u8, self.size) }
    }

    #[cfg(test)]
    pub(crate) fn run_with(&self, locals: &mut [ValueUntyped], out: &mut [ValueUntyped]) {
        let locals_ptr = locals.as_ptr();
        let out_ptr = out.as_ptr();
        // TODO: fill these in
        (self.code)(locals_ptr, out_ptr, std::ptr::null(), std::ptr::null());
    }
}

#[inline(never)]
fn call(
    vm: *mut Vm,
    addr: Addr<Func>,
    args_ptr: *const ValueUntyped,
    args_len: usize,
    out_bp: *mut *const ValueUntyped,
) -> (u8, *const ValueUntyped) {
    unsafe {
        let args = slice::from_raw_parts(args_ptr, args_len);
        (*vm).stack.extend_from_slice(args);
        let f = &(*vm).store.functions[addr];
        let bp = (*vm).frame.bp;
        let trap_code = if let Err(trap) = (*vm).call_raw(addr) {
            trap as u8
        } else {
            0
        };
        // Update the out_bp pointer to the stack. The stack has a chance of being reallocated
        // after the call, so we need to update the pointer.
        std::ptr::write(out_bp, (*vm).stack.as_ptr().add(bp));
        (
            trap_code,
            (*vm).stack.as_ptr().add((*vm).stack.len() - f.ty().1.len()),
        )
    }
}

impl Drop for Executable {
    fn drop(&mut self) {
        let executable_memory_ptr = self.code as *mut libc::c_void;
        // SAFETY: executable_memory_ptr is a pointer to a valid memory region. size should be
        // greater than zero, due to our assert in `new`. Looking at the docs, it looks like this
        // should be pretty much infallible.
        //
        // According to the man pages, munmap will error with:
        // - EINVAL (22)    if the address is not a multiple of the page size
        // - EINVAL (22)    if the length is 0
        // - EINVAL (22)    if the address is not a valid mapping
        //
        // All three of these should be impossible, given our code.
        unsafe {
            libc::munmap(executable_memory_ptr, self.size);
        }
    }
}
