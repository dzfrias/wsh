use std::{iter, mem, slice};

use thiserror::Error;

use crate::{value::ValueUntyped, vm::Vm, Value};

#[derive(Debug)]
pub struct Executable {
    /// Function pointer to the executable code. The first arg is a pointer to the locals, and the
    /// second arg is a pointer to the outputs.
    code: extern "C" fn(*const ValueUntyped, *const ValueUntyped),
    /// The size of the executable memory region.
    size: usize,
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
    pub unsafe fn map(code: &[u8]) -> Result<Self, ExecMapError> {
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
        })
    }

    /// Runs the executable code using the vm. It requires the base pointer to the stack frame and
    /// the number of results that the executable should write to the stack.
    pub fn run(&self, vm: &mut Vm, bp: usize, num_results: usize) {
        vm.stack
            .extend(iter::repeat(ValueUntyped::from(Value::I32(0))).take(num_results));
        // It is important that we take these pointers AFTER we extend the stack, since the stack
        // might've been reallocated.
        let locals_ptr = vm.stack[bp..].as_ptr();
        let out_ptr = vm.stack[vm.stack.len() - num_results..].as_ptr();
        // We pass in the locals and the output arrays as pointers to the executable function.
        // These are pointers to the same stack, in different positions. This will allow the
        // executable to use the locals on the stack and write to the outputs on the stack.
        (self.code)(locals_ptr, out_ptr);
    }

    /// Returns a slice of the executable memory region.
    #[cfg(test)]
    pub fn as_bytes(&self) -> &[u8] {
        // SAFETY: self.code is a function pointer to the executable memory region. It should be
        // safe to cast it to a pointer to u8, since it is a pointer to memory.
        unsafe { slice::from_raw_parts(self.code as *const u8, self.size) }
    }

    #[cfg(test)]
    pub(crate) fn run_with(&self, locals: &mut [ValueUntyped], out: &mut [ValueUntyped]) {
        let locals_ptr = locals.as_ptr();
        let out_ptr = out.as_ptr();
        // We pass in the locals and the output arrays as pointers to the executable function.
        // These are pointers to the same stack, in different positions. This will allow the
        // executable to use the locals on the stack and write to the outputs on the stack.
        (self.code)(locals_ptr, out_ptr);
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
