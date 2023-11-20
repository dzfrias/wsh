use thiserror::Error;

#[derive(Debug)]
pub struct Executable {
    code: extern "C" fn(),
    size: usize,
}

#[derive(Debug, Error)]
pub enum ExecMapError {
    #[error("mmap failed with errno {0}")]
    MapFailed(i32),
    #[error("mprotect failed with errno {0}")]
    MprotectFailed(i32),
    #[error("empty code buffer")]
    EmtpyCodeBuffer,
}

impl Executable {
    pub fn map(code: &[u8]) -> Result<Self, ExecMapError> {
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
            unsafe { std::slice::from_raw_parts(executable_memory_ptr.cast::<u8>(), size) };
        // SAFETY: executable_memory points to an executable memory region code. It is a slice.
        let executable_function: extern "C" fn() =
            unsafe { std::mem::transmute(executable_memory.as_ptr()) };

        Ok(Self {
            code: executable_function,
            size,
        })
    }

    pub fn run(&self) {
        (self.code)();
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
