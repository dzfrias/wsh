use std::cell::UnsafeCell;

use wiggle::{borrow::BorrowChecker, BorrowHandle, GuestError, GuestMemory, Region};

pub struct ShwasiGuestMemory<'a> {
    mem: &'a [u8],
    bc: BorrowChecker,
}

impl<'a> ShwasiGuestMemory<'a> {
    pub fn new(mem: &'a [u8]) -> Self {
        Self {
            mem,
            bc: BorrowChecker::new(),
        }
    }
}

unsafe impl GuestMemory for ShwasiGuestMemory<'_> {
    fn base(&self) -> &[UnsafeCell<u8>] {
        unsafe { std::mem::transmute(self.mem) }
    }

    fn has_outstanding_borrows(&self) -> bool {
        self.bc.has_outstanding_borrows()
    }

    fn is_shared_borrowed(&self, r: Region) -> bool {
        self.bc.is_shared_borrowed(r)
    }

    fn is_mut_borrowed(&self, r: Region) -> bool {
        self.bc.is_mut_borrowed(r)
    }

    fn shared_borrow(&self, r: Region) -> Result<BorrowHandle, GuestError> {
        self.bc.shared_borrow(r)
    }

    fn mut_borrow(&self, r: Region) -> Result<BorrowHandle, GuestError> {
        self.bc.mut_borrow(r)
    }

    fn shared_unborrow(&self, h: BorrowHandle) {
        self.bc.shared_unborrow(h);
    }

    fn mut_unborrow(&self, h: BorrowHandle) {
        self.bc.mut_unborrow(h);
    }
}
