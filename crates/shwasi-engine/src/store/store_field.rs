use std::{
    fmt,
    marker::PhantomData,
    mem::MaybeUninit,
    ops::{Index, IndexMut},
};

use bitvec::vec::BitVec;

#[derive(Debug)]
pub struct StoreField<T> {
    free: BitVec,
    inner: Vec<MaybeUninit<T>>,
}

impl<T> Default for StoreField<T> {
    fn default() -> Self {
        Self {
            inner: Vec::new(),
            free: BitVec::new(),
        }
    }
}

impl<T> StoreField<T> {
    /// Get the length of the store field.
    pub fn len(&self) -> usize {
        self.inner.len()
    }

    /// Allocate a new item in the store field and return a handle to it.
    pub fn alloc(&mut self, val: T) -> Addr<T> {
        if let Some(i) = self.free.first_one() {
            // We know that this must be uninitialized memory because of `free`, so no leak
            self.inner.get_mut(i).unwrap().write(val);
            self.free.get_mut(i).unwrap().set(false);
            Addr::new(i)
        } else {
            self.inner.push(MaybeUninit::new(val));
            self.free.push(false);
            Addr::new(self.inner.len() - 1)
        }
    }

    /// Get an iterator yielding all indices into the store that are free.
    pub fn all_free(&self) -> impl IntoIterator<Item = usize> + '_ {
        self.free.iter_ones()
    }

    /// Free a value from the store field, allowing space to be saved.
    pub fn free(&mut self, addr: Addr<T>) -> T {
        if self.is_uninit(addr) {
            panic!("cannot free store memory location twice");
        }

        let val = std::mem::replace(
            self.inner
                .get_mut(addr.as_usize())
                .expect("free address out of bounds"),
            MaybeUninit::uninit(),
        );
        self.free.get_mut(addr.as_usize()).unwrap().set(true);
        // SAEFETY: we did a check to make sure that this access is safe
        unsafe { val.assume_init() }
    }

    /// Clear all items from the store.
    pub fn clear(&mut self) {
        // Manually drop all initialized elements
        for (val, free) in self.inner.iter_mut().zip(self.free.iter()) {
            if *free {
                continue;
            }
            // SAFETY: this is safe because we've already checked if it's free
            unsafe { val.assume_init_drop() };
        }
        self.inner.clear();
        self.free.clear();
    }

    /// Get two mutable references to items in the store.
    ///
    /// This function will return [`None`] if the two addresses are equal, or if the larger address
    /// is greater than the length of the store.
    pub fn get_pair_mut(&mut self, first: Addr<T>, second: Addr<T>) -> Option<(&mut T, &mut T)> {
        // Four cases:
        // 1. first == second => None
        // 2. second > len => None
        // 3. first > second => swap and call again
        // 4. first < second => split and get the exclusive references
        if self.is_uninit(first) || self.is_uninit(second) {
            panic!("attempted to get already freed value of store field");
        }

        if first == second || second.as_usize() > self.len() {
            return None;
        }
        if first > second {
            let (tbl1, tbl2) = self.get_pair_mut(second, first)?;
            return Some((tbl2, tbl1));
        }
        // By this point, we know that first < second
        let (a1, a2) = self.inner.split_at_mut(second.as_usize());
        // SAFETY: since we already made sure that the two indices are initialized, they must be
        // initialized.
        Some((
            unsafe { a1.get_mut(first.as_usize())?.assume_init_mut() },
            unsafe { a2.get_mut(0)?.assume_init_mut() },
        ))
    }

    /// Returns `true` if `addr` points to uninitialized memory. Note that this returns `false` if
    /// `addr` is out of bounds.
    fn is_uninit(&self, addr: Addr<T>) -> bool {
        self.free.get(addr.as_usize()).is_some_and(|free| *free)
    }
}

impl<T> Drop for StoreField<T> {
    fn drop(&mut self) {
        // We need to make sure to drop elements in the store, so clear here.
        self.clear();
    }
}

impl<T> Index<Addr<T>> for StoreField<T> {
    type Output = T;

    fn index(&self, addr: Addr<T>) -> &Self::Output {
        if self.is_uninit(addr) {
            panic!("attempted to get already freed value of store field");
        }

        // SAFETY: we made a check to make sure it's initialized
        unsafe { self.inner.index(addr.as_usize()).assume_init_ref() }
    }
}

impl<T> IndexMut<Addr<T>> for StoreField<T> {
    fn index_mut(&mut self, addr: Addr<T>) -> &mut Self::Output {
        if self.is_uninit(addr) {
            panic!("attempted to get already freed value of store field");
        }

        // SAFETY: we made a check to make sure it's initialized
        unsafe { self.inner.index_mut(addr.as_usize()).assume_init_mut() }
    }
}

/// An address into a [`StoreField`] of a [`Store`].
pub struct Addr<T> {
    addr: usize,
    _phantom: PhantomData<T>,
}

impl<T> Addr<T> {
    pub fn new(u: usize) -> Self {
        Self {
            addr: u,
            _phantom: PhantomData,
        }
    }

    pub fn as_usize(&self) -> usize {
        self.addr
    }
}

impl<T> Default for Addr<T> {
    fn default() -> Self {
        Self {
            addr: 0,
            _phantom: PhantomData,
        }
    }
}

impl<T> PartialEq for Addr<T> {
    fn eq(&self, other: &Self) -> bool {
        self.addr == other.addr
    }
}

impl<T> std::hash::Hash for Addr<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.addr.hash(state);
    }
}

impl<T> PartialOrd for Addr<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T> fmt::Debug for Addr<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.addr)
    }
}

impl<T> fmt::Display for Addr<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.addr)
    }
}

impl<T> Ord for Addr<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.addr.cmp(&other.addr)
    }
}

impl<T> Eq for Addr<T> {}

impl<T> Clone for Addr<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for Addr<T> {}
