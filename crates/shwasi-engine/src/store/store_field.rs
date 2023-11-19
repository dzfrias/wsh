use std::{
    fmt,
    marker::PhantomData,
    ops::{Deref, DerefMut, Index, IndexMut},
};

#[derive(Debug, Clone)]
pub(crate) struct StoreField<T> {
    inner: Vec<T>,
}

impl<T> Default for StoreField<T> {
    fn default() -> Self {
        Self { inner: Vec::new() }
    }
}

impl<T> StoreField<T> {
    pub fn len(&self) -> usize {
        self.inner.len()
    }

    /// Allocate a new item in the store and return a handle to it.
    pub fn alloc(&mut self, val: T) -> Addr<T> {
        self.inner.push(val);
        Addr::new(self.inner.len() - 1)
    }

    pub fn clear(&mut self) {
        self.inner.clear();
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

        if first == second || second.as_usize() > self.len() {
            return None;
        }
        if first > second {
            let (tbl1, tbl2) = self.get_pair_mut(second, first)?;
            return Some((tbl2, tbl1));
        }
        // By this point, we know that first < second
        let (a1, a2) = self.inner.split_at_mut(second.as_usize());
        Some((a1.get_mut(first.as_usize())?, a2.get_mut(0)?))
    }
}

impl<T> Index<Addr<T>> for StoreField<T> {
    type Output = T;

    fn index(&self, idx: Addr<T>) -> &Self::Output {
        self.inner.index(idx.as_usize())
    }
}

impl<T> IndexMut<Addr<T>> for StoreField<T> {
    fn index_mut(&mut self, idx: Addr<T>) -> &mut Self::Output {
        self.inner.index_mut(idx.as_usize())
    }
}

impl<T> Deref for StoreField<T> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<T> DerefMut for StoreField<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
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
