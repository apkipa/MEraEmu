use std::collections::BinaryHeap;

use rayon::iter::{IndexedParallelIterator, ParallelBridge, ParallelIterator};

/// A trait for converting a parallel iterator into a sequential one.
/// The ordering of the elements is preserved.
pub trait SequentialBridge: IndexedParallelIterator {
    /// Convert the parallel iterator into a sequential one, then call `for_each` on it,
    /// on the current thread. This allows you to take advantage of the parallelism of Rayon
    /// while still running the final computation on the current thread.
    fn seq_for_each<F>(self, f: F)
    where
        F: FnMut(Self::Item);
}

#[derive(Debug)]
struct ReversedHeapItem<T> {
    index: usize,
    item: T,
}

impl<T> PartialEq for ReversedHeapItem<T> {
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index
    }
}

impl<T> Eq for ReversedHeapItem<T> {}

impl<T> PartialOrd for ReversedHeapItem<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(other.index.cmp(&self.index))
    }
}

impl<T> Ord for ReversedHeapItem<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        other.index.cmp(&self.index)
    }
}

impl<T: IndexedParallelIterator> SequentialBridge for T {
    fn seq_for_each<F>(self, mut f: F)
    where
        F: FnMut(Self::Item),
    {
        unsafe fn spawn_as_static<F>(f: F)
        where
            F: FnOnce() + Send,
        {
            let f: Box<dyn FnOnce() + Send> = Box::new(f);
            let f: Box<dyn FnOnce() + Send + 'static> = std::mem::transmute(f);
            rayon::spawn(f);
        }

        // HACK: `rayon_active` is used to ensure that no UB occurs on panic unwinding.
        let rayon_active = &std::sync::atomic::AtomicU32::new(1);
        let (tx, rx) = std::sync::mpsc::channel();
        unsafe {
            spawn_as_static(move || {
                scopeguard::defer! {
                    rayon_active.store(0, std::sync::atomic::Ordering::Relaxed);
                    atomic_wait::wake_one(rayon_active);
                };
                self.enumerate().for_each(|item| {
                    _ = tx.send(item);
                });
            });
        }
        let mut temp_recv_elems = BinaryHeap::new();
        let mut current_index = 0;
        rx.into_iter().for_each(|(index, item)| {
            temp_recv_elems.push(ReversedHeapItem { index, item });
            while temp_recv_elems.peek().map(|x| x.index) == Some(current_index) {
                if let Some(ReversedHeapItem { item, .. }) = temp_recv_elems.pop() {
                    f(item);
                    current_index += 1;
                }
            }
        });
        atomic_wait::wait(rayon_active, 1);
    }
}

/// A sealed index type.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ProtectedIndex(usize);

impl ProtectedIndex {
    /// Get the inner index value.
    pub fn get(&self) -> usize {
        self.0
    }
}

/// A trait for converting a sequential iterator into a indexed parallel one.
/// The ordering of the elements is preserved.
pub trait IndexedSequentialParallelBridge: Iterator + Sized
where
    Self::Item: Send,
{
    fn par_idx_bridge(self) -> impl ParallelIterator<Item = (ProtectedIndex, Self::Item)>;
}

impl<T> IndexedSequentialParallelBridge for T
where
    T: Iterator + Send,
    T::Item: Send,
{
    fn par_idx_bridge(self) -> impl ParallelIterator<Item = (ProtectedIndex, Self::Item)> {
        self.enumerate()
            .map(|(index, item)| (ProtectedIndex(index), item))
            .par_bridge()
    }
}

pub trait ParallelIndexedSequentialBridge<U> {
    fn par_seq_for_each<F>(self, f: F)
    where
        F: FnMut(U);
}

impl<T, U> ParallelIndexedSequentialBridge<U> for T
where
    T: ParallelIterator<Item = (ProtectedIndex, U)> + Send,
    U: Send,
{
    fn par_seq_for_each<F>(self, mut f: F)
    where
        F: FnMut(U),
    {
        unsafe fn spawn_as_static<F>(f: F)
        where
            F: FnOnce() + Send,
        {
            let f: Box<dyn FnOnce() + Send> = Box::new(f);
            let f: Box<dyn FnOnce() + Send + 'static> = std::mem::transmute(f);
            rayon::spawn(f);
        }

        // HACK: `rayon_active` is used to ensure that no UB occurs on panic unwinding.
        let rayon_active = &std::sync::atomic::AtomicU32::new(1);
        let (tx, rx) = std::sync::mpsc::channel();
        unsafe {
            spawn_as_static(move || {
                scopeguard::defer! {
                    rayon_active.store(0, std::sync::atomic::Ordering::Relaxed);
                    atomic_wait::wake_one(rayon_active);
                };
                self.for_each(|(index, item)| {
                    _ = tx.send((index.get(), item));
                });
            });
        }
        let mut temp_recv_elems = BinaryHeap::new();
        let mut current_index = 0;
        rx.into_iter().for_each(|(index, item)| {
            temp_recv_elems.push(ReversedHeapItem { index, item });
            while temp_recv_elems.peek().map(|x| x.index) == Some(current_index) {
                if let Some(ReversedHeapItem { item, .. }) = temp_recv_elems.pop() {
                    f(item);
                    current_index += 1;
                }
            }
        });
        atomic_wait::wait(&rayon_active, 1);
    }
}
