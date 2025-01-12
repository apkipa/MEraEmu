use std::collections::BinaryHeap;

use rayon::iter::{IndexedParallelIterator, ParallelIterator};

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

        let (tx, rx) = std::sync::mpsc::channel();
        unsafe {
            spawn_as_static(move || {
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
    }
}
