use std::collections::BinaryHeap;

use rayon::iter::{IndexedParallelIterator, ParallelIterator};

/// A trait for converting a parallel iterator into a sequential one.
/// The ordering of the elements is preserved.
pub trait SequentialBridge: IndexedParallelIterator {
    fn seq_bridge(self) -> SeqIterBridge<'static, 'static, Self>;
    fn seq_bridge_with_scope<'a, 'scope>(
        self,
        scope: &'a rayon::Scope<'scope>,
    ) -> SeqIterBridge<'a, 'scope, Self>;
}

impl<T: IndexedParallelIterator> SequentialBridge for T {
    fn seq_bridge(self) -> SeqIterBridge<'static, 'static, Self> {
        SeqIterBridge {
            i: SeqIterBridgeInner::Idle(self, None),
        }
    }

    fn seq_bridge_with_scope<'a, 'scope>(
        self,
        scope: &'a rayon::Scope<'scope>,
    ) -> SeqIterBridge<'a, 'scope, Self> {
        SeqIterBridge {
            i: SeqIterBridgeInner::Idle(self, Some(scope)),
        }
    }
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

pub struct SeqIterBridge<'a, 'scope, T: IndexedParallelIterator> {
    i: SeqIterBridgeInner<'a, 'scope, T>,
}

enum SeqIterBridgeInner<'a, 'scope, T: IndexedParallelIterator> {
    Idle(T, Option<&'a rayon::Scope<'scope>>),
    Started {
        rx: std::sync::mpsc::Receiver<(usize, T::Item)>,
        recv_count: usize,
        recv_elems: BinaryHeap<ReversedHeapItem<T::Item>>,
    },
}

impl<'a, 'scope, T: IndexedParallelIterator + 'scope> Iterator for SeqIterBridge<'a, 'scope, T> {
    type Item = T::Item;

    fn next(&mut self) -> Option<Self::Item> {
        match &mut self.i {
            SeqIterBridgeInner::Idle(..) => {
                let (tx, rx) = std::sync::mpsc::channel();
                let this = std::mem::replace(
                    self,
                    SeqIterBridge {
                        i: SeqIterBridgeInner::Started {
                            rx,
                            recv_count: 0,
                            recv_elems: BinaryHeap::new(),
                        },
                    },
                );
                let SeqIterBridge {
                    i: SeqIterBridgeInner::Idle(iter, scope),
                } = this
                else {
                    unreachable!();
                };
                if let Some(scope) = scope {
                    scope.spawn(move |_| {
                        iter.enumerate().for_each(|item| {
                            tx.send(item).unwrap();
                        });
                    });
                } else {
                    unsafe fn spawn_as_static<F>(f: F)
                    where
                        F: FnOnce() + Send,
                    {
                        let f: Box<dyn FnOnce() + Send> = Box::new(f);
                        let f: Box<dyn FnOnce() + Send + 'static> = std::mem::transmute(f);
                        rayon::spawn(f);
                    }
                    // SAFETY: Hitting this branch means that the scope was not provided,
                    //         which means `seq_bridge` was called, i.e. 'scope is 'static.
                    unsafe {
                        spawn_as_static(move || {
                            iter.enumerate().for_each(|item| {
                                tx.send(item).unwrap();
                            });
                        });
                    }
                    // rayon::spawn(move || {
                    //     iter.enumerate().for_each(|item| {
                    //         tx.send(item).unwrap();
                    //     });
                    // });
                }
                self.next()
            }
            SeqIterBridgeInner::Started {
                rx,
                recv_count,
                recv_elems,
            } => {
                loop {
                    if recv_elems.peek().map(|x| x.index) == Some(*recv_count) {
                        break;
                    }

                    if let Ok((index, item)) = rx.try_recv() {
                        recv_elems.push(ReversedHeapItem { index, item });
                    } else {
                        break;
                    }
                }
                // while let Ok((index, item)) = rx.try_recv() {
                //     recv_elems.push(ReversedHeapItem { index, item });

                //     if recv_elems.peek().unwrap().index == *recv_count {
                //         break;
                //     }
                // }

                recv_elems.pop().map(|ReversedHeapItem { item, .. }| {
                    *recv_count += 1;
                    item
                })
            }
        }
    }
}
