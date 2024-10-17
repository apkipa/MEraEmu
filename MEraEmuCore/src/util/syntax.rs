/// A simplified version of [`cstree::syntax`] that is single-threaded and reference-counted.
use std::{
    cell::UnsafeCell,
    fmt,
    iter::FusedIterator,
    marker::PhantomData,
    mem::ManuallyDrop,
    ptr::NonNull,
    sync::atomic::{AtomicUsize, Ordering},
};

use cstree::{green::*, interning::*, text::*, util::NodeOrToken, RawSyntaxKind, Syntax};
use hashbrown::HashMap;
use rustc_hash::FxBuildHasher;

type FxHashMap<K, V> = HashMap<K, V, FxBuildHasher>;

// (RootNodeData / TokenData, ChildNodeData) depending on the context
type PackedChildElemPtr = ptr_union::Union2<NonNull<usize>, &'static ChildNodeData>;

pub type SyntaxElement<S> = NodeOrToken<SyntaxNode<S>, SyntaxToken<S>>;
pub type SyntaxElementRef<'a, S> = NodeOrToken<&'a SyntaxNode<S>, &'a SyntaxToken<S>>;

#[derive(Debug)]
struct RootNodeData {
    green: GreenNode,
    ref_count: AtomicUsize,
    arena_map: FxHashMap<*const (), &'static PackedChildElemPtr>,
    arena: bumpalo::Bump,
}

#[derive(Debug)]
struct ChildNodeData {
    green: &'static GreenNode,
    root: NonNull<RootNodeData>,
    parent: &'static PackedChildElemPtr,
    index: u32,
    text_offset: TextSize,
}

/// A node in the syntax tree. Single-threaded, reference-counted.
#[derive(Debug)]
#[repr(transparent)]
pub struct SyntaxNode<S: Syntax> {
    // NOTE: Do not use `&'static mut RootNodeData` here; it is UB to have
    //       multiple mutable references to the same data.
    data: ManuallyDrop<PackedChildElemPtr>,
    _phantom: PhantomData<S>,
}

const NODE_BUILDER: ptr_union::Builder2<NonNull<RootNodeData>, &'static ChildNodeData> = {
    assert!(std::mem::align_of::<RootNodeData>() == std::mem::align_of::<ChildNodeData>());
    unsafe { ptr_union::Builder2::new_unchecked() }
};
const CHILD_BUILDER: ptr_union::Builder2<&'static TokenData, &'static ChildNodeData> = {
    assert!(std::mem::align_of::<TokenData>() == std::mem::align_of::<ChildNodeData>());
    unsafe { ptr_union::Builder2::new_unchecked() }
};
// SAFETY: `RootNodeData`, `TokenData` and `ChildNodeData` have the same alignment.
const BUILDER: ptr_union::Builder2<NonNull<usize>, &'static ChildNodeData> =
    unsafe { ptr_union::Builder2::new_unchecked() };

impl<S: Syntax> SyntaxNode<S> {
    pub fn new_root(green: GreenNode) -> Self {
        let root_data = RootNodeData {
            green,
            ref_count: AtomicUsize::new(1),
            arena_map: FxHashMap::with_capacity_and_hasher(1, Default::default()),
            arena: bumpalo::Bump::with_capacity(1024),
        };
        let root_data = Box::leak(Box::new(root_data));
        // Insert root node into the arena map for parent lookup.
        let data = unsafe { NonNull::new_unchecked(root_data as *mut _ as *mut _) };
        let data = BUILDER.a(data);
        let key = &root_data.green as *const _ as *const ();
        _ = root_data
            .arena_map
            .insert(key, root_data.arena.alloc(data.clone()));
        SyntaxNode {
            data: ManuallyDrop::new(data),
            _phantom: PhantomData,
        }
    }
    pub fn green(&self) -> &GreenNode {
        match (*self.data).clone().unpack() {
            ptr_union::Enum2::A(root) => unsafe {
                let root = &*(root.as_ptr() as *const RootNodeData);
                &root.green
            },
            ptr_union::Enum2::B(child) => child.green,
        }
    }
    pub fn text_offset(&self) -> TextSize {
        match (*self.data).clone().unpack() {
            ptr_union::Enum2::A(_) => 0.into(),
            ptr_union::Enum2::B(child) => child.text_offset,
        }
    }
    pub fn text_len(&self) -> TextSize {
        self.green().text_len()
    }
    pub fn text_range(&self) -> TextRange {
        let offset = self.text_offset();
        TextRange::at(offset, self.text_len())
    }
    pub fn syntax_kind(&self) -> RawSyntaxKind {
        self.green().kind()
    }
    pub fn kind(&self) -> S {
        S::from_raw(self.syntax_kind())
    }
    pub fn arity(&self) -> usize {
        self.green()
            .children()
            .filter(|child| matches!(child, NodeOrToken::Node(_)))
            .count()
    }
    pub fn arity_with_tokens(&self) -> usize {
        self.green().children().count()
    }
    pub fn parent(&self) -> Option<&Self> {
        match (*self.data).clone().unpack() {
            ptr_union::Enum2::A(_) => None,
            ptr_union::Enum2::B(child) => Some(unsafe { std::mem::transmute(child.parent) }),
        }
    }
    pub fn children(&self) -> SyntaxNodeChildren<S> {
        SyntaxNodeChildren {
            iter: self.children_with_tokens(),
        }
    }
    pub fn children_with_tokens(&self) -> SyntaxElementChildren<S> {
        SyntaxElementChildren {
            parent: self,
            index: 0,
        }
    }
    pub fn write_display<R>(&self, resolver: &R, target: &mut impl fmt::Write) -> fmt::Result
    where
        R: Resolver<TokenKey> + ?Sized,
    {
        for child in self.children_with_tokens() {
            match child {
                NodeOrToken::Node(node) => node.write_display(resolver, target)?,
                NodeOrToken::Token(token) => write!(target, "{}", token.resolve_text(resolver))?,
            }
        }
        Ok(())
    }
    pub fn display<R>(&self, resolver: &R) -> String
    where
        R: Resolver<TokenKey> + ?Sized,
    {
        let mut buf = String::new();
        self.write_display(resolver, &mut buf).unwrap();
        buf
    }
    pub fn resolve_text<R>(&self, resolver: &R) -> String
    where
        R: Resolver<TokenKey> + ?Sized,
    {
        self.display(resolver)
    }

    unsafe fn root_data(&self) -> *mut RootNodeData {
        match (*self.data).clone().unpack() {
            ptr_union::Enum2::A(root) => root.as_ptr() as *mut RootNodeData,
            ptr_union::Enum2::B(child) => child.root.as_ptr(),
        }
    }
    unsafe fn child_data(&self) -> Option<&'static ChildNodeData> {
        self.data.copy_b()
    }
    unsafe fn get_or_add_child(&self, index: usize) -> &'static PackedChildElemPtr {
        let mut green: &'static GreenNode = std::mem::transmute(self.green());
        let green_child = green.children().nth(index).unwrap_unchecked();
        let key = match green_child {
            NodeOrToken::Node(node) => node as *const _ as *const (),
            NodeOrToken::Token(token) => token as *const _ as *const (),
        };
        let root_data = self.root_data();
        let root_ptr = NonNull::new_unchecked(root_data);
        let root_data = &mut *root_data;
        if let Some(child) = root_data.arena_map.get(&key) {
            return *child;
        }

        // NOTE: We add all children immediately to avoid repeated calculations of `text_offset`.
        let parent = if self.child_data().is_some() {
            // SAFETY: `self` is allocated in the arena, so `parent` is valid.
            std::mem::transmute(self)
        } else {
            // HACK: Make Miri happy
            green = std::mem::transmute(&root_data.green);

            let key = &root_data.green as *const _ as *const ();
            *root_data.arena_map.get(&key).unwrap_unchecked()
        };
        let parent: &'static PackedChildElemPtr = std::mem::transmute(parent);
        let mut offset = self.text_offset();
        root_data.arena_map.reserve(green.children().count());
        for (i, child) in green.children().enumerate() {
            let i = i as u32;
            let key = match child {
                NodeOrToken::Node(node) => node as *const _ as *const (),
                NodeOrToken::Token(token) => token as *const _ as *const (),
            };
            let child_text_len = child.text_len();
            let child = match child {
                NodeOrToken::Node(node) => {
                    let child = root_data.arena.alloc(ChildNodeData {
                        green: node,
                        root: root_ptr,
                        parent,
                        index: i,
                        text_offset: offset,
                    });
                    root_data.arena.alloc(BUILDER.b(child))
                }
                NodeOrToken::Token(token) => {
                    let child = root_data.arena.alloc(TokenData {
                        green: token,
                        root: root_ptr,
                        parent,
                        index: i,
                        text_offset: offset,
                    });
                    let child = NonNull::new_unchecked(child as *mut _ as *mut _);
                    root_data.arena.alloc(BUILDER.a(child))
                }
            };
            {
                let result = root_data.arena_map.insert(key, child);
                debug_assert!(
                    result.is_none(),
                    "duplicate child while adding children to the arena"
                );
            }
            offset += child_text_len;
        }

        return *root_data.arena_map.get(&key).unwrap_unchecked();
    }
}

impl<S: Syntax> Clone for SyntaxNode<S> {
    fn clone(&self) -> Self {
        unsafe {
            let root_data = &mut *self.root_data();
            root_data.ref_count.fetch_add(1, Ordering::Relaxed);
        }
        SyntaxNode {
            data: self.data.clone(),
            _phantom: PhantomData,
        }
    }
}

impl<S: Syntax> Drop for SyntaxNode<S> {
    fn drop(&mut self) {
        unsafe {
            let root_data = &mut *self.root_data();
            if root_data.ref_count.fetch_sub(1, Ordering::AcqRel) == 1 {
                // SAFETY: We are the only owner of `root_data`.
                let _ = Box::from_raw(root_data);
            }
        }
    }
}

#[derive(Debug)]
struct TokenData {
    green: &'static GreenToken,
    root: NonNull<RootNodeData>,
    parent: &'static PackedChildElemPtr,
    index: u32,
    text_offset: TextSize,
}

/// A token in the syntax tree. Single-threaded, reference-counted.
#[derive(Debug)]
#[repr(transparent)]
pub struct SyntaxToken<S: Syntax> {
    // NOTE: Only the first discriminant is used.
    data: ManuallyDrop<PackedChildElemPtr>,
    _phantom: PhantomData<S>,
}

impl<S: Syntax> SyntaxToken<S> {
    pub fn green(&self) -> &GreenToken {
        unsafe { self.token_data().green }
    }
    pub fn text_offset(&self) -> TextSize {
        unsafe { self.token_data().text_offset }
    }
    pub fn text_len(&self) -> TextSize {
        self.green().text_len()
    }
    pub fn text_range(&self) -> TextRange {
        let offset = self.text_offset();
        TextRange::at(offset, self.text_len())
    }
    pub fn syntax_kind(&self) -> RawSyntaxKind {
        self.green().kind()
    }
    pub fn kind(&self) -> S {
        S::from_raw(self.syntax_kind())
    }
    pub fn text_key(&self) -> Option<TokenKey> {
        self.green().text_key()
    }
    pub fn static_text(&self) -> Option<&'static str> {
        S::static_text(self.kind())
    }
    pub fn resolve_text<'i, I>(&self, resolver: &'i I) -> &'i str
    where
        I: Resolver<TokenKey> + ?Sized,
    {
        // one of the two must be present upon construction
        self.static_text()
            .or_else(|| self.green().text(resolver))
            .unwrap()
    }
    pub fn parent(&self) -> &SyntaxNode<S> {
        unsafe { std::mem::transmute(self.token_data().parent) }
    }

    unsafe fn root_data(&self) -> &'static mut RootNodeData {
        &mut *self.token_data().root.as_ptr()
    }
    unsafe fn token_data(&self) -> &'static TokenData {
        &*(self.data.copy_a().unwrap_unchecked().as_ptr() as *const TokenData)
    }
}

impl<S: Syntax> Clone for SyntaxToken<S> {
    fn clone(&self) -> Self {
        unsafe {
            let root_data = self.root_data();
            root_data.ref_count.fetch_add(1, Ordering::Relaxed);
        }
        SyntaxToken {
            data: self.data.clone(),
            _phantom: PhantomData,
        }
    }
}

impl<S: Syntax> Drop for SyntaxToken<S> {
    fn drop(&mut self) {
        unsafe {
            let root_data = self.root_data();
            if root_data.ref_count.fetch_sub(1, Ordering::AcqRel) == 1 {
                // SAFETY: We are the only owner of `root_data`.
                let _ = Box::from_raw(root_data);
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct SyntaxElementChildren<'n, S: Syntax> {
    parent: &'n SyntaxNode<S>,
    index: usize,
}

impl<'n, S: Syntax> Iterator for SyntaxElementChildren<'n, S> {
    type Item = SyntaxElementRef<'n, S>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index >= self.parent.arity_with_tokens() {
            return None;
        }

        Some(unsafe {
            let child = self.parent.get_or_add_child(self.index);
            self.index += 1;
            match child.clone().unpack() {
                ptr_union::Enum2::A(_) => NodeOrToken::Token(std::mem::transmute(child)),
                ptr_union::Enum2::B(_) => NodeOrToken::Node(std::mem::transmute(child)),
            }
        })
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let remaining = self.len();
        (remaining, Some(remaining))
    }

    fn count(self) -> usize
    where
        Self: Sized,
    {
        self.len()
    }
}

impl<S: Syntax> FusedIterator for SyntaxElementChildren<'_, S> {}
impl<S: Syntax> ExactSizeIterator for SyntaxElementChildren<'_, S> {
    fn len(&self) -> usize {
        self.parent.arity_with_tokens() - self.index
    }
}

#[derive(Debug, Clone)]
pub struct SyntaxNodeChildren<'n, S: Syntax> {
    iter: SyntaxElementChildren<'n, S>,
}

impl<'n, S: Syntax> Iterator for SyntaxNodeChildren<'n, S> {
    type Item = &'n SyntaxNode<S>;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.find_map(|elem| elem.into_node())
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let remaining = self.len();
        (remaining, Some(remaining))
    }

    fn count(self) -> usize
    where
        Self: Sized,
    {
        self.len()
    }
}

impl<S: Syntax> FusedIterator for SyntaxNodeChildren<'_, S> {}
impl<S: Syntax> ExactSizeIterator for SyntaxNodeChildren<'_, S> {
    fn len(&self) -> usize {
        self.iter
            .clone()
            .filter(|elem| matches!(elem, NodeOrToken::Node(_)))
            .count()
    }
}

impl<S: Syntax> From<SyntaxNode<S>> for SyntaxElement<S> {
    fn from(node: SyntaxNode<S>) -> SyntaxElement<S> {
        NodeOrToken::Node(node)
    }
}

impl<S: Syntax> From<SyntaxToken<S>> for SyntaxElement<S> {
    fn from(token: SyntaxToken<S>) -> SyntaxElement<S> {
        NodeOrToken::Token(token)
    }
}

pub trait SyntaxElementExt {
    type S: Syntax;

    fn text_range(&self) -> TextRange;
    fn syntax_kind(&self) -> RawSyntaxKind;
    fn kind(&self) -> Self::S;
    fn resolve_text<R>(&self, resolver: &R) -> String
    where
        R: Resolver<TokenKey> + ?Sized;
}

impl<S: Syntax> SyntaxElementExt for SyntaxElement<S> {
    type S = S;

    fn text_range(&self) -> TextRange {
        match self {
            NodeOrToken::Node(it) => it.text_range(),
            NodeOrToken::Token(it) => it.text_range(),
        }
    }
    fn syntax_kind(&self) -> RawSyntaxKind {
        match self {
            NodeOrToken::Node(it) => it.syntax_kind(),
            NodeOrToken::Token(it) => it.syntax_kind(),
        }
    }
    fn kind(&self) -> Self::S {
        match self {
            NodeOrToken::Node(it) => it.kind(),
            NodeOrToken::Token(it) => it.kind(),
        }
    }
    fn resolve_text<R>(&self, resolver: &R) -> String
    where
        R: Resolver<TokenKey> + ?Sized,
    {
        match self {
            NodeOrToken::Node(it) => it.resolve_text(resolver),
            NodeOrToken::Token(it) => it.resolve_text(resolver).to_string(),
        }
    }
}

impl<'a, S: Syntax> From<&'a SyntaxNode<S>> for SyntaxElementRef<'a, S> {
    fn from(node: &'a SyntaxNode<S>) -> Self {
        NodeOrToken::Node(node)
    }
}

impl<'a, S: Syntax> From<&'a SyntaxToken<S>> for SyntaxElementRef<'a, S> {
    fn from(token: &'a SyntaxToken<S>) -> Self {
        NodeOrToken::Token(token)
    }
}

impl<S: Syntax> SyntaxElementExt for SyntaxElementRef<'_, S> {
    type S = S;

    fn text_range(&self) -> TextRange {
        match self {
            NodeOrToken::Node(it) => it.text_range(),
            NodeOrToken::Token(it) => it.text_range(),
        }
    }
    fn syntax_kind(&self) -> RawSyntaxKind {
        match self {
            NodeOrToken::Node(it) => it.syntax_kind(),
            NodeOrToken::Token(it) => it.syntax_kind(),
        }
    }
    fn kind(&self) -> Self::S {
        match self {
            NodeOrToken::Node(it) => it.kind(),
            NodeOrToken::Token(it) => it.kind(),
        }
    }
    fn resolve_text<R>(&self, resolver: &R) -> String
    where
        R: Resolver<TokenKey> + ?Sized,
    {
        match self {
            NodeOrToken::Node(it) => it.resolve_text(resolver),
            NodeOrToken::Token(it) => it.resolve_text(resolver).to_string(),
        }
    }
}
