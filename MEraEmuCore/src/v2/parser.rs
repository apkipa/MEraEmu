use std::ops::ControlFlow;

use crate::util::SmallSlice;
use crate::v2::lexer::{EraLexer, EraLexerMode, EraLexerNextResult};
use crate::{
    types::*,
    util::{
        interning::ThreadedTokenInterner,
        rcstr::{self, ArcStr},
        Ascii,
    },
    v2::routines,
};

use cstree::interning::{Interner, Resolver, TokenKey};
use enumset::EnumSet;
use hashbrown::{HashMap, HashSet};
use indexmap::IndexMap;
use rustc_hash::FxBuildHasher;
use EraLexerMode as Mode;
use EraTokenKind as Token;

type FxHashMap<K, V> = HashMap<K, V, FxBuildHasher>;
type FxHashSet<K> = HashSet<K, FxBuildHasher>;
type FxIndexMap<K, V> = IndexMap<K, V, FxBuildHasher>;

type ParseResult<T> = Result<T, ()>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(align(4))]
pub struct EraLiteralI64(pub [u8; 8]);

impl From<i64> for EraLiteralI64 {
    fn from(value: i64) -> Self {
        Self(value.to_le_bytes())
    }
}

impl From<EraLiteralI64> for i64 {
    fn from(value: EraLiteralI64) -> Self {
        i64::from_le_bytes(value.0)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(align(4))]
pub struct EraLiteralF64(pub [u8; 8]);

impl From<f64> for EraLiteralF64 {
    fn from(value: f64) -> Self {
        Self(value.to_le_bytes())
    }
}

impl From<EraLiteralF64> for f64 {
    fn from(value: EraLiteralF64) -> Self {
        f64::from_le_bytes(value.0)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct EraNodeRef(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct EraExtraDataRef(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(align(2))]
pub struct Pad2<T>(pub T);

impl<T> From<T> for Pad2<T> {
    fn from(value: T) -> Self {
        Self(value)
    }
}

// impl<T> From<Pad2<T>> for T {
//     fn from(value: Pad2<T>) -> Self {
//         value.0
//     }
// }

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(align(4))]
pub struct Pad4<T>(pub T);

impl<T> From<T> for Pad4<T> {
    fn from(value: T) -> Self {
        Self(value)
    }
}

// impl<T> From<Pad4<T>> for T {
//     fn from(value: Pad4<T>) -> Self {
//         value.0
//     }
// }

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct EraExtraDataRefWithLen {
    pub len: u32,
    pub data: EraExtraDataRef,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct EraSmallExtraData {
    pub len: u32,
    /// When `len` is 1, this is the data stored directly. When `len` is larger,
    /// this is a reference to the extra data.
    pub data_or_ref: u32,
}

// TODO: Maybe We don't need `List*` nodes most of the time?

// NOTE: Vec<T> is [len, T...]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EraNode {
    Invalid,
    Empty,

    // (len, [children...])
    Program(EraExtraDataRefWithLen),

    VarModifier(EraTokenKind),

    ExprPreUnary(Token, EraNodeRef),
    ExprPostUnary(EraNodeRef, Token),
    ExprBinary(EraNodeRef, EraSmallTokenKind, EraNodeRef),
    // (cond, [then, else])
    ExprTernary(EraNodeRef, EraExtraDataRef),
    // (name, args_list)
    ExprFunCall(EraNodeRef, EraNodeRef),
    // BinaryAdd(EraNodeRef, EraNodeRef),
    // BinarySub(EraNodeRef, EraNodeRef),
    // BinaryMul(EraNodeRef, EraNodeRef),
    // BinaryDiv(EraNodeRef, EraNodeRef),
    // BinaryMod(EraNodeRef, EraNodeRef),
    // BinaryBitAnd(EraNodeRef, EraNodeRef),
    // BinaryBitOr(EraNodeRef, EraNodeRef),
    // BinaryBitXor(EraNodeRef, EraNodeRef),
    // UnaryBitNot(EraNodeRef),
    // BinaryBitShiftL(EraNodeRef, EraNodeRef),
    // BinaryBitShiftR(EraNodeRef, EraNodeRef),
    // UnaryLogicalNot(EraNodeRef),
    // BinaryLogicalAnd(EraNodeRef, EraNodeRef),
    // BinaryLogicalOr(EraNodeRef, EraNodeRef),
    // BinaryCmpEq(EraNodeRef, EraNodeRef),
    // BinaryCmpNEq(EraNodeRef, EraNodeRef),
    // BinaryCmpLT(EraNodeRef, EraNodeRef),
    // BinaryCmpGT(EraNodeRef, EraNodeRef),
    // BinaryCmpLEq(EraNodeRef, EraNodeRef),
    // BinaryCmpGEq(EraNodeRef, EraNodeRef),
    // BinaryAssign(EraNodeRef, EraNodeRef),
    // BinaryExprAssign(EraNodeRef, EraNodeRef),
    // BinaryAddAssign(EraNodeRef, EraNodeRef),
    // BinarySubAssign(EraNodeRef, EraNodeRef),
    // BinaryMulAssign(EraNodeRef, EraNodeRef),
    // BinaryDivAssign(EraNodeRef, EraNodeRef),
    // BinaryModAssign(EraNodeRef, EraNodeRef),
    // BinaryBitAndAssign(EraNodeRef, EraNodeRef),
    // BinaryBitOrAssign(EraNodeRef, EraNodeRef),
    // BinaryBitXorAssign(EraNodeRef, EraNodeRef),
    // UnaryIncrement(EraNodeRef),
    // UnaryDecrement(EraNodeRef),
    ExprParen(EraNodeRef),
    // ([var, indices...])
    ExprVarIdx(EraSmallExtraData),
    // (var, namespace)
    ExprVarNamespace(EraNodeRef, EraNodeRef),
    LiteralInt(EraLiteralI64),
    // NOTE: Use `EraLiteralI64`-encoded f64 instead for now
    // LiteralFloat(EraLiteralF64),
    LiteralStr(TokenKey),
    Identifier(TokenKey),
    // (parts_count, node_ref / [node_ref...])
    StringForm(EraSmallExtraData),
    // ([expr, width, alignment])
    StringFormInterpPart(EraSmallExtraData),

    ListExpr(EraExtraDataRefWithLen),
    ListStmt(EraExtraDataRefWithLen),
    ListSharpDecl(EraExtraDataRefWithLen),
    ListSelectCasePred(EraExtraDataRefWithLen),

    // (name, [Vec<qualifiers>, dimensions_list, initializers_list])
    DeclVar(EraNodeRef, EraExtraDataRef),
    DeclVarS(EraNodeRef, EraExtraDataRef),
    DeclLocalSize(EraNodeRef),
    DeclLocalSSize(EraNodeRef),
    // NOTE: Macros are not handled by codegen
    DeclDefine(EraNodeRef, EraNodeRef),
    DeclEventKind(Pad4<EraEventFuncKind>),
    DeclFunction,
    DeclFunctionS,
    DeclTransient,
    // (name, [args_node, sharp_decls_node, stmts_node])
    ItemFunction(EraNodeRef, EraExtraDataRef),

    StmtLabel(EraNodeRef),
    StmtNop,
    StmtExpr(EraNodeRef),
    // (base_assign, extra_values)
    StmtRowAssign(EraNodeRef, EraNodeRef),
    // (command, arguments)
    StmtResultCmdCall(EraNodeRef, EraNodeRef),
    // // NOTE: We squeeze out the discriminants' room for flags
    // StmtDebugPrint(Pad2<EraPrintExtendedFlags>, EraExtraDataRefWithLen),
    // StmtPrint(Pad2<EraPrintExtendedFlags>, EraExtraDataRefWithLen),
    StmtDebugPrint(EraPrintExtendedFlags, EraNodeRef),
    StmtPrint(EraPrintExtendedFlags, EraNodeRef),
    // (dest, [Vec<data | data_list>])
    // NOTE: data_list is of type `ListExpr`
    StmtPrintData(Pad2<EraPrintExtendedFlags>, EraNodeRef, EraExtraDataRef),
    // TODO: Remove this
    // StmtPrintDataList(EraNodeRef, EraExtraDataRef),
    StmtWait,
    StmtForceWait,
    StmtWaitAnyKey,
    StmtIf(EraExtraDataRefWithLen),
    StmtQuit,
    // (cond, [Vec<pred_list | stmt_list>])
    StmtSelectCase(EraNodeRef, EraExtraDataRef),
    // (cond, stmt_list)
    StmtWhile(EraNodeRef, EraNodeRef),
    // (function (ident | expr), args_list)
    StmtCall(EraNodeRef, EraNodeRef),
    // (function (ident | expr), args_list)
    StmtTryCall(EraNodeRef, EraNodeRef),
    // (function (ident | expr), [args_list, then_stmts, catch_stmts])
    StmtTryCCall(EraNodeRef, EraExtraDataRef),
    StmtJump(EraNodeRef, EraNodeRef),
    StmtTryJump(EraNodeRef, EraNodeRef),
    StmtTryCJump(EraNodeRef, EraExtraDataRef),
    // (expr_list)
    StmtReturn(EraNodeRef),
    StmtContinue,
    StmtBreak,
    StmtRestart,
    StmtThrow(EraNodeRef),
    // (count, stmt_list)
    StmtRepeat(EraNodeRef, EraNodeRef),
    StmtGoto(EraNodeRef),
    // (var, [start, end, step (maybe Empty), stmt_list])
    StmtFor(EraNodeRef, EraExtraDataRef),
    // (stmt_list, cond)
    StmtDoLoop(EraNodeRef, EraNodeRef),
    // StmtGCreate,
    // StmtGDispose,
    // StmtGDrawSprite,
    StmtSplit(EraNodeRef),
    // (var, factor (f64))
    StmtTimes(EraNodeRef, EraNodeRef),
    StmtSetBit(EraNodeRef),
    StmtClearBit(EraNodeRef),
    StmtInvertBit(EraNodeRef),
    StmtSetColor(EraNodeRef),
    StmtResetColor,
    StmtSetBgColor(EraNodeRef),
    StmtResetBgColor,
    StmtVarSet(EraNodeRef),
    StmtCVarSet(EraNodeRef),
    StmtVarSize(EraNodeRef),
    StmtSwap(EraNodeRef),
    StmtHtmlPrint(EraNodeRef),
    StmtPrintButton(EraNodeRef),
    StmtPrintButtonC(EraNodeRef),
    StmtPrintButtonLC(EraNodeRef),
    StmtArrayRemove(EraNodeRef),
    StmtArraySort(EraNodeRef),
    StmtArrayMSort(EraNodeRef),
    StmtArrayCopy(EraNodeRef),
    StmtArrayShift(EraNodeRef),
    StmtInput(EraNodeRef),
    StmtInputS(EraNodeRef),
    StmtTInput(EraNodeRef),
    StmtTInputS(EraNodeRef),
    StmtOneInput(EraNodeRef),
    StmtOneInputS(EraNodeRef),
    StmtTOneInput(EraNodeRef),
    StmtTOneInputS(EraNodeRef),
    StmtReuseLastLine(EraNodeRef),
    StmtClearLine(EraNodeRef),
    StmtDrawLine,
    StmtCustomDrawLine(EraNodeRef),
    StmtTWait(EraNodeRef),
    StmtFontStyle(EraNodeRef),
    StmtFontBold,
    StmtFontItalic,
    StmtFontRegular,
    StmtSetFont(EraNodeRef),
    StmtStrData(EraNodeRef, EraExtraDataRef),
    StmtPutForm(EraNodeRef),
    StmtSkipDisp(EraNodeRef),
    StmtBegin(EraNodeRef),
    StmtDoTrain(EraNodeRef),
    StmtRedraw(EraNodeRef),
    StmtStrLen(EraNodeRef),
    StmtStrLenU(EraNodeRef),
    StmtAlignment(EraNodeRef),
    StmtToolTipSetDelay(EraNodeRef),
    StmtToolTipSetDuration(EraNodeRef),
    StmtRandomize(EraNodeRef),
    StmtDumpRand,
    StmtInitRand,
    StmtBar(EraNodeRef),
    StmtBarL(EraNodeRef),
    StmtAddChara(EraNodeRef),
    StmtPickUpChara(EraNodeRef),
    StmtDelChara(EraNodeRef),
    StmtSwapChara(EraNodeRef),
    StmtAddCopyChara(EraNodeRef),
    StmtResetStain(EraNodeRef),
    StmtSaveChara(EraNodeRef),
    StmtLoadChara(EraNodeRef),
    StmtSetAnimeTimer(EraNodeRef),
    StmtHtmlTagSplit(EraNodeRef),
    StmtPower(EraNodeRef),
    StmtLoadData(EraNodeRef),
    StmtSaveData(EraNodeRef),
    // StmtCheckData(EraNodeRef),
    StmtGetTime,
    StmtLoadGlobal,
    StmtSaveGlobal,
    StmtLoadGame,
    StmtSaveGame,
    StmtDebugClear,
    StmtResetData,

    SelectCaseCondSingle(EraNodeRef),
    SelectCaseCondRange(EraNodeRef, EraNodeRef),
    SelectCaseCondOperator(Token, EraNodeRef),
}

const _ERA_NODE_SIZE_ASSERTION: [u8; std::mem::size_of::<EraNode>()] = [0; 12];

#[derive(Debug, Clone)]
pub struct EraNodeExprTernary {
    pub cond: EraNodeRef,
    pub then_expr: EraNodeRef,
    pub else_expr: EraNodeRef,
}

impl EraNodeExprTernary {
    pub fn get_from(arena: &EraNodeArena, node_ref: EraNodeRef) -> Self {
        let EraNode::ExprTernary(cond, extra_ref) = arena.get_node(node_ref) else {
            panic!(
                "expected ExprTernary node, found {:?}",
                arena.get_node(node_ref)
            );
        };
        let extra_data = arena.get_extra_data_view_given_len(extra_ref, 2);
        let then_expr = EraNodeRef(extra_data[0]);
        let else_expr = EraNodeRef(extra_data[1]);
        Self {
            cond,
            then_expr,
            else_expr,
        }
    }
}

#[derive(Debug, Clone)]
pub struct EraNodeListExpr<'a> {
    pub children: &'a [u32],
}

impl<'a> EraNodeListExpr<'a> {
    pub fn get_from(arena: &'a EraNodeArena, node_ref: EraNodeRef) -> Self {
        let EraNode::ListExpr(extra_ref) = arena.get_node(node_ref) else {
            panic!(
                "expected ListExpr node, found {:?}",
                arena.get_node(node_ref)
            );
        };
        let children = arena.get_extra_data_view(extra_ref);
        Self { children }
    }
}

#[derive(Debug, Clone)]
pub struct EraNodeStringFormInterpPart {
    pub expr: EraNodeRef,
    pub width: Option<EraNodeRef>,
    pub alignment: Option<EraNodeRef>,
}

impl EraNodeStringFormInterpPart {
    pub fn try_get_from(arena: &EraNodeArena, node_ref: EraNodeRef) -> Option<Self> {
        let EraNode::StringFormInterpPart(extra_ref) = arena.get_node(node_ref) else {
            return None;
        };

        let extra_view = arena.get_small_extra_data_view(extra_ref);
        Some(Self {
            expr: EraNodeRef(extra_view[0]),
            width: extra_view.get(1).map(|&x| EraNodeRef(x)),
            alignment: extra_view.get(2).map(|&x| EraNodeRef(x)),
        })
    }
}

#[derive(Debug, Clone)]
pub struct EraNodeDeclVarHomo<'a> {
    pub is_string: bool,
    pub name: EraNodeRef,
    pub qualifiers: &'a [u32],
    pub dimensions: EraNodeRef,
    pub initializers: EraNodeRef,
}

impl<'a> EraNodeDeclVarHomo<'a> {
    pub fn try_get_from(arena: &'a EraNodeArena, node_ref: EraNodeRef) -> Option<Self> {
        let (is_string, name, extra_ref) = match arena.get_node(node_ref) {
            EraNode::DeclVar(name, extra_ref) => (false, name, extra_ref),
            EraNode::DeclVarS(name, extra_ref) => (true, name, extra_ref),
            _ => return None,
        };
        let len = arena.get_extra_data(extra_ref);
        let extra_data = arena.get_extra_data_view_given_len(extra_ref, len + 3);
        let qualifiers = &extra_data[1..(len as usize + 1)];
        let dimensions = EraNodeRef(extra_data[len as usize + 1]);
        let initializers = EraNodeRef(extra_data[len as usize + 2]);
        Some(Self {
            is_string,
            name,
            qualifiers,
            dimensions,
            initializers,
        })
    }
}

#[derive(Debug)]
pub struct EraNodeArena {
    /// Source spans of each node. Is of the same length as `nodes`.
    pub spans: Vec<SrcSpan>,
    /// Source spans of each token (if the node has one). Is of the same length as `nodes`.
    /// For example, Node `1 + 2` would have token_span refering to the span of `+`.
    // TODO: Maybe we can repurpose this for `BinaryExpr` as (start, EraTokenKind)?
    pub token_spans: Vec<SrcSpan>,
    /// All nodes in the arena.
    pub nodes: Vec<EraNode>,
    /// Extra data for each node.
    pub extra_data: Vec<u32>,
}

/** Design notes:
 *
 * - If a node is of variable length, its parse function must not fail. This ensures that
 *   the node buffer is recovered properly, even when the parse function encounters an error.
 *   This way, callers can always rely on the node buffer being in a consistent state.
 * - For normal nodes, the parse function may return `ParseResult<EraNodeRef>` to indicate
 *   that the node might fail to construct.
 * - When you want to fail the current node, use `?` to short-circuit.
 *   Otherwise, manually sync instead to properly handle local failures.
 */

impl EraNodeArena {
    pub fn new() -> Self {
        Self {
            spans: Vec::new(),
            token_spans: Vec::new(),
            nodes: Vec::new(),
            extra_data: Vec::new(),
        }
    }

    pub fn add_node(&mut self, node: EraNode, span: SrcSpan, token_span: SrcSpan) -> EraNodeRef {
        // eprintln!("[DBG] {:?}", node);
        let node_ref = EraNodeRef(self.nodes.len() as u32);
        self.nodes.push(node);
        self.spans.push(span);
        self.token_spans.push(token_span);
        node_ref
    }

    pub fn span_from_nodes(&self, start: EraNodeRef, end: EraNodeRef) -> SrcSpan {
        let start = self.spans[start.0 as usize].start();
        let end = self.spans[end.0 as usize].end();
        SrcSpan::with_ends(start, end)
    }

    pub fn add_extra(&mut self, data: u32) -> EraExtraDataRef {
        let data_ref = EraExtraDataRef(self.extra_data.len() as u32);
        self.extra_data.push(data);
        data_ref
    }

    pub fn extend_extra(&mut self, iter: impl IntoIterator<Item = u32>) -> (EraExtraDataRef, u32) {
        let iter = iter.into_iter();
        let start = self.extra_data.len() as u32;
        let mut len = 0;
        self.extra_data.extend(iter.inspect(|_| len += 1));
        (EraExtraDataRef(start), len)
    }

    pub fn extend_extra_with_len(
        &mut self,
        iter: impl IntoIterator<Item = u32>,
    ) -> (EraExtraDataRef, u32) {
        let iter = iter.into_iter();
        let start = self.extra_data.len() as u32;
        self.extra_data.push(0);
        let mut len = 1;
        self.extra_data.extend(iter.inspect(|_| len += 1));
        self.extra_data[start as usize] = len - 1;
        (EraExtraDataRef(start), len)
    }

    pub fn make_extra_data_ref_with_len(
        &mut self,
        iter: impl IntoIterator<Item = u32>,
    ) -> EraExtraDataRefWithLen {
        let iter = iter.into_iter();
        let (data_ref, len) = self.extend_extra(iter);
        EraExtraDataRefWithLen {
            len,
            data: data_ref,
        }
    }

    pub fn make_small_extra_data(
        &mut self,
        iter: impl IntoIterator<Item = u32>,
    ) -> EraSmallExtraData {
        let iter = iter.into_iter();
        let start = self.extra_data.len() as u32;
        let mut len = 0;
        self.extra_data.extend(iter.inspect(|_| len += 1));
        if len == 0 {
            EraSmallExtraData {
                len: 0,
                data_or_ref: 0,
            }
        } else if len == 1 {
            let data = self.extra_data[start as usize];
            self.extra_data.truncate(start as usize);
            EraSmallExtraData {
                len: 1,
                data_or_ref: data,
            }
        } else {
            EraSmallExtraData {
                len,
                data_or_ref: start,
            }
        }
    }

    pub fn get_node(&self, node_ref: EraNodeRef) -> EraNode {
        self.nodes[node_ref.0 as usize]
    }

    pub fn get_node_span(&self, node_ref: EraNodeRef) -> SrcSpan {
        self.spans[node_ref.0 as usize]
    }

    pub fn get_node_token_span(&self, node_ref: EraNodeRef) -> SrcSpan {
        self.token_spans[node_ref.0 as usize]
    }

    pub fn get_extra_data(&self, data_ref: EraExtraDataRef) -> u32 {
        self.extra_data[data_ref.0 as usize]
    }

    pub fn get_list_extra_data(&self, data_ref: EraExtraDataRef) -> &[u32] {
        let len = self.get_extra_data(data_ref);
        let start = data_ref.0 as usize + 1;
        let end = start + len as usize;
        &self.extra_data[start..end]
    }

    pub fn get_extra_data_view(&self, data_ref: EraExtraDataRefWithLen) -> &[u32] {
        let start = data_ref.data.0 as usize;
        let end = start + data_ref.len as usize;
        &self.extra_data[start..end]
    }

    pub fn get_extra_data_view_given_len(&self, data_ref: EraExtraDataRef, len: u32) -> &[u32] {
        let start = data_ref.0 as usize;
        let end = start + len as usize;
        &self.extra_data[start..end]
    }

    pub fn get_small_extra_data_view(&self, data_ref: EraSmallExtraData) -> SmallSlice<u32> {
        if data_ref.len == 0 {
            SmallSlice::empty()
        } else if data_ref.len == 1 {
            SmallSlice::new_inline(data_ref.data_or_ref)
        } else {
            let start = data_ref.data_or_ref as usize;
            let end = start + data_ref.len as usize;
            SmallSlice::new(&self.extra_data[start..end])
        }
        // if data_ref.len == 0 {
        //     &[]
        // } else if data_ref.len == 1 {
        //     std::slice::from_ref(&data_ref.data_or_ref)
        // } else {
        //     let start = data_ref.data_or_ref as usize;
        //     let end = start + data_ref.len as usize;
        //     &self.extra_data[start..end]
        // }
    }
}

/// A checkpoint for wrapping a node (specifically, its children).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct EraNodeBuilderCheckpoint(u32);

struct EraNodeBuilder<'i, I> {
    interner: &'i I,
    children: Vec<EraNodeRef>,
}

impl<'i, I> EraNodeBuilder<'i, I> {
    fn with_interner(interner: &'i I) -> Self {
        Self {
            interner,
            children: Vec::new(),
        }
    }

    fn interner(&self) -> &'i I {
        self.interner
    }

    fn checkpoint(&self) -> EraNodeBuilderCheckpoint {
        EraNodeBuilderCheckpoint(self.children.len() as u32)
    }

    fn push_child(&mut self, child: EraNodeRef) {
        self.children.push(child);
    }

    fn finish_node(&mut self, checkpoint: EraNodeBuilderCheckpoint) -> std::vec::Drain<EraNodeRef> {
        assert!(
            checkpoint.0 as usize <= self.children.len(),
            "checkpoint out of bounds: checkpoint = {}, children.len() = {}",
            checkpoint.0,
            self.children.len()
        );
        self.children.drain((checkpoint.0 as usize)..)
    }
}

pub struct EraParsedProgram {
    pub root_node: EraNodeRef,
    pub nodes: EraNodeArena,
    pub macro_map: EraMacroMap,
    pub defines: EraDefineScope,
}

pub struct EraParser<'a, 'b, 'i> {
    callback: &'b mut dyn EraCompilerCallback,
    lexer: &'b mut EraLexer<'a>,
    interner: &'i ThreadedTokenInterner,
    replaces: &'b EraDefineScope,
    defines: &'b EraDefineScope,
    is_str_var_fn: &'b mut dyn FnMut(&str) -> bool,
    is_header: bool,
}

impl<'a, 'b, 'i> EraParser<'a, 'b, 'i> {
    pub fn new(
        callback: &'b mut dyn EraCompilerCallback,
        lexer: &'b mut EraLexer<'a>,
        interner: &'i ThreadedTokenInterner,
        replaces: &'b EraDefineScope,
        defines: &'b EraDefineScope,
        is_str_var_fn: &'b mut dyn FnMut(&str) -> bool,
        is_header: bool,
    ) -> Self {
        EraParser {
            callback,
            lexer,
            interner,
            replaces,
            defines,
            is_str_var_fn,
            is_header,
        }
    }

    pub fn parse_program(&mut self) -> EraParsedProgram {
        let builder = EraNodeBuilder::with_interner(self.interner);
        let mut site = EraParserSite::new(
            self.callback,
            self.lexer,
            builder,
            self.replaces,
            self.defines,
            self.is_str_var_fn,
            self.is_header,
        );
        // NOTE: We use error-tolerant parsing here, so theoretically this should never fail.
        let program_root = site.program().unwrap();
        let (node_arena, macro_map, defines) = site.into_inner();
        EraParsedProgram {
            root_node: program_root,
            nodes: node_arena,
            macro_map,
            defines,
        }
    }
}

struct EraParserOuter<'a, 'b, 'i> {
    callback: &'b mut dyn EraCompilerCallback,
    // lexer
    l: &'b mut EraLexer<'a>,
    // builder
    b: EraNodeBuilder<'i, ThreadedTokenInterner>,
    replaces: &'b EraDefineScope,
    defines: &'b EraDefineScope,
    local_defines: EraDefineScope,
    is_str_var_fn: &'b mut dyn FnMut(&str) -> bool,
    is_header: bool,
    is_panicking: bool,
    macro_place: EraParserMacroPlace<'a>,
    node_arena: EraNodeArena,
}

// ((macro_span, covering_span, start_pos), macro_map, src)
#[derive(Debug, Default)]
struct EraParserMacroPlace<'src>(Option<(SrcSpan, SrcSpan, u32)>, EraMacroMap, &'src str);

impl EraParserMacroPlace<'_> {
    fn handle_macro_token(&mut self, result: &EraLexerNextResult, start_pos: u32, end_pos: u32) {
        if !result.is_replaced && self.0.is_none() {
            return;
        }

        // Extend macro place
        let token_span = result.token.span;
        let extend_fn = |span: SrcSpan| {
            let start = span.start().min(token_span.start());
            let end = span.end().max(token_span.end());
            SrcSpan::with_ends(start, end)
        };
        if let Some((macro_span, covering_span, _)) = self.0.as_mut() {
            if result.is_replaced {
                *macro_span = extend_fn(*macro_span);
            }
            *covering_span = extend_fn(*covering_span);
        } else {
            self.0 = Some((token_span, token_span, start_pos));
        }

        // Close over macro if needed
        self.close_over_macro(end_pos);
    }

    fn has_macro(&self) -> bool {
        self.0.is_some()
    }

    fn is_closed_over(&self) -> bool {
        self.0.map_or(false, |(macro_span, covering_span, _)| {
            // Ensure that we have left the macro region (avoids repeated macro tokens)
            // NOTE: No equal here because then we won't be able to tell if we have
            //       left the macro region.
            covering_span.end() > macro_span.end()
        })
    }

    fn close_over_macro(&mut self, end_pos: u32) -> bool {
        if !self.is_closed_over() {
            return false;
        }

        let (macro_span, covering_span, start_pos) = self.0.take().unwrap();
        let in_span = SrcSpan::with_ends(SrcPos(start_pos), SrcPos(end_pos));
        let text = &self.2[covering_span.start().0 as usize..covering_span.end().0 as usize];
        self.1.push(in_span, text.into());

        true
    }
}

impl<'a, 'b, 'i> EraParserOuter<'a, 'b, 'i> {
    fn new(
        callback: &'b mut dyn EraCompilerCallback,
        l: &'b mut EraLexer<'a>,
        b: EraNodeBuilder<'i, ThreadedTokenInterner>,
        replaces: &'b EraDefineScope,
        defines: &'b EraDefineScope,
        is_str_var_fn: &'b mut dyn FnMut(&str) -> bool,
        is_header: bool,
    ) -> Self {
        let mut macro_place = EraParserMacroPlace::default();
        macro_place.2 = l.get_src();
        EraParserOuter {
            callback,
            l,
            b,
            replaces,
            defines,
            local_defines: Default::default(),
            is_str_var_fn,
            is_header,
            is_panicking: false,
            macro_place,
            node_arena: EraNodeArena::new(),
        }
    }

    fn into_inner(self) -> (EraNodeArena, EraMacroMap, EraDefineScope) {
        (self.node_arena, self.macro_place.1, self.local_defines)
    }

    fn set_is_panicking(&mut self, is_panicking: bool) {
        self.is_panicking = is_panicking;
    }

    fn emit_diag(&mut self, diag: Diagnostic<'a>) {
        if self.is_panicking {
            diag.cancel();
            return;
        }
        let provider = DiagnosticProvider::new(&diag, None, None);
        self.callback.emit_diag(&provider);
        diag.cancel();
    }

    fn next_token(&mut self, mode: EraLexerMode) -> EraLexerNextResult {
        // SAFETY: This requires the Polonius borrow checker.
        let result: EraLexerNextResult = unsafe { std::mem::transmute(self.peek_token(mode)) };
        if result.token.kind != Token::LineBreak {
            self.next_token_with_newline(mode)
        } else {
            result
        }
    }

    fn next_token_with_newline(&mut self, mode: EraLexerMode) -> EraLexerNextResult {
        loop {
            // self.l.skip_whitespace(mode);

            // SAFETY: This requires the Polonius borrow checker.
            let result: EraLexerNextResult = unsafe {
                std::mem::transmute(self.l.read(
                    mode,
                    self.callback,
                    self.replaces,
                    &[self.defines, &self.local_defines],
                ))
            };
            let start_pos = result.token.span.start().0;
            // self.b.token(result.token.kind, result.lexeme);
            let end_pos = result.token.span.end().0;
            // self.macro_place
            //     .handle_macro_token(&result, start_pos, end_pos);
            if !matches!(result.token.kind, Token::WhiteSpace | Token::Comment) {
                break result;
            }
        }
    }

    fn peek_token(&mut self, mode: EraLexerMode) -> EraLexerNextResult {
        loop {
            // self.l.skip_whitespace(mode);

            // SAFETY: This requires the Polonius borrow checker.
            let result: EraLexerNextResult = unsafe {
                std::mem::transmute(self.l.peek(
                    mode,
                    self.callback,
                    self.replaces,
                    &[self.defines, &self.local_defines],
                ))
            };
            if !matches!(result.token.kind, Token::WhiteSpace | Token::Comment) {
                break result;
            }
            let result = self.l.read(
                mode,
                self.callback,
                self.replaces,
                &[self.defines, &self.local_defines],
            );
            let start_pos = result.token.span.start().0;
            // self.b.token(result.token.kind, result.lexeme);
            let end_pos = result.token.span.end().0;
            // self.macro_place
            //     .handle_macro_token(&result, start_pos, end_pos);
        }
    }

    fn bump(&mut self) -> EraLexerNextResult {
        let result = self.l.bump();
        let start_pos = result.token.span.start().0;
        // self.b.token(result.token.kind, result.lexeme);
        let end_pos = result.token.span.end().0;
        // self.macro_place
        //     .handle_macro_token(&result, start_pos, end_pos);
        result
    }

    fn bump_as(&mut self, kind: Token) -> EraLexerNextResult {
        let result = self.l.bump();
        let start_pos = result.token.span.start().0;
        // self.b.token(kind, result.lexeme);
        let end_pos = result.token.span.end().0;
        // self.macro_place
        //     .handle_macro_token(&result, start_pos, end_pos);
        result
    }

    fn previous_token(&self) -> Token {
        self.l.previous_token()
    }
}

struct EraParserSite<'a, 'b, 'i> {
    o: EraParserOuter<'a, 'b, 'i>,
    base_diag: Diagnostic<'a>,
    local_str_vars: FxHashSet<&'i Ascii<str>>,
}

impl<'a, 'b, 'i> EraParserSite<'a, 'b, 'i> {
    fn new(
        callback: &'b mut dyn EraCompilerCallback,
        l: &'b mut EraLexer<'a>,
        b: EraNodeBuilder<'i, ThreadedTokenInterner>,
        replaces: &'b EraDefineScope,
        defines: &'b EraDefineScope,
        is_str_var_fn: &'b mut dyn FnMut(&str) -> bool,
        is_header: bool,
    ) -> Self {
        let base_diag = l.make_diag();
        let o = EraParserOuter::new(callback, l, b, replaces, defines, is_str_var_fn, is_header);
        EraParserSite {
            o,
            base_diag,
            local_str_vars: FxHashSet::default(),
        }
    }

    fn into_inner(self) -> (EraNodeArena, EraMacroMap, EraDefineScope) {
        self.o.into_inner()
    }

    /// Executes the given function and returns the node on success, or an Invalid node on failure.
    /// Useful when you need a node, but don't care about its validity.
    fn or_sync_to(
        &mut self,
        f: impl FnOnce(&mut Self) -> ParseResult<EraNodeRef>,
        terminals: EnumSet<EraTerminalTokenKind>,
    ) -> EraNodeRef {
        let cur_span = self.o.l.current_src_span();
        let node = f(self).unwrap_or_else(|_| {
            _ = self.sync_to_open(terminals);
            let span = SrcSpan::new_covering(cur_span, self.o.l.current_src_span());
            self.o.node_arena.add_node(EraNode::Invalid, span, span)
        });
        node
    }

    /// Synchronize to the next terminal token, without consuming it.
    /// If encountered LineBreak, Err is returned. Used when you need to explicitly
    /// clean up garbage tokens.
    fn sync_to_open(&mut self, terminals: EnumSet<EraTerminalTokenKind>) -> ParseResult<()> {
        let result = loop {
            let next = self.o.peek_token(Mode::Normal).token.kind;
            if next == Token::Eof {
                break Err(());
            }
            let Ok(next) = next.try_into() else {
                _ = self.o.bump();
                continue;
            };
            if terminals.contains(next) {
                if next == Terminal::LineBreak {
                    break Err(());
                }
                break Ok(());
            }
            _ = self.o.bump();
        };

        result
    }

    fn expect_sync_to_newline(&mut self) -> ParseResult<()> {
        let token = self.o.peek_token(Mode::Normal).token;
        if token.kind != Token::LineBreak && !self.o.l.at_start_of_line() {
            let mut diag = self.base_diag.clone();
            diag.span_err(
                Default::default(),
                token.span,
                format!("expected newline, found {:?}", token.kind),
            );
            self.o.emit_diag(diag);
            _ = self.sync_to_open(Terminal::LineBreak.into());
            Err(())
        } else {
            Ok(())
        }
    }

    fn consume_raw(&mut self, mode: Mode, token: Token) -> ParseResult<EraLexerNextResult> {
        let next = self.o.peek_token(mode);
        if next.token.kind == token {
            Ok(self.o.bump())
        } else {
            let mut diag = self.base_diag.clone();
            diag.span_err(
                Default::default(),
                next.token.span,
                format!("expected {:?}, found {:?}", token, next.token.kind),
            );
            self.o.emit_diag(diag);
            Err(())
        }
    }

    fn try_consume_raw(&mut self, mode: Mode, token: Token) -> Option<EraLexerNextResult> {
        let next = self.o.peek_token(mode);
        if next.token.kind == token {
            Some(self.o.bump())
        } else {
            None
        }
    }

    /// Eats the given token, or **complains** about the failure.
    fn eat(&mut self, mode: Mode, token: Token) -> ParseResult<EraLexerNextResult> {
        self.consume_raw(mode, token)
    }

    /// Eats the given token, without complaining if failed.
    fn try_eat(&mut self, mode: Mode, token: Token) -> Option<EraLexerNextResult> {
        self.try_consume_raw(mode, token)
    }

    /// Eats the given token, or **complains** then synchronizes to the next terminal token.
    /// If that terminal token is the given token, it is consumed. Useful when you want to
    /// finish the current reading and move on to next field.
    fn eat_sync(
        &mut self,
        mode: Mode,
        token: Token,
        terminals: EnumSet<Terminal>,
    ) -> Option<EraLexerNextResult> {
        match self.eat(mode, token) {
            Ok(result) => {
                // SAFETY: This requires the Polonius borrow checker.
                let result: EraLexerNextResult = unsafe { std::mem::transmute(result) };
                Some(result)
            }
            Err(_) => {
                _ = self.sync_to_open(terminals);
                self.try_eat(mode, token)
            }
        }
    }

    // fn eat_or_sync(
    //     &mut self,
    //     mode: Mode,
    //     token: Token,
    //     terminals: EnumSet<EraTerminalTokenKind>,
    // ) -> ParseResult<EraLexerNextResult> {
    //     // SAFETY: This requires the Polonius borrow checker.
    //     let result: ParseResult<EraLexerNextResult> =
    //         unsafe { std::mem::transmute(self.eat(mode, token)) };
    //     let Ok(result) = result else {
    //         _ = self.sync_to(terminals);
    //         return Err(());
    //     };
    //     Ok(result)
    // }

    fn skip_newline(&mut self) {
        while self.o.peek_token(Mode::Normal).token.kind == Token::LineBreak {
            _ = self.o.bump();
        }
        self.o.set_is_panicking(false);
    }

    fn is_var_str(&mut self, name: &str) -> bool {
        self.local_str_vars.contains(Ascii::new_str(name)) || (self.o.is_str_var_fn)(name)
    }

    fn span_to_now(&self, start_span: SrcSpan) -> SrcSpan {
        SrcSpan::new_covering(start_span, self.o.l.current_src_span())
    }

    fn make_identifier(&mut self, name: &str) -> EraNodeRef {
        let name = self.o.b.interner().get_or_intern(name);
        let span = self.o.l.current_src_span();
        self.o
            .node_arena
            .add_node(EraNode::Identifier(name), span, span)
    }

    fn make_int_literal(&mut self, val: i64) -> EraNodeRef {
        let span = self.o.l.current_src_span();
        self.o
            .node_arena
            .add_node(EraNode::LiteralInt(val.into()), span, span)
    }

    fn make_str_literal(&mut self, val: &str) -> EraNodeRef {
        let val = self.o.b.interner().get_or_intern(val);
        let span = self.o.l.current_src_span();
        self.o
            .node_arena
            .add_node(EraNode::LiteralStr(val), span, span)
    }

    fn make_empty(&mut self) -> EraNodeRef {
        let span = self.o.l.current_src_span();
        self.o.node_arena.add_node(EraNode::Empty, span, span)
    }

    /// `{NODE OWN}`
    fn program(&mut self) -> ParseResult<EraNodeRef> {
        let span = self.o.l.current_src_span();
        self.skip_newline();
        let cp = self.o.b.checkpoint();
        while self.o.peek_token(Mode::Normal).token.kind != Token::Eof {
            let node = self.or_sync_to(Self::declaration, Terminal::LineBreak.into());
            self.o.b.push_child(node);
            self.skip_newline();
            self.o.set_is_panicking(false);
        }
        // TODO: Force close over macros at EOF?
        // self.o.close_over_macro();
        let children = self.o.b.finish_node(cp).map(|x| x.0);
        let data = self.o.node_arena.make_extra_data_ref_with_len(children);
        let span = SrcSpan::new_covering(span, self.o.l.current_src_span());
        let node = EraNode::Program(data);
        Ok(self.o.node_arena.add_node(node, span, span))
    }

    fn declaration(&mut self) -> ParseResult<EraNodeRef> {
        if self.o.peek_token(Mode::Normal).token.kind == Token::NumberSign {
            self.sharp_declaration()
        } else {
            self.func_declaration()
        }
    }

    fn sharp_declaration(&mut self) -> ParseResult<EraNodeRef> {
        let span = self.o.l.current_src_span();

        self.eat(Mode::Normal, Token::NumberSign)?;

        let token = self.o.peek_token(Mode::SharpDecl).token;
        let node = match token.kind {
            Token::KwDim => {
                self.o.bump();
                self.var_declaration(false, span)?
            }
            Token::KwDimS => {
                self.o.bump();
                self.var_declaration(true, span)?
            }
            Token::KwLocalSize => {
                self.o.bump();
                let node = self.or_sync_to(|s| s.expression(true), Terminal::LineBreak.into());
                self.o
                    .node_arena
                    .add_node(EraNode::DeclLocalSize(node), span, span)
            }
            Token::KwLocalSSize => {
                self.o.bump();
                let node = self.or_sync_to(|s| s.expression(true), Terminal::LineBreak.into());
                self.o
                    .node_arena
                    .add_node(EraNode::DeclLocalSSize(node), span, span)
            }
            Token::KwFunction => {
                self.o.bump();
                self.o
                    .node_arena
                    .add_node(EraNode::DeclFunction, span, span)
            }
            Token::KwFunctionS => {
                self.o.bump();
                self.o
                    .node_arena
                    .add_node(EraNode::DeclFunctionS, span, span)
            }
            Token::KwOnly | Token::KwPri | Token::KwLater => {
                self.o.bump();
                let event_kind = match token.kind {
                    Token::KwOnly => EraEventFuncKind::Only,
                    Token::KwPri => EraEventFuncKind::Pri,
                    Token::KwLater => EraEventFuncKind::Later,
                    _ => unreachable!(),
                };
                self.o
                    .node_arena
                    .add_node(EraNode::DeclEventKind(event_kind.into()), span, span)
            }
            Token::KwDefine => {
                self.o.bump();
                self.define_declaration(span)?
            }
            Token::KwTransient => {
                self.o.bump();
                self.o
                    .node_arena
                    .add_node(EraNode::DeclTransient, span, span)
            }
            _ => {
                let mut diag = self.base_diag.clone();
                diag.span_err(
                    Default::default(),
                    token.span,
                    format!("expected declaration keyword, found {:?}", token.kind),
                );
                self.o.emit_diag(diag);
                return Err(());
            }
        };

        Ok(node)
    }

    fn var_declaration(&mut self, is_str: bool, span: SrcSpan) -> ParseResult<EraNodeRef> {
        let cp = self.o.b.checkpoint();

        let name_node = loop {
            let token = self.o.peek_token(Mode::SharpDecl).token;
            match token.kind {
                Token::KwDynamic
                | Token::KwGlobal
                | Token::KwRef
                | Token::KwConst
                | Token::KwSavedata
                | Token::KwCharadata => {
                    _ = self.o.bump();
                    _ = self.o.b.push_child(self.o.node_arena.add_node(
                        EraNode::VarModifier(token.kind),
                        token.span,
                        token.span,
                    ));
                }
                Token::Identifier => {
                    break self.identifier().unwrap();
                }
                _ => {
                    let mut diag = self.base_diag.clone();
                    diag.span_err(
                        Default::default(),
                        token.span,
                        format!("expected identifier, found {:?}", token.kind),
                    );
                    self.o.emit_diag(diag);
                    return Err(());
                }
            }
        };

        let EraNode::Identifier(name) = self.o.node_arena.get_node(name_node) else {
            unreachable!();
        };
        let name = self.o.b.interner().resolve(name);

        // HACK: For string assignment check
        if is_str {
            self.local_str_vars.insert(Ascii::new_str(name));
        }

        // Array dimensions
        let dims_list = if self.try_eat(Mode::Normal, Token::Comma).is_some() {
            // Break at `=`
            let min_bp = infix_binding_power(Token::Assign).unwrap().1 + 2;
            self.comma_expr_list(min_bp).unwrap()
        } else {
            self.empty_comma_expr_list()
        };

        // Initializer
        let inits_list = if self.try_eat(Mode::Normal, Token::Assign).is_some() {
            self.comma_expr_list(0).unwrap()
        } else {
            self.empty_comma_expr_list()
        };

        let (extra_data, _) = self
            .o
            .node_arena
            .extend_extra_with_len(self.o.b.finish_node(cp).map(|x| x.0));
        _ = self.o.node_arena.add_extra(dims_list.0);
        _ = self.o.node_arena.add_extra(inits_list.0);
        let span = SrcSpan::new_covering(span, self.o.l.current_src_span());
        let constructor = if is_str {
            EraNode::DeclVarS
        } else {
            EraNode::DeclVar
        };
        Ok(self
            .o
            .node_arena
            .add_node(constructor(name_node, extra_data), span, span))
    }

    fn define_declaration(&mut self, span: SrcSpan) -> ParseResult<EraNodeRef> {
        let in_def = self.eat(Mode::Normal, Token::Identifier)?;
        let (in_def, in_def_span) = (ArcStr::from(in_def.lexeme), in_def.token.span);
        let out_def =
            arcstr::ArcStr::from(self.eat(Mode::RawStr, Token::PlainStringLiteral)?.lexeme);
        let define_data = EraDefineData {
            filename: self.o.l.current_filename().clone(),
            span: in_def_span,
            data: out_def.clone(),
        };
        if let Some(prev_def) = self.o.local_defines.insert(in_def.clone(), define_data) {
            let mut diag = self.base_diag.clone();
            diag.span_err(
                Default::default(),
                in_def_span,
                format!("redefinition of macro `{}`", in_def,),
            );
            diag.span_note(
                Default::default(),
                prev_def.span,
                "previous definition here",
            );
            self.o.emit_diag(diag);
        }

        // TODO: Complete define_declaration
        let span = SrcSpan::new_covering(span, self.o.l.current_src_span());
        let node = EraNode::DeclDefine(EraNodeRef(u32::MAX), EraNodeRef(u32::MAX));
        Ok(self.o.node_arena.add_node(node, span, span))
    }

    fn func_declaration(&mut self) -> ParseResult<EraNodeRef> {
        let span = self.o.l.current_src_span();

        self.eat(Mode::Normal, Token::At)?;

        // Function name
        let name = self.identifier()?;

        // Local string vars
        self.local_str_vars.clear();
        self.local_str_vars.insert(Ascii::new_str("LOCALS"));
        self.local_str_vars.insert(Ascii::new_str("ARGS"));

        let token = self.o.peek_token(Mode::Normal).token;
        let args_list = match token.kind {
            Token::LParen => self.paren_expr_list().unwrap(),
            Token::Comma => {
                _ = self.o.bump();
                self.comma_expr_list(0).unwrap()
            }
            Token::LineBreak => self.empty_comma_expr_list(),
            _ => {
                let mut diag = self.base_diag.clone();
                diag.span_err(
                    Default::default(),
                    self.o.peek_token(Mode::Normal).token.span,
                    format!("expected `(` or `,`, found {:?}", token.kind),
                );
                self.o.emit_diag(diag);

                self.empty_comma_expr_list()
            }
        };

        // Function body
        _ = self.eat_sync(Mode::Normal, Token::LineBreak, Terminal::LineBreak.into());
        self.skip_newline();

        // Local declarations (appears before statements)
        // let cp = self.o.b.checkpoint();
        // while self.o.peek_token(Mode::Normal).token.kind == Token::NumberSign {
        //     let node = self.or_sync_to(Self::sharp_declaration, Terminal::LineBreak.into());
        //     self.o.b.push_child(node);
        //     _ = self.eat_sync(Mode::Normal, Token::LineBreak, Terminal::LineBreak.into());
        //     self.skip_newline();
        // }
        // let local_decls = self.o.b.finish_node(cp).map(|x| x.0);
        // let local_decls = self.o.node_arena.make_extra_data_ref_with_len(local_decls);
        // let local_decls =
        //     self.o
        //         .node_arena
        //         .add_node(EraNode::ListSharpDecl(local_decls), span, span);
        let local_decls = self.sharp_declarations_list();

        // Statements
        let (stmts_list, _) = self.statements_list(None);

        // Finish function item
        let (extra_data, _) =
            self.o
                .node_arena
                .extend_extra([args_list.0, local_decls.0, stmts_list.0]);
        let span = self.span_to_now(span);
        let node = EraNode::ItemFunction(name, extra_data);
        Ok(self.o.node_arena.add_node(node, span, span))
    }

    fn sharp_declarations_list(&mut self) -> EraNodeRef {
        let span = self.o.l.current_src_span();
        let cp = self.o.b.checkpoint();
        self.skip_newline();
        while self.o.peek_token(Mode::Normal).token.kind == Token::NumberSign {
            let node = self.or_sync_to(Self::sharp_declaration, Terminal::LineBreak.into());
            self.o.b.push_child(node);
            _ = self.expect_sync_to_newline();
            self.skip_newline();
        }
        let children = self.o.b.finish_node(cp).map(|x| x.0);
        let data = self.o.node_arena.make_extra_data_ref_with_len(children);
        let span = self.span_to_now(span);
        self.o
            .node_arena
            .add_node(EraNode::ListSharpDecl(data), span, span)
    }

    /// Parses a list of statements, with optional terminal command (such as `ENDIF`).
    ///
    /// Returns: `(node, is_terminal_cmd_found)`
    fn statements_list(&mut self, terminal_cmd: Option<&str>) -> (EraNodeRef, bool) {
        let span = self.o.l.current_src_span();
        let cp = self.o.b.checkpoint();
        self.skip_newline();
        let found_terminal = loop {
            // First check for terminal command
            if let Some(cmd) = terminal_cmd {
                let result: EraLexerNextResult<'_> = self.o.peek_token(Mode::Normal);
                if result.token.kind == Token::Identifier && result.lexeme.eq_ignore_ascii_case(cmd)
                {
                    // Consume the terminal command
                    _ = self.o.bump();
                    break true;
                }
            }
            // Then proceed as usual
            let node = match self.safe_statement() {
                ControlFlow::Break(()) => break false,
                ControlFlow::Continue(node) => node,
            };
            self.o.b.push_child(node);
            self.skip_newline();
        };
        let children = self.o.b.finish_node(cp).map(|x| x.0);
        let data = self.o.node_arena.make_extra_data_ref_with_len(children);
        let span = self.span_to_now(span);
        let node = self
            .o
            .node_arena
            .add_node(EraNode::ListStmt(data), span, span);
        (node, found_terminal)
    }

    /// Parses a list of statements, with optional termination checking.
    ///
    /// Returns: `(node, is_user_break)`
    fn statements_list_with<F, R>(&mut self, mut check_fn: F) -> (EraNodeRef, R)
    where
        R: Default,
        F: FnMut(&mut Self) -> ControlFlow<R>,
    {
        let span = self.o.l.current_src_span();
        let cp = self.o.b.checkpoint();
        self.skip_newline();
        let ret_val = loop {
            // First check for terminal command
            if let ControlFlow::Break(r) = check_fn(self) {
                break r;
            }
            // Then proceed as usual
            let node = match self.safe_statement() {
                ControlFlow::Break(()) => break Default::default(),
                ControlFlow::Continue(node) => node,
            };
            self.o.b.push_child(node);
            self.skip_newline();
        };
        let children = self.o.b.finish_node(cp).map(|x| x.0);
        let data = self.o.node_arena.make_extra_data_ref_with_len(children);
        let span = self.span_to_now(span);
        let node = self
            .o
            .node_arena
            .add_node(EraNode::ListStmt(data), span, span);
        (node, ret_val)
    }

    /// Parses a statement or syncs to end of line, with avoidance of `@` and `<EOF>`.
    fn safe_statement(&mut self) -> ControlFlow<(), EraNodeRef> {
        let token = self.o.peek_token(Mode::Normal).token;
        match token.kind {
            Token::At | Token::Eof => return ControlFlow::Break(()),
            Token::NumberSign => {
                let mut diag = self.base_diag.clone();
                diag.span_warn(
                    Default::default(),
                    token.span,
                    "sharp declaration should not appear within function body; ignoring",
                );
                self.o.emit_diag(diag);

                let span = self.o.l.current_src_span();
                _ = self.sync_to_open(Terminal::LineBreak.into());
                let span = self.span_to_now(span);
                return ControlFlow::Continue(self.o.node_arena.add_node(
                    EraNode::Invalid,
                    span,
                    span,
                ));
            }
            _ => (),
        }

        let node = self.or_sync_to(Self::statement, Terminal::LineBreak.into());
        _ = self.expect_sync_to_newline();

        ControlFlow::Continue(node)
    }

    fn identifier(&mut self) -> ParseResult<EraNodeRef> {
        let mut interner = self.o.b.interner();
        let token = self.eat(Mode::Normal, Token::Identifier)?;
        let span = token.token.span;
        let token_key = interner.get_or_intern(token.lexeme);
        Ok(self
            .o
            .node_arena
            .add_node(EraNode::Identifier(token_key), span, span))
    }

    fn integer_literal(&mut self) -> ParseResult<EraNodeRef> {
        let token = self.eat(Mode::Normal, Token::IntLiteral)?;
        let span = token.token.span;
        let val = routines::parse_int_literal(token.lexeme.as_bytes()).unwrap();
        Ok(self
            .o
            .node_arena
            .add_node(EraNode::LiteralInt(val.into()), span, span))
    }

    fn plain_string_literal(&mut self) -> ParseResult<EraNodeRef> {
        self.eat(Mode::Normal, Token::DoubleQuote)?;

        let span = self.o.l.current_src_span();
        let mut buf = String::new();
        loop {
            let result = self.o.peek_token(Mode::PlainStr);
            let token = result.token;
            if token.kind == Token::PlainStringLiteral {
                routines::unescape_to_sink(result.lexeme, &mut buf);
                _ = self.o.bump();
                continue;
            }
            match token.kind {
                _ => break,
            }
        }

        _ = self.eat_sync(
            Mode::PlainStr,
            Token::DoubleQuote,
            Terminal::DoubleQuote | Terminal::LineBreak,
        );

        let token_key = self.o.b.interner().get_or_intern(&buf);
        let span = self.span_to_now(span);
        Ok(self
            .o
            .node_arena
            .add_node(EraNode::LiteralStr(token_key), span, span))
    }

    /// Parses expressions like `'string content`. May break at comma.
    fn quote_raw_strform(&mut self) -> ParseResult<EraNodeRef> {
        self.eat(Mode::Normal, Token::SingleQuote)?;

        let span = self.o.l.current_src_span();
        let mut buf = String::new();

        loop {
            let result = self.o.peek_token(Mode::CommaRawStr);
            let token = result.token;
            if token.kind == Token::PlainStringLiteral {
                routines::unescape_to_sink(result.lexeme, &mut buf);
                _ = self.o.bump();
                continue;
            }
            match token.kind {
                _ => break,
            }
        }

        // NOTE: No need to sync, leave it to the caller

        let token_key = self.o.b.interner().get_or_intern(&buf);
        let span = self.span_to_now(span);
        Ok(self
            .o
            .node_arena
            .add_node(EraNode::LiteralStr(token_key), span, span))
    }

    /// Parses expression lists like `(arg1, arg2)`.
    fn paren_expr_list(&mut self) -> ParseResult<EraNodeRef> {
        let span = self.o.l.current_src_span();
        let cp = self.o.b.checkpoint();

        self.eat(Mode::Normal, Token::LParen)?;

        if self.o.peek_token(Mode::Normal).token.kind == Token::RParen {
            // Empty expression list
            _ = self.o.bump();
            let data = self
                .o
                .node_arena
                .make_extra_data_ref_with_len(self.o.b.finish_node(cp).map(|x| x.0));
            let span = SrcSpan::new_covering(span, self.o.l.current_src_span());
            return Ok(self
                .o
                .node_arena
                .add_node(EraNode::ListExpr(data), span, span));
        }

        let comma_ended = loop {
            match self.o.peek_token(Mode::Normal).token.kind {
                Token::RParen => break true,
                Token::Comma => {
                    // Empty expression
                    self.o
                        .b
                        .push_child(self.o.node_arena.add_node(EraNode::Empty, span, span));
                    _ = self.o.bump();
                    continue;
                }
                _ => (),
            }

            let terminals = Terminal::Comma | Terminal::RParen | Terminal::LineBreak;
            let node = self.or_sync_to(|s| s.expression(true), terminals);
            self.o.b.push_child(node);
            if self.o.peek_token(Mode::Normal).token.kind == Token::RParen
                || self
                    .eat_sync(Mode::Normal, Token::Comma, terminals)
                    .is_none()
            {
                // No comma, must be end of expression list
                break false;
            }
        };

        if comma_ended {
            // We want more expressions, and the last one was a comma
            let span = self.o.l.current_src_span();
            self.o
                .b
                .push_child(self.o.node_arena.add_node(EraNode::Empty, span, span));
        }

        _ = self.eat_sync(
            Mode::Normal,
            Token::RParen,
            Terminal::RParen | Terminal::LineBreak,
        );

        let data = self
            .o
            .node_arena
            .make_extra_data_ref_with_len(self.o.b.finish_node(cp).map(|x| x.0));
        let span = SrcSpan::new_covering(span, self.o.l.current_src_span());
        Ok(self
            .o
            .node_arena
            .add_node(EraNode::ListExpr(data), span, span))
    }

    /// Parses residual of expressions like `, var1, var2`.
    fn comma_expr_list_limit(&mut self, min_bp: u8, size_limit: u64) -> ParseResult<EraNodeRef> {
        assert!(size_limit > 0, "size limit must be greater than 0");
        let span = self.o.l.current_src_span();
        let cp = self.o.b.checkpoint();
        let mut count = 0;
        let comma_ended = loop {
            // Handle min_bp
            if min_bp != 0 {
                let token = self.o.peek_token(Mode::Normal).token;
                if let Some((l_bp, _)) = infix_binding_power(token.kind) {
                    if l_bp < min_bp {
                        break true;
                    }
                }
            }

            let token = self.o.peek_token(Mode::Normal).token;
            match token.kind {
                Token::Comma => {
                    // Empty expression
                    self.o.b.push_child(self.o.node_arena.add_node(
                        EraNode::Empty,
                        token.span,
                        token.span,
                    ));
                    _ = self.o.bump();

                    // Check limit
                    count += 1;
                    if count >= size_limit {
                        // Too many expressions, exit early
                        break true;
                    }

                    continue;
                }
                Token::LineBreak => break true,
                _ => (),
            }

            let terminals = Terminal::Comma | Terminal::LineBreak;
            let node = self.or_sync_to(|s| s.expression_bp(min_bp, true, terminals), terminals);
            self.o.b.push_child(node);

            if min_bp != 0 {
                let token = self.o.peek_token(Mode::Normal).token;
                if let Some((l_bp, _)) = infix_binding_power(token.kind) {
                    if l_bp < min_bp {
                        break false;
                    }
                }
            }
            if self.o.peek_token(Mode::Normal).token.kind == Token::LineBreak
                || self
                    .eat_sync(Mode::Normal, Token::Comma, terminals)
                    .is_none()
            {
                // No comma, must be end of expression list
                break false;
            }
        };

        if count < size_limit && comma_ended {
            // We want more expressions, and the last one was a comma
            let span = self.o.l.current_src_span();
            self.o
                .b
                .push_child(self.o.node_arena.add_node(EraNode::Empty, span, span));
            count += 1;
        }

        let data = self
            .o
            .node_arena
            .make_extra_data_ref_with_len(self.o.b.finish_node(cp).map(|x| x.0));
        let span = SrcSpan::new_covering(span, self.o.l.current_src_span());
        Ok(self
            .o
            .node_arena
            .add_node(EraNode::ListExpr(data), span, span))
    }

    /// Parses residual of expressions like `, var1, var2`.
    fn comma_expr_list(&mut self, min_bp: u8) -> ParseResult<EraNodeRef> {
        self.comma_expr_list_limit(min_bp, u64::MAX)
    }

    fn empty_comma_expr_list(&mut self) -> EraNodeRef {
        let span = self.o.l.current_src_span();
        let cp = self.o.b.checkpoint();
        let data = self
            .o
            .node_arena
            .make_extra_data_ref_with_len(self.o.b.finish_node(cp).map(|x| x.0));
        let span = SrcSpan::new_covering(span, self.o.l.current_src_span());
        self.o
            .node_arena
            .add_node(EraNode::ListExpr(data), span, span)
    }

    fn expression(&mut self, pure: bool) -> ParseResult<EraNodeRef> {
        self.expression_bp(0, pure, Terminal::LineBreak.into())
    }

    /// Parses an expression with given binding power.
    ///
    /// `pure` is used to determine whether to allow raw string literals in assignments
    /// (i.e. whether we are parsing a statement or an expression).
    fn expression_bp(
        &mut self,
        min_bp: u8,
        pure: bool,
        terminals: enumset::EnumSet<EraTerminalTokenKind>,
    ) -> ParseResult<EraNodeRef> {
        // NOTE: Skipping whitespace may affect generation of macro mappings. Use with caution.
        self.o.l.skip_whitespace(Mode::Normal);

        // HACK: Support special break_at symbols
        let lexer_mode = if terminals.contains(Terminal::Percentage) {
            // HACK: Do not parse `%value%=2` as `%value` and `%=2`
            Mode::InlineNormal
        } else {
            Mode::Normal
        };
        let span = self.o.l.current_src_span();
        let first = self.o.peek_token(lexer_mode);
        let (first, first_lexeme) = (first.token, first.lexeme);
        // Read lhs
        let mut lhs = match first.kind {
            Token::IntLiteral => self.integer_literal().unwrap(),
            // TODO: Remove the branch `StringLiteral` since it's never produced by the lexer
            Token::StringLiteral => self.plain_string_literal().unwrap(),
            Token::StringFormStart => self.expression_strform().unwrap(),
            Token::DoubleQuote => self.plain_string_literal().unwrap(),
            Token::SingleQuote => self.quote_raw_strform().unwrap(),
            Token::TernaryStrFormMarker => self.ternary_strform().unwrap(),
            Token::LParen => {
                _ = self.o.bump();
                let terminals = terminals | Terminal::RParen;
                let node = self.or_sync_to(|s| s.expression(true), terminals);
                _ = self.eat_sync(lexer_mode, Token::RParen, terminals);
                let span = self.span_to_now(span);
                self.o
                    .node_arena
                    .add_node(EraNode::ExprParen(node), span, span)
            }
            Token::Identifier => self.identifier().unwrap(),
            _ => {
                // Handle prefix
                if let Some(((), r_bp)) = prefix_binding_power(first.kind) {
                    _ = self.o.bump();
                    let node =
                        self.or_sync_to(|s| s.expression_bp(r_bp, pure, terminals), terminals);
                    let span = self.span_to_now(span);
                    self.o
                        .node_arena
                        .add_node(EraNode::ExprPreUnary(first.kind, node), span, span)
                } else {
                    let mut diag = self.base_diag.clone();
                    diag.span_err(
                        Default::default(),
                        first.span,
                        format!("unexpected token in expression: {:?}", first.kind),
                    );
                    self.o.emit_diag(diag);
                    return Err(());
                }
            }
        };

        // Got lhs, now try to read the rest
        loop {
            let peek_mode = lexer_mode;
            let token = self.o.peek_token(peek_mode).token;

            // HACK: Break at token
            if let Ok(token) = token.kind.try_into() {
                if terminals.contains(token) {
                    break;
                }
            }

            // Handle postfix
            if let Some((l_bp, ())) = postfix_binding_power(token.kind) {
                if l_bp < min_bp {
                    break;
                }

                lhs = match token.kind {
                    Token::LParen => {
                        let args_list = self.paren_expr_list().unwrap();
                        let span = self.span_to_now(span);
                        self.o
                            .node_arena
                            .add_node(EraNode::ExprFunCall(lhs, args_list), span, span)
                    }
                    _ => {
                        _ = self.o.bump();
                        let span = self.span_to_now(span);
                        self.o.node_arena.add_node(
                            EraNode::ExprPostUnary(lhs, token.kind),
                            span,
                            span,
                        )
                    }
                };

                continue;
            }

            // Handle infix
            if let Some((l_bp, r_bp)) = infix_binding_power(token.kind) {
                if l_bp < min_bp {
                    break;
                }

                lhs = match token.kind {
                    Token::QuestionMark => {
                        _ = self.o.bump();
                        let then_expr = self
                            .or_sync_to(|s| s.expression(true), terminals | Terminal::NumberSign);
                        _ = self.eat_sync(
                            lexer_mode,
                            Token::NumberSign,
                            terminals | Terminal::NumberSign,
                        );
                        let else_expr = self.expression_bp(r_bp, pure, terminals)?;
                        let span = self.span_to_now(span);
                        let (extra_data, _) =
                            self.o.node_arena.extend_extra([then_expr.0, else_expr.0]);
                        self.o.node_arena.add_node(
                            EraNode::ExprTernary(lhs, extra_data),
                            span,
                            span,
                        )
                    }
                    Token::Colon => {
                        _ = self.o.bump();
                        let cp = self.o.b.checkpoint();
                        self.o.b.push_child(lhs);
                        loop {
                            let node = self
                                .or_sync_to(|s| s.expression_bp(r_bp, pure, terminals), terminals);
                            self.o.b.push_child(node);
                            if self.try_eat(Mode::Normal, Token::Colon).is_none() {
                                break;
                            }
                        }
                        let data = self
                            .o
                            .node_arena
                            .make_small_extra_data(self.o.b.finish_node(cp).map(|x| x.0));
                        let span = self.span_to_now(span);
                        self.o
                            .node_arena
                            .add_node(EraNode::ExprVarIdx(data), span, span)
                    }
                    Token::Assign => {
                        _ = self.o.bump();
                        let mut is_str_assign_mode = false;
                        if !pure {
                            // Check whether lhs is string variable
                            let lhs = if let EraNode::ExprVarIdx(extra_data) =
                                self.o.node_arena.get_node(lhs)
                            {
                                let children =
                                    self.o.node_arena.get_small_extra_data_view(extra_data);
                                EraNodeRef(children[0])
                            } else {
                                lhs
                            };
                            if let EraNode::Identifier(name) = self.o.node_arena.get_node(lhs) {
                                let name = self.o.b.interner().resolve(name);
                                if self.is_var_str(name) {
                                    is_str_assign_mode = true;
                                }
                            }
                        }
                        let rhs = if is_str_assign_mode {
                            self.raw_strform()
                        } else {
                            self.or_sync_to(|s| s.expression_bp(r_bp, pure, terminals), terminals)
                        };
                        let span = self.span_to_now(span);
                        self.o.node_arena.add_node(
                            EraNode::ExprBinary(lhs, token.kind.into(), rhs),
                            span,
                            span,
                        )
                    }
                    Token::At => {
                        _ = self.o.bump();
                        let rhs =
                            self.or_sync_to(|s| s.expression_bp(r_bp, pure, terminals), terminals);
                        let span = self.span_to_now(span);
                        self.o
                            .node_arena
                            .add_node(EraNode::ExprVarNamespace(lhs, rhs), span, span)
                    }
                    _ => {
                        _ = self.o.bump();
                        let rhs =
                            self.or_sync_to(|s| s.expression_bp(r_bp, pure, terminals), terminals);
                        let span = self.span_to_now(span);
                        self.o.node_arena.add_node(
                            EraNode::ExprBinary(lhs, token.kind.into(), rhs),
                            span,
                            span,
                        )
                    }
                };

                continue;
            }

            break;
        }

        Ok(lhs)
    }

    fn raw_string(&mut self) -> EraNodeRef {
        let span = self.o.l.current_src_span();
        let mut buf = String::new();
        loop {
            let result = self.o.peek_token(Mode::RawStr);
            let token = result.token;
            if token.kind == Token::PlainStringLiteral {
                routines::unescape_to_sink(result.lexeme, &mut buf);
                _ = self.o.bump();
                continue;
            }
            match token.kind {
                _ => break,
            }
        }

        let token_key = self.o.b.interner().get_or_intern(&buf);
        let span = self.span_to_now(span);
        self.o
            .node_arena
            .add_node(EraNode::LiteralStr(token_key), span, span)
    }

    fn raw_strform(&mut self) -> EraNodeRef {
        let mut buf = String::new();
        let cp = self.o.b.checkpoint();
        let span = self.o.l.current_src_span();
        let mut literal_span = Default::default();
        loop {
            let result = self.o.peek_token(Mode::RawStrForm);
            let token = result.token;
            // Try concatenating plain string literals
            if token.kind == Token::PlainStringLiteral {
                if buf.is_empty() {
                    literal_span = token.span;
                }
                routines::unescape_to_sink(result.lexeme, &mut buf);
                _ = self.o.bump();

                continue;
            }
            // Finish current string literal, prepare for expressions
            if !buf.is_empty() {
                let token_key = self.o.b.interner().get_or_intern(&buf);
                let span = self.span_to_now(literal_span);
                let node = self
                    .o
                    .node_arena
                    .add_node(EraNode::LiteralStr(token_key), span, span);
                self.o.b.push_child(node);
                buf.clear();
            }
            match token.kind {
                Token::LBrace => {
                    let span = token.span;
                    _ = self.o.bump();
                    let node = self.strform_interp_part_rest(Terminal::RBrace, span);
                    self.o.b.push_child(node);
                }
                Token::Percentage => {
                    let span = token.span;
                    _ = self.o.bump();
                    let node = self.strform_interp_part_rest(Terminal::Percentage, span);
                    self.o.b.push_child(node);
                }
                Token::TernaryStrFormMarker => {
                    let node = self.ternary_strform().unwrap();
                    self.o.b.push_child(node);
                }
                _ => break,
            }
        }

        _ = self.expect_sync_to_newline();

        let data = self
            .o
            .node_arena
            .make_small_extra_data(self.o.b.finish_node(cp).map(|x| x.0));
        let span = self.span_to_now(span);
        self.o
            .node_arena
            .add_node(EraNode::StringForm(data), span, span)
    }

    fn expression_strform(&mut self) -> ParseResult<EraNodeRef> {
        let span = self.eat(Mode::Normal, Token::StringFormStart)?.token.span;
        self.quoted_strform_rest(Mode::StrForm, span)
    }

    fn plain_expression_strform(&mut self) -> ParseResult<EraNodeRef> {
        let span = self.eat(Mode::Normal, Token::DoubleQuote)?.token.span;
        self.quoted_strform_rest(Mode::PlainStr, span)
    }

    /// Parses residual of quoted string form expressions (i.e. @"1{1+1}3").
    fn quoted_strform_rest(&mut self, mode: Mode, span: SrcSpan) -> ParseResult<EraNodeRef> {
        let mut buf = String::new();
        let cp = self.o.b.checkpoint();
        let mut literal_span = Default::default();
        loop {
            let result = self.o.peek_token(mode);
            let token = result.token;
            // Try concatenating plain string literals
            if token.kind == Token::PlainStringLiteral {
                if buf.is_empty() {
                    literal_span = token.span;
                }
                routines::unescape_to_sink(result.lexeme, &mut buf);
                _ = self.o.bump();

                continue;
            }
            // Finish current string literal, prepare for expressions
            if !buf.is_empty() {
                let token_key = self.o.b.interner().get_or_intern(&buf);
                let span = self.span_to_now(literal_span);
                let node = self
                    .o
                    .node_arena
                    .add_node(EraNode::LiteralStr(token_key), span, span);
                self.o.b.push_child(node);
                buf.clear();
            }
            match token.kind {
                Token::LBrace => {
                    let span = token.span;
                    _ = self.o.bump();
                    let node = self.strform_interp_part_rest(Terminal::RBrace, span);
                    self.o.b.push_child(node);
                }
                Token::Percentage => {
                    let span = token.span;
                    _ = self.o.bump();
                    let node = self.strform_interp_part_rest(Terminal::Percentage, span);
                    self.o.b.push_child(node);
                }
                Token::TernaryStrFormMarker => {
                    let node = self.ternary_strform().unwrap();
                    self.o.b.push_child(node);
                }
                _ => break,
            }
        }

        _ = self.eat_sync(
            mode,
            Token::DoubleQuote,
            Terminal::DoubleQuote | Terminal::LineBreak,
        );

        let data = self
            .o
            .node_arena
            .make_small_extra_data(self.o.b.finish_node(cp).map(|x| x.0));
        let span = self.span_to_now(span);
        Ok(self
            .o
            .node_arena
            .add_node(EraNode::StringForm(data), span, span))
    }

    fn strform_interp_part_rest(&mut self, terminal: Terminal, span: SrcSpan) -> EraNodeRef {
        let terminal_token: Token = terminal.into();
        let terminals = terminal | Terminal::Comma | Terminal::LineBreak;
        let mode = if terminal == Terminal::Percentage {
            Mode::InlineNormal
        } else {
            Mode::Normal
        };
        let cp = self.o.b.checkpoint();
        // expr
        let node = self.or_sync_to(|s| s.expression_bp(0, true, terminal.into()), terminals);
        self.o.b.push_child(node);
        // width
        if self.o.peek_token(mode).token.kind != terminal_token
            && self.eat_sync(mode, Token::Comma, terminals).is_some()
        {
            let node = self.or_sync_to(|s| s.expression_bp(0, true, terminal.into()), terminals);
            self.o.b.push_child(node);
        }
        // alignment
        if self.o.peek_token(mode).token.kind != terminal_token
            && self.eat_sync(mode, Token::Comma, terminals).is_some()
        {
            // self.eat(mode, Token::Identifier)?;
            let node = self.or_sync_to(|s| s.expression_bp(0, true, terminal.into()), terminals);
            self.o.b.push_child(node);
        }

        _ = self.eat_sync(mode, terminal_token, terminal | Terminal::LineBreak);

        let data = self
            .o
            .node_arena
            .make_small_extra_data(self.o.b.finish_node(cp).map(|x| x.0));
        let span = self.span_to_now(span);
        self.o
            .node_arena
            .add_node(EraNode::StringFormInterpPart(data), span, span)
    }

    fn ternary_strform(&mut self) -> ParseResult<EraNodeRef> {
        let initial_span = self.o.l.current_src_span();
        let mut buf = String::new();
        let mut literal_span = Default::default();

        self.eat(Mode::Normal, Token::TernaryStrFormMarker)?;

        // cond
        let terminals =
            Terminal::QuestionMark | Terminal::TernaryStrFormMarker | Terminal::LineBreak;
        let cond = self.or_sync_to(|s| s.expression_bp(0, true, terminals), terminals);
        _ = self.eat_sync(Mode::Normal, Token::QuestionMark, terminals);

        // then
        let cp = self.o.b.checkpoint();
        let span = self.o.l.current_src_span();
        buf.clear();
        loop {
            let result = self.o.peek_token(Mode::TernaryStrForm);
            let token = result.token;
            if token.kind == Token::PlainStringLiteral {
                if buf.is_empty() {
                    literal_span = token.span;
                }
                routines::unescape_to_sink(result.lexeme, &mut buf);
                _ = self.o.bump();
                continue;
            }
            if !buf.is_empty() {
                let token_key = self.o.b.interner().get_or_intern(&buf);
                let span = self.span_to_now(literal_span);
                let node = self
                    .o
                    .node_arena
                    .add_node(EraNode::LiteralStr(token_key), span, span);
                self.o.b.push_child(node);
                buf.clear();
            }
            match token.kind {
                Token::LBrace => {
                    let span = token.span;
                    _ = self.o.bump();
                    let node = self.strform_interp_part_rest(Terminal::RBrace, span);
                    self.o.b.push_child(node);
                }
                Token::Percentage => {
                    let span = token.span;
                    _ = self.o.bump();
                    let node = self.strform_interp_part_rest(Terminal::Percentage, span);
                    self.o.b.push_child(node);
                }
                // Token::NumberSign => {
                //     _ = self.o.bump();
                //     break;
                // }
                _ => break,
            }
        }

        let data = self
            .o
            .node_arena
            .make_small_extra_data(self.o.b.finish_node(cp).map(|x| x.0));
        let span = self.span_to_now(span);
        let then = self
            .o
            .node_arena
            .add_node(EraNode::StringForm(data), span, span);

        _ = self.eat_sync(
            Mode::TernaryStrForm,
            Token::NumberSign,
            Terminal::NumberSign | Terminal::TernaryStrFormMarker | Terminal::LineBreak,
        );

        // else
        let cp = self.o.b.checkpoint();
        let span = self.o.l.current_src_span();
        buf.clear();
        loop {
            let result = self.o.peek_token(Mode::RawStrForm);
            let token = result.token;
            if token.kind == Token::PlainStringLiteral {
                if buf.is_empty() {
                    literal_span = token.span;
                }
                routines::unescape_to_sink(result.lexeme, &mut buf);
                _ = self.o.bump();
                continue;
            }
            if !buf.is_empty() {
                let token_key = self.o.b.interner().get_or_intern(&buf);
                let span = self.span_to_now(literal_span);
                let node = self
                    .o
                    .node_arena
                    .add_node(EraNode::LiteralStr(token_key), span, span);
                self.o.b.push_child(node);
                buf.clear();
            }
            match token.kind {
                Token::LBrace => {
                    let span = token.span;
                    _ = self.o.bump();
                    let node = self.strform_interp_part_rest(Terminal::RBrace, span);
                    self.o.b.push_child(node);
                }
                Token::Percentage => {
                    let span = token.span;
                    _ = self.o.bump();
                    let node = self.strform_interp_part_rest(Terminal::Percentage, span);
                    self.o.b.push_child(node);
                }
                _ => break,
            }
        }

        let data = self
            .o
            .node_arena
            .make_small_extra_data(self.o.b.finish_node(cp).map(|x| x.0));
        let span = self.span_to_now(span);
        let else_ = self
            .o
            .node_arena
            .add_node(EraNode::StringForm(data), span, span);

        _ = self.eat_sync(
            Mode::RawStrForm,
            Token::TernaryStrFormMarker,
            Terminal::TernaryStrFormMarker | Terminal::LineBreak,
        );

        let (extra_data, _) = self.o.node_arena.extend_extra([then.0, else_.0]);
        let span = self.span_to_now(initial_span);
        Ok(self
            .o
            .node_arena
            .add_node(EraNode::ExprTernary(cond, extra_data), span, span))
    }

    fn statement(&mut self) -> ParseResult<EraNodeRef> {
        use EraCmdArgFmt as CmdArg;

        self.o.l.skip_whitespace(Mode::Normal);

        let span = self.o.l.current_src_span();
        let token = self.o.peek_token(Mode::Normal);
        let (token, lexeme) = (token.token, token.lexeme);

        match token.kind {
            // Hold on; identifiers may be commands or expressions
            Token::Identifier => (),
            Token::Dollar => {
                _ = self.o.bump();
                let label = self.identifier()?;
                let span = self.span_to_now(span);
                let node = self
                    .o
                    .node_arena
                    .add_node(EraNode::StmtLabel(label), span, span);
                return Ok(node);
            }
            Token::At => {
                // TODO: Add suggestion for the most recent open block
                let mut diag = self.base_diag.clone();
                diag.span_err(
                    Default::default(),
                    token.span,
                    "unexpected start of function; did you forget to close a block?",
                );
                self.o.emit_diag(diag);
                return Err(());
            }
            Token::Eof => {
                // TODO: Add suggestion for the most recent open block
                let mut diag = self.base_diag.clone();
                diag.span_err(
                    Default::default(),
                    token.span,
                    "unexpected end of file; did you forget to close a block?",
                );
                self.o.emit_diag(diag);
                return Err(());
            }
            _ => {
                return Ok(self.stmt_expression());
            }
        }

        trait Adhoc {
            fn r(&mut self) -> &mut Self;
        }
        impl Adhoc for EraParserSite<'_, '_, '_> {
            fn r(&mut self) -> &mut Self {
                _ = self.o.bump();
                self
            }
        }

        macro_rules! make {
            // ($node:expr) => {{
            //     _ = self.o.bump();
            //     let node = $node;
            //     let span = self.span_to_now(span);
            //     self.o.node_arena.add_node(node, span, span)
            // }};
            // ($node:expr, $method:ident($($method_args:expr),*)) => {{
            //     _ = self.o.bump();
            //     let node = $node($method($($method_args),*));
            //     let span = self.span_to_now(span);
            //     self.o.node_arena.add_node(node, span, span)
            // }};
            ($node:expr) => {{
                _ = self.o.bump();
                let node = $node;
                let span = self.span_to_now(span);
                self.o.node_arena.add_node(node, span, span)
            }};
        }

        // NOTE: We assume that built-in commands are not too long. If that's not the case,
        //       remember to revisit and modify the `MAX_BUF_LEN` below.
        const MAX_BUF_LEN: usize = 32;
        let Some(cmd) = crate::util::inline_to_ascii_uppercase::<MAX_BUF_LEN>(lexeme.as_bytes())
        else {
            return Ok(self.stmt_expression());
        };
        let cmd = &cmd[..];
        let node_ref = if let Some((arg_fmt, flags)) = routines::recognize_print_cmd(cmd) {
            _ = self.o.bump();
            let args_list = self.command_arg(arg_fmt);
            let node = EraNode::StmtPrint(flags, args_list);
            let span = self.span_to_now(span);
            self.o.node_arena.add_node(node, span, span)
        } else if let Some((arg_fmt, flags)) = routines::recognize_debugprint_cmd(cmd) {
            make!(EraNode::StmtDebugPrint(flags, self.command_arg(arg_fmt)))
        } else if let Some(flags) = routines::recognize_printdata_cmd(cmd) {
            self.r().stmt_printdata(span, flags)
        } else {
            match cmd {
                b"STRDATA" => self.r().stmt_strdata(span),
                b"IF" => self.r().stmt_if(span),
                b"SIF" => self.r().stmt_sif(span),
                b"SELECTCASE" => self.r().stmt_selectcase(span),
                b"WHILE" => self.r().stmt_while(span),
                b"REPEAT" => self.r().stmt_repeat(span),
                b"FOR" => self.r().stmt_for(span),
                b"DO" => self.r().stmt_do_loop(span),
                b"CALL" | b"CALLF" => make!(self.stmt_call_node(false, EraNode::StmtCall)),
                b"CALLFORM" | b"CALLFORMF" => make!(self.stmt_call_node(true, EraNode::StmtCall)),
                b"TRYCALL" | b"TRYCALLF" => make!(self.stmt_call_node(false, EraNode::StmtTryCall)),
                b"TRYCALLFORM" | b"TRYCALLFORMF" => {
                    make!(self.stmt_call_node(true, EraNode::StmtTryCall))
                }
                b"TRYCCALL" | b"TRYCCALLF" => {
                    make!(self.stmt_tryccall_node(false, EraNode::StmtTryCCall))
                }
                b"TRYCCALLFORM" | b"TRYCCALLFORMF" => {
                    make!(self.stmt_tryccall_node(true, EraNode::StmtTryCCall))
                }
                b"JUMP" => make!(self.stmt_call_node(false, EraNode::StmtJump)),
                b"JUMPFORM" => make!(self.stmt_call_node(true, EraNode::StmtJump)),
                b"TRYJUMP" => make!(self.stmt_call_node(false, EraNode::StmtTryJump)),
                b"TRYJUMPFORM" => make!(self.stmt_call_node(true, EraNode::StmtTryJump)),
                b"TRYCJUMP" => make!(self.stmt_tryccall_node(false, EraNode::StmtTryCJump)),
                b"TRYCJUMPFORM" => make!(self.stmt_tryccall_node(true, EraNode::StmtTryCJump)),
                b"TIMES" => make!(self.stmt_times()),
                b"NOP" => make!(EraNode::StmtNop),
                b"CONTINUE" => make!(EraNode::StmtContinue),
                b"BREAK" => make!(EraNode::StmtBreak),
                b"RESTART" => make!(EraNode::StmtRestart),
                b"RETURN" | b"RETURNF" => {
                    make!(EraNode::StmtReturn(self.command_arg(CmdArg::Expression)))
                }
                b"THROW" => make!(EraNode::StmtThrow(self.command_arg(CmdArg::RawStringForm))),
                b"GOTO" => make!(EraNode::StmtGoto(self.command_arg(CmdArg::Expression))),
                b"QUIT" => make!(EraNode::StmtQuit),
                b"WAIT" => make!(EraNode::StmtWait),
                b"FORCEWAIT" => make!(EraNode::StmtForceWait),
                b"WAITANYKEY" => make!(EraNode::StmtWaitAnyKey),
                // b"GCREATE" => make!(EraNode::StmtGCreate(self.command_arg(CmdArg::Expression))),
                // b"GDISPOSE" => make!(EraNode::StmtGDispose(self.command_arg(CmdArg::Expression))),
                // b"GDRAWSPRITE" => make!(EraNode::StmtGDrawSprite(self.command_arg(CmdArg::Expression))),
                b"SPLIT" => make!(EraNode::StmtSplit(self.cmd_arg_limit(3, 4))),
                b"SETBIT" => make!(EraNode::StmtSetBit(self.command_arg(CmdArg::Expression))),
                b"CLEARBIT" => make!(EraNode::StmtClearBit(self.command_arg(CmdArg::Expression))),
                b"INVERTBIT" => make!(EraNode::StmtInvertBit(self.command_arg(CmdArg::Expression))),
                b"SETCOLOR" => make!(EraNode::StmtSetColor(self.command_arg(CmdArg::Expression))),
                b"RESETCOLOR" => make!(EraNode::StmtResetColor),
                b"SETBGCOLOR" => make!(EraNode::StmtSetBgColor(
                    self.command_arg(CmdArg::Expression)
                )),
                b"RESETBGCOLOR" => make!(EraNode::StmtResetBgColor),
                b"VARSET" => make!(EraNode::StmtVarSet(self.cmd_arg_limit(1, 4))),
                b"CVARSET" => make!(EraNode::StmtCVarSet(self.cmd_arg_limit(1, 5))),
                b"VARSIZE" => make!(EraNode::StmtVarSize(self.cmd_arg_limit(1, 1))),
                b"SWAP" => make!(EraNode::StmtSwap(self.cmd_arg_limit(2, 2))),
                b"HTML_PRINT" => make!(EraNode::StmtHtmlPrint(self.cmd_arg_limit(1, 1))),
                b"PRINTBUTTON" => make!(EraNode::StmtPrintButton(self.cmd_arg_limit(2, 2))),
                b"PRINTBUTTONC" => make!(EraNode::StmtPrintButtonC(self.cmd_arg_limit(2, 2))),
                b"PRINTBUTTONLC" => make!(EraNode::StmtPrintButtonLC(self.cmd_arg_limit(2, 2))),
                b"ARRAYREMOVE" => make!(EraNode::StmtArrayRemove(self.cmd_arg_limit(3, 3))),
                b"ARRAYSORT" => make!(EraNode::StmtArraySort(self.cmd_arg_limit(1, 4))),
                b"ARRAYMSORT" => make!(EraNode::StmtArrayMSort(
                    self.command_arg(CmdArg::Expression)
                )),
                b"ARRAYCOPY" => make!(EraNode::StmtArrayCopy(self.cmd_arg_limit(2, 2))),
                b"ARRAYSHIFT" => make!(EraNode::StmtArrayShift(self.cmd_arg_limit(3, 5))),
                b"INPUT" => make!(EraNode::StmtInput(self.cmd_arg_limit(0, 3))),
                b"INPUTS" => make!(EraNode::StmtInputS(self.cmd_arg_limit(0, 3))),
                b"TINPUT" => make!(EraNode::StmtTInput(self.cmd_arg_limit(2, 5))),
                b"TINPUTS" => make!(EraNode::StmtTInputS(self.cmd_arg_limit(2, 5))),
                b"ONEINPUT" => make!(EraNode::StmtOneInput(self.cmd_arg_limit(0, 1))),
                b"ONEINPUTS" => make!(EraNode::StmtOneInputS(self.cmd_arg_limit(0, 1))),
                b"TONEINPUT" => make!(EraNode::StmtTOneInput(self.cmd_arg_limit(2, 5))),
                b"TONEINPUTS" => make!(EraNode::StmtTOneInputS(self.cmd_arg_limit(2, 5))),
                b"REUSELASTLINE" => {
                    make!(EraNode::StmtReuseLastLine(
                        self.command_arg(CmdArg::RawStringForm)
                    ))
                }
                b"CLEARLINE" => make!(EraNode::StmtClearLine(self.cmd_arg_limit(1, 1))),
                b"DRAWLINE" => make!(EraNode::StmtDrawLine),
                b"CUSTOMDRAWLINE" => {
                    make!(EraNode::StmtCustomDrawLine(
                        self.command_arg(CmdArg::RawString)
                    ))
                }
                b"DRAWLINEFORM" => make!(EraNode::StmtCustomDrawLine(
                    self.command_arg(CmdArg::RawStringForm)
                )),
                b"TWAIT" => make!(EraNode::StmtTWait(self.cmd_arg_limit(2, 2))),
                b"FONTSTYLE" => make!(EraNode::StmtFontStyle(self.cmd_arg_limit(1, 1))),
                b"FONTBOLD" => make!(EraNode::StmtFontBold),
                b"FONTITALIC" => make!(EraNode::StmtFontItalic),
                b"FONTREGULAR" => make!(EraNode::StmtFontRegular),
                b"SETFONT" => make!(EraNode::StmtSetFont(self.cmd_arg_limit(1, 1))),
                b"PUTFORM" => make!(EraNode::StmtPutForm(
                    self.command_arg(CmdArg::RawStringForm)
                )),
                b"SKIPDISP" => make!(EraNode::StmtSkipDisp(self.cmd_arg_limit(1, 1))),
                b"BEGIN" => make!(EraNode::StmtBegin(self.cmd_arg_limit(1, 1))),
                b"DOTRAIN" => make!(EraNode::StmtDoTrain(self.cmd_arg_limit(1, 1))),
                b"REDRAW" => make!(EraNode::StmtRedraw(self.cmd_arg_limit(1, 1))),
                b"STRLEN" => make!(EraNode::StmtStrLen(self.command_arg(CmdArg::RawString))),
                b"STRLENFORM" => {
                    make!(EraNode::StmtStrLen(self.command_arg(CmdArg::RawStringForm)))
                }
                b"STRLENU" => make!(EraNode::StmtStrLenU(self.command_arg(CmdArg::RawString))),
                b"STRLENFORMU" => make!(EraNode::StmtStrLenU(
                    self.command_arg(CmdArg::RawStringForm)
                )),
                b"ALIGNMENT" => make!(EraNode::StmtAlignment(self.cmd_arg_limit(1, 1))),
                b"TOOLTIP_SETDELAY" => {
                    make!(EraNode::StmtToolTipSetDelay(self.cmd_arg_limit(1, 1)))
                }
                b"TOOLTIP_SETDURATION" => {
                    make!(EraNode::StmtToolTipSetDuration(self.cmd_arg_limit(1, 1)))
                }
                b"RANDOMIZE" => make!(EraNode::StmtRandomize(self.cmd_arg_limit(1, 1))),
                b"DUMPRAND" => make!(EraNode::StmtDumpRand),
                b"INITRAND" => make!(EraNode::StmtInitRand),
                b"BAR" => make!(EraNode::StmtBar(self.cmd_arg_limit(3, 3))),
                b"BARL" => make!(EraNode::StmtBarL(self.cmd_arg_limit(3, 3))),
                b"ADDCHARA" => make!(EraNode::StmtAddChara(self.command_arg(CmdArg::Expression))),
                b"PICKUPCHARA" => {
                    make!(EraNode::StmtPickUpChara(
                        self.command_arg(CmdArg::Expression)
                    ))
                }
                b"DELCHARA" => make!(EraNode::StmtDelChara(self.command_arg(CmdArg::Expression))),
                b"SWAPCHARA" => make!(EraNode::StmtSwapChara(self.cmd_arg_limit(2, 2))),
                b"ADDCOPYCHARA" => make!(EraNode::StmtAddCopyChara(self.cmd_arg_limit(1, 1))),
                b"RESET_STAIN" => make!(EraNode::StmtResetStain(self.cmd_arg_limit(1, 1))),
                b"SAVECHARA" => make!(EraNode::StmtSaveChara(self.cmd_arg_limit(3, u64::MAX))),
                b"LOADCHARA" => make!(EraNode::StmtLoadChara(self.cmd_arg_limit(1, 1))),
                b"SETANIMETIMER" => {
                    make!(EraNode::StmtSetAnimeTimer(self.cmd_arg_limit(1, 1)))
                }
                b"HTML_TAGSPLIT" => make!(EraNode::StmtHtmlTagSplit(self.cmd_arg_limit(1, 3))),
                b"POWER" => make!(EraNode::StmtPower(self.cmd_arg_limit(3, 3))),
                b"LOADDATA" => make!(EraNode::StmtLoadData(self.cmd_arg_limit(1, 1))),
                b"SAVEDATA" => make!(EraNode::StmtSaveData(self.cmd_arg_limit(2, 2))),
                // b"CHKDATA" => Cmd::CheckData(self.r().stmt_chkdata()?),
                b"GETTIME" => make!(EraNode::StmtGetTime),
                b"LOADGLOBAL" => make!(EraNode::StmtLoadGlobal),
                b"SAVEGLOBAL" => make!(EraNode::StmtSaveGlobal),
                b"LOADGAME" => make!(EraNode::StmtLoadGame),
                b"SAVEGAME" => make!(EraNode::StmtSaveGame),
                b"DEBUGCLEAR" => make!(EraNode::StmtDebugClear),
                b"RESETDATA" => make!(EraNode::StmtResetData),
                b"GCREATE"
                | b"GCREATEFROMFILE"
                | b"GDISPOSE"
                | b"GCREATED"
                | b"GDRAWSPRITE"
                | b"GCLEAR"
                | b"SPRITECREATE"
                | b"SPRITEDISPOSE"
                | b"SPRITECREATED"
                | b"SPRITEANIMECREATE"
                | b"SPRITEANIMEADDFRAME"
                | b"SPRITEWIDTH"
                | b"SPRITEHEIGHT"
                | b"GETBIT"
                | b"GETSTYLE"
                | b"CHKFONT"
                | b"GETFONT"
                | b"REPLACE"
                | b"SUBSTRING"
                | b"SUBSTRINGU"
                | b"STRFIND"
                | b"STRFINDU"
                | b"STRLENS"
                | b"STRLENSU"
                | b"STRCOUNT"
                | b"CHARATU"
                | b"CURRENTREDRAW"
                | b"CURRENTALIGN"
                | b"MAX"
                | b"MIN"
                | b"LIMIT"
                | b"INRANGE"
                | b"CHKCHARADATA"
                | b"SAVETEXT"
                | b"LOADTEXT"
                | b"FINDELEMENT"
                | b"FINDLASTELEMENT"
                | b"FINDCHARA"
                | b"FINDLASTCHARA"
                | b"GETCOLOR"
                | b"GETBGCOLOR"
                | b"GETDEFCOLOR"
                | b"GETDEFBGCOLOR"
                | b"GETFOCUSCOLOR"
                | b"GETNUM"
                | b"GROUPMATCH"
                | b"NOSAMES"
                | b"ALLSAMES"
                | b"GETMILLISECOND"
                | b"GETSECOND"
                | b"CSVNAME"
                | b"CSVCALLNAME"
                | b"CSVNICKNAME"
                | b"CSVMASTERNAME"
                | b"CSVBASE"
                | b"CSVCSTR"
                | b"CSVABL"
                | b"CSVTALENT"
                | b"CSVMARK"
                | b"CSVEXP"
                | b"CSVRELATION"
                | b"CSVJUEL"
                | b"CSVEQUIP"
                | b"CSVCFLAG"
                | b"SQRT"
                | b"CBRT"
                | b"LOG"
                | b"LOG10"
                | b"EXPONENT"
                | b"ABS"
                | b"SIGN"
                | b"TOSTR"
                | b"TOINT"
                | b"ISNUMERIC"
                | b"SUMARRAY"
                | b"SUMCARRAY"
                | b"MATCH"
                | b"CMATCH"
                | b"MAXARRAY"
                | b"MAXCARRAY"
                | b"MINARRAY"
                | b"MINCARRAY"
                | b"INRANGEARRAY"
                | b"INRANGECARRAY"
                | b"TOUPPER"
                | b"TOLOWER"
                | b"TOHALF"
                | b"TOFULL"
                | b"EXISTCSV"
                | b"MONEYSTR"
                | b"GETPALAMLV"
                | b"GETEXPLV"
                | b"GETCHARA"
                | b"ESCAPE"
                | b"LINEISEMPTY"
                | b"GETCONFIG"
                | b"GETCONFIGS"
                | b"ISSKIP"
                | b"MOUSESKIP"
                | b"MESSKIP"
                | b"CONVERT"
                | b"PRINTCPERLINE"
                | b"PRINTCLENGTH"
                | b"COLOR_FROMNAME"
                | b"COLOR_FROMRGB"
                | b"HTML_TOPLAINTEXT"
                | b"HTML_ESCAPE"
                | b"GETKEY"
                | b"GETKEYTRIGGERED"
                | b"FIND_CHARADATA"
                | b"CHKDATA" => {
                    // NOTE: Cannot use `make!` because we cannot bump the token here.
                    let node = EraNode::StmtResultCmdCall(
                        self.identifier().unwrap(),
                        self.command_arg(CmdArg::Expression),
                    );
                    let span = self.span_to_now(span);
                    self.o.node_arena.add_node(node, span, span)
                }
                _ => self.stmt_expression(),
            }
        };

        Ok(node_ref)
    }

    /// Generates a node of type `ExprList` containing the arguments of a command.
    fn command_arg(&mut self, arg_fmt: EraCmdArgFmt) -> EraNodeRef {
        match arg_fmt {
            EraCmdArgFmt::Expression
            | EraCmdArgFmt::ExpressionS
            | EraCmdArgFmt::ExpressionSForm => self.comma_expr_list(0).unwrap(),
            EraCmdArgFmt::RawStringForm => self.raw_strform(),
            EraCmdArgFmt::RawString => self.raw_string(),
        }
    }

    fn cmd_arg_limit(&mut self, lower_limit: u64, upper_limit: u64) -> EraNodeRef {
        // NOTE: arg limits are intentionally ignored for now
        self.comma_expr_list(0).unwrap()
    }

    fn stmt_expression(&mut self) -> EraNodeRef {
        let span = self.o.l.current_src_span();
        let terminals = Terminal::LineBreak.into();
        let expr = self.or_sync_to(|s| s.expression_bp(0, false, terminals), terminals);
        // Handle special case: row assignment (e.g. `v = 1, 2, 3`)
        let mut handle_row_assign = false;
        if self.o.peek_token(Mode::Normal).token.kind == Token::Comma {
            if let EraNode::ExprBinary(_, op, _) = self.o.node_arena.get_node(expr) {
                let op: Token = op.into();
                if matches!(op, Token::Assign | Token::ExprAssign) {
                    handle_row_assign = true;
                }
            }
        }

        if handle_row_assign {
            _ = self.o.bump();
            let rhs_list = self.comma_expr_list(0).unwrap();
            let span = self.span_to_now(span);
            self.o
                .node_arena
                .add_node(EraNode::StmtRowAssign(expr, rhs_list), span, span)
        } else {
            let span = self.span_to_now(span);
            self.o
                .node_arena
                .add_node(EraNode::StmtExpr(expr), span, span)
        }
    }

    /// Parses rest of `PRINTDATA` command.
    fn stmt_printdata(&mut self, span: SrcSpan, flags: EraPrintExtendedFlags) -> EraNodeRef {
        let terminals = Terminal::LineBreak.into();
        let dest = if self.try_eat(Mode::Normal, Token::LineBreak).is_none() {
            // var expression
            self.or_sync_to(|s| s.expression_bp(0, true, terminals), terminals)
        } else {
            self.make_empty()
        };

        _ = self.expect_sync_to_newline();

        self.skip_newline();
        let data = self.stmt_printdata_data();
        let span = self.span_to_now(span);
        self.o
            .node_arena
            .add_node(EraNode::StmtPrintData(flags.into(), dest, data), span, span)
    }

    fn stmt_strdata(&mut self, span: SrcSpan) -> EraNodeRef {
        let terminals = Terminal::LineBreak.into();
        let dest = if self.try_eat(Mode::Normal, Token::LineBreak).is_none() {
            // var expression
            self.or_sync_to(|s| s.expression_bp(0, true, terminals), terminals)
        } else {
            self.make_empty()
        };

        _ = self.expect_sync_to_newline();

        self.skip_newline();
        let data = self.stmt_printdata_data();
        let span = self.span_to_now(span);
        self.o
            .node_arena
            .add_node(EraNode::StmtStrData(dest, data), span, span)
    }

    fn stmt_printdata_data(&mut self) -> EraExtraDataRef {
        let cp = self.o.b.checkpoint();

        loop {
            if !self.expect_not_function_end() {
                break;
            }

            let child = if self.try_match_command("ENDDATA") {
                _ = self.o.bump();
                break;
            } else if self.try_match_command("DATA") {
                _ = self.o.bump();
                self.raw_string()
            } else if self.try_match_command("DATAFORM") {
                _ = self.o.bump();
                self.raw_strform()
            } else if self.try_match_command("DATALIST") {
                let span = self.o.l.current_src_span();
                _ = self.o.bump();
                _ = self.expect_sync_to_newline();
                self.skip_newline();
                let cp = self.o.b.checkpoint();
                loop {
                    if !self.expect_not_function_end() {
                        break;
                    }

                    let child = if self.try_match_command("ENDLIST") {
                        _ = self.o.bump();
                        break;
                    } else if self.try_match_command("DATA") {
                        _ = self.o.bump();
                        self.raw_string()
                    } else if self.try_match_command("DATAFORM") {
                        _ = self.o.bump();
                        self.raw_strform()
                    } else {
                        let mut diag = self.base_diag.clone();
                        diag.span_err(
                            Default::default(),
                            self.o.peek_token(Mode::Normal).token.span,
                            "unknown command inside DATALIST",
                        );
                        self.o.emit_diag(diag);
                        _ = self.sync_to_open(Terminal::LineBreak.into());
                        continue;
                    };
                    self.o.b.push_child(child);
                    _ = self.expect_sync_to_newline();
                    self.skip_newline();
                }
                let data = self
                    .o
                    .node_arena
                    .make_extra_data_ref_with_len(self.o.b.finish_node(cp).map(|x| x.0));
                let span = self.span_to_now(span);
                self.o
                    .node_arena
                    .add_node(EraNode::ListExpr(data), span, span)
            } else {
                let mut diag = self.base_diag.clone();
                diag.span_err(
                    Default::default(),
                    self.o.peek_token(Mode::Normal).token.span,
                    "unknown command inside PRINTDATA",
                );
                self.o.emit_diag(diag);
                _ = self.sync_to_open(Terminal::LineBreak.into());
                continue;
            };
            self.o.b.push_child(child);
            _ = self.expect_sync_to_newline();
            self.skip_newline();
        }

        let (extra_data, _) = self
            .o
            .node_arena
            .extend_extra_with_len(self.o.b.finish_node(cp).map(|x| x.0));

        extra_data
    }

    fn stmt_if(&mut self, span: SrcSpan) -> EraNodeRef {
        let mut can_else = true;
        let cp = self.o.b.checkpoint();

        // Condition
        let cond = self.or_sync_to(|s| s.expression(true), Terminal::LineBreak.into());
        self.o.b.push_child(cond);
        _ = self.expect_sync_to_newline();

        // Body
        self.skip_newline();
        loop {
            let mut cond = None;
            let (stmts, should_continue) = self.statements_list_with(|s| {
                if can_else && s.try_match_command("ELSEIF") {
                    // Transition to next condition
                    _ = s.o.bump();
                    cond = Some(s.or_sync_to(|s| s.expression(true), Terminal::LineBreak.into()));
                    _ = s.expect_sync_to_newline();
                    ControlFlow::Break(Some(true))
                } else if can_else && s.try_match_command("ELSE") {
                    // Transition to else block
                    can_else = false;
                    _ = s.o.bump();
                    ControlFlow::Break(Some(true))
                } else if s.try_match_command("ENDIF") {
                    // End of if block
                    _ = s.o.bump();
                    ControlFlow::Break(Some(false))
                } else {
                    ControlFlow::Continue(())
                }
            });
            self.o.b.push_child(stmts);
            if let Some(cond) = cond {
                self.o.b.push_child(cond);
            }
            let Some(should_continue) = should_continue else {
                let mut diag = self.base_diag.clone();
                diag.span_err(
                    Default::default(),
                    self.o.peek_token(Mode::Normal).token.span,
                    "unexpected end of block in IF; did you forget to close a block?",
                );
                self.o.emit_diag(diag);
                break;
            };
            if !should_continue {
                break;
            }
            _ = self.expect_sync_to_newline();
        }
        let extra_data = self
            .o
            .node_arena
            .make_extra_data_ref_with_len(self.o.b.finish_node(cp).map(|x| x.0));
        let span = self.span_to_now(span);
        self.o
            .node_arena
            .add_node(EraNode::StmtIf(extra_data), span, span)
    }

    fn stmt_sif(&mut self, span: SrcSpan) -> EraNodeRef {
        // Condition
        let cond = self.or_sync_to(|s| s.expression(true), Terminal::LineBreak.into());
        _ = self.expect_sync_to_newline();

        // Body
        self.skip_newline();
        let stmt = match self.safe_statement() {
            ControlFlow::Break(_) => {
                let mut diag = self.base_diag.clone();
                diag.span_err(
                    Default::default(),
                    self.o.peek_token(Mode::Normal).token.span,
                    "unexpected end of block in SIF; did you forget to close a block?",
                );
                self.o.emit_diag(diag);
                self.or_sync_to(|_| Err(()), Terminal::LineBreak.into())
            }
            ControlFlow::Continue(stmt) => stmt,
        };

        let extra_data = self
            .o
            .node_arena
            .make_extra_data_ref_with_len([cond.0, stmt.0]);
        let span = self.span_to_now(span);
        self.o
            .node_arena
            .add_node(EraNode::StmtIf(extra_data), span, span)
    }

    fn stmt_selectcase(&mut self, span: SrcSpan) -> EraNodeRef {
        let mut can_else = true;
        let mut last_is_case = false;
        let cp = self.o.b.checkpoint();

        // Expression
        let expr = self.or_sync_to(|s| s.expression(true), Terminal::LineBreak.into());
        _ = self.expect_sync_to_newline();

        // Body
        self.skip_newline();
        loop {
            let mut is_read_stmt = false;
            let mut preds = None;
            let (stmts, should_continue) = self.statements_list_with(|s| {
                if s.try_match_command("ENDSELECT") {
                    // End of select statement
                    _ = s.o.bump();
                    ControlFlow::Break(Some(false))
                } else if can_else && s.try_match_command("CASE") {
                    // Transition to next case
                    _ = s.o.bump();
                    preds = Some(s.stmt_selectcase_preds());
                    ControlFlow::Break(Some(true))
                } else if can_else && s.try_match_command("CASEELSE") {
                    // Transition to else block
                    can_else = false;
                    _ = s.o.bump();
                    ControlFlow::Break(Some(true))
                } else {
                    is_read_stmt = true;
                    ControlFlow::Continue(())
                }
            });
            if is_read_stmt && !last_is_case {
                // Need to filter out statements before any cases
                let mut diag = self.base_diag.clone();
                diag.span_err(
                    Default::default(),
                    self.o.peek_token(Mode::Normal).token.span,
                    "unreachable statements that do not belong to any case; ignoring",
                );
                self.o.emit_diag(diag);
                _ = self.sync_to_open(Terminal::LineBreak.into());
            } else {
                self.o.b.push_child(stmts);
            }
            if let Some(preds) = preds {
                self.o.b.push_child(preds);
            }
            let Some(should_continue) = should_continue else {
                let mut diag = self.base_diag.clone();
                diag.span_err(
                    Default::default(),
                    self.o.peek_token(Mode::Normal).token.span,
                    "unexpected end of block in SELECTCASE; did you forget to close a block?",
                );
                self.o.emit_diag(diag);
                break;
            };
            if !should_continue {
                break;
            }
            _ = self.expect_sync_to_newline();
            last_is_case = true;
        }

        let (extra_ref, _) = self
            .o
            .node_arena
            .extend_extra_with_len(self.o.b.finish_node(cp).map(|x| x.0));
        let span = self.span_to_now(span);
        self.o
            .node_arena
            .add_node(EraNode::StmtSelectCase(expr, extra_ref), span, span)
    }

    fn stmt_selectcase_preds(&mut self) -> EraNodeRef {
        let terminals = Terminal::Comma | Terminal::LineBreak;
        let cp = self.o.b.checkpoint();
        let span = self.o.l.current_src_span();
        loop {
            match self.o.peek_token(Mode::Normal).token.kind {
                Token::Comma => {
                    // Empty condition
                    _ = self.o.bump();
                    let child = self.make_empty();
                    self.o.b.push_child(child);
                    continue;
                }
                Token::LineBreak => break,
                _ => (),
            }

            let span = self.o.l.current_src_span();

            // Condition
            if self.try_match_command("IS") {
                // `IS <op> <expr>`
                _ = self.o.bump();
                let token = self.o.peek_token(Mode::Normal).token;
                if !matches!(
                    token.kind,
                    Token::CmpEq
                        | Token::CmpNEq
                        | Token::CmpLT
                        | Token::CmpGT
                        | Token::CmpLEq
                        | Token::CmpGEq
                        | Token::BitAnd
                ) {
                    let mut diag = self.base_diag.clone();
                    diag.span_err(
                        Default::default(),
                        token.span,
                        "expected comparison operator in SELECTCASE",
                    );
                    self.o.emit_diag(diag);
                    _ = self.sync_to_open(Terminal::Comma | Terminal::LineBreak);
                } else {
                    _ = self.o.bump();

                    let op = token.kind;
                    let rhs = self.or_sync_to(|s| s.expression(true), terminals);

                    let span = self.span_to_now(span);
                    let node = self.o.node_arena.add_node(
                        EraNode::SelectCaseCondOperator(op, rhs),
                        span,
                        span,
                    );
                    self.o.b.push_child(node);
                }
            } else {
                // Single or range
                let lhs = self.or_sync_to(|s| s.expression(true), terminals);
                let node = if self.try_match_command("TO") {
                    // Range
                    _ = self.o.bump();
                    let rhs = self.or_sync_to(|s| s.expression(true), terminals);
                    let span = self.span_to_now(span);
                    self.o
                        .node_arena
                        .add_node(EraNode::SelectCaseCondRange(lhs, rhs), span, span)
                } else {
                    // Single
                    let span = self.span_to_now(span);
                    self.o
                        .node_arena
                        .add_node(EraNode::SelectCaseCondSingle(lhs), span, span)
                };
                self.o.b.push_child(node);
            }

            if self.o.peek_token(Mode::Normal).token.kind == Token::LineBreak
                || self
                    .eat_sync(Mode::Normal, Token::Comma, terminals)
                    .is_none()
            {
                break;
            }
        }
        let extra_ref = self
            .o
            .node_arena
            .make_extra_data_ref_with_len(self.o.b.finish_node(cp).map(|x| x.0));
        let span = self.span_to_now(span);
        self.o
            .node_arena
            .add_node(EraNode::ListSelectCasePred(extra_ref), span, span)
    }

    fn stmt_while(&mut self, span: SrcSpan) -> EraNodeRef {
        let cond = self.or_sync_to(|s| s.expression(true), Terminal::LineBreak.into());
        _ = self.expect_sync_to_newline();
        let (stmts, cmd_ended) = self.statements_list(Some("WEND"));
        if !cmd_ended {
            let mut diag = self.base_diag.clone();
            diag.span_err(
                Default::default(),
                self.o.peek_token(Mode::Normal).token.span,
                "unexpected end of block in WHILE; did you forget to close a block?",
            );
            self.o.emit_diag(diag);
        }
        let span = self.span_to_now(span);
        self.o
            .node_arena
            .add_node(EraNode::StmtWhile(cond, stmts), span, span)
    }

    fn stmt_repeat(&mut self, span: SrcSpan) -> EraNodeRef {
        let count = self.or_sync_to(|s| s.expression(true), Terminal::LineBreak.into());
        _ = self.expect_sync_to_newline();
        let (stmts, cmd_ended) = self.statements_list(Some("REND"));
        if !cmd_ended {
            let mut diag = self.base_diag.clone();
            diag.span_err(
                Default::default(),
                self.o.peek_token(Mode::Normal).token.span,
                "unexpected end of block in REPEAT; did you forget to close a block?",
            );
            self.o.emit_diag(diag);
        }
        let span = self.span_to_now(span);
        self.o
            .node_arena
            .add_node(EraNode::StmtRepeat(count, stmts), span, span)
    }

    fn stmt_for(&mut self, span: SrcSpan) -> EraNodeRef {
        let terminals = Terminal::Comma | Terminal::LineBreak;
        let var = self.or_sync_to(|s| s.expression(true), terminals);
        self.eat_sync(Mode::Normal, Token::Comma, terminals);
        let start = self.or_sync_to(|s| s.expression(true), terminals);
        self.eat_sync(Mode::Normal, Token::Comma, terminals);
        let end = self.or_sync_to(|s| s.expression(true), terminals);
        let step = if self.next_is_newline()
            || self
                .eat_sync(Mode::Normal, Token::Comma, terminals)
                .is_none()
        {
            self.make_empty()
        } else {
            self.or_sync_to(|s| s.expression(true), terminals)
        };
        _ = self.expect_sync_to_newline();
        let (stmts, cmd_ended) = self.statements_list(Some("NEXT"));
        if !cmd_ended {
            let mut diag = self.base_diag.clone();
            diag.span_err(
                Default::default(),
                self.o.peek_token(Mode::Normal).token.span,
                "unexpected end of block in FOR; did you forget to close a block?",
            );
            self.o.emit_diag(diag);
        }
        let (extra_ref, _) = self
            .o
            .node_arena
            .extend_extra([start.0, end.0, step.0, stmts.0]);
        let span = self.span_to_now(span);
        self.o
            .node_arena
            .add_node(EraNode::StmtFor(var, extra_ref), span, span)
    }

    fn stmt_do_loop(&mut self, span: SrcSpan) -> EraNodeRef {
        _ = self.expect_sync_to_newline();
        let (stmts, cmd_ended) = self.statements_list(Some("LOOP"));
        if !cmd_ended {
            let mut diag = self.base_diag.clone();
            diag.span_err(
                Default::default(),
                self.o.peek_token(Mode::Normal).token.span,
                "unexpected end of block in DO-LOOP; did you forget to close a block?",
            );
            self.o.emit_diag(diag);
        }
        let cond = self.or_sync_to(|s| s.expression(true), Terminal::LineBreak.into());
        let span = self.span_to_now(span);
        self.o
            .node_arena
            .add_node(EraNode::StmtDoLoop(stmts, cond), span, span)
    }

    fn stmt_call_part(&mut self, is_form: bool) -> (EraNodeRef, EraNodeRef) {
        let func = if is_form {
            self.call_nameform()
        } else {
            self.or_sync_to(Self::identifier, Terminal::LineBreak.into())
        };
        let args = self.stmt_call_args();
        (func, args)
    }

    fn stmt_call_node<F>(&mut self, is_form: bool, f: F) -> EraNode
    where
        F: FnOnce(EraNodeRef, EraNodeRef) -> EraNode,
    {
        let (func, args) = self.stmt_call_part(is_form);
        f(func, args)
    }

    fn stmt_tryccall_node<F>(&mut self, is_form: bool, f: F) -> EraNode
    where
        F: FnOnce(EraNodeRef, EraExtraDataRef) -> EraNode,
    {
        let (func, args) = self.stmt_call_part(is_form);
        _ = self.expect_sync_to_newline();

        let (then_stmts, good) = self.statements_list(Some("CATCH"));
        if !good {
            let mut diag = self.base_diag.clone();
            diag.span_err(
                Default::default(),
                self.o.peek_token(Mode::Normal).token.span,
                "unexpected end of block in TRYCCALL; did you forget to close a block?",
            );
            self.o.emit_diag(diag);
        }
        let (catch_stmts, good) = self.statements_list(Some("ENDCATCH"));
        if !good {
            let mut diag = self.base_diag.clone();
            diag.span_err(
                Default::default(),
                self.o.peek_token(Mode::Normal).token.span,
                "unexpected end of block in TRYCCALL; did you forget to close a block?",
            );
            self.o.emit_diag(diag);
        }

        let (extra_data, _) = self
            .o
            .node_arena
            .extend_extra([args.0, then_stmts.0, catch_stmts.0]);
        f(func, extra_data)
    }

    fn call_nameform(&mut self) -> EraNodeRef {
        let mut buf = String::new();
        let cp = self.o.b.checkpoint();
        let span = self.o.l.current_src_span();
        let mut literal_span = Default::default();
        loop {
            let result = self.o.peek_token(Mode::CallForm);
            let token = result.token;
            // Try concatenating plain string literals
            if token.kind == Token::PlainStringLiteral {
                if buf.is_empty() {
                    literal_span = token.span;
                }
                routines::unescape_to_sink(result.lexeme, &mut buf);
                _ = self.o.bump();

                continue;
            }
            // Finish current string literal, prepare for expressions
            if !buf.is_empty() {
                let token_key = self.o.b.interner().get_or_intern(&buf);
                let span = self.span_to_now(literal_span);
                let node = self
                    .o
                    .node_arena
                    .add_node(EraNode::LiteralStr(token_key), span, span);
                self.o.b.push_child(node);
                buf.clear();
            }
            match token.kind {
                Token::LBrace => {
                    let span = token.span;
                    _ = self.o.bump();
                    let node = self.strform_interp_part_rest(Terminal::RBrace, span);
                    self.o.b.push_child(node);
                }
                Token::Percentage => {
                    let span = token.span;
                    _ = self.o.bump();
                    let node = self.strform_interp_part_rest(Terminal::Percentage, span);
                    self.o.b.push_child(node);
                }
                _ => break,
            }
        }

        let data = self
            .o
            .node_arena
            .make_small_extra_data(self.o.b.finish_node(cp).map(|x| x.0));
        let span = self.span_to_now(span);
        self.o
            .node_arena
            .add_node(EraNode::StringForm(data), span, span)
    }

    fn stmt_call_args(&mut self) -> EraNodeRef {
        let token = self.o.peek_token(Mode::Normal).token.kind;
        if token == Token::Comma {
            self.comma_expr_list(0).unwrap()
        } else if token == Token::LParen {
            self.paren_expr_list().unwrap()
        } else {
            self.empty_comma_expr_list()
        }
    }

    fn stmt_times(&mut self) -> EraNode {
        let terminals = Terminal::Comma | Terminal::LineBreak;
        let var = self.or_sync_to(|s| s.expression(true), terminals);
        _ = self.eat_sync(Mode::Normal, Token::Comma, terminals);
        let factor = self.raw_string();
        EraNode::StmtTimes(var, factor)
    }

    fn is_at_function_end(&mut self) -> bool {
        let token = self.o.peek_token(Mode::Normal).token;
        matches!(token.kind, Token::At | Token::Eof)
    }

    fn next_is_newline(&mut self) -> bool {
        self.o.peek_token(Mode::Normal).token.kind == Token::LineBreak
    }

    /// Returns whether not at the end of a function.
    fn expect_not_function_end(&mut self) -> bool {
        let token = self.o.peek_token(Mode::Normal).token;
        if matches!(token.kind, Token::At | Token::Eof) {
            let mut diag = self.base_diag.clone();
            diag.span_err(
                Default::default(),
                token.span,
                "unexpected end of function; did you forget to close a block?",
            );
            self.o.emit_diag(diag);
            false
        } else {
            true
        }
    }

    fn try_match_command(&mut self, cmd: &str) -> bool {
        let token = self.o.peek_token(Mode::Normal);
        token.token.kind == Token::Identifier && token.lexeme.eq_ignore_ascii_case(cmd)
    }
}

// Source: https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
// Operator precedence table, resembles
// https://en.cppreference.com/w/c/language/operator_precedence
fn prefix_binding_power(op: EraTokenKind) -> Option<((), u8)> {
    use EraTokenKind::*;
    Some(match op {
        Plus | Minus => ((), 25),
        Increment | Decrement => ((), 25),
        LogicalNot => ((), 25),
        BitNot => ((), 25),
        _ => return None,
    })
}
fn infix_binding_power(op: EraTokenKind) -> Option<(u8, u8)> {
    use EraTokenKind::*;
    Some(match op {
        Assign | ExprAssign | PlusAssign | MinusAssign | MultiplyAssign | DivideAssign
        | ModuloAssign | BitAndAssign | BitOrAssign | BitXorAssign => (2, 1),
        QuestionMark => (4, 3), // Ternary conditional
        LogicalOr => (5, 6),
        LogicalAnd => (7, 8),
        BitOr => (9, 10),
        BitXor => (11, 12),
        BitAnd => (13, 14),
        CmpEq | CmpNEq => (15, 16),
        CmpLT | CmpLEq | CmpGT | CmpGEq => (17, 18),
        BitShiftL | BitShiftR => (19, 20),
        Plus | Minus => (21, 22),
        Multiply | Divide | Percentage => (23, 24),
        Colon => (27, 28), // Array subscripting
        At => (31, 32),    // Special (namespaced) variable access
        _ => return None,
    })
}
fn postfix_binding_power(op: EraTokenKind) -> Option<(u8, ())> {
    use EraTokenKind::*;
    Some(match op {
        Increment | Decrement => (27, ()),
        LParen => (29, ()),
        _ => return None,
    })
}

#[derive(Debug, enumset::EnumSetType)]
enum EraTerminalTokenKind {
    LineBreak,
    Percentage,
    RParen,
    RBrace,
    Comma,
    TernaryStrFormMarker,
    QuestionMark,
    NumberSign,
    DoubleQuote,
}

use EraTerminalTokenKind as Terminal;

impl TryFrom<EraTokenKind> for EraTerminalTokenKind {
    type Error = ();

    fn try_from(kind: EraTokenKind) -> Result<Self, Self::Error> {
        match kind {
            EraTokenKind::LineBreak => Ok(Self::LineBreak),
            EraTokenKind::Percentage => Ok(Self::Percentage),
            EraTokenKind::RParen => Ok(Self::RParen),
            EraTokenKind::RBrace => Ok(Self::RBrace),
            EraTokenKind::Comma => Ok(Self::Comma),
            EraTokenKind::TernaryStrFormMarker => Ok(Self::TernaryStrFormMarker),
            EraTokenKind::QuestionMark => Ok(Self::QuestionMark),
            EraTokenKind::NumberSign => Ok(Self::NumberSign),
            EraTokenKind::DoubleQuote => Ok(Self::DoubleQuote),
            _ => Err(()),
        }
    }
}

impl From<EraTerminalTokenKind> for EraTokenKind {
    fn from(kind: EraTerminalTokenKind) -> Self {
        match kind {
            EraTerminalTokenKind::LineBreak => Self::LineBreak,
            EraTerminalTokenKind::Percentage => Self::Percentage,
            EraTerminalTokenKind::RParen => Self::RParen,
            EraTerminalTokenKind::RBrace => Self::RBrace,
            EraTerminalTokenKind::Comma => Self::Comma,
            EraTerminalTokenKind::TernaryStrFormMarker => Self::TernaryStrFormMarker,
            EraTerminalTokenKind::QuestionMark => Self::QuestionMark,
            EraTerminalTokenKind::NumberSign => Self::NumberSign,
            EraTerminalTokenKind::DoubleQuote => Self::DoubleQuote,
        }
    }
}

trait EraTerminalTokenKindAdhoc {
    fn to_tokens_vec(&self) -> Vec<EraTokenKind>;
}

impl EraTerminalTokenKindAdhoc for enumset::EnumSet<EraTerminalTokenKind> {
    fn to_tokens_vec(&self) -> Vec<EraTokenKind> {
        self.iter().map(Into::into).collect()
    }
}
