// TODO: Migrate constants & types to this file

use std::{
    borrow::Cow,
    cell::RefCell,
    collections::BTreeMap,
    marker::PhantomData,
    ops::{ControlFlow, Deref},
};

use cstree::{
    interning::{Resolver, TokenInterner, TokenKey},
    syntax::{SyntaxElement, SyntaxElementRef, SyntaxNode, SyntaxToken},
    util::NodeOrToken,
    Syntax,
};
use either::Either;
use hashbrown::HashMap;
use indexmap::IndexMap;
use rclite::Rc;
use rustc_hash::FxBuildHasher;
use safer_ffi::{derive_ReprC, prelude::VirtualPtr};
use tinyvec::ArrayVec;

use crate::util::{interning::ThreadedTokenInterner, rcstr::ArcStr, Ascii};

type FxHashMap<K, V> = HashMap<K, V, FxBuildHasher>;
type FxIndexMap<K, V> = IndexMap<K, V, FxBuildHasher>;

#[derive_ReprC]
#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SrcPos(pub u32);

impl Default for SrcPos {
    fn default() -> Self {
        SrcPos(0)
    }
}

#[derive_ReprC]
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct SrcSpan {
    start: SrcPos,
    len: u32,
}

impl Default for SrcSpan {
    fn default() -> Self {
        SrcSpan {
            start: SrcPos::default(),
            len: 0,
        }
    }
}

impl From<cstree::text::TextRange> for SrcSpan {
    fn from(range: cstree::text::TextRange) -> Self {
        SrcSpan {
            start: SrcPos(range.start().into()),
            len: range.len().into(),
        }
    }
}

impl SrcSpan {
    pub fn new(start: SrcPos, len: u32) -> SrcSpan {
        SrcSpan { start, len }
    }
    pub fn empty() -> SrcSpan {
        Default::default()
    }
    pub fn with_ends(start: SrcPos, end: SrcPos) -> SrcSpan {
        assert!(start <= end, "end position is before start position");
        SrcSpan {
            start,
            len: end.0 - start.0,
        }
    }
    pub fn new_covering(span1: SrcSpan, span2: SrcSpan) -> SrcSpan {
        let start = span1.start().min(span2.start());
        let end = span1.end().max(span2.end());
        SrcSpan::with_ends(start, end)
    }
    pub fn start(&self) -> SrcPos {
        self.start
    }
    pub fn len(&self) -> u32 {
        self.len
    }
    pub fn end(&self) -> SrcPos {
        SrcPos(self.start.0 + self.len)
    }
    // pub fn is_tagged(&self) -> bool {
    //     const TAG_MASK: u32 = 0x80000000;
    //     (self.len & TAG_MASK) != 0
    // }
    // pub fn len_without_tag(&self) -> u32 {
    //     self.len & 0x7FFFFFFF
    // }
    pub fn contains_pos(&self, pos: SrcPos) -> bool {
        self.start() <= pos && pos < self.end()
    }
    pub fn contains_span(&self, other: SrcSpan) -> bool {
        self.start() <= other.start() && other.end() <= self.end()
    }
    pub fn intersects(&self, other: SrcSpan) -> bool {
        self.start() < other.end() && other.start() < self.end()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SrcLoc {
    /// The 1-based line number.
    pub line: u32,
    /// The 0-based column number.
    pub col: u32,
}

impl Default for SrcLoc {
    fn default() -> Self {
        SrcLoc { line: 1, col: 0 }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct EraSmallTokenKind {
    kind: u16,
}

impl From<EraTokenKind> for EraSmallTokenKind {
    fn from(kind: EraTokenKind) -> Self {
        EraSmallTokenKind { kind: kind as _ }
    }
}

impl From<EraSmallTokenKind> for EraTokenKind {
    fn from(kind: EraSmallTokenKind) -> Self {
        unsafe { std::mem::transmute(kind.kind as u32) }
    }
}

#[derive_ReprC]
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, cstree::Syntax)]
pub enum EraTokenKind {
    // ----- Misc -----
    /// The error token.
    Invalid,
    Eof,
    WhiteSpace,
    LineBreak,
    Comment,
    Macro,
    // ----- Symbols -----
    #[static_text("+")]
    Plus,
    #[static_text("-")]
    Minus,
    #[static_text("*")]
    Multiply,
    #[static_text("/")]
    Divide,
    #[static_text("%")]
    Percentage,
    #[static_text("&")]
    BitAnd,
    #[static_text("|")]
    BitOr,
    #[static_text("^")]
    BitXor,
    #[static_text("~")]
    BitNot,
    #[static_text("<<")]
    BitShiftL,
    #[static_text(">>")]
    BitShiftR,
    #[static_text("!")]
    LogicalNot,
    #[static_text("&&")]
    LogicalAnd,
    #[static_text("||")]
    LogicalOr,
    #[static_text("==")]
    CmpEq,
    #[static_text("!=")]
    CmpNEq,
    #[static_text("<")]
    CmpLT,
    #[static_text(">")]
    CmpGT,
    #[static_text("<=")]
    CmpLEq,
    #[static_text(">=")]
    CmpGEq,
    #[static_text("?")]
    QuestionMark,
    #[static_text("=")]
    Assign,
    #[static_text("'=")]
    ExprAssign,
    #[static_text("+=")]
    PlusAssign,
    #[static_text("-=")]
    MinusAssign,
    #[static_text("*=")]
    MultiplyAssign,
    #[static_text("/=")]
    DivideAssign,
    #[static_text("%=")]
    ModuloAssign,
    #[static_text("&=")]
    BitAndAssign,
    #[static_text("|=")]
    BitOrAssign,
    #[static_text("^=")]
    BitXorAssign,
    #[static_text("++")]
    Increment,
    #[static_text("--")]
    Decrement,
    #[static_text(",")]
    Comma,
    // TODO: Remove SemiColon enum?
    #[static_text(";")]
    SemiColon,
    #[static_text(":")]
    Colon,
    #[static_text("$")]
    Dollar,
    #[static_text("(")]
    LParen,
    #[static_text(")")]
    RParen,
    #[static_text("{")]
    LBrace,
    #[static_text("}")]
    RBrace,
    #[static_text("@")]
    At,
    #[static_text("#")]
    NumberSign,
    IntLiteral,
    StringLiteral,
    PlainStringLiteral,
    #[static_text("@\"")]
    StringFormStart, // Caller should manually enter StrForm mode on encountering this
    #[static_text("\\@")]
    TernaryStrFormMarker,
    SingleComment,
    Identifier, // Even built-in commands are recognized as identifiers first
    #[static_text("'")]
    SingleQuote,
    #[static_text("\"")]
    DoubleQuote,
    // ----- Keywords -----
    /// A generic keyword identifier.
    KwIdent,
    KwDim,
    KwDimS,
    KwGlobal,
    KwDynamic,
    KwRef,
    KwConst,
    KwSavedata,
    KwCharadata,
    KwLocalSize,
    KwLocalSSize,
    KwFunction,
    KwFunctionS,
    KwDefine,
    KwOnly,
    KwPri,
    KwLater,
    KwSingle,
    KwTransient,
    // ----- Nodes -----
    MacroNode,

    Program,

    VarDecl,
    VarSDecl,
    LocalSizeDecl,
    LocalSSizeDecl,
    DefineDecl,
    EventKindDecl,
    FunctionDecl,  // `#FUNCTION`
    FunctionSDecl, // `#FUNCTIONS`
    TransientDecl,
    SharpDeclList,

    FunctionItem,

    LabelStmt,
    NopStmt,
    ExprStmt,
    RowAssignStmt, // `var:idx = expr1, expr2, expr3`
    ResultCmdCallStmt,
    DebugPrintStmt,
    PrintStmt,
    PrintDataStmt,
    WaitStmt,
    ForceWaitStmt,
    WaitAnyKeyStmt,
    IfStmt,
    QuitStmt,
    SelectCaseStmt,
    WhileStmt,
    CallStmt,
    TryCallStmt,
    TryCCallStmt,
    JumpStmt,
    TryJumpStmt,
    TryCJumpStmt,
    ReturnStmt,
    ContinueStmt,
    BreakStmt,
    RestartStmt,
    ThrowStmt,
    RepeatStmt,
    GotoStmt,
    ForStmt,
    DoLoopStmt,
    // GCreateStmt,
    // GDisposeStmt,
    // GDrawSpriteStmt,
    SplitStmt,
    TimesStmt,
    SetBitStmt,
    ClearBitStmt,
    InvertBitStmt,
    SetColorStmt,
    ResetColorStmt,
    SetBgColorStmt,
    ResetBgColorStmt,
    VarSetStmt,
    CVarSetStmt,
    VarSizeStmt,
    SwapStmt,
    HtmlPrintStmt,
    PrintButtonStmt,
    PrintButtonCStmt,
    PrintButtonLCStmt,
    ArrayRemoveStmt,
    ArraySortStmt,
    ArrayMSortStmt,
    ArrayCopyStmt,
    ArrayShiftStmt,
    InputStmt,
    InputSStmt,
    TInputStmt,
    TInputSStmt,
    OneInputStmt,
    OneInputSStmt,
    TOneInputStmt,
    TOneInputSStmt,
    ReuseLastLineStmt,
    ClearLineStmt,
    DrawLineStmt,
    CustomDrawLineStmt,
    TWaitStmt,
    FontStyleStmt,
    FontBoldStmt,
    FontItalicStmt,
    FontRegularStmt,
    SetFontStmt,
    StrDataStmt,
    PutFormStmt,
    SkipDispStmt,
    BeginStmt,
    DoTrainStmt,
    RedrawStmt,
    StrLenStmt,
    StrLenUStmt,
    AlignmentStmt,
    ToolTipSetDelayStmt,
    ToolTipSetDurationStmt,
    RandomizeStmt,
    DumpRandStmt,
    InitRandStmt,
    BarStmt,
    BarLStmt,
    AddCharaStmt,
    PickUpCharaStmt,
    DelCharaStmt,
    SwapCharaStmt,
    AddCopyCharaStmt,
    ResetStainStmt,
    SaveCharaStmt,
    LoadCharaStmt,
    SetAnimeTimerStmt,
    HtmlTagSplitStmt,
    PowerStmt,
    LoadDataStmt,
    SaveDataStmt,
    // CheckDataStmt,
    GetTimeStmt,
    LoadGlobalStmt,
    SaveGlobalStmt,
    LoadGameStmt,
    SaveGameStmt,
    DebugClearStmt,
    ResetDataStmt,
    DataStmt,
    StmtList,

    SelectCaseSingle,
    SelectCaseRange,
    SelectCaseCond,
    SelectCasePredList,

    PreUnaryExpr,
    PostUnaryExpr,
    BinaryExpr,
    TernaryExpr,
    FunCallExpr,
    ParenExpr,
    VarIdxExpr,       // `var:idx1:idx2`
    VarNamespaceExpr, // `var@namespace`
    EmptyExpr,
    ExprList,
    StringForm, // StringForm(or also *Expr)
    StringFormInterpPart,
}

impl EraTokenKind {
    pub fn is_newline_eof(&self) -> bool {
        use EraTokenKind::*;
        matches!(self, Eof | LineBreak)
    }

    pub fn is_sharp_decl(&self) -> bool {
        use EraTokenKind::*;
        match self {
            VarDecl | VarSDecl | LocalSizeDecl | LocalSSizeDecl | DefineDecl | EventKindDecl
            | FunctionDecl | FunctionSDecl => true,
            _ => false,
        }
    }

    pub fn is_decl(&self) -> bool {
        use EraTokenKind::*;
        match self {
            FunctionItem => true,
            _ => self.is_sharp_decl(),
        }
    }

    pub fn is_operator(&self) -> bool {
        use EraTokenKind::*;
        match self {
            Plus | Minus | Multiply | Divide | Percentage | BitAnd | BitOr | BitXor | BitNot
            | BitShiftL | BitShiftR | LogicalNot | LogicalAnd | LogicalOr | CmpEq | CmpNEq
            | CmpLT | CmpGT | CmpLEq | CmpGEq | QuestionMark | Assign | ExprAssign | PlusAssign
            | MinusAssign | MultiplyAssign | DivideAssign | ModuloAssign | BitAndAssign
            | BitOrAssign | BitXorAssign | Increment | Decrement => true,
            _ => false,
        }
    }

    pub fn is_stmt(&self) -> bool {
        use EraTokenKind::*;
        match self {
            LabelStmt
            | NopStmt
            | ExprStmt
            | RowAssignStmt
            | ResultCmdCallStmt
            | DebugPrintStmt
            | PrintStmt
            | PrintDataStmt
            | WaitStmt
            | IfStmt
            | QuitStmt
            | SelectCaseStmt
            | WhileStmt
            | CallStmt
            | TryCallStmt
            | TryCCallStmt
            | JumpStmt
            | TryJumpStmt
            | TryCJumpStmt
            | ReturnStmt
            | ContinueStmt
            | BreakStmt
            | ThrowStmt
            | RepeatStmt
            | GotoStmt
            | ForStmt
            | DoLoopStmt
            | SplitStmt
            | TimesStmt
            | SetBitStmt
            | ClearBitStmt
            | InvertBitStmt
            | SetColorStmt
            | ResetColorStmt
            | SetBgColorStmt
            | ResetBgColorStmt
            | VarSetStmt
            | CVarSetStmt
            | VarSizeStmt
            | SwapStmt
            | HtmlPrintStmt
            | PrintButtonStmt
            | ArrayRemoveStmt
            | ArraySortStmt
            | ArrayMSortStmt
            | ArrayCopyStmt
            | ArrayShiftStmt
            | InputStmt
            | InputSStmt
            | TInputStmt
            | TInputSStmt
            | OneInputStmt
            | OneInputSStmt
            | TOneInputStmt
            | TOneInputSStmt
            | ReuseLastLineStmt
            | ClearLineStmt
            | DrawLineStmt
            | CustomDrawLineStmt
            | TWaitStmt
            | FontStyleStmt
            | FontBoldStmt
            | FontItalicStmt
            | FontRegularStmt
            | SetFontStmt
            | StrDataStmt
            | PutFormStmt
            | SkipDispStmt
            | BeginStmt
            | DoTrainStmt
            | RedrawStmt
            | StrLenStmt
            | StrLenUStmt
            | AlignmentStmt
            | ToolTipSetDelayStmt
            | ToolTipSetDurationStmt
            | RandomizeStmt
            | DumpRandStmt
            | InitRandStmt
            | BarStmt
            | BarLStmt
            | AddCharaStmt
            | PickUpCharaStmt
            | DelCharaStmt
            | SwapCharaStmt
            | AddCopyCharaStmt
            | ResetStainStmt
            | SaveCharaStmt
            | LoadCharaStmt
            | SetAnimeTimerStmt
            | HtmlTagSplitStmt
            | PowerStmt
            | LoadDataStmt
            | SaveDataStmt
            | RestartStmt
            | GetTimeStmt
            | LoadGlobalStmt
            | SaveGlobalStmt
            | LoadGameStmt
            | SaveGameStmt
            | DebugClearStmt
            | ResetDataStmt
            | DataStmt => true,
            _ => false,
        }
    }

    pub fn is_expr(&self) -> bool {
        use EraTokenKind::*;
        match self {
            IntLiteral | StringLiteral => true,
            PreUnaryExpr | PostUnaryExpr | BinaryExpr | TernaryExpr | FunCallExpr | ParenExpr
            | VarIdxExpr | VarNamespaceExpr | EmptyExpr | StringForm => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct EraToken {
    pub kind: EraTokenKind,
    pub span: SrcSpan,
}

// TODO: Remove this
// TODO: EraAstNode, EraAstCursor, ...
// NOTE: Primarily for generic AST access, useful for live editing, etc.
// pub trait EraAstNode {
//     fn span(&self) -> SrcSpan;
//     fn kind(&self) -> EraAstNodeKind;
//     fn text(&self) -> String;
// }

#[derive(Debug)]
pub struct EraDefineData {
    pub filename: ArcStr,
    pub span: SrcSpan,
    // HACK: Ensure StableDeref by disabling SSO
    pub data: arcstr::ArcStr,
}

/// A `#define` list, lexically scoped.
// pub struct EraDefineScope<'parent> {
//     parent: Option<&'parent EraDefineScope<'parent>>,
//     defines: HashMap<Box<[u8]>, Box<[u8]>>,
// }
#[derive(Debug, Default)]
pub struct EraDefineScope {
    defines: FxHashMap<ArcStr, EraDefineData>,
}

impl EraDefineScope {
    pub fn new() -> Self {
        EraDefineScope {
            defines: FxHashMap::default(),
        }
    }

    pub fn insert(&mut self, key: ArcStr, data: EraDefineData) -> Option<EraDefineData> {
        self.defines.insert(key, data)
    }

    pub fn get(&self, key: &str) -> Option<&EraDefineData> {
        self.defines.get(key)
    }

    /// Extends the current scope with the given scope.
    /// If a collision occurs, the `on_collision` callback is called.
    /// The callback is called with the macro name, the new and the old data.
    pub fn extend<F>(&mut self, other: EraDefineScope, mut on_collision: F)
    where
        F: FnMut(&str, &EraDefineData, &EraDefineData),
    {
        let extend_len = other.defines.len();
        let reserve_len = if self.defines.is_empty() {
            extend_len
        } else {
            (extend_len + 1) / 2
        };
        self.defines.reserve(reserve_len);
        for (key, data) in other.defines {
            use hashbrown::hash_map::Entry::*;
            match self.defines.entry(key) {
                Occupied(mut entry) => {
                    on_collision(entry.key(), &data, entry.get());
                    entry.insert(data);
                }
                Vacant(entry) => {
                    entry.insert(data);
                }
            }
        }
    }
}

/// Macro mappings, from the replaced source span to the original text.
#[derive(Debug, Default)]
pub struct EraMacroMap {
    /// The list of macro mappings (monotonic).
    mappings: Vec<(SrcSpan, ArcStr)>,
}

impl EraMacroMap {
    pub fn iter(&self) -> std::slice::Iter<'_, (SrcSpan, ArcStr)> {
        self.mappings.iter()
    }

    pub fn push(&mut self, span: SrcSpan, text: ArcStr) {
        if let Some((last_span, last_text)) = self.mappings.last() {
            // Ensure that the mappings are monotonic.
            assert!(
                last_span.end() <= span.start(),
                "non-monotonic macro mappings: {:?} -> {} and {:?} -> {}",
                last_span,
                last_text,
                span,
                text
            );
        }
        let _: u32 = text.len().try_into().expect("text length overflow");
        self.mappings.push((span, text));
    }

    pub fn get(&self, span: &SrcSpan) -> Option<&ArcStr> {
        self.mappings
            .binary_search_by_key(span, |(span, _)| *span)
            .ok()
            .map(|idx| &self.mappings[idx].1)
    }

    /// Translates the given (replaced) span to the original source span.
    pub fn translate_span(&self, mut span: SrcSpan) -> SrcSpan {
        let mut delta = 0;
        for (from_span, text) in self.mappings.iter() {
            let from_len = from_span.len();
            let text_len = text.len() as u32;
            // NOTE: `text_span` has its end pos altered to adapt to offset.
            let text_span = SrcSpan::new(from_span.start(), text_len);
            let cur_delta = text_len as i32 - from_len as i32;
            // TODO: Remove this part later, because the logic is redundant
            // if from_span.contains_span(span) {
            //     // Extend to whole macro
            //     span = text_span;
            //     break;
            // }
            if from_span.intersects(span) {
                // Extend one side to whole macro
                let start = if span.start() < from_span.start() {
                    span.start()
                } else {
                    text_span.start()
                };
                let end = if span.end() > from_span.end() {
                    // Fix end position
                    let end = span.end().0.wrapping_add_signed(cur_delta);
                    SrcPos(end)
                } else {
                    text_span.end()
                };
                span = SrcSpan::with_ends(start, end);
                break;
            }
            // Otherwise, not intersecting in any way
            if span.end() <= from_span.start() {
                break;
            }
            delta += cur_delta;
        }
        let new_start_pos = span.start().0.wrapping_add_signed(delta);
        SrcSpan::new(SrcPos(new_start_pos), span.len())
    }

    /// Translates the given (original) span to the replaced source span.
    pub fn inverse_translate_span(&self, mut span: SrcSpan) -> SrcSpan {
        let mut delta = 0;
        for (from_span, text) in self.mappings.iter() {
            let from_len = from_span.len();
            let text_len = text.len() as u32;
            let text_start_pos = SrcPos(from_span.start().0.wrapping_add_signed(delta));
            let text_span = SrcSpan::new(text_start_pos, text_len);
            let cur_delta = text_len as i32 - from_len as i32;
            if text_span.intersects(span) {
                let start = if span.start() < text_span.start() {
                    let diff = text_span.start().0 - span.start().0;
                    SrcPos(from_span.start().0 - diff)
                } else {
                    from_span.start()
                };
                let end = if span.end() > text_span.end() {
                    let diff = span.end().0 - text_span.end().0;
                    SrcPos(from_span.end().0 + diff)
                } else {
                    from_span.end()
                };
                span = SrcSpan::with_ends(start, end);
                return span;
            }
            if span.end() <= text_span.start() {
                break;
            }
            delta += cur_delta;
        }
        let new_start_pos = span.start().0.wrapping_add_signed(-delta);
        SrcSpan::new(SrcPos(new_start_pos), span.len())
    }

    pub fn translate_pos(&self, pos: SrcPos) -> SrcPos {
        self.translate_span(SrcSpan::new(pos, 0)).start()
    }

    pub fn inverse_translate_pos(&self, pos: SrcPos) -> SrcPos {
        self.inverse_translate_span(SrcSpan::new(pos, 0)).start()
    }

    /// Translates given source text to the original source text.
    pub fn translate_source(&self, src: &str) -> String {
        let mut result = String::new();
        for (from_span, text) in self.mappings.iter() {
            // TODO...
        }
        result
    }
}

#[derive(Debug)]
pub struct EraSourceFile {
    pub filename: ArcStr,
    /// The original source text. Reduces memory usage.
    pub text: Option<String>,
    /// The compressed original source text. Reduces memory usage.
    pub compressed_text: Option<Box<[u8]>>,
    /// The root AST node of the lossless syntax tree, with macros replaced.
    pub cst_root: Option<cstree::green::GreenNode>,
    pub ast_data: Option<(
        crate::v2::parser::EraNodeRef,
        crate::v2::parser::EraNodeArena,
    )>,
    /// The list of macro substitution mappings (from CST to original text).
    pub macro_map: EraMacroMap,
    /// The list of `#define`'s.
    pub defines: EraDefineScope,
    /// Whether the file is a header file.
    pub is_header: bool,
    /// The list of newline positions (before expansion of macros).
    pub newline_pos: Vec<SrcPos>,
}

#[derive(Debug)]
pub struct EraCompilerCtx<Callback> {
    // TODO: Wrap callback in Mutex; this makes it easier for concurrent access, while also
    //       ensuring zero overhead for single-threaded access (i.e. &mut Ctx).
    pub callback: Callback,
    /// Replacements defined in `_Rename.csv`.
    pub global_replace: EraDefineScope,
    /// `#define`'s from erh files are considered global.
    // global_define: EraDefineScope<'static>,
    pub global_define: EraDefineScope,
    /// The source file map.
    pub source_map: Rc<FxIndexMap<ArcStr, EraSourceFile>>,
    /// Current active source file name. Empty if no source file is active.
    pub active_source: ArcStr,
    pub variables: EraVarPool,
    /// The list of character templates. Used by runtime.
    pub chara_templates: BTreeMap<u32, EraCharaInitTemplate>,
    /// The list of CSV contextual indices. Used by CSV variable access.
    pub csv_indices: FxHashMap<Ascii<ArcStr>, Vec<(EraCsvVarKind, u32)>>,
    // NOTE: We usually never remove chunks, so we use `Vec` instead of `HashMap`.
    pub bc_chunks: Rc<Vec<EraBcChunk>>,
    /// The list of function entries. Note that the value is wrapped in `Option` to
    /// allow for soft deletion.
    pub func_entries: Rc<FxIndexMap<&'static Ascii<str>, Option<EraFuncInfo>>>,
    // TODO: Always use `&'i ThreadedRodeo`. We also need to implement a `ThreadedNodeCache` later.
    //       This helps with concurrent interning, and also helps to eliminate the usage of `unsafe`.
    /// The node cache used for CST building.
    pub node_cache: &'static mut cstree::build::NodeCache<'static, ThreadedTokenInterner>,
}

trait IndexMapExt {
    type Key;
    type Value;

    fn get_flatten<Q>(&self, key: &Q) -> Option<&Self::Value>
    where
        Q: ?Sized + std::hash::Hash + indexmap::Equivalent<Self::Key>;
}

impl<K, V> IndexMapExt for IndexMap<K, Option<V>> {
    type Key = K;
    type Value = V;

    fn get_flatten<Q>(&self, key: &Q) -> Option<&Self::Value>
    where
        Q: ?Sized + std::hash::Hash + indexmap::Equivalent<Self::Key>,
    {
        self.get(key).and_then(Option::as_ref)
    }
}

impl<Callback: EraCompilerCallback> EraCompilerCtx<Callback> {
    pub fn new(
        callback: Callback,
        node_cache: &'static mut cstree::build::NodeCache<'static, ThreadedTokenInterner>,
    ) -> Self {
        EraCompilerCtx {
            callback,
            global_replace: EraDefineScope::new(),
            global_define: EraDefineScope::new(),
            source_map: Default::default(),
            active_source: ArcStr::default(),
            variables: EraVarPool::new(),
            node_cache,
            chara_templates: BTreeMap::new(),
            csv_indices: FxHashMap::default(),
            bc_chunks: Rc::new(Vec::new()),
            func_entries: Default::default(),
        }
    }

    pub fn make_diag(&self) -> Diagnostic<'static> {
        Diagnostic::with_file(self.active_source.clone())
    }

    pub fn emit_diag(&mut self, diag: Diagnostic) {
        let provider = DiagnosticProvider::new(
            &diag,
            Some(&self.source_map),
            Some(self.node_cache.interner()),
        );
        self.callback.emit_diag(&provider);
        diag.cancel();
    }

    pub fn interner(&self) -> &ThreadedTokenInterner {
        self.node_cache.interner()
    }

    // pub fn interner_mut(&mut self) -> &mut ThreadedTokenInterner {
    //     self.node_cache.interner_mut()
    // }

    /// # Safety
    ///
    /// You must ensure that the returned reference is not used after the `EraCompilerCtx` is dropped.
    /// [`lasso::Rodeo`] guarantees that the strings are not deallocated until the interner is dropped.
    pub fn resolve_static_str(&self, key: TokenKey) -> &'static str {
        unsafe { std::mem::transmute(self.interner().resolve(key)) }
    }
}

pub trait EraCompilerCallback {
    // ----- Diagnostics -----
    fn emit_diag(&mut self, diag: &DiagnosticProvider);
    // ----- Virtual Machine -----
    fn on_get_rand(&mut self) -> u64;
    fn on_print(&mut self, content: &str, flags: EraPrintExtendedFlags);
    // TODO: Debug is a global flag inside VM?
    fn on_debugprint(&mut self, content: &str, flags: EraPrintExtendedFlags);
    fn on_html_print(&mut self, content: &str);
    fn on_wait(&mut self, any_key: bool, is_force: bool);
    fn on_twait(&mut self, duration: i64, is_force: bool);
    fn on_input_int(
        &mut self,
        default_value: Option<i64>,
        can_click: bool,
        allow_skip: bool,
    ) -> ControlFlow<(), Option<i64>>;
    fn on_input_str(
        &mut self,
        default_value: Option<&str>,
        can_click: bool,
        allow_skip: bool,
    ) -> ControlFlow<(), Option<String>>;
    fn on_tinput_int(
        &mut self,
        time_limit: i64,
        default_value: i64,
        show_prompt: bool,
        expiry_msg: &str,
        can_click: bool,
    ) -> ControlFlow<(), Option<i64>>;
    fn on_tinput_str(
        &mut self,
        time_limit: i64,
        default_value: &str,
        show_prompt: bool,
        expiry_msg: &str,
        can_click: bool,
    ) -> ControlFlow<(), Option<String>>;
    fn on_oneinput_int(&mut self, default_value: Option<i64>) -> ControlFlow<(), Option<i64>>;
    fn on_oneinput_str(&mut self, default_value: Option<&str>) -> ControlFlow<(), Option<String>>;
    fn on_toneinput_int(
        &mut self,
        time_limit: i64,
        default_value: i64,
        show_prompt: bool,
        expiry_msg: &str,
        can_click: bool,
    ) -> ControlFlow<(), Option<i64>>;
    fn on_toneinput_str(
        &mut self,
        time_limit: i64,
        default_value: &str,
        show_prompt: bool,
        expiry_msg: &str,
        can_click: bool,
    ) -> ControlFlow<(), Option<String>>;
    fn on_reuselastline(&mut self, content: &str);
    fn on_clearline(&mut self, count: i64);
    fn on_var_get_int(&mut self, name: &str, idx: usize) -> Result<i64, anyhow::Error>;
    fn on_var_get_str(&mut self, name: &str, idx: usize) -> Result<String, anyhow::Error>;
    fn on_var_set_int(&mut self, name: &str, idx: usize, val: i64) -> Result<(), anyhow::Error>;
    fn on_var_set_str(&mut self, name: &str, idx: usize, val: &str) -> Result<(), anyhow::Error>;
    fn on_print_button(&mut self, content: &str, value: &str, flags: EraPrintExtendedFlags);
    // Graphics subsystem
    fn on_gcreate(&mut self, gid: i64, width: i64, height: i64) -> i64;
    fn on_gcreatefromfile(&mut self, gid: i64, path: &str) -> i64;
    fn on_gdispose(&mut self, gid: i64) -> i64;
    fn on_gcreated(&mut self, gid: i64) -> i64;
    fn on_gdrawsprite(
        &mut self,
        gid: i64,
        sprite_name: &str,
        dest_x: i64,
        dest_y: i64,
        dest_width: i64,
        dest_height: i64,
        color_matrix: Option<&EraColorMatrix>,
    ) -> i64;
    fn on_gclear(&mut self, gid: i64, color: i64) -> i64;
    fn on_spritecreate(
        &mut self,
        name: &str,
        gid: i64,
        x: i64,
        y: i64,
        width: i64,
        height: i64,
    ) -> i64;
    fn on_spritedispose(&mut self, name: &str) -> i64;
    fn on_spritecreated(&mut self, name: &str) -> i64;
    fn on_spriteanimecreate(&mut self, name: &str, width: i64, height: i64) -> i64;
    fn on_spriteanimeaddframe(
        &mut self,
        name: &str,
        gid: i64,
        x: i64,
        y: i64,
        width: i64,
        height: i64,
        offset_x: i64,
        offset_y: i64,
        delay: i64,
    ) -> i64;
    fn on_spritewidth(&mut self, name: &str) -> i64;
    fn on_spriteheight(&mut self, name: &str) -> i64;
    // Filesystem subsystem
    fn on_open_host_file(
        &mut self,
        path: &str,
        can_write: bool,
    ) -> anyhow::Result<Box<dyn EraCompilerHostFile>>;
    fn on_check_host_file_exists(&mut self, path: &str) -> anyhow::Result<bool>;
    fn on_delete_host_file(&mut self, path: &str) -> anyhow::Result<()>;
    fn on_list_host_file(&mut self, path: &str) -> anyhow::Result<Vec<String>>;
    // Others
    fn on_check_font(&mut self, font_name: &str) -> i64;
    // NOTE: Returns UTC timestamp (in milliseconds).
    fn on_get_host_time(&mut self) -> u64;
    fn on_get_config_int(&mut self, name: &str) -> anyhow::Result<i64>;
    fn on_get_config_str(&mut self, name: &str) -> anyhow::Result<String>;
    // NOTE: Returns { b15 = <key down>, b0 = <key triggered> }. For key codes, refer
    //       to https://learn.microsoft.com/en-us/windows/win32/inputdev/virtual-key-codes.
    fn on_get_key_state(&mut self, key_code: i64) -> i64;
}

pub trait EraCompilerHostFile {
    fn read(&mut self, buf: &mut [u8]) -> anyhow::Result<u64>;
    fn write(&mut self, buf: &[u8]) -> anyhow::Result<()>;
    fn flush(&mut self) -> anyhow::Result<()>;
    fn truncate(&mut self) -> anyhow::Result<()>;
    fn seek(&mut self, pos: i64, mode: EraCompilerFileSeekMode) -> anyhow::Result<u64>;
}

#[derive_ReprC]
#[repr(u32)]
pub enum EraCompilerFileSeekMode {
    Start,
    End,
    Current,
}

#[derive_ReprC]
#[repr(u32)]
#[non_exhaustive]
pub enum EraExecutionBreakReason {
    /// User requested break from inside the callback.
    CallbackBreak,
    /// User requested break from the run / stop flag.
    StopFlag,
    /// Comes from the statement `THROW`.
    CodeThrows,
    /// Comes from the instruction `FailWithMsg`.
    FailInstruction,
    /// Comes from the instruction `DebugBreak`.
    DebugBreakInstruction,
    /// Comes from the instruction `Quit`.
    CodeQuit,
    /// The execution reached the maximum number of instructions.
    ReachedMaxInstructions,
    /// Cannot decode or execute the instruction at this time.
    IllegalInstruction,
    /// Arguments to the instruction are invalid.
    IllegalArguments,
    /// An exception occurred during execution.
    InternalError,
}

// Reference: https://learn.microsoft.com/en-us/dotnet/api/system.drawing.imaging.colormatrix
#[derive_ReprC]
#[repr(C)]
pub struct EraColorMatrix {
    pub m00: f32,
    pub m01: f32,
    pub m02: f32,
    pub m03: f32,
    pub m04: f32,
    pub m10: f32,
    pub m11: f32,
    pub m12: f32,
    pub m13: f32,
    pub m14: f32,
    pub m20: f32,
    pub m21: f32,
    pub m22: f32,
    pub m23: f32,
    pub m24: f32,
    pub m30: f32,
    pub m31: f32,
    pub m32: f32,
    pub m33: f32,
    pub m34: f32,
    pub m40: f32,
    pub m41: f32,
    pub m42: f32,
    pub m43: f32,
    pub m44: f32,
}

impl Default for EraColorMatrix {
    fn default() -> Self {
        EraColorMatrix {
            m00: 1.0,
            m01: 0.0,
            m02: 0.0,
            m03: 0.0,
            m04: 0.0,
            m10: 0.0,
            m11: 1.0,
            m12: 0.0,
            m13: 0.0,
            m14: 0.0,
            m20: 0.0,
            m21: 0.0,
            m22: 1.0,
            m23: 0.0,
            m24: 0.0,
            m30: 0.0,
            m31: 0.0,
            m32: 0.0,
            m33: 1.0,
            m34: 0.0,
            m40: 0.0,
            m41: 0.0,
            m42: 0.0,
            m43: 0.0,
            m44: 1.0,
        }
    }
}

impl From<[[f32; 5]; 5]> for EraColorMatrix {
    fn from(value: [[f32; 5]; 5]) -> Self {
        EraColorMatrix {
            m00: value[0][0],
            m01: value[0][1],
            m02: value[0][2],
            m03: value[0][3],
            m04: value[0][4],
            m10: value[1][0],
            m11: value[1][1],
            m12: value[1][2],
            m13: value[1][3],
            m14: value[1][4],
            m20: value[2][0],
            m21: value[2][1],
            m22: value[2][2],
            m23: value[2][3],
            m24: value[2][4],
            m30: value[3][0],
            m31: value[3][1],
            m32: value[3][2],
            m33: value[3][3],
            m34: value[3][4],
            m40: value[4][0],
            m41: value[4][1],
            m42: value[4][2],
            m43: value[4][3],
            m44: value[4][4],
        }
    }
}

#[derive_ReprC]
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DiagnosticLevel {
    Error,
    Warning,
    Note,
    Help,
    Suggestion,
}

#[derive(Debug, Clone)]
pub struct DiagnosticEntry {
    pub level: DiagnosticLevel,
    /// The filename of the source file. Empty if the source file is not specified.
    pub filename: ArcStr,
    pub span: SrcSpan,
    pub message: String,
}

#[derive(Debug, Clone)]
/// A collection of diagnostics. Note that it is an error for a `Diagnostic` not to be
/// emitted or canceled before it is dropped, i.e. `Diagnostic` is linear.
pub struct Diagnostic<'a> {
    filename: ArcStr,
    src: &'a [u8],
    entries: Vec<DiagnosticEntry>,
}

impl<'a> Diagnostic<'a> {
    pub const fn new() -> Diagnostic<'static> {
        Diagnostic {
            filename: ArcStr::new(),
            src: &[],
            entries: Vec::new(),
        }
    }

    pub const fn with_file(filename: ArcStr) -> Diagnostic<'a> {
        Diagnostic {
            filename,
            src: &[],
            entries: Vec::new(),
        }
    }

    pub fn with_src(filename: ArcStr, src: &'a [u8]) -> Diagnostic<'a> {
        Diagnostic {
            filename,
            src,
            entries: Vec::new(),
        }
    }

    pub fn cancel(mut self) {
        self.entries.clear();
    }

    pub fn default_filename(&self) -> ArcStr {
        self.filename.clone()
    }

    pub fn span_entry(&mut self, mut entry: DiagnosticEntry) -> &mut Self {
        entry.filename = self.conv_filename(entry.filename);
        self.entries.push(entry);
        self
    }

    // NOTE: Use empty filename to refer to `self.filename`.
    pub fn span_msg(
        &mut self,
        filename: ArcStr,
        span: SrcSpan,
        level: DiagnosticLevel,
        message: impl Into<String>,
    ) -> &mut Self {
        self.span_entry(DiagnosticEntry {
            level,
            filename,
            span,
            message: message.into(),
        })
    }

    pub fn span_err(
        &mut self,
        filename: ArcStr,
        span: SrcSpan,
        message: impl Into<String>,
    ) -> &mut Self {
        self.span_msg(filename, span, DiagnosticLevel::Error, message)
    }

    pub fn span_warn(
        &mut self,
        filename: ArcStr,
        span: SrcSpan,
        message: impl Into<String>,
    ) -> &mut Self {
        self.span_msg(filename, span, DiagnosticLevel::Warning, message)
    }

    pub fn span_note(
        &mut self,
        filename: ArcStr,
        span: SrcSpan,
        message: impl Into<String>,
    ) -> &mut Self {
        self.span_msg(filename, span, DiagnosticLevel::Note, message)
    }

    pub fn emit_to<Callback: EraCompilerCallback>(self, ctx: &mut EraCompilerCtx<Callback>) {
        ctx.emit_diag(self);
    }

    fn conv_filename(&self, filename: ArcStr) -> ArcStr {
        if filename.is_empty() {
            self.filename.clone()
        } else {
            filename
        }
    }
}

impl Drop for Diagnostic<'_> {
    fn drop(&mut self) {
        // Is unwinding?
        if std::thread::panicking() {
            // Avoid potential double panic
            return;
        }
        assert!(
            self.entries.is_empty(),
            "diagnostics not emitted or canceled"
        );
    }
}

pub struct DiagnosticProvider<'a, 'src> {
    diag: &'a Diagnostic<'src>,
    src_map: Option<&'a FxIndexMap<ArcStr, EraSourceFile>>,
    resolver: Option<&'a ThreadedTokenInterner>,
}

impl<'a, 'src> DiagnosticProvider<'a, 'src> {
    pub fn new(
        diag: &'a Diagnostic<'src>,
        src_map: Option<&'a FxIndexMap<ArcStr, EraSourceFile>>,
        resolver: Option<&'a ThreadedTokenInterner>,
    ) -> Self {
        DiagnosticProvider {
            diag,
            src_map,
            resolver,
        }
    }

    pub fn get_entries(&self) -> &'a [DiagnosticEntry] {
        &self.diag.entries
    }

    // NOTE: Use empty filename to resolve from default file, if there is one.
    pub fn resolve_src_span(
        &self,
        filename: &str,
        span: SrcSpan,
    ) -> Option<DiagnosticResolveSrcSpanResult> {
        use bstr::ByteSlice;

        let src = if filename.is_empty() {
            if self.diag.filename.is_empty() {
                return None;
            }
            Cow::Borrowed(self.diag.src)
        } else if self.diag.filename.as_str() == filename && !self.diag.src.is_empty() {
            Cow::Borrowed(self.diag.src)
        } else {
            let input_span = span;
            let len = span.len();
            let src_file = self.src_map?.get(filename)?;

            if let (Some(final_root), Some(resolver)) = (&src_file.cst_root, self.resolver) {
                // Only resolve necessary lines from CST to improve performance.
                let final_root_len = {
                    let len = final_root.text_len().into();
                    let span = SrcSpan::new(SrcPos(len), 0);
                    src_file.macro_map.translate_span(span).start().0
                };
                // NOTE: We can guarantee that `start_line` never overflows.
                let start_line = src_file
                    .newline_pos
                    .partition_point(|&pos| pos <= span.start())
                    - 1;
                let end_line = src_file
                    .newline_pos
                    .partition_point(|&pos| pos <= span.end());
                let start_pos = src_file.newline_pos[start_line];
                let end_pos = src_file
                    .newline_pos
                    .get(end_line)
                    .copied()
                    .unwrap_or_else(|| SrcPos(final_root_len));
                let snippet_span = SrcSpan::with_ends(start_pos, end_pos);

                // HACK: Fix src pos from original to CST (should replace text to original instead)
                let input_span = src_file.macro_map.inverse_translate_span(input_span);
                let len = input_span.len();
                let snippet_span = src_file.macro_map.inverse_translate_span(snippet_span);

                // Find covering element first, then find the children elements.
                // if let Some(mut node) = src_file.final_root.as_ref() {
                let mut node = final_root;
                let mut covering_snippet = String::new();
                let mut span = SrcSpan::new(SrcPos(0), final_root_len);
                let mut covering_snippet_start_pos = SrcPos(0);
                if !span.contains_span(snippet_span) {
                    return None;
                }
                'outer: loop {
                    for i in node.children() {
                        let i_len = i.text_len().into();
                        span = SrcSpan::new(span.start(), i_len);
                        if span.contains_span(snippet_span) {
                            // Enter the covering element.
                            node = match i {
                                NodeOrToken::Node(n) => n,
                                NodeOrToken::Token(t) => {
                                    covering_snippet += t.resolve_text::<EraTokenKind, _>(resolver);
                                    break 'outer;
                                }
                            };
                            continue 'outer;
                        }
                        if span.intersects(snippet_span) {
                            if covering_snippet.is_empty() {
                                covering_snippet_start_pos = span.start();
                            }
                            i.write_display::<EraTokenKind, _>(resolver, &mut covering_snippet)
                                .unwrap();
                        } else if !covering_snippet.is_empty() {
                            break 'outer;
                        }
                        span = SrcSpan::new(span.end(), 0);
                    }
                }

                // Trim the covering snippet to the actual snippet.
                // if cfg!(debug_assertions) {
                //     println!(
                //         "covering_snippet: {:?}, len: {} -> {}",
                //         covering_snippet,
                //         covering_snippet.len(),
                //         snippet_span.len()
                //     );
                // }
                let covering_snippet_span =
                    SrcSpan::new(covering_snippet_start_pos, covering_snippet.len() as _);
                covering_snippet.replace_range(
                    (covering_snippet.len()
                        - ((covering_snippet_span.end().0 - snippet_span.end().0) as usize))..,
                    "",
                );
                covering_snippet.replace_range(
                    ..(snippet_span.start().0 - covering_snippet_start_pos.0) as usize,
                    "",
                );
                while covering_snippet.ends_with(['\r', '\n']) {
                    covering_snippet.pop();
                }

                let offset = (input_span.start().0 - snippet_span.start().0) as _;
                let loc = SrcLoc {
                    line: (start_line + 1) as _,
                    col: covering_snippet[..offset as usize].chars().count() as _,
                };

                return Some(DiagnosticResolveSrcSpanResult {
                    snippet: covering_snippet,
                    offset,
                    loc,
                    len,
                });
            } else if let Some(text) = &src_file.text {
                Cow::Borrowed(text.as_bytes())
            } else {
                // Must decompress the entire source text.
                let src = src_file.compressed_text.as_ref()?;
                Cow::Owned(lz4_flex::decompress_size_prepended(src).unwrap())
            }
        };

        let start = span.start().0 as usize;
        let end = span.end().0 as usize;
        let span_text = src.get(start..end)?;
        let snippet_start = src[..start]
            .rfind_byte(b'\n')
            .map(|idx| idx + 1)
            .unwrap_or(0);
        let snippet_end = src[end..]
            .find_byteset(b"\r\n")
            .map(|idx| end + idx)
            .unwrap_or(src.len());
        let snippet = src[snippet_start..snippet_end].to_str_lossy().into_owned();
        let offset = (start - snippet_start) as u32;
        let loc = SrcLoc {
            line: src[..start].split(|&x| x == b'\n').count() as u32,
            col: src[snippet_start..start].chars().count() as u32,
        };
        let len = span.len();

        Some(DiagnosticResolveSrcSpanResult {
            snippet,
            offset,
            loc,
            len,
        })
    }
}

pub struct DiagnosticResolveSrcSpanResult {
    /// Single line of source code containing the span.
    pub snippet: String,
    /// Offset into the snippet where the span starts.
    pub offset: u32,
    /// Location of the span.
    pub loc: SrcLoc,
    /// Length of the span.
    pub len: u32,
}

#[modular_bitfield::bitfield]
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct EraPrintExtendedFlags {
    pub is_single: bool,
    pub use_kana: bool,
    pub ignore_color: bool,
    pub is_line: bool,
    pub is_wait: bool,
    pub force_plain: bool,
    pub left_pad: bool,
    pub right_pad: bool,
}

#[modular_bitfield::bitfield]
#[repr(u8)]
#[derive(Debug, Clone, Copy)]
pub struct EraWaitFlags {
    pub any_key: bool,
    pub is_force: bool,
    #[skip]
    __: modular_bitfield::specifiers::B6,
}

#[modular_bitfield::bitfield]
#[repr(u8)]
#[derive(Debug, Clone, Copy)]
pub struct EraPadStringFlags {
    pub left_pad: bool,
    pub right_pad: bool,
    #[skip]
    __: modular_bitfield::specifiers::B6,
}

#[modular_bitfield::bitfield]
#[repr(u8)]
#[derive(Debug, Clone, Copy)]
pub struct EraInputExtendedFlags {
    pub is_string: bool,
    pub is_one: bool,
    pub is_timed: bool,
    pub has_default_value: bool,
    #[skip]
    __: modular_bitfield::specifiers::B4,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
pub struct IntValue {
    pub val: i64,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
pub struct StrValue {
    pub val: ArcStr,
}

// NOTE: Arr*Value are used as variable references
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ArrIntValue {
    pub vals: Vec<IntValue>,
    pub dims: EraVarDims,
    pub flags: EraValueFlags,
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ArrStrValue {
    pub vals: Vec<StrValue>,
    pub dims: EraVarDims,
    pub flags: EraValueFlags,
}

#[modular_bitfield::bitfield]
#[repr(u8)]
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct EraValueFlags {
    pub is_trap: bool,
    pub is_const: bool,
    pub is_charadata: bool,
    #[skip]
    __: modular_bitfield::specifiers::B5,
}

impl IntValue {
    #[inline]
    pub fn new(val: i64) -> Self {
        IntValue { val }
    }
}

impl StrValue {
    #[inline]
    pub fn new(val: ArcStr) -> Self {
        StrValue { val }
    }
}

// TODO: Compressed array values (extend on demand)?
impl ArrIntValue {
    #[inline]
    pub fn flat_get(&self, idx: usize) -> Option<&IntValue> {
        self.vals.get(idx)
    }
    #[inline]
    pub fn flat_get_mut(&mut self, idx: usize) -> Option<&mut IntValue> {
        self.vals.get_mut(idx)
    }
    #[inline]
    pub fn get(&self, idxs: &[u32]) -> Option<&IntValue> {
        let index = self.calc_idx(idxs)?;
        self.vals.get(index)
    }
    #[inline]
    pub fn get_mut(&mut self, idxs: &[u32]) -> Option<&mut IntValue> {
        let index = self.calc_idx(idxs)?;
        self.vals.get_mut(index)
    }
    #[inline]
    pub fn calc_idx(&self, idxs: &[u32]) -> Option<usize> {
        if idxs.len() > self.dims.len() {
            return None;
        }
        if idxs.iter().zip(self.dims.iter()).any(|(&a, &b)| a >= b) {
            return None;
        }
        let (_, idx) =
            self.dims
                .iter()
                .enumerate()
                .rev()
                .fold((1, 0), |(stride, idx), (i, &dim)| {
                    (
                        stride * (dim as usize),
                        idx + idxs.get(i).map(|&x| x as usize).unwrap_or(0) * stride,
                    )
                });
        Some(idx)
    }
    pub fn ensure_alloc(&mut self) {
        if self.vals.is_empty() {
            let size = self.dims.iter().fold(1, |acc, x| acc * (*x as usize));
            self.vals.resize(size, Default::default());
        }
    }
    pub fn clear_alloc(&mut self) {
        self.vals.clear();
        self.vals.shrink_to_fit();
    }
}
impl ArrStrValue {
    #[must_use]
    #[inline]
    pub fn flat_get(&self, idx: usize) -> Option<&StrValue> {
        self.vals.get(idx)
    }
    #[must_use]
    #[inline]
    pub fn flat_get_mut(&mut self, idx: usize) -> Option<&mut StrValue> {
        self.vals.get_mut(idx)
    }
    #[must_use]
    #[inline]
    pub fn get(&self, idxs: &[u32]) -> Option<&StrValue> {
        let index = self.calc_idx(idxs)?;
        self.vals.get(index)
    }
    #[must_use]
    #[inline]
    pub fn get_mut(&mut self, idxs: &[u32]) -> Option<&mut StrValue> {
        let index = self.calc_idx(idxs)?;
        self.vals.get_mut(index)
    }
    #[must_use]
    #[inline]
    pub fn calc_idx(&self, idxs: &[u32]) -> Option<usize> {
        if idxs.len() > self.dims.len() {
            return None;
        }
        if idxs.iter().zip(self.dims.iter()).any(|(&a, &b)| a >= b) {
            return None;
        }
        let (_, idx) =
            self.dims
                .iter()
                .enumerate()
                .rev()
                .fold((1, 0), |(stride, idx), (i, &dim)| {
                    (
                        stride * (dim as usize),
                        idx + idxs.get(i).map(|&x| x as usize).unwrap_or(0) * stride,
                    )
                });
        Some(idx)
    }
    pub fn ensure_alloc(&mut self) {
        if self.vals.is_empty() {
            let size = self.dims.iter().fold(1, |acc, x| acc * (*x as usize));
            self.vals.resize(size, Default::default());
        }
    }
    pub fn clear_alloc(&mut self) {
        self.vals.clear();
        self.vals.shrink_to_fit();
    }
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, strum::Display)]
pub enum ValueKind {
    Int,
    Str,
    ArrInt,
    ArrStr,
}

impl ValueKind {
    pub fn without_arr(&self) -> Self {
        use ValueKind::*;
        match self {
            Int | ArrInt => Int,
            Str | ArrStr => Str,
        }
    }
    pub fn with_arr(&self) -> Self {
        use ValueKind::*;
        match self {
            Int | ArrInt => ArrInt,
            Str | ArrStr => ArrStr,
        }
    }
    pub fn is_arr(&self) -> bool {
        use ValueKind::*;
        match self {
            ArrInt | ArrStr => true,
            Int | Str => false,
        }
    }
    pub fn is_int(&self) -> bool {
        matches!(self, ValueKind::Int | ValueKind::ArrInt)
    }
    pub fn is_str(&self) -> bool {
        matches!(self, ValueKind::Str | ValueKind::ArrStr)
    }
    pub fn to_scalar(&self) -> ScalarValueKind {
        use ValueKind::*;
        match self {
            Int | ArrInt => ScalarValueKind::Int,
            Str | ArrStr => ScalarValueKind::Str,
        }
    }
}

//pub type Value = ptr_union::Union4<Box<IntValue>, Box<StrValue>, Box<ArrValue>, Box<()>>;
// pub type ValueInner = ptr_union::Union4<
//     Rc<IntValue>,
//     Rc<StrValue>,
//     Rc<RefCell<ArrIntValue>>,
//     Rc<RefCell<ArrStrValue>>,
// >;
pub type ValueInner = FlatValue;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Value(ValueInner);
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FlatValue {
    Int(IntValue),
    Str(StrValue),
    ArrInt(Rc<RefCell<ArrIntValue>>),
    ArrStr(Rc<RefCell<ArrStrValue>>),
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RefFlatValue<'a> {
    Int(&'a IntValue),
    Str(&'a StrValue),
    ArrInt(&'a Rc<RefCell<ArrIntValue>>),
    ArrStr(&'a Rc<RefCell<ArrStrValue>>),
}
impl RefFlatValue<'_> {
    pub fn kind(&self) -> ValueKind {
        use RefFlatValue::*;
        match self {
            Int(_) => ValueKind::Int,
            Str(_) => ValueKind::Str,
            ArrInt(_) => ValueKind::ArrInt,
            ArrStr(_) => ValueKind::ArrStr,
        }
    }
}

impl Value {
    pub fn inner_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        use std::hash::Hash;

        match self.as_unpacked() {
            RefFlatValue::Int(x) => x.val.hash(state),
            RefFlatValue::Str(x) => x.val.hash(state),
            RefFlatValue::ArrInt(x) => x.borrow().vals.hash(state),
            RefFlatValue::ArrStr(x) => x.borrow().vals.hash(state),
        }
    }
}

impl Value {
    #[inline]
    pub fn new_int(val: i64) -> Self {
        Value(ValueInner::Int(IntValue { val }))
    }
    #[inline]
    pub fn new_str(val: ArcStr) -> Self {
        Value(ValueInner::Str(StrValue { val }))
    }
    #[inline]
    pub fn new_int_obj(val: IntValue) -> Self {
        Value(ValueInner::Int(val))
    }
    #[inline]
    pub fn new_str_obj(val: StrValue) -> Self {
        Value(ValueInner::Str(val))
    }
    pub fn new_int_arr(mut dims: EraVarDims, mut vals: Vec<IntValue>) -> Self {
        if dims.is_empty() {
            dims.push(1);
        }
        let size = dims.iter().fold(1, |acc, x| acc * (*x as usize));
        //assert_ne!(size, 0, "dimension must not contain zero");
        if !vals.is_empty() {
            vals.resize(size, Default::default());
        }
        Value(ValueInner::ArrInt(Rc::new(RefCell::new(ArrIntValue {
            vals,
            dims,
            flags: EraValueFlags::new(),
        }))))
    }
    pub fn new_str_arr(mut dims: EraVarDims, mut vals: Vec<StrValue>) -> Self {
        if dims.is_empty() {
            dims.push(1);
        }
        let size = dims.iter().fold(1, |acc, x| acc * (*x as usize));
        //assert_ne!(size, 0, "dimension must not contain zero");
        if !vals.is_empty() {
            vals.resize(size, Default::default());
        }
        Value(ValueInner::ArrStr(Rc::new(RefCell::new(ArrStrValue {
            vals,
            dims,
            flags: EraValueFlags::new(),
        }))))
    }
    pub fn new_int_0darr(val: i64) -> Self {
        Self::new_int_arr(smallvec::smallvec![1], vec![IntValue { val }])
    }
    pub fn new_str_0darr(val: ArcStr) -> Self {
        Self::new_str_arr(smallvec::smallvec![1], vec![StrValue { val }])
    }
    pub fn into_unpacked(self) -> FlatValue {
        use ptr_union::Enum4::*;
        use FlatValue::*;
        match self.0 {
            Int(x) => Int(x),
            Str(x) => Str(x),
            ArrInt(x) => ArrInt(x),
            ArrStr(x) => ArrStr(x),
        }
    }
    pub fn as_unpacked(&self) -> RefFlatValue {
        use FlatValue::*;
        match &self.0 {
            Int(x) => RefFlatValue::Int(x),
            Str(x) => RefFlatValue::Str(x),
            ArrInt(x) => RefFlatValue::ArrInt(x),
            ArrStr(x) => RefFlatValue::ArrStr(x),
        }
    }
    pub fn deep_clone(&self) -> Self {
        // use ptr_union::Enum4::*;
        // match self.0.clone().unpack() {
        //     A(x) => Value(ValueInner::new_a(x.deref().clone().into()).unwrap()),
        //     B(x) => Value(ValueInner::new_b(x.deref().clone().into()).unwrap()),
        //     C(x) => Value(ValueInner::new_c(x.deref().clone().into()).unwrap()),
        //     D(x) => Value(ValueInner::new_d(x.deref().clone().into()).unwrap()),
        // }
        self.clone().into_unpacked().deep_clone().into_packed()
    }
    pub fn kind(&self) -> ValueKind {
        self.as_unpacked().kind()
    }
    pub fn is_default(&self) -> bool {
        use RefFlatValue::*;
        match self.as_unpacked() {
            Int(x) => x.val == 0,
            Str(x) => x.val.is_empty(),
            ArrInt(x) => x.borrow().vals.iter().all(|x| x.val == 0),
            ArrStr(x) => x.borrow().vals.iter().all(|x| x.val.is_empty()),
        }
    }
    pub fn as_int(&self) -> Option<&IntValue> {
        match self.as_unpacked() {
            RefFlatValue::Int(x) => Some(x),
            _ => None,
        }
    }
    pub fn as_str(&self) -> Option<&StrValue> {
        match self.as_unpacked() {
            RefFlatValue::Str(x) => Some(x),
            _ => None,
        }
    }
    pub fn as_arrint(&self) -> Option<&Rc<RefCell<ArrIntValue>>> {
        match self.as_unpacked() {
            RefFlatValue::ArrInt(x) => Some(x),
            _ => None,
        }
    }
    pub fn as_arrstr(&self) -> Option<&Rc<RefCell<ArrStrValue>>> {
        match self.as_unpacked() {
            RefFlatValue::ArrStr(x) => Some(x),
            _ => None,
        }
    }
    pub fn dims(&self) -> Option<EraVarDims> {
        match self.as_unpacked() {
            RefFlatValue::ArrInt(x) => Some(x.borrow().dims.clone()),
            RefFlatValue::ArrStr(x) => Some(x.borrow().dims.clone()),
            _ => None,
        }
    }
    pub fn dims_cnt(&self) -> Option<usize> {
        match self.as_unpacked() {
            RefFlatValue::ArrInt(x) => Some(x.borrow().dims.len()),
            RefFlatValue::ArrStr(x) => Some(x.borrow().dims.len()),
            _ => None,
        }
    }
    pub fn ensure_alloc(&self) {
        match self.as_unpacked() {
            RefFlatValue::ArrInt(x) => x.borrow_mut().ensure_alloc(),
            RefFlatValue::ArrStr(x) => x.borrow_mut().ensure_alloc(),
            _ => {}
        }
    }
    pub fn clear_alloc(&self) {
        match self.as_unpacked() {
            RefFlatValue::ArrInt(x) => x.borrow_mut().clear_alloc(),
            RefFlatValue::ArrStr(x) => x.borrow_mut().clear_alloc(),
            _ => {}
        }
    }
}
impl FlatValue {
    pub fn into_packed(self) -> Value {
        use FlatValue::*;
        Value(match self {
            Int(x) => ValueInner::Int(x),
            Str(x) => ValueInner::Str(x),
            ArrInt(x) => ValueInner::ArrInt(x),
            ArrStr(x) => ValueInner::ArrStr(x),
        })
    }
    pub fn deep_clone(&self) -> Self {
        use FlatValue::*;
        match self {
            // NOTE: We cannot mutate IntValue and StrValue, so it is safe to perform
            //       shallow copies on them
            // Int(x) => Int(x.deref().clone().into()),
            // Str(x) => Str(x.deref().clone().into()),
            Int(x) => Int(x.clone()),
            Str(x) => Str(x.clone()),
            ArrInt(x) => ArrInt(x.deref().clone().into()),
            ArrStr(x) => ArrStr(x.deref().clone().into()),
        }
    }
    pub fn kind(&self) -> ValueKind {
        match self {
            FlatValue::Int(_) => ValueKind::Int,
            FlatValue::Str(_) => ValueKind::Str,
            FlatValue::ArrInt(_) => ValueKind::ArrInt,
            FlatValue::ArrStr(_) => ValueKind::ArrStr,
        }
    }
}

pub type EraVarDims = smallvec::SmallVec<[u32; 2]>;

#[derive(Debug, Default)]
pub struct EraVarPool {
    /// Mapping from names to indices.
    var_names: HashMap<Ascii<ArcStr>, usize>,
    chara_var_idxs: Vec<usize>,
    normal_var_idxs: Vec<usize>,
    vars: Vec<EraVarInfo>,
    /// Initial values for variables. Used when resetting variables.
    init_vars: Vec<(usize, Value)>,
}

#[derive(Debug, Clone)]
pub struct EraVarInfo {
    pub name: Ascii<ArcStr>,
    pub val: Value,
    // TODO: Compress with modular-bitfield
    pub is_const: bool,
    pub is_charadata: bool,
    pub is_global: bool,
    pub never_trap: bool,
}

impl EraVarInfo {
    pub fn new(name: ArcStr, val: Value) -> Self {
        EraVarInfo {
            name: Ascii::new(name),
            val,
            is_const: false,
            is_charadata: false,
            is_global: false,
            never_trap: false,
        }
    }
}

impl EraVarPool {
    pub fn new() -> Self {
        EraVarPool {
            var_names: HashMap::new(),
            chara_var_idxs: Vec::new(),
            normal_var_idxs: Vec::new(),
            vars: Vec::new(),
            init_vars: Vec::new(),
        }
    }

    #[must_use]
    pub fn add_var(&mut self, mut info: EraVarInfo) -> Option<usize> {
        use hashbrown::hash_map::RawEntryMut;

        let var_idx = self.vars.len();
        match self.var_names.raw_entry_mut().from_key(&info.name) {
            RawEntryMut::Occupied(_) => return None,
            RawEntryMut::Vacant(e) => {
                e.insert(info.name.clone(), var_idx);
            }
        }

        if !info.is_const && !info.val.is_default() {
            // When resetting, we need to keep original variable values
            self.init_vars.push((var_idx, info.val.clone()));
            info.val = info.val.deep_clone();
        }

        if info.is_charadata {
            self.chara_var_idxs.push(var_idx);
        } else {
            self.normal_var_idxs.push(var_idx);
        }

        self.vars.push(info);

        Some(var_idx)
    }

    #[must_use]
    pub fn add_var_force(&mut self, info: EraVarInfo) -> (usize, bool) {
        let is_replaced;
        let (var_idx, info) = match self.var_names.entry(info.name.clone()) {
            hashbrown::hash_map::Entry::Occupied(e) => {
                // Update existing variable
                is_replaced = true;

                let var_idx = *e.get();
                self.init_vars.retain(|x| x.0 != var_idx);
                self.chara_var_idxs.retain(|&x| x != var_idx);
                self.normal_var_idxs.retain(|&x| x != var_idx);
                (var_idx, &mut self.vars[var_idx])
            }
            hashbrown::hash_map::Entry::Vacant(e) => {
                // Add new variable
                is_replaced = false;

                let var_idx = self.vars.len();
                e.insert(var_idx);
                self.vars.push(info);
                (var_idx, &mut self.vars[var_idx])
            }
        };

        if !info.is_const && !info.val.is_default() {
            // When resetting, we need to keep original variable values
            self.init_vars.push((var_idx, info.val.clone()));
            info.val = info.val.deep_clone();
        }

        if info.is_charadata {
            self.chara_var_idxs.push(var_idx);
        } else {
            self.normal_var_idxs.push(var_idx);
        }

        (var_idx, is_replaced)
    }

    #[must_use]
    pub fn get_var(&self, name: &str) -> Option<&Value> {
        self.var_names
            .get(Ascii::new_str(name))
            .map(|x| &self.vars[*x].val)
    }

    #[must_use]
    pub fn get_var_mut(&mut self, name: &str) -> Option<&mut Value> {
        self.var_names
            .get_mut(Ascii::new_str(name))
            .map(|x| &mut self.vars[*x].val)
    }

    #[must_use]
    pub fn get_var_by_idx(&mut self, idx: usize) -> Option<&Value> {
        self.vars.get(idx).map(|x| &x.val)
    }

    #[must_use]
    pub fn get_var_by_idx_mut(&mut self, idx: usize) -> Option<&mut Value> {
        self.vars.get_mut(idx).map(|x| &mut x.val)
    }

    #[must_use]
    pub fn get_var_idx(&self, name: &str) -> Option<usize> {
        self.var_names.get(Ascii::new_str(name)).copied()
    }

    #[must_use]
    pub fn get_var_info(&self, idx: usize) -> Option<&EraVarInfo> {
        self.vars.get(idx)
    }

    #[must_use]
    pub fn get_var_info_by_name(&self, name: &str) -> Option<&EraVarInfo> {
        self.get_var_idx(name).and_then(|idx| self.vars.get(idx))
    }

    pub fn iter(&self) -> impl Iterator<Item = &EraVarInfo> {
        self.vars.iter()
    }

    pub fn chara_vars_iter(&self) -> impl Iterator<Item = &EraVarInfo> {
        self.chara_var_idxs.iter().map(|&x| &self.vars[x])
    }

    pub fn normal_vars_iter(&self) -> impl Iterator<Item = &EraVarInfo> {
        self.normal_var_idxs.iter().map(|&x| &self.vars[x])
    }
}

pub enum EraCmdArgFmt {
    /// Interprets the argument as a string expression, then parses its content as string form.
    ExpressionSForm,
    /// Interprets the argument as string expressions.
    ExpressionS,
    /// Interprets the argument as expressions.
    Expression,
    /// Interprets the argument as a raw string.
    RawString,
    /// Interprets the argument as a raw string form.
    RawStringForm,
}

#[derive(Debug, Default, Clone)]
pub struct EraCharaInitTemplate {
    pub no: u32,
    pub csv_no: u32,
    pub name: ArcStr,
    pub callname: ArcStr,
    pub nickname: ArcStr,
    pub mastername: ArcStr,
    pub maxbase: BTreeMap<u32, i64>,
    pub mark: BTreeMap<u32, i64>,
    pub exp: BTreeMap<u32, i64>,
    pub abl: BTreeMap<u32, i64>,
    pub talent: BTreeMap<u32, i64>,
    pub relation: BTreeMap<u32, i64>,
    pub cflag: BTreeMap<u32, i64>,
    pub equip: BTreeMap<u32, i64>,
    pub juel: BTreeMap<u32, i64>,
    pub cstr: BTreeMap<u32, ArcStr>,
}

#[repr(u8)]
#[derive(num_derive::FromPrimitive, num_derive::ToPrimitive, Debug, Clone, Copy, PartialEq, Eq)]
pub enum EraCharaCsvPropType {
    CsvName,
    CsvCallName,
    CsvNickName,
    CsvMasterName,
    CsvBase,
    CsvCStr,
    CsvAbl,
    CsvTalent,
    CsvMark,
    CsvExp,
    CsvRelation,
    CsvJuel,
    CsvEquip,
    CsvCFlag,
}
impl EraCharaCsvPropType {
    // pub fn from_i(value: u8) -> Self {
    //     num_traits::FromPrimitive::from_u8(value).unwrap_or(Self::Invalid)
    // }
    pub fn try_from_i(value: u8) -> Option<Self> {
        num_traits::FromPrimitive::from_u8(value)
    }
    pub fn to_i(self) -> u8 {
        num_traits::ToPrimitive::to_u8(&self).unwrap()
    }
    pub fn try_from_var(var: &str) -> Option<Self> {
        use EraCharaCsvPropType::*;
        let is = |x| var.eq_ignore_ascii_case(x);
        Some(if is("BASE") || is("MAXBASE") {
            CsvBase
        } else if is("CSTR") {
            CsvCStr
        } else if is("ABL") {
            CsvAbl
        } else if is("TALENT") {
            CsvTalent
        } else if is("MARK") {
            CsvMark
        } else if is("EXP") {
            CsvExp
        } else if is("RELATION") {
            CsvRelation
        } else if is("JUEL") {
            CsvJuel
        } else if is("EQUIP") {
            CsvEquip
        } else if is("CFLAG") {
            CsvCFlag
        } else {
            return None;
        })
    }
}

#[repr(u8)]
#[derive(num_derive::FromPrimitive, num_derive::ToPrimitive, Debug, Clone, Copy, PartialEq, Eq)]
pub enum EraCsvVarKind {
    CsvAbl,
    CsvExp,
    CsvTalent,
    CsvPalam,
    CsvTrain,
    CsvMark,
    CsvItem,
    CsvBase,
    CsvSource,
    CsvEx,
    CsvStr,
    CsvEquip,
    CsvTEquip,
    CsvFlag,
    CsvTFlag,
    CsvCFlag,
    CsvTCVar,
    CsvCStr,
    CsvStain,
    //CsvStrName,
    CsvTStr,
    CsvSaveStr,
    CsvGlobal,
    CsvGlobals,
}
impl EraCsvVarKind {
    // pub fn from_i(value: u8) -> Self {
    //     num_traits::FromPrimitive::from_u8(value).unwrap_or(Self::Invalid)
    // }
    pub fn try_from_i(value: u8) -> Option<Self> {
        num_traits::FromPrimitive::from_u8(value)
    }
    pub fn to_i(self) -> u8 {
        num_traits::ToPrimitive::to_u8(&self).unwrap()
    }
    pub fn try_from_var(var: &str) -> Option<Self> {
        use EraCsvVarKind::*;
        let is = |x| var.eq_ignore_ascii_case(x);
        Some(if is("ABL") {
            CsvAbl
        } else if is("EXP") {
            CsvExp
        } else if is("TALENT") {
            CsvTalent
        } else if is("PALAM")
            || is("JUEL")
            || is("GOTJUEL")
            || is("UP")
            || is("DOWN")
            || is("CUP")
            || is("CDOWN")
        {
            CsvPalam
        } else if is("TRAIN") {
            CsvTrain
        } else if is("MARK") {
            CsvMark
        } else if is("ITEM") || is("ITEMPRICE") || is("ITEMSALES") {
            CsvItem
        } else if is("BASE") || is("MAXBASE") || is("DOWNBASE") || is("LOSEBASE") {
            CsvBase
        } else if is("SOURCE") {
            CsvSource
        } else if is("EX") || is("NOWEX") {
            CsvEx
        } else if is("STR") {
            CsvStr
        } else if is("EQUIP") {
            CsvEquip
        } else if is("TEQUIP") {
            CsvTEquip
        } else if is("FLAG") {
            CsvFlag
        } else if is("TFLAG") {
            CsvTFlag
        } else if is("CFLAG") {
            CsvCFlag
        } else if is("TCVAR") {
            CsvTCVar
        } else if is("CSTR") {
            CsvCStr
        } else if is("STAIN") {
            CsvStain
        } else if is("TSTR") {
            CsvTStr
        } else if is("SAVESTR") {
            CsvSaveStr
        } else if is("GLOBAL") {
            CsvGlobal
        } else if is("GLOBALS") {
            CsvGlobals
        } else {
            return None;
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ScalarValue {
    Int(i64),
    Str(String),
    Void,
    /// A special value that represents a omitted argument. Usually it can
    /// coerce to the default value of Int or Str.
    Empty,
}

impl ScalarValue {
    pub fn kind(&self) -> ScalarValueKind {
        match self {
            ScalarValue::Int(_) => ScalarValueKind::Int,
            ScalarValue::Str(_) => ScalarValueKind::Str,
            ScalarValue::Void => ScalarValueKind::Void,
            ScalarValue::Empty => ScalarValueKind::Empty,
        }
    }

    pub fn is_int(&self) -> bool {
        self.kind().is_int()
    }

    pub fn is_str(&self) -> bool {
        self.kind().is_str()
    }

    pub fn is_void(&self) -> bool {
        self.kind().is_void()
    }

    pub fn is_empty(&self) -> bool {
        self.kind().is_empty()
    }

    pub fn is_value(&self) -> bool {
        self.kind().is_value()
    }

    pub fn coerce_int(self) -> Self {
        if self.is_empty() {
            ScalarValue::Int(0)
        } else {
            self
        }
    }

    pub fn coerce_str(self) -> Self {
        if self.is_empty() {
            ScalarValue::Str(String::new())
        } else {
            self
        }
    }

    pub fn as_int(&self) -> Option<i64> {
        match self {
            ScalarValue::Int(x) => Some(*x),
            _ => None,
        }
    }

    pub fn as_str(&self) -> Option<&str> {
        match self {
            ScalarValue::Str(x) => Some(x),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, strum::Display)]
pub enum ScalarValueKind {
    Int,
    Str,
    Void,
    Empty,
}

impl ScalarValueKind {
    pub fn is_int(&self) -> bool {
        matches!(self, ScalarValueKind::Int)
    }

    pub fn is_str(&self) -> bool {
        matches!(self, ScalarValueKind::Str)
    }

    pub fn is_void(&self) -> bool {
        matches!(self, ScalarValueKind::Void)
    }

    pub fn is_empty(&self) -> bool {
        matches!(self, ScalarValueKind::Empty)
    }

    pub fn is_value(&self) -> bool {
        !matches!(self, ScalarValueKind::Void | ScalarValueKind::Empty)
    }
}

pub trait CstreeNodeOrTokenExt<N, T> {
    fn to_cloned(&self) -> cstree::util::NodeOrToken<N, T>;
}

impl<N: Clone, T: Clone> CstreeNodeOrTokenExt<N, T> for cstree::util::NodeOrToken<&N, &T> {
    fn to_cloned(&self) -> cstree::util::NodeOrToken<N, T> {
        match *self {
            cstree::util::NodeOrToken::Node(x) => cstree::util::NodeOrToken::Node(x.clone()),
            cstree::util::NodeOrToken::Token(x) => cstree::util::NodeOrToken::Token(x.clone()),
        }
    }
}

pub trait CstreeNodeOrTokenExt2 {
    fn src_span(&self) -> SrcSpan;
    fn resolve_text<R>(&self, resolver: &R) -> String
    where
        R: cstree::interning::Resolver<cstree::interning::TokenKey> + ?Sized;
}

impl<'a, S: cstree::Syntax, D> CstreeNodeOrTokenExt2 for SyntaxElementRef<'a, S, D> {
    fn src_span(&self) -> SrcSpan {
        match self {
            cstree::util::NodeOrToken::Node(x) => x.text_range().into(),
            cstree::util::NodeOrToken::Token(x) => x.text_range().into(),
        }
    }

    fn resolve_text<R>(&self, resolver: &R) -> String
    where
        R: cstree::interning::Resolver<cstree::interning::TokenKey> + ?Sized,
    {
        self.display(resolver)
    }
}

impl<S: cstree::Syntax, D> CstreeNodeOrTokenExt2 for SyntaxElement<S, D> {
    fn src_span(&self) -> SrcSpan {
        self.text_range().into()
    }

    fn resolve_text<R>(&self, resolver: &R) -> String
    where
        R: cstree::interning::Resolver<cstree::interning::TokenKey> + ?Sized,
    {
        self.display(resolver)
    }
}

pub trait CstreeGreenTokenExt {
    fn resolve_text<'r, S: Syntax, R>(&self, resolver: &'r R) -> &'r str
    where
        R: cstree::interning::Resolver<cstree::interning::TokenKey> + ?Sized;
}

impl CstreeGreenTokenExt for cstree::green::GreenToken {
    fn resolve_text<'r, S: Syntax, R>(&self, resolver: &'r R) -> &'r str
    where
        R: cstree::interning::Resolver<cstree::interning::TokenKey> + ?Sized,
    {
        let kind = S::from_raw(self.kind());
        kind.static_text().or_else(|| self.text(resolver)).unwrap()
    }
}

pub trait CstreeGreenExt {
    fn write_display<'r, S: Syntax, R>(
        &self,
        resolver: &'r R,
        target: &mut impl std::fmt::Write,
    ) -> std::fmt::Result
    where
        R: cstree::interning::Resolver<cstree::interning::TokenKey> + ?Sized;

    fn display<'r, S: Syntax, R>(&self, resolver: &'r R) -> String
    where
        R: cstree::interning::Resolver<cstree::interning::TokenKey> + ?Sized,
    {
        let mut buf = String::new();
        self.write_display::<S, _>(resolver, &mut buf).unwrap();
        buf
    }
}

impl CstreeGreenExt for cstree::green::GreenToken {
    fn write_display<'r, S: Syntax, R>(
        &self,
        resolver: &'r R,
        target: &mut impl std::fmt::Write,
    ) -> std::fmt::Result
    where
        R: cstree::interning::Resolver<cstree::interning::TokenKey> + ?Sized,
    {
        target.write_str(self.resolve_text::<S, _>(resolver))
    }
}

impl CstreeGreenExt for cstree::green::GreenNode {
    fn write_display<'r, S: Syntax, R>(
        &self,
        resolver: &'r R,
        target: &mut impl std::fmt::Write,
    ) -> std::fmt::Result
    where
        R: cstree::interning::Resolver<cstree::interning::TokenKey> + ?Sized,
    {
        for child in self.children() {
            match child {
                NodeOrToken::Node(x) => x.write_display::<S, _>(resolver, target)?,
                NodeOrToken::Token(x) => x.write_display::<S, _>(resolver, target)?,
            }
        }
        Ok(())
    }
}

impl CstreeGreenExt for NodeOrToken<cstree::green::GreenNode, cstree::green::GreenToken> {
    fn write_display<'r, S: Syntax, R>(
        &self,
        resolver: &'r R,
        target: &mut impl std::fmt::Write,
    ) -> std::fmt::Result
    where
        R: cstree::interning::Resolver<cstree::interning::TokenKey> + ?Sized,
    {
        match self {
            NodeOrToken::Node(x) => x.write_display::<S, _>(resolver, target),
            NodeOrToken::Token(x) => x.write_display::<S, _>(resolver, target),
        }
    }
}

impl CstreeGreenExt for NodeOrToken<&cstree::green::GreenNode, &cstree::green::GreenToken> {
    fn write_display<'r, S: Syntax, R>(
        &self,
        resolver: &'r R,
        target: &mut impl std::fmt::Write,
    ) -> std::fmt::Result
    where
        R: cstree::interning::Resolver<cstree::interning::TokenKey> + ?Sized,
    {
        match self {
            NodeOrToken::Node(x) => x.write_display::<S, _>(resolver, target),
            NodeOrToken::Token(x) => x.write_display::<S, _>(resolver, target),
        }
    }
}

pub trait CstreeElementExt {
    fn src_span(&self) -> SrcSpan;
}

impl<S: Syntax> CstreeElementExt for crate::util::syntax::SyntaxElement<S> {
    fn src_span(&self) -> SrcSpan {
        use crate::util::syntax::SyntaxElementExt;
        self.text_range().into()
    }
}

impl<S: Syntax> CstreeElementExt for crate::util::syntax::SyntaxElementRef<'_, S> {
    fn src_span(&self) -> SrcSpan {
        use crate::util::syntax::SyntaxElementExt;
        self.text_range().into()
    }
}

#[derive_ReprC]
#[repr(u8)]
#[derive(num_derive::FromPrimitive, num_derive::ToPrimitive, Debug, Clone, Copy)]
pub enum EraAlignmentKind {
    Left,
    Center,
    Right,
}

#[derive_ReprC]
#[repr(u8)]
#[derive(num_derive::FromPrimitive, num_derive::ToPrimitive, Debug, Clone, Copy)]
pub enum EraPriBytecode {
    /// Fails the execution with a message, stored on stack.
    FailWithMsg,
    /// Issues a debug interrupt for current control flow.
    DebugBreak,
    /// Ends the execution normally.
    Quit,
    /// Fails the execution with a message, stored on stack.
    Throw,
    /// No operation.
    Nop,
    ReturnVoid,
    ReturnInt,
    ReturnStr,
    /// `(args..., func_idx: Int) -> return value of function`
    ///
    /// `[imm8: count of arguments]`
    CallFun,
    /// `(args..., func_name: Str) -> function exists`
    ///
    /// `[imm8: count of arguments]`
    TryCallFun,
    /// `(args..., func_name: Str) -> Void`
    ///
    /// `[imm8: count of arguments]`
    TryCallFunForce,
    /// `(func_idx: Int) -> Void`
    ///
    /// Resets the current call stack, then restarts the execution at
    /// the specified function.
    ///
    /// WARN: The function must not take any arguments.
    /// Failure to do so will result in unspecified behavior.
    RestartExecAtFun,
    JumpWW,
    /// Jumps if the top of the stack is a truthy integer.
    JumpIfWW,
    /// Jumps if the top of the stack is a falsy integer.
    JumpIfNotWW,
    /// Pushes a constant string (from interner) onto the stack.
    LoadConstStr,
    LoadImm8,
    LoadImm16,
    /// Pushes a constant integer onto the stack, encoded in the instruction. Sign-extended.
    LoadImm32,
    /// Pushes a constant integer onto the stack, encoded in the instruction.
    LoadImm64,
    /// `[imm32: global variable index]`
    ///
    /// Pushes a variable from global frame onto the stack, encoded in the instruction.
    LoadVarWW,
    /// `[imm32: interned variable index]`
    ///
    /// Like `LoadVar`, but deep clones the value. Usually used for constant values.
    /// For example, `#DIM DYNAMIC x = 1, 2, 3` will cause the compiler to add
    /// an interned `[1, 2, 3]` into the variable pool, then issue a `LoadConstVar`.
    LoadConstVarWW,
    /// `[imm8: local variable index]`
    ///
    /// Pushes a variable from local frame onto the stack, encoded in the instruction.
    LoadLocalVar,
    Pop,
    /// `[imm8: count to pop]`
    PopAllN,
    /// `[imm8: index to pop, from stack top]`
    PopOneN,
    Swap2,
    Duplicate,
    /// `[imm8: count of duplicates]`
    DuplicateAllN,
    /// `[imm8: index of duplicate, from stack top]`
    DuplicateOneN,
    AddInt,
    SubInt,
    MulInt,
    DivInt,
    ModInt,
    NegInt,
    BitAndInt,
    BitOrInt,
    BitXorInt,
    BitNotInt,
    ShlInt,
    ShrInt,
    CmpIntLT,
    CmpIntLEq,
    CmpIntGT,
    CmpIntGEq,
    CmpIntEq,
    CmpIntNEq,
    CmpStrLT,
    CmpStrLEq,
    CmpStrGT,
    CmpStrGEq,
    CmpStrEq,
    CmpStrNEq,
    LogicalNot,
    MaxInt,
    MinInt,
    ClampInt,
    InRangeInt,
    InRangeStr,
    GetBit,
    SetBit,
    ClearBit,
    InvertBit,
    /// `(strings...: Str) -> (str: Str)`
    ///
    /// `[imm8: count of strings]`
    ///
    /// Concatenates strings on the stack.
    BuildString,
    /// `(str: Str, width: Int) -> (str: Str)`
    ///
    /// `[imm8: EraPadStringFlags]`
    PadString,
    /// `(str: Str, count: Int) -> (str: Str)`
    RepeatStr,
    /// `[imm8: count of indices]`
    BuildArrIdxFromMD,
    /// `(arr: Arr, idx: Int) -> (val: Any)`
    GetArrValFlat,
    /// `(arr: Arr, idx: Int, val: Any) -> (arr: Arr)`
    SetArrValFlat,
    /// `(val: Int, factor_float_encoded: Int) -> (val: Int)`
    TimesFloat,
    /// `(func_name: Str) -> (func_exists: Int)`
    ///
    /// Whether the function exists.
    FunExists,
    ReplaceStr,
    SubStr,
    SubStrU,
    StrFind,
    StrFindU,
    StrLen,
    StrLenU,
    CountSubStr,
    StrCharAtU,
    IntToStr,
    StrToInt,
    FormatIntToStr,
    StrIsValidInt,
    StrToUpper,
    StrToLower,
    /// `(str: Str) -> (str: Str)`
    ///
    /// Converts characters in the string to half-width, if they have a full-width equivalent.
    StrToHalf,
    /// `(str: Str) -> (str: Str)`
    ///
    /// Converts characters in the string to full-width, if they have a half-width equivalent.
    StrToFull,
    BuildBarStr,
    EscapeRegexStr,
    /// `(str: Str, pos: Int) -> (ch: Int)`
    ///
    /// Returns the Unicode code point of the character at the specified position.
    EncodeToUnicode,
    UnicodeToStr,
    IntToStrWithBase,
    HtmlTagSplit,
    HtmlToPlainText,
    HtmlEscape,
    PowerInt,
    SqrtInt,
    CbrtInt,
    LogInt,
    Log10Int,
    ExponentInt,
    AbsInt,
    SignInt,
    GroupMatch,
    ArrayCountMatches,
    CArrayCountMatches,
    SumArray,
    SumCArray,
    MaxArray,
    MaxCArray,
    MinArray,
    MinCArray,
    InRangeArray,
    InRangeCArray,
    ArrayRemove,
    ArraySortAsc,
    ArraySortDesc,
    ArrayMSort,
    ArrayCopy,
    ArrayShift,
    // -----
    Print,
    PrintLine,
    /// `[imm8: EraPrintExtendedFlags]`
    PrintExtended,
    ReuseLastLine,
    ClearLine,
    /// `[imm8: EraWaitFlags]`
    Wait,
    TWait,
    /// `[imm8: EraInputExtendedFlags]`
    Input,
    KbGetKeyState, // Returns i64 with b15 = <key down>, b0 = <key triggered>
    /// `() -> (func_name: Str)`
    ///
    /// Get the name of the caller function. Returns an empty string if current function
    /// is the only one in the call stack.
    GetCallerFunName,
    GetCharaNum,
    CsvGetNum,
    GetRandomRange,
    GetRandomMax,
    /// Extra (fused) operations group #1. Introduced for performance reasons.
    ExtOp1,
}

#[derive_ReprC]
#[repr(u8)]
#[derive(num_derive::FromPrimitive, num_derive::ToPrimitive, Debug, Clone, Copy)]
pub enum EraExtBytecode1 {
    RowAssign,
    /// `(var: Arr, flat_idx: Int, end: Int, step: Int) -> (Self, do_next: Int)`
    ///
    /// Steps the current FOR loop, and returns whether the loop should continue.
    ForLoopStep,
    ExtendStrToWidth,
    HtmlPrint,
    PrintButton,
    PrintImg,
    // PrintImg2,
    // PrintImg3,
    PrintImg4,
    SplitString,
    GCreate,
    GCreateFromFile,
    GDispose,
    GCreated,
    GDrawSprite,
    GDrawSpriteWithColorMatrix,
    GClear,
    SpriteCreate,
    SpriteDispose,
    SpriteCreated,
    SpriteAnimeCreate,
    SpriteAnimeAddFrame,
    SpriteWidth,
    SpriteHeight,
    CheckFont,
    SaveText,
    LoadText,
    FindElement,
    FindLastElement,
    FindChara,
    FindLastChara,
    VarSet,
    CVarSet,
    GetVarSizeByName,
    GetVarAllSize,
    GetHostTimeRaw,
    GetHostTime,
    GetHostTimeS,
    CsvGetProp2,
    CharaCsvExists,
    GetPalamLv,
    GetExpLv,
    AddChara,
    AddVoidChara,
    PickUpChara,
    DeleteChara,
    SwapChara,
    AddCopyChara,
    LoadData,
    SaveData,
    CheckData,
    GetCharaRegNum,
    LoadGlobal,
    SaveGlobal,
    ResetData,
    ResetCharaStain,
    SaveChara,
    LoadChara,
    GetConfig,
    GetConfigS,
    FindCharaDataFile,
}

/// Strongly typed version of Era bytecode. All integer values are encoded in platform-endian.
#[derive(Debug, Clone)]
pub enum EraBytecodeKind {
    FailWithMsg,
    DebugBreak,
    Quit,
    Throw,
    Nop,
    ReturnVoid,
    ReturnInt,
    ReturnStr,
    CallFun { args_cnt: u8 },
    TryCallFun { args_cnt: u8 },
    TryCallFunForce { args_cnt: u8 },
    RestartExecAtFun,
    JumpWW { offset: i32 },
    JumpIfWW { offset: i32 },
    JumpIfNotWW { offset: i32 },
    LoadConstStr { idx: u32 },
    LoadImm8 { imm: i8 },
    LoadImm16 { imm: i16 },
    LoadImm32 { imm: i32 },
    LoadImm64 { imm: i64 },
    LoadVarWW { idx: u32 },
    LoadConstVarWW { idx: u32 },
    LoadLocalVar { idx: u8 },
    Pop,
    PopAllN { count: u8 },
    PopOneN { idx: u8 },
    Swap2,
    Duplicate,
    DuplicateAllN { count: u8 },
    DuplicateOneN { idx: u8 },
    AddInt,
    SubInt,
    MulInt,
    DivInt,
    ModInt,
    NegInt,
    BitAndInt,
    BitOrInt,
    BitXorInt,
    BitNotInt,
    ShlInt,
    ShrInt,
    CmpIntLT,
    CmpIntLEq,
    CmpIntGT,
    CmpIntGEq,
    CmpIntEq,
    CmpIntNEq,
    CmpStrLT,
    CmpStrLEq,
    CmpStrGT,
    CmpStrGEq,
    CmpStrEq,
    CmpStrNEq,
    LogicalNot,
    MaxInt,
    MinInt,
    ClampInt,
    InRangeInt,
    InRangeStr,
    GetBit,
    SetBit,
    ClearBit,
    InvertBit,
    BuildString { count: u8 },
    PadString { flags: EraPadStringFlags },
    RepeatStr,
    BuildArrIdxFromMD { count: u8 },
    GetArrValFlat,
    SetArrValFlat,
    TimesFloat,
    FunExists,
    ReplaceStr,
    SubStr,
    SubStrU,
    StrFind,
    StrFindU,
    StrLen,
    StrLenU,
    CountSubStr,
    StrCharAtU,
    IntToStr,
    StrToInt,
    FormatIntToStr,
    StrIsValidInt,
    StrToUpper,
    StrToLower,
    StrToHalf,
    StrToFull,
    BuildBarStr,
    EscapeRegexStr,
    EncodeToUnicode,
    UnicodeToStr,
    IntToStrWithBase,
    HtmlTagSplit,
    HtmlToPlainText,
    HtmlEscape,
    PowerInt,
    SqrtInt,
    CbrtInt,
    LogInt,
    Log10Int,
    ExponentInt,
    AbsInt,
    SignInt,
    GroupMatch { count: u8 },
    ArrayCountMatches,
    CArrayCountMatches,
    SumArray,
    SumCArray,
    MaxArray,
    MaxCArray,
    MinArray,
    MinCArray,
    InRangeArray,
    InRangeCArray,
    ArrayRemove,
    ArraySortAsc,
    ArraySortDesc,
    ArrayMSort { subs_cnt: u8 },
    ArrayCopy,
    ArrayShift,
    // -----
    Print,
    PrintLine,
    PrintExtended { flags: EraPrintExtendedFlags },
    ReuseLastLine,
    ClearLine,
    Wait { flags: EraWaitFlags },
    TWait,
    Input { flags: EraInputExtendedFlags },
    KbGetKeyState,
    GetCallerFuncName,
    GetCharaNum,
    CsvGetNum { kind: EraCsvVarKind },
    GetRandomRange,
    GetRandomMax,
    // ----- ExtOp1 -----
    RowAssign { dims_cnt: u8, vals_cnt: u8 },
    ForLoopStep,
    ExtendStrToWidth,
    HtmlPrint,
    PrintButton { flags: EraPrintExtendedFlags },
    PrintImg,
    PrintImg4,
    SplitString,
    GCreate,
    GCreateFromFile,
    GDispose,
    GCreated,
    GDrawSprite,
    GDrawSpriteWithColorMatrix,
    GClear,
    SpriteCreate,
    SpriteDispose,
    SpriteCreated,
    SpriteAnimeCreate,
    SpriteAnimeAddFrame,
    SpriteWidth,
    SpriteHeight,
    CheckFont,
    SaveText,
    LoadText,
    FindElement,
    FindLastElement,
    FindChara,
    FindLastChara,
    VarSet,
    CVarSet,
    GetVarSizeByName,
    GetVarAllSize,
    GetHostTimeRaw,
    GetHostTime,
    GetHostTimeS,
    CsvGetProp2 { csv_kind: EraCharaCsvPropType },
    CharaCsvExists,
    GetPalamLv,
    GetExpLv,
    AddChara,
    AddVoidChara,
    PickUpChara { charas_cnt: u8 },
    DeleteChara { charas_cnt: u8 },
    SwapChara,
    AddCopyChara,
    LoadData,
    SaveData,
    CheckData,
    GetCharaRegNum,
    LoadGlobal,
    SaveGlobal,
    ResetData,
    ResetCharaStain,
    SaveChara { charas_cnt: u8 },
    LoadChara,
    GetConfig,
    GetConfigS,
    FindCharaDataFile,
}

impl EraBytecodeKind {
    pub fn from_bytes(bytes: &[u8]) -> Option<Self> {
        use EraBytecodeKind::*;
        use EraExtBytecode1 as Ext1;
        use EraPriBytecode as Pri;
        let byte0 = *bytes.get(0)?;
        let byte1 = || bytes.get(1).copied();
        let byte2 = || bytes.get(1).copied();
        let byte1_3 = || bytes.get(1..3);
        let byte1_5 = || bytes.get(1..5);
        let byte1_9 = || bytes.get(1..9);
        let get = |idx: usize| bytes.get(idx).copied();
        let gets = |s: usize, e: usize| bytes.get(s..e);
        match num_traits::FromPrimitive::from_u8(byte0)? {
            Pri::FailWithMsg => Some(FailWithMsg),
            Pri::DebugBreak => Some(DebugBreak),
            Pri::Quit => Some(Quit),
            Pri::Throw => Some(Throw),
            Pri::Nop => Some(Nop),
            Pri::ReturnVoid => Some(ReturnVoid),
            Pri::ReturnInt => Some(ReturnInt),
            Pri::ReturnStr => Some(ReturnStr),
            Pri::CallFun => Some(CallFun { args_cnt: byte1()? }),
            Pri::TryCallFun => Some(TryCallFun { args_cnt: byte1()? }),
            Pri::TryCallFunForce => Some(TryCallFunForce { args_cnt: byte1()? }),
            Pri::RestartExecAtFun => Some(RestartExecAtFun),
            Pri::JumpWW => Some(JumpWW {
                offset: i32::from_ne_bytes(byte1_5()?.try_into().unwrap()),
            }),
            Pri::JumpIfWW => Some(JumpIfWW {
                offset: i32::from_ne_bytes(byte1_5()?.try_into().unwrap()),
            }),
            Pri::JumpIfNotWW => Some(JumpIfNotWW {
                offset: i32::from_ne_bytes(byte1_5()?.try_into().unwrap()),
            }),
            Pri::LoadConstStr => Some(LoadConstStr {
                idx: u32::from_ne_bytes(byte1_5()?.try_into().unwrap()),
            }),
            Pri::LoadImm8 => Some(LoadImm8 { imm: byte1()? as _ }),
            Pri::LoadImm16 => Some(LoadImm16 {
                imm: i16::from_ne_bytes(byte1_3()?.try_into().unwrap()),
            }),
            Pri::LoadImm32 => Some(LoadImm32 {
                imm: i32::from_ne_bytes(byte1_5()?.try_into().unwrap()),
            }),
            Pri::LoadImm64 => Some(LoadImm64 {
                imm: i64::from_ne_bytes(byte1_9()?.try_into().unwrap()),
            }),
            Pri::LoadVarWW => Some(LoadVarWW {
                idx: u32::from_ne_bytes(byte1_5()?.try_into().unwrap()),
            }),
            Pri::LoadConstVarWW => Some(LoadConstVarWW {
                idx: u32::from_ne_bytes(byte1_5()?.try_into().unwrap()),
            }),
            Pri::LoadLocalVar => Some(LoadLocalVar { idx: byte1()? }),
            Pri::Pop => Some(Pop),
            Pri::PopAllN => Some(PopAllN { count: byte1()? }),
            Pri::PopOneN => Some(PopOneN { idx: byte1()? }),
            Pri::Swap2 => Some(Swap2),
            Pri::Duplicate => Some(Duplicate),
            Pri::DuplicateAllN => Some(DuplicateAllN { count: byte1()? }),
            Pri::DuplicateOneN => Some(DuplicateOneN { idx: byte1()? }),
            Pri::AddInt => Some(AddInt),
            Pri::SubInt => Some(SubInt),
            Pri::MulInt => Some(MulInt),
            Pri::DivInt => Some(DivInt),
            Pri::ModInt => Some(ModInt),
            Pri::NegInt => Some(NegInt),
            Pri::BitAndInt => Some(BitAndInt),
            Pri::BitOrInt => Some(BitOrInt),
            Pri::BitXorInt => Some(BitXorInt),
            Pri::BitNotInt => Some(BitNotInt),
            Pri::ShlInt => Some(ShlInt),
            Pri::ShrInt => Some(ShrInt),
            Pri::CmpIntLT => Some(CmpIntLT),
            Pri::CmpIntLEq => Some(CmpIntLEq),
            Pri::CmpIntGT => Some(CmpIntGT),
            Pri::CmpIntGEq => Some(CmpIntGEq),
            Pri::CmpIntEq => Some(CmpIntEq),
            Pri::CmpIntNEq => Some(CmpIntNEq),
            Pri::CmpStrLT => Some(CmpStrLT),
            Pri::CmpStrLEq => Some(CmpStrLEq),
            Pri::CmpStrGT => Some(CmpStrGT),
            Pri::CmpStrGEq => Some(CmpStrGEq),
            Pri::CmpStrEq => Some(CmpStrEq),
            Pri::CmpStrNEq => Some(CmpStrNEq),
            Pri::LogicalNot => Some(LogicalNot),
            Pri::MaxInt => Some(MaxInt),
            Pri::MinInt => Some(MinInt),
            Pri::ClampInt => Some(ClampInt),
            Pri::InRangeInt => Some(InRangeInt),
            Pri::InRangeStr => Some(InRangeStr),
            Pri::GetBit => Some(GetBit),
            Pri::SetBit => Some(SetBit),
            Pri::ClearBit => Some(ClearBit),
            Pri::InvertBit => Some(InvertBit),
            Pri::BuildString => Some(BuildString { count: byte1()? }),
            Pri::PadString => Some(PadString {
                flags: EraPadStringFlags::from(byte1()?),
            }),
            Pri::RepeatStr => Some(RepeatStr),
            Pri::BuildArrIdxFromMD => Some(BuildArrIdxFromMD { count: byte1()? }),
            Pri::GetArrValFlat => Some(GetArrValFlat),
            Pri::SetArrValFlat => Some(SetArrValFlat),
            Pri::TimesFloat => Some(TimesFloat),
            Pri::FunExists => Some(FunExists),
            Pri::ReplaceStr => Some(ReplaceStr),
            Pri::SubStr => Some(SubStr),
            Pri::SubStrU => Some(SubStrU),
            Pri::StrFind => Some(StrFind),
            Pri::StrFindU => Some(StrFindU),
            Pri::StrLen => Some(StrLen),
            Pri::StrLenU => Some(StrLenU),
            Pri::CountSubStr => Some(CountSubStr),
            Pri::StrCharAtU => Some(StrCharAtU),
            Pri::IntToStr => Some(IntToStr),
            Pri::StrToInt => Some(StrToInt),
            Pri::FormatIntToStr => Some(FormatIntToStr),
            Pri::StrIsValidInt => Some(StrIsValidInt),
            Pri::StrToUpper => Some(StrToUpper),
            Pri::StrToLower => Some(StrToLower),
            Pri::StrToHalf => Some(StrToHalf),
            Pri::StrToFull => Some(StrToFull),
            Pri::BuildBarStr => Some(BuildBarStr),
            Pri::EscapeRegexStr => Some(EscapeRegexStr),
            Pri::EncodeToUnicode => Some(EncodeToUnicode),
            Pri::UnicodeToStr => Some(UnicodeToStr),
            Pri::IntToStrWithBase => Some(IntToStrWithBase),
            Pri::HtmlTagSplit => Some(HtmlTagSplit),
            Pri::HtmlToPlainText => Some(HtmlToPlainText),
            Pri::HtmlEscape => Some(HtmlEscape),
            Pri::PowerInt => Some(PowerInt),
            Pri::SqrtInt => Some(SqrtInt),
            Pri::CbrtInt => Some(CbrtInt),
            Pri::LogInt => Some(LogInt),
            Pri::Log10Int => Some(Log10Int),
            Pri::ExponentInt => Some(ExponentInt),
            Pri::AbsInt => Some(AbsInt),
            Pri::SignInt => Some(SignInt),
            Pri::GroupMatch => Some(GroupMatch { count: byte1()? }),
            Pri::ArrayCountMatches => Some(ArrayCountMatches),
            Pri::CArrayCountMatches => Some(CArrayCountMatches),
            Pri::SumArray => Some(SumArray),
            Pri::SumCArray => Some(SumCArray),
            Pri::MaxArray => Some(MaxArray),
            Pri::MaxCArray => Some(MaxCArray),
            Pri::MinArray => Some(MinArray),
            Pri::MinCArray => Some(MinCArray),
            Pri::InRangeArray => Some(InRangeArray),
            Pri::InRangeCArray => Some(InRangeCArray),
            Pri::ArrayRemove => Some(ArrayRemove),
            Pri::ArraySortAsc => Some(ArraySortAsc),
            Pri::ArraySortDesc => Some(ArraySortDesc),
            Pri::ArrayMSort => Some(ArrayMSort { subs_cnt: byte1()? }),
            Pri::ArrayCopy => Some(ArrayCopy),
            Pri::ArrayShift => Some(ArrayShift),
            Pri::Print => Some(Print),
            Pri::PrintLine => Some(PrintLine),
            Pri::PrintExtended => Some(PrintExtended {
                flags: EraPrintExtendedFlags::from(byte1()?),
            }),
            Pri::ReuseLastLine => Some(ReuseLastLine),
            Pri::ClearLine => Some(ClearLine),
            Pri::Wait => Some(Wait {
                flags: EraWaitFlags::from(byte1()?),
            }),
            Pri::TWait => Some(TWait),
            Pri::Input => Some(Input {
                flags: EraInputExtendedFlags::from(byte1()?),
            }),
            Pri::KbGetKeyState => Some(KbGetKeyState),
            Pri::GetCallerFunName => Some(GetCallerFuncName),
            Pri::GetCharaNum => Some(GetCharaNum),
            Pri::CsvGetNum => Some(CsvGetNum {
                kind: EraCsvVarKind::try_from_i(byte1()?)?,
            }),
            Pri::GetRandomRange => Some(GetRandomRange),
            Pri::GetRandomMax => Some(GetRandomMax),
            // ----- ExtOp1 -----
            Pri::ExtOp1 => match num_traits::FromPrimitive::from_u8(get(1)?)? {
                Ext1::RowAssign => Some(RowAssign {
                    dims_cnt: get(2)?,
                    vals_cnt: get(3)?,
                }),
                Ext1::ForLoopStep => Some(ForLoopStep),
                Ext1::ExtendStrToWidth => Some(ExtendStrToWidth),
                Ext1::HtmlPrint => Some(HtmlPrint),
                Ext1::PrintButton => Some(PrintButton {
                    flags: EraPrintExtendedFlags::from(get(2)?),
                }),
                Ext1::PrintImg => Some(PrintImg),
                Ext1::PrintImg4 => Some(PrintImg4),
                Ext1::SplitString => Some(SplitString),
                Ext1::GCreate => Some(GCreate),
                Ext1::GCreateFromFile => Some(GCreateFromFile),
                Ext1::GDispose => Some(GDispose),
                Ext1::GCreated => Some(GCreated),
                Ext1::GDrawSprite => Some(GDrawSprite),
                Ext1::GDrawSpriteWithColorMatrix => Some(GDrawSpriteWithColorMatrix),
                Ext1::GClear => Some(GClear),
                Ext1::SpriteCreate => Some(SpriteCreate),
                Ext1::SpriteDispose => Some(SpriteDispose),
                Ext1::SpriteCreated => Some(SpriteCreated),
                Ext1::SpriteAnimeCreate => Some(SpriteAnimeCreate),
                Ext1::SpriteAnimeAddFrame => Some(SpriteAnimeAddFrame),
                Ext1::SpriteWidth => Some(SpriteWidth),
                Ext1::SpriteHeight => Some(SpriteHeight),
                Ext1::CheckFont => Some(CheckFont),
                Ext1::SaveText => Some(SaveText),
                Ext1::LoadText => Some(LoadText),
                Ext1::FindElement => Some(FindElement),
                Ext1::FindLastElement => Some(FindLastElement),
                Ext1::FindChara => Some(FindChara),
                Ext1::FindLastChara => Some(FindLastChara),
                Ext1::VarSet => Some(VarSet),
                Ext1::CVarSet => Some(CVarSet),
                Ext1::GetVarSizeByName => Some(GetVarSizeByName),
                Ext1::GetVarAllSize => Some(GetVarAllSize),
                Ext1::GetHostTimeRaw => Some(GetHostTimeRaw),
                Ext1::GetHostTime => Some(GetHostTime),
                Ext1::GetHostTimeS => Some(GetHostTimeS),
                Ext1::CsvGetProp2 => Some(CsvGetProp2 {
                    csv_kind: EraCharaCsvPropType::try_from_i(get(2)?)?,
                }),
                Ext1::CharaCsvExists => Some(CharaCsvExists),
                Ext1::GetPalamLv => Some(GetPalamLv),
                Ext1::GetExpLv => Some(GetExpLv),
                Ext1::AddChara => Some(AddChara),
                Ext1::AddVoidChara => Some(AddVoidChara),
                Ext1::PickUpChara => Some(PickUpChara {
                    charas_cnt: get(2)?,
                }),
                Ext1::DeleteChara => Some(DeleteChara {
                    charas_cnt: get(2)?,
                }),
                Ext1::SwapChara => Some(SwapChara),
                Ext1::AddCopyChara => Some(AddCopyChara),
                Ext1::LoadData => Some(LoadData),
                Ext1::SaveData => Some(SaveData),
                Ext1::CheckData => Some(CheckData),
                Ext1::GetCharaRegNum => Some(GetCharaRegNum),
                Ext1::LoadGlobal => Some(LoadGlobal),
                Ext1::SaveGlobal => Some(SaveGlobal),
                Ext1::ResetData => Some(ResetData),
                Ext1::ResetCharaStain => Some(ResetCharaStain),
                Ext1::SaveChara => Some(SaveChara {
                    charas_cnt: get(2)?,
                }),
                Ext1::LoadChara => Some(LoadChara),
                Ext1::GetConfig => Some(GetConfig),
                Ext1::GetConfigS => Some(GetConfigS),
                Ext1::FindCharaDataFile => Some(FindCharaDataFile),
                _ => None,
            },
            _ => todo!(),
        }
    }

    pub fn to_bytes(&self) -> ArrayVec<[u8; 12]> {
        use EraBytecodeKind::*;
        use EraExtBytecode1 as Ext1;
        use EraPriBytecode as Pri;
        let mut bytes = ArrayVec::new();
        match *self {
            FailWithMsg => bytes.push(Pri::FailWithMsg as u8),
            DebugBreak => bytes.push(Pri::DebugBreak as u8),
            Quit => bytes.push(Pri::Quit as u8),
            Throw => bytes.push(Pri::Throw as u8),
            Nop => bytes.push(Pri::Nop as u8),
            ReturnVoid => bytes.push(Pri::ReturnVoid as u8),
            ReturnInt => bytes.push(Pri::ReturnInt as u8),
            ReturnStr => bytes.push(Pri::ReturnStr as u8),
            CallFun { args_cnt } => {
                bytes.push(Pri::CallFun as u8);
                bytes.push(args_cnt);
            }
            TryCallFun { args_cnt } => {
                bytes.push(Pri::TryCallFun as u8);
                bytes.push(args_cnt);
            }
            TryCallFunForce { args_cnt } => {
                bytes.push(Pri::TryCallFunForce as u8);
                bytes.push(args_cnt);
            }
            RestartExecAtFun => bytes.push(Pri::RestartExecAtFun as u8),
            JumpWW { offset } => {
                bytes.push(Pri::JumpWW as u8);
                bytes.extend_from_slice(&offset.to_ne_bytes());
            }
            JumpIfWW { offset } => {
                bytes.push(Pri::JumpIfWW as u8);
                bytes.extend_from_slice(&offset.to_ne_bytes());
            }
            JumpIfNotWW { offset } => {
                bytes.push(Pri::JumpIfNotWW as u8);
                bytes.extend_from_slice(&offset.to_ne_bytes());
            }
            LoadConstStr { idx } => {
                bytes.push(Pri::LoadConstStr as u8);
                bytes.extend_from_slice(&idx.to_ne_bytes());
            }
            LoadImm8 { imm } => {
                bytes.push(Pri::LoadImm8 as u8);
                bytes.push(imm as u8);
            }
            LoadImm16 { imm } => {
                bytes.push(Pri::LoadImm16 as u8);
                bytes.extend_from_slice(&imm.to_ne_bytes());
            }
            LoadImm32 { imm } => {
                bytes.push(Pri::LoadImm32 as u8);
                bytes.extend_from_slice(&imm.to_ne_bytes());
            }
            LoadImm64 { imm } => {
                bytes.push(Pri::LoadImm64 as u8);
                bytes.extend_from_slice(&imm.to_ne_bytes());
            }
            LoadVarWW { idx } => {
                bytes.push(Pri::LoadVarWW as u8);
                bytes.extend_from_slice(&idx.to_ne_bytes());
            }
            LoadConstVarWW { idx } => {
                bytes.push(Pri::LoadConstVarWW as u8);
                bytes.extend_from_slice(&idx.to_ne_bytes());
            }
            LoadLocalVar { idx } => {
                bytes.push(Pri::LoadLocalVar as u8);
                bytes.push(idx);
            }
            Pop => bytes.push(Pri::Pop as u8),
            PopAllN { count } => {
                bytes.push(Pri::PopAllN as u8);
                bytes.push(count);
            }
            PopOneN { idx } => {
                bytes.push(Pri::PopOneN as u8);
                bytes.push(idx);
            }
            Swap2 => bytes.push(Pri::Swap2 as u8),
            Duplicate => bytes.push(Pri::Duplicate as u8),
            DuplicateAllN { count } => {
                bytes.push(Pri::DuplicateAllN as u8);
                bytes.push(count);
            }
            DuplicateOneN { idx } => {
                bytes.push(Pri::DuplicateOneN as u8);
                bytes.push(idx);
            }
            AddInt => bytes.push(Pri::AddInt as u8),
            SubInt => bytes.push(Pri::SubInt as u8),
            MulInt => bytes.push(Pri::MulInt as u8),
            DivInt => bytes.push(Pri::DivInt as u8),
            ModInt => bytes.push(Pri::ModInt as u8),
            NegInt => bytes.push(Pri::NegInt as u8),
            BitAndInt => bytes.push(Pri::BitAndInt as u8),
            BitOrInt => bytes.push(Pri::BitOrInt as u8),
            BitXorInt => bytes.push(Pri::BitXorInt as u8),
            BitNotInt => bytes.push(Pri::BitNotInt as u8),
            ShlInt => bytes.push(Pri::ShlInt as u8),
            ShrInt => bytes.push(Pri::ShrInt as u8),
            CmpIntLT => bytes.push(Pri::CmpIntLT as u8),
            CmpIntLEq => bytes.push(Pri::CmpIntLEq as u8),
            CmpIntGT => bytes.push(Pri::CmpIntGT as u8),
            CmpIntGEq => bytes.push(Pri::CmpIntGEq as u8),
            CmpIntEq => bytes.push(Pri::CmpIntEq as u8),
            CmpIntNEq => bytes.push(Pri::CmpIntNEq as u8),
            CmpStrLT => bytes.push(Pri::CmpStrLT as u8),
            CmpStrLEq => bytes.push(Pri::CmpStrLEq as u8),
            CmpStrGT => bytes.push(Pri::CmpStrGT as u8),
            CmpStrGEq => bytes.push(Pri::CmpStrGEq as u8),
            CmpStrEq => bytes.push(Pri::CmpStrEq as u8),
            CmpStrNEq => bytes.push(Pri::CmpStrNEq as u8),
            LogicalNot => bytes.push(Pri::LogicalNot as u8),
            MaxInt => bytes.push(Pri::MaxInt as u8),
            MinInt => bytes.push(Pri::MinInt as u8),
            ClampInt => bytes.push(Pri::ClampInt as u8),
            InRangeInt => bytes.push(Pri::InRangeInt as u8),
            InRangeStr => bytes.push(Pri::InRangeStr as u8),
            GetBit => bytes.push(Pri::GetBit as u8),
            SetBit => bytes.push(Pri::SetBit as u8),
            ClearBit => bytes.push(Pri::ClearBit as u8),
            InvertBit => bytes.push(Pri::InvertBit as u8),
            BuildString { count } => {
                bytes.push(Pri::BuildString as u8);
                bytes.push(count);
            }
            PadString { flags } => {
                bytes.push(Pri::PadString as u8);
                bytes.push(flags.into());
            }
            RepeatStr => bytes.push(Pri::RepeatStr as u8),
            BuildArrIdxFromMD { count } => {
                bytes.push(Pri::BuildArrIdxFromMD as u8);
                bytes.push(count);
            }
            GetArrValFlat => bytes.push(Pri::GetArrValFlat as u8),
            SetArrValFlat => bytes.push(Pri::SetArrValFlat as u8),
            TimesFloat => bytes.push(Pri::TimesFloat as u8),
            FunExists => bytes.push(Pri::FunExists as u8),
            ReplaceStr => bytes.push(Pri::ReplaceStr as u8),
            SubStr => bytes.push(Pri::SubStr as u8),
            SubStrU => bytes.push(Pri::SubStrU as u8),
            StrFind => bytes.push(Pri::StrFind as u8),
            StrFindU => bytes.push(Pri::StrFindU as u8),
            StrLen => bytes.push(Pri::StrLen as u8),
            StrLenU => bytes.push(Pri::StrLenU as u8),
            CountSubStr => bytes.push(Pri::CountSubStr as u8),
            StrCharAtU => bytes.push(Pri::StrCharAtU as u8),
            IntToStr => bytes.push(Pri::IntToStr as u8),
            StrToInt => bytes.push(Pri::StrToInt as u8),
            FormatIntToStr => bytes.push(Pri::FormatIntToStr as u8),
            StrIsValidInt => bytes.push(Pri::StrIsValidInt as u8),
            StrToUpper => bytes.push(Pri::StrToUpper as u8),
            StrToLower => bytes.push(Pri::StrToLower as u8),
            StrToHalf => bytes.push(Pri::StrToHalf as u8),
            StrToFull => bytes.push(Pri::StrToFull as u8),
            BuildBarStr => bytes.push(Pri::BuildBarStr as u8),
            EscapeRegexStr => bytes.push(Pri::EscapeRegexStr as u8),
            EncodeToUnicode => bytes.push(Pri::EncodeToUnicode as u8),
            UnicodeToStr => bytes.push(Pri::UnicodeToStr as u8),
            IntToStrWithBase => bytes.push(Pri::IntToStrWithBase as u8),
            HtmlTagSplit => bytes.push(Pri::HtmlTagSplit as u8),
            HtmlToPlainText => bytes.push(Pri::HtmlToPlainText as u8),
            HtmlEscape => bytes.push(Pri::HtmlEscape as u8),
            PowerInt => bytes.push(Pri::PowerInt as u8),
            SqrtInt => bytes.push(Pri::SqrtInt as u8),
            CbrtInt => bytes.push(Pri::CbrtInt as u8),
            LogInt => bytes.push(Pri::LogInt as u8),
            Log10Int => bytes.push(Pri::Log10Int as u8),
            ExponentInt => bytes.push(Pri::ExponentInt as u8),
            AbsInt => bytes.push(Pri::AbsInt as u8),
            SignInt => bytes.push(Pri::SignInt as u8),
            GroupMatch { count } => {
                bytes.push(Pri::GroupMatch as u8);
                bytes.push(count);
            }
            ArrayCountMatches => bytes.push(Pri::ArrayCountMatches as u8),
            CArrayCountMatches => bytes.push(Pri::CArrayCountMatches as u8),
            SumArray => bytes.push(Pri::SumArray as u8),
            SumCArray => bytes.push(Pri::SumCArray as u8),
            MaxArray => bytes.push(Pri::MaxArray as u8),
            MaxCArray => bytes.push(Pri::MaxCArray as u8),
            MinArray => bytes.push(Pri::MinArray as u8),
            MinCArray => bytes.push(Pri::MinCArray as u8),
            InRangeArray => bytes.push(Pri::InRangeArray as u8),
            InRangeCArray => bytes.push(Pri::InRangeCArray as u8),
            ArrayRemove => bytes.push(Pri::ArrayRemove as u8),
            ArraySortAsc => bytes.push(Pri::ArraySortAsc as u8),
            ArraySortDesc => bytes.push(Pri::ArraySortDesc as u8),
            ArrayMSort { subs_cnt } => {
                bytes.push(Pri::ArrayMSort as u8);
                bytes.push(subs_cnt);
            }
            ArrayCopy => bytes.push(Pri::ArrayCopy as u8),
            ArrayShift => bytes.push(Pri::ArrayShift as u8),
            Print => bytes.push(Pri::Print as u8),
            PrintLine => bytes.push(Pri::PrintLine as u8),
            PrintExtended { flags } => {
                bytes.push(Pri::PrintExtended as u8);
                bytes.push(flags.into());
            }
            ReuseLastLine => bytes.push(Pri::ReuseLastLine as u8),
            ClearLine => bytes.push(Pri::ClearLine as u8),
            Wait { flags } => {
                bytes.push(Pri::Wait as u8);
                bytes.push(flags.into());
            }
            TWait => bytes.push(Pri::TWait as u8),
            Input { flags } => {
                bytes.push(Pri::Input as u8);
                bytes.push(flags.into());
            }
            KbGetKeyState => bytes.push(Pri::KbGetKeyState as u8),
            GetCallerFuncName => bytes.push(Pri::GetCallerFunName as u8),
            GetCharaNum => bytes.push(Pri::GetCharaNum as u8),
            CsvGetNum { kind } => {
                bytes.push(Pri::CsvGetNum as u8);
                bytes.push(kind.to_i());
            }
            GetRandomRange => bytes.push(Pri::GetRandomRange as u8),
            GetRandomMax => bytes.push(Pri::GetRandomMax as u8),
            // ----- ExtOp1 -----
            // TODO...
            RowAssign { dims_cnt, vals_cnt } => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::RowAssign as u8);
                bytes.push(dims_cnt);
                bytes.push(vals_cnt);
            }
            ForLoopStep => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::ForLoopStep as u8);
            }
            ExtendStrToWidth => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::ExtendStrToWidth as u8);
            }
            HtmlPrint => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::HtmlPrint as u8);
            }
            PrintButton { flags } => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::PrintButton as u8);
                bytes.push(flags.into());
            }
            PrintImg => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::PrintImg as u8);
            }
            PrintImg4 => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::PrintImg4 as u8);
            }
            SplitString => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::SplitString as u8);
            }
            GCreate => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::GCreate as u8);
            }
            GCreateFromFile => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::GCreateFromFile as u8);
            }
            GDispose => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::GDispose as u8);
            }
            GCreated => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::GCreated as u8);
            }
            GDrawSprite => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::GDrawSprite as u8);
            }
            GDrawSpriteWithColorMatrix => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::GDrawSpriteWithColorMatrix as u8);
            }
            GClear => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::GClear as u8);
            }
            SpriteCreate => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::SpriteCreate as u8);
            }
            SpriteDispose => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::SpriteDispose as u8);
            }
            SpriteCreated => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::SpriteCreated as u8);
            }
            SpriteAnimeCreate => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::SpriteAnimeCreate as u8);
            }
            SpriteAnimeAddFrame => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::SpriteAnimeAddFrame as u8);
            }
            SpriteWidth => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::SpriteWidth as u8);
            }
            SpriteHeight => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::SpriteHeight as u8);
            }
            CheckFont => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::CheckFont as u8);
            }
            SaveText => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::SaveText as u8);
            }
            LoadText => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::LoadText as u8);
            }
            FindElement => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::FindElement as u8);
            }
            FindLastElement => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::FindLastElement as u8);
            }
            FindChara => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::FindChara as u8);
            }
            FindLastChara => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::FindLastChara as u8);
            }
            VarSet => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::VarSet as u8);
            }
            CVarSet => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::CVarSet as u8);
            }
            GetVarSizeByName => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::GetVarSizeByName as u8);
            }
            GetVarAllSize => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::GetVarAllSize as u8);
            }
            GetHostTimeRaw => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::GetHostTimeRaw as u8);
            }
            GetHostTime => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::GetHostTime as u8);
            }
            GetHostTimeS => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::GetHostTimeS as u8);
            }
            CsvGetProp2 { csv_kind } => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::CsvGetProp2 as u8);
                bytes.push(csv_kind.to_i());
            }
            CharaCsvExists => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::CharaCsvExists as u8);
            }
            GetPalamLv => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::GetPalamLv as u8);
            }
            GetExpLv => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::GetExpLv as u8);
            }
            AddChara => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::AddChara as u8);
            }
            AddVoidChara => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::AddVoidChara as u8);
            }
            PickUpChara { charas_cnt } => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::PickUpChara as u8);
                bytes.push(charas_cnt);
            }
            DeleteChara { charas_cnt } => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::DeleteChara as u8);
                bytes.push(charas_cnt);
            }
            SwapChara => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::SwapChara as u8);
            }
            AddCopyChara => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::AddCopyChara as u8);
            }
            LoadData => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::LoadData as u8);
            }
            SaveData => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::SaveData as u8);
            }
            CheckData => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::CheckData as u8);
            }
            GetCharaRegNum => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::GetCharaRegNum as u8);
            }
            LoadGlobal => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::LoadGlobal as u8);
            }
            SaveGlobal => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::SaveGlobal as u8);
            }
            ResetData => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::ResetData as u8);
            }
            ResetCharaStain => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::ResetCharaStain as u8);
            }
            SaveChara { charas_cnt } => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::SaveChara as u8);
                bytes.push(charas_cnt);
            }
            LoadChara => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::LoadChara as u8);
            }
            GetConfig => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::GetConfig as u8);
            }
            GetConfigS => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::GetConfigS as u8);
            }
            FindCharaDataFile => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::FindCharaDataFile as u8);
            }
            _ => todo!(),
        }
        bytes
    }

    pub fn bytes_len(&self) -> usize {
        self.to_bytes().len()
    }
}

#[derive(Debug, Clone)]
struct PackedSrcSpans {
    // (num_size, starts)
    starts: (u8, Vec<u8>),
    // (num_size, lens)
    lens: (u8, Vec<u8>),
}

#[derive(Debug, Clone)]
pub struct EraBcChunk {
    pub name: ArcStr,
    bc: Vec<u8>,
    packed_src_spans: PackedSrcSpans,
}

impl EraBcChunk {
    pub fn new(name: ArcStr, bc: Vec<u8>, src_spans: Vec<SrcSpan>) -> Self {
        let mut bc = bc;
        bc.shrink_to_fit();
        let packed_src_spans = {
            use crate::util::pack_u32_from_iter;
            let starts = pack_u32_from_iter(src_spans.iter().map(|span| span.start().0));
            let lens = pack_u32_from_iter(src_spans.iter().map(|span| span.len()));
            PackedSrcSpans { starts, lens }
        };

        EraBcChunk {
            name,
            bc,
            packed_src_spans,
        }
    }

    #[inline]
    pub fn get_bc(&self) -> &[u8] {
        &self.bc
    }

    pub fn lookup_src(&self, idx: usize) -> Option<SrcSpan> {
        use crate::util::read_packed_u32;
        let start = {
            let num_size = self.packed_src_spans.starts.0 as usize;
            let range = (num_size * idx)..(num_size * (idx + 1));
            let bytes = self.packed_src_spans.starts.1.get(range)?;
            read_packed_u32(num_size as u8, bytes)
        };
        let len = {
            let num_size = self.packed_src_spans.lens.0 as usize;
            let range = (num_size * idx)..(num_size * (idx + 1));
            let bytes = self.packed_src_spans.lens.1.get(range)?;
            read_packed_u32(num_size as u8, bytes)
        };
        Some(SrcSpan::new(SrcPos(start), len))
    }

    pub fn clear(&mut self) {
        self.bc.clear();
        self.packed_src_spans.starts.1.clear();
        self.packed_src_spans.lens.1.clear();
    }

    pub fn len(&self) -> usize {
        self.bc.len()
    }
}

#[derive(Debug, Clone)]
pub struct EraFuncFrameArgInfo {
    /// The type of the argument. If it is of array type, the variable is REF-qualified.
    pub var_kind: ValueKind,
    // /// The dimension count of the argument, if it is REF-qualified (i.e. var_kind is `Arr*`).
    pub dims_cnt: u8,
    /// The default value of the argument.
    pub default_value: ScalarValue,
}

impl EraFuncFrameArgInfo {
    pub fn is_ref(&self) -> bool {
        self.var_kind.is_arr()
    }
}

#[derive(Debug, Clone)]
pub struct EraFuncFrameVarInfo {
    // NOTE: Name is always interned; this does not affect FFI, since we always
    //       communicate with the engine (/ debugger) via JSON, skipping the
    //       internal representation.
    pub name: TokenKey,
    pub span: SrcSpan,
    pub is_ref: bool,
    pub is_const: bool,
    pub is_charadata: bool,
    pub in_local_frame: bool,
    /// Index of the variable in the frame.
    pub var_idx: u32,
    /// The type of the variable. Only `Arr*` are used for now.
    pub var_kind: ValueKind,
    /// The dimension count of the variable.
    pub dims_cnt: u8,
}

#[derive(Debug, Clone, Default)]
pub struct EraFuncFrameInfo {
    /// Argument slots for the function.
    pub args: Vec<EraFuncFrameArgInfo>,
    /// Declared variables in the function scope.
    pub vars: FxIndexMap<&'static Ascii<str>, EraFuncFrameVarInfo>,
}

#[derive(Debug, Clone)]
pub struct EraFuncInfo {
    pub name: TokenKey,
    pub name_span: SrcSpan,
    pub frame_info: EraFuncFrameInfo,
    pub chunk_idx: u32,
    pub bc_offset: u32,
    pub ret_kind: ScalarValueKind,
    /// Whether the function is transient. Transient functions cannot access their locals;
    /// they share the same local frame with the caller. Used by `EVAL(S)` and `EXEC`. Not
    /// intended for user-defined functions.
    pub is_transient: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, strum::Display)]
pub enum EraEventFuncKind {
    Only,
    Pri,
    Normal,
    Later,
}