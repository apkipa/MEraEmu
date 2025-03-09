// TODO: Migrate constants & types to this file

use std::{
    borrow::Cow,
    cell::RefCell,
    collections::{BTreeMap, BTreeSet},
    marker::PhantomData,
    mem::ManuallyDrop,
    ops::{ControlFlow, Deref, DerefMut},
};

use anyhow::Context;
use bincode::Options;
use cstree::{
    interning::{Resolver, TokenInterner, TokenKey},
    syntax::{SyntaxElement, SyntaxElementRef, SyntaxNode, SyntaxToken},
    util::NodeOrToken,
    Syntax,
};
use hashbrown::HashMap;
use indexmap::IndexMap;
use rclite::{Arc, Rc};
use rustc_hash::FxBuildHasher;
use safer_ffi::derive_ReprC;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use tinyvec::ArrayVec;

use crate::{
    util::{
        impl_serde_for_modular_bitfield,
        interning::ThreadedTokenInterner,
        rcstr::{ArcStr, RcStr},
        Ascii,
    },
    v2::{
        codegen::EraCodeGenerator,
        lexer::EraLexer,
        parser::{EraParsedAst, EraParser},
    },
};

type FxHashMap<K, V> = HashMap<K, V, FxBuildHasher>;
type FxIndexMap<K, V> = IndexMap<K, V, FxBuildHasher>;

#[derive_ReprC]
#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct SrcPos(pub u32);

impl Default for SrcPos {
    fn default() -> Self {
        SrcPos(0)
    }
}

#[derive_ReprC]
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[derive_ReprC]
#[repr(C)]
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

#[derive(Debug, Serialize, Deserialize)]
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
#[derive(Debug, Default, Serialize, Deserialize)]
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
#[derive(Debug, Default, Serialize, Deserialize)]
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
            todo!()
        }
        result
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum EraSourceFileKind {
    /// A normal source file.
    Source,
    /// A header file.
    Header,
    /// A system file.
    System,
    /// A csv file.
    Csv,
}

impl Default for EraSourceFileKind {
    fn default() -> Self {
        EraSourceFileKind::Source
    }
}

#[derive(Debug, Default, Serialize, Deserialize)]
pub struct EraSourceFile {
    pub filename: ArcStr,
    /// The original source text.
    pub text: Option<arcstr::ArcStr>,
    /// The compressed original source text. Reduces memory usage.
    #[serde(skip)]
    pub compressed_text: Option<Box<[u8]>>,
    #[serde(skip)]
    pub ast_data: Option<(
        crate::v2::parser::EraNodeRef,
        crate::v2::parser::EraNodeArena,
    )>,
    /// The list of macro substitution mappings (from CST to original text).
    pub macro_map: EraMacroMap,
    /// The list of `#define`'s.
    pub defines: EraDefineScope,
    /// The kind of source file.
    pub kind: EraSourceFileKind,
    /// The list of newline positions (before expansion of macros).
    pub newline_pos: Vec<SrcPos>,
    pub is_transient: bool,
}

#[derive(Debug)]
pub struct EraCompilerCtx<'i, Callback> {
    // TODO: Wrap callback in Mutex; this makes it easier for concurrent access, while also
    //       ensuring zero overhead for single-threaded access (i.e. &mut Ctx).
    pub callback: Callback,
    pub i: EraCompilerCtxInner<'i>,
}

impl<'i, Callback> Deref for EraCompilerCtx<'i, Callback> {
    type Target = EraCompilerCtxInner<'i>;

    fn deref(&self) -> &Self::Target {
        &self.i
    }
}

impl<'i, Callback> DerefMut for EraCompilerCtx<'i, Callback> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.i
    }
}

#[derive(Debug, Default, Serialize, Deserialize)]
pub struct EraCompilerCtxTransient {
    // pub source_map: FxHashMap<ArcStr, EraSourceFile>,
    pub interner: ThreadedTokenInterner,
    // pub bc_chunks: Vec<EraBcChunk>,
}

#[derive(Debug)]
pub struct EraCompilerCtxInner<'i> {
    /// Replacements defined in `_Rename.csv`.
    pub global_replace: EraDefineScope,
    /// `#define`'s from erh files are considered global.
    pub global_define: EraDefineScope,
    /// The source file map.
    pub source_map: Arc<FxIndexMap<ArcStr, EraSourceFile>>,
    /// Current active source file name. Empty if no source file is active.
    pub active_source: ArcStr,
    /// The list of character templates. Used by runtime.
    pub chara_templates: BTreeMap<u32, EraCharaInitTemplate>,
    /// The list of CSV contextual indices. Used by CSV variable access.
    pub csv_indices: FxHashMap<Ascii<ArcStr>, Vec<(EraCsvVarKind, u32)>>,
    // NOTE: We usually never remove chunks, so we use `Vec` instead of `HashMap`.
    pub bc_chunks: Arc<Vec<EraBcChunk>>,
    /// The list of function entries. Note that the value is wrapped in `Option` to
    /// allow for soft deletion.
    pub func_entries: Arc<FxIndexMap<Ascii<ArcStr>, Option<EraFuncInfo<'i>>>>,
    /// The node cache used for CST building.
    pub node_cache: cstree::build::NodeCache<'i, &'i ThreadedTokenInterner>,
    /// All globally stored variables.
    pub variables: EraVarPool,
    /// The transient data, for eval purposes.
    pub transient_ctx: Arc<EraCompilerCtxTransient>,
    /// The file id counter for evaluation source files.
    pub eval_id_counter: u32,
    /// The list of event functions.
    pub event_func_registry: FxHashMap<Ascii<ArcStr>, [Vec<Ascii<ArcStr>>; 4]>,
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

impl<'i, Callback: EraCompilerCallback> EraCompilerCtx<'i, Callback> {
    pub fn new(callback: Callback, interner: &'i ThreadedTokenInterner) -> Self {
        EraCompilerCtx {
            callback,
            i: EraCompilerCtxInner {
                global_replace: EraDefineScope::new(),
                global_define: EraDefineScope::new(),
                source_map: Default::default(),
                active_source: ArcStr::default(),
                node_cache: cstree::build::NodeCache::from_interner(interner),
                chara_templates: BTreeMap::new(),
                csv_indices: FxHashMap::default(),
                bc_chunks: Arc::new(Vec::new()),
                func_entries: Default::default(),
                variables: EraVarPool::new(),
                transient_ctx: Arc::new(Default::default()),
                eval_id_counter: 0,
                event_func_registry: Default::default(),
            },
        }
    }

    pub fn emit_diag(&mut self, diag: Diagnostic) {
        self.i.emit_diag_to(diag, &mut self.callback);
    }

    pub fn push_vm_source_and_parse<'a, F, T>(
        &mut self,
        src: &'a str,
        f: F,
    ) -> anyhow::Result<(ArcStr, T)>
    where
        F: for<'b> FnOnce(EraParser<'a, 'b, 'i>) -> anyhow::Result<T>,
    {
        // Add to sources map and compile
        let filename = ArcStr::from(self.generate_next_transient_src_name());
        if !self.push_source_file(
            filename.clone(),
            arcstr::ArcStr::from(src),
            EraSourceFileKind::Source,
            true,
        ) {
            anyhow::bail!("source file `{filename}` already exists");
        }
        let mut lexer = EraLexer::new(filename.clone(), src, false);
        lexer.set_ignore_newline_suppression(true);
        let mut is_str_var_fn = |_: &str| false;
        // TODO: Use interner from transient_ctx
        let parser = EraParser::new(
            &mut self.callback,
            &self.i,
            &mut lexer,
            &self.i.interner(),
            &self.i.global_replace,
            &self.i.global_define,
            &mut is_str_var_fn,
            false,
            true,
        );
        f(parser).map(|x| (filename, x))
    }

    pub fn compile_vm_ast<'me, F, T>(&'me mut self, env_func: usize, f: F) -> anyhow::Result<T>
    where
        F: for<'a> FnOnce(
            EraCodeGenerator<'me, 'i, Callback>,
            &'a EraFuncInfo<'i>,
        ) -> Result<T, ()>,
    {
        // TODO: Use interner from transient_ctx
        let func_entries = self.func_entries.clone();
        let env_func = func_entries
            .get_index(env_func)
            .and_then(|(_, x)| x.as_ref())
            .with_context(|| format!("invalid env_func index: {}", env_func))?;
        let compiler = EraCodeGenerator::new(self, true, true, true);
        let Ok(result) = f(compiler, env_func) else {
            anyhow::bail!("compiler returned an error");
        };
        Ok(result)
    }
}

impl<'i> EraCompilerCtxInner<'i> {
    pub fn make_diag(&self) -> Diagnostic {
        Diagnostic::with_file(self.active_source.clone())
    }

    pub fn make_diag_resolver(&self) -> DiagnosticResolver {
        DiagnosticResolver {
            src_map: Some(&self.source_map),
            resolver: Some(self.interner()),
        }
    }

    pub fn make_diag_provider(&self, diag: Diagnostic) -> DiagnosticProvider {
        DiagnosticProvider::new(diag, self.make_diag_resolver())
    }

    pub fn emit_diag_to<T: EraEmitDiagnostic + ?Sized>(&self, diag: Diagnostic, target: &mut T) {
        target.emit_diag(self.make_diag_provider(diag));
    }

    pub fn resolve_src_span(
        &self,
        filename: &str,
        span: SrcSpan,
    ) -> Option<DiagnosticResolveSrcSpanResult> {
        let resolver = self.make_diag_resolver();
        resolver.resolve_src_span(&Diagnostic::new(), filename, span)
    }

    pub fn interner(&self) -> &'i ThreadedTokenInterner {
        self.node_cache.interner()
    }

    pub fn resolve_str(&self, key: TokenKey) -> &'i str {
        self.interner().resolve(key)
    }

    pub fn source_info_from_chunk_pos(
        &self,
        chunk_idx: usize,
        bc_offset: usize,
    ) -> Option<(&ArcStr, SrcSpan)> {
        let chunks = self.bc_chunks.as_ref();
        if chunk_idx >= chunks.len() {
            return None;
        }
        let chunk = &chunks[chunk_idx];
        let src_span = chunk.lookup_src(bc_offset)?;
        Some((&chunk.name, src_span))
    }

    pub fn func_idx_and_info_from_chunk_pos(
        &self,
        chunk_idx: usize,
        bc_offset: usize,
    ) -> Option<(usize, &EraFuncInfo<'i>)> {
        let chunks = self.bc_chunks.as_ref();
        if chunk_idx >= chunks.len() {
            return None;
        }
        let chunk_idx = chunk_idx as u32;
        let bc_offset = bc_offset as u32;
        self.func_entries
            .iter()
            .enumerate()
            .find_map(|(idx, (name, info))| {
                if info.is_none() {
                    return None;
                }
                let info = info.as_ref().unwrap();
                if info.chunk_idx == chunk_idx
                    && (info.bc_offset..info.bc_offset + info.bc_size).contains(&bc_offset)
                {
                    Some((idx, info))
                } else {
                    None
                }
            })
    }

    pub fn func_info_from_chunk_pos(
        &self,
        chunk_idx: usize,
        bc_offset: usize,
    ) -> Option<&EraFuncInfo<'i>> {
        self.func_idx_and_info_from_chunk_pos(chunk_idx, bc_offset)
            .map(|(_, info)| info)
    }

    pub fn get_csv_num(&self, kind: EraCsvVarKind, name: &str) -> Option<u32> {
        self.csv_indices
            .get(Ascii::new_str(name))
            .and_then(|x| x.iter().find(|x| x.0 == kind))
            .map(|x| x.1)
    }

    pub fn get_global_save_path(&self) -> String {
        format!(".\\sav\\global.sav")
    }

    pub fn get_save_path(&self, save_id: u32) -> String {
        format!(".\\sav\\save{save_id:02}.sav")
    }

    pub fn get_var_save_path(&self, save_id: &str) -> String {
        format!(".\\dat\\var_{save_id}.dat")
    }

    pub fn get_chara_save_path(&self, save_id: &str) -> String {
        format!(".\\dat\\chara_{save_id}.dat")
    }

    pub fn get_transient_src_name(&self, idx: u32) -> String {
        format!("<VM>/VM{idx}")
    }

    pub fn generate_next_transient_src_name(&mut self) -> String {
        self.eval_id_counter += 1;
        let idx = self.eval_id_counter;
        self.get_transient_src_name(idx)
    }

    pub fn push_source_file(
        &mut self,
        filename: ArcStr,
        content: arcstr::ArcStr,
        kind: EraSourceFileKind,
        is_transient: bool,
    ) -> bool {
        use indexmap::map::Entry;

        let source_map = Arc::get_mut(&mut self.source_map).expect("source map is shared");
        match source_map.entry(filename.clone()) {
            Entry::Occupied(_) => false,
            Entry::Vacant(entry) => {
                entry.insert(EraSourceFile {
                    filename,
                    kind,
                    text: Some(content),
                    compressed_text: None,
                    ast_data: None,
                    macro_map: Default::default(),
                    defines: Default::default(),
                    newline_pos: Vec::new(),
                    is_transient,
                });
                true
            }
        }
    }

    pub fn remove_source_file(&mut self, filename: &str) -> bool {
        let source_map = Arc::get_mut(&mut self.source_map).expect("source map is shared");
        source_map.swap_remove(filename).is_some()
    }

    pub fn serialize_source_code_into(&self, w: &mut impl std::io::Write) -> bincode::Result<()> {
        struct Data<'a, 'i> {
            s: &'a EraCompilerCtxInner<'i>,
        }
        impl Serialize for Data<'_, '_> {
            fn serialize<S: Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
                s.collect_seq(self.s.source_map.iter().map(|(filename, file)| {
                    let text = file.text.as_deref();
                    (filename, file.kind, text)
                }))
            }
        }
        let data = Data { s: self };
        bincode::serialize_into(w, &data)
    }

    pub fn deserialize_source_code_from(
        &mut self,
        r: &mut impl std::io::Read,
        verify_mode: bool,
    ) -> anyhow::Result<()> {
        let data: Vec<(ArcStr, EraSourceFileKind, Option<arcstr::ArcStr>)> =
            bincode::deserialize_from(r)?;
        for (filename, kind, text) in data {
            if verify_mode {
                // Check whether source files match
                let Some(src_file) = self.source_map.get(&filename) else {
                    anyhow::bail!("source file not found: {:?}", filename);
                };
                if src_file.kind != kind {
                    anyhow::bail!(
                        "source file kind mismatch: {:?} != {:?}",
                        src_file.kind,
                        kind
                    );
                }
                if let Some(text) = text {
                    if let Some(src_text) = src_file.text.as_deref() {
                        if text != src_text {
                            anyhow::bail!(
                                "source file text mismatch: {:?} != {:?}",
                                text,
                                src_text
                            );
                        }
                    }
                }
            } else {
                // Overwrite source file
                let source_map =
                    Arc::get_mut(&mut self.source_map).context("source map is immutable")?;
                let src_file = source_map.entry(filename).or_default();
                src_file.kind = kind;
                src_file.text = text;
            }
        }
        Ok(())
    }

    pub fn serialize_global_var_into(&self, w: &mut impl std::io::Write) -> bincode::Result<()> {
        bincode::serialize_into(w, &self.variables)
    }

    pub fn deserialize_global_var_from(
        &mut self,
        r: &mut impl std::io::Read,
    ) -> anyhow::Result<()> {
        // Avoid memory hogging
        self.variables = Default::default();
        self.variables = bincode::deserialize_from(r)?;
        Ok(())
    }
}

pub trait EraEmitDiagnostic {
    fn emit_diag(&mut self, diag: DiagnosticProvider);
}

impl<T: EraCompilerCallback> EraEmitDiagnostic for T {
    fn emit_diag(&mut self, diag: DiagnosticProvider) {
        EraCompilerCallback::emit_diag(self, diag)
    }
}

pub struct EraDiagnosticAccumulator {
    diags: Vec<Diagnostic>,
}

impl EraDiagnosticAccumulator {
    pub fn new() -> Self {
        Self { diags: Vec::new() }
    }

    pub fn push(&mut self, diag: Diagnostic) {
        self.diags.push(diag);
    }

    pub fn emit_all_to(self, emit_diag: &mut impl EraEmitDiagnostic, ctx: &EraCompilerCtxInner) {
        for diag in self.diags {
            ctx.emit_diag_to(diag, emit_diag);
        }
    }

    pub fn emit_all_to_with_resolver(
        &mut self,
        emit_diag: &mut impl EraEmitDiagnostic,
        resolver: &DiagnosticResolver,
    ) {
        for diag in self.diags.drain(..) {
            emit_diag.emit_diag(DiagnosticProvider::new(diag, resolver.clone()));
        }
    }
}

impl EraEmitDiagnostic for EraDiagnosticAccumulator {
    fn emit_diag(&mut self, diag: DiagnosticProvider) {
        self.push(diag.into_diag());
    }
}

pub trait EraCompilerCallback {
    // ----- Diagnostics -----
    fn emit_diag(&mut self, diag: DiagnosticProvider);
    // ----- Virtual Machine -----
    fn on_get_rand(&mut self) -> u64;
    fn on_print(&mut self, content: &str, flags: EraPrintExtendedFlags);
    // TODO: Debug is a global flag inside VM?
    fn on_debugprint(&mut self, content: &str, flags: EraPrintExtendedFlags);
    fn on_html_print(&mut self, content: &str, no_single: i64);
    fn on_html_popprintingstr(&mut self) -> String;
    fn on_html_getprintedstr(&mut self, line_no: i64) -> String;
    fn on_html_stringlen(&mut self, content: &str, return_pixel: bool) -> i64;
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
    // ----- Graphics subsystem -----
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
    // ----- Filesystem subsystem -----
    fn on_open_host_file(
        &mut self,
        path: &str,
        can_write: bool,
    ) -> anyhow::Result<Box<dyn EraCompilerHostFile>>;
    fn on_check_host_file_exists(&mut self, path: &str) -> anyhow::Result<bool>;
    fn on_delete_host_file(&mut self, path: &str) -> anyhow::Result<()>;
    fn on_list_host_file(&mut self, path: &str) -> anyhow::Result<Vec<String>>;
    // Multimedia subsystem
    // NOTE: Returns sound id (always positive).
    fn on_play_sound(&mut self, path: &str, loop_count: i64, is_bgm: bool) -> i64;
    // NOTE: If id == 0, stops all bgms. If id == i64::MAX, stops all sounds.
    fn on_stop_sound(&mut self, sound_id: i64) -> i64;
    // ----- Others -----
    fn on_check_font(&mut self, font_name: &str) -> i64;
    // NOTE: Returns UTC timestamp (in milliseconds).
    fn on_get_host_time(&mut self) -> u64;
    fn on_get_config_int(&mut self, name: &str) -> anyhow::Result<i64>;
    fn on_get_config_str(&mut self, name: &str) -> anyhow::Result<String>;
    // NOTE: Returns { b15 = <key down>, b0 = <key triggered> }. For key codes, refer
    //       to https://learn.microsoft.com/en-us/windows/win32/inputdev/virtual-key-codes.
    fn on_get_key_state(&mut self, key_code: i64) -> i64;
    fn on_await(&mut self, milliseconds: i64);
}

pub trait EraCompilerHostFile {
    fn read(&mut self, buf: &mut [u8]) -> anyhow::Result<u64>;
    fn write(&mut self, buf: &[u8]) -> anyhow::Result<()>;
    fn flush(&mut self) -> anyhow::Result<()>;
    fn truncate(&mut self) -> anyhow::Result<()>;
    fn seek(&mut self, pos: i64, mode: EraCompilerFileSeekMode) -> anyhow::Result<u64>;
}

impl std::io::Read for dyn EraCompilerHostFile {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        self.read(buf)
            .map(|x| x as usize)
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e))
    }
}

impl std::io::Write for dyn EraCompilerHostFile {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.write(buf)
            .map(|_| buf.len())
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e))
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.flush()
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e))
    }
}

impl std::io::Seek for dyn EraCompilerHostFile {
    fn seek(&mut self, pos: std::io::SeekFrom) -> std::io::Result<u64> {
        let (pos, mode) = match pos {
            std::io::SeekFrom::Start(pos) => (pos as i64, EraCompilerFileSeekMode::Start),
            std::io::SeekFrom::End(pos) => (pos as i64, EraCompilerFileSeekMode::End),
            std::io::SeekFrom::Current(pos) => (pos as i64, EraCompilerFileSeekMode::Current),
        };
        self.seek(pos, mode)
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e))
    }
}

#[derive_ReprC]
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EraCompilerFileSeekMode {
    Start,
    End,
    Current,
}

#[derive_ReprC]
#[repr(u32)]
#[non_exhaustive]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
    UserDefinedBreakReasonStart = 100001,
}

impl Default for EraExecutionBreakReason {
    fn default() -> Self {
        EraExecutionBreakReason::InternalError
    }
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

#[derive(Debug, Clone, Default)]
/// A collection of diagnostics. Note that it is an error for a `Diagnostic` not to be
/// emitted or canceled before it is dropped, i.e. `Diagnostic` is linear.
pub struct Diagnostic {
    filename: ArcStr,
    src: Option<Box<[u8]>>,
    entries: Vec<DiagnosticEntry>,
}

impl Diagnostic {
    pub fn new() -> Diagnostic {
        Diagnostic {
            filename: ArcStr::new(),
            src: None,
            entries: Vec::new(),
        }
    }

    pub fn with_file(filename: ArcStr) -> Diagnostic {
        Diagnostic {
            filename,
            src: None,
            entries: Vec::new(),
        }
    }

    pub fn with_src(filename: ArcStr, src: &[u8]) -> Diagnostic {
        Diagnostic {
            filename,
            src: Some(src.into()),
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

#[cfg(debug_assertions)]
impl Drop for Diagnostic {
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

#[derive_ReprC]
#[repr(opaque)]
pub struct DiagnosticProvider<'a> {
    diag: Diagnostic,
    resolver: DiagnosticResolver<'a>,
}

impl<'a> DiagnosticProvider<'a> {
    pub fn new(diag: Diagnostic, resolver: DiagnosticResolver<'a>) -> Self {
        DiagnosticProvider { diag, resolver }
    }

    pub fn into_diag(self) -> Diagnostic {
        self.diag
    }

    pub fn get_entries(&self) -> &[DiagnosticEntry] {
        &self.diag.entries
    }

    pub fn resolve_src_span(
        &self,
        filename: &str,
        span: SrcSpan,
    ) -> Option<DiagnosticResolveSrcSpanResult> {
        self.resolver.resolve_src_span(&self.diag, filename, span)
    }
}

#[derive_ReprC]
#[repr(opaque)]
#[derive(Debug, Clone)]
pub struct DiagnosticResolver<'a> {
    src_map: Option<&'a FxIndexMap<ArcStr, EraSourceFile>>,
    resolver: Option<&'a ThreadedTokenInterner>,
}

impl<'a> DiagnosticResolver<'a> {
    pub fn new(
        src_map: Option<&'a FxIndexMap<ArcStr, EraSourceFile>>,
        resolver: Option<&'a ThreadedTokenInterner>,
    ) -> Self {
        DiagnosticResolver { src_map, resolver }
    }

    // NOTE: Use empty filename to resolve from default file, if there is one.
    pub fn resolve_src_span(
        &self,
        diag: &Diagnostic,
        filename: &str,
        span: SrcSpan,
    ) -> Option<DiagnosticResolveSrcSpanResult> {
        use bstr::ByteSlice;

        let src = if filename.is_empty() {
            if diag.filename.is_empty() {
                return None;
            }
            Cow::Borrowed(diag.src.as_deref()?)
        } else if diag.filename.as_str() == filename && !diag.src.is_none() {
            Cow::Borrowed(diag.src.as_deref()?)
        } else {
            let input_span = span;
            let len = span.len();
            let src_file = self.src_map?.get(filename)?;

            if let Some(text) = &src_file.text {
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

#[derive(Debug, Clone, Serialize, Deserialize)]
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

#[repr(C)]
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
impl_serde_for_modular_bitfield!(EraPrintExtendedFlags, u8);

#[repr(C)]
#[modular_bitfield::bitfield]
#[repr(u8)]
#[derive(Debug, Clone, Copy)]
pub struct EraWaitFlags {
    pub any_key: bool,
    pub is_force: bool,
    #[skip]
    __: modular_bitfield::specifiers::B6,
}
impl_serde_for_modular_bitfield!(EraWaitFlags, u8);

#[repr(C)]
#[modular_bitfield::bitfield]
#[repr(u8)]
#[derive(Debug, Clone, Copy)]
pub struct EraPadStringFlags {
    pub left_pad: bool,
    pub right_pad: bool,
    #[skip]
    __: modular_bitfield::specifiers::B6,
}
impl_serde_for_modular_bitfield!(EraPadStringFlags, u8);

#[repr(C)]
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
impl_serde_for_modular_bitfield!(EraInputExtendedFlags, u8);

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(C)]
pub struct IntValue {
    pub val: i64,
}

impl Serialize for IntValue {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        self.val.serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for IntValue {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        i64::deserialize(deserializer).map(Self::new)
    }
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(C)]
pub struct StrValue {
    pub val: ArcStr,
}

impl Serialize for StrValue {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        self.val.serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for StrValue {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        ArcStr::deserialize(deserializer).map(Self::new)
    }
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(C)]
pub struct RcStrValue {
    pub val: RcStr,
}

impl Serialize for RcStrValue {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        self.val.serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for RcStrValue {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        RcStr::deserialize(deserializer).map(Self::new)
    }
}

// NOTE: Arr*Value are used as variable references
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ArrIntValue {
    pub vals: Vec<IntValue>,
    pub dims: EraVarDims,
    pub flags: EraValueFlags,
}
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ArrStrValue {
    pub vals: Vec<StrValue>,
    pub dims: EraVarDims,
    pub flags: EraValueFlags,
}

pub trait ArrayLikeValue: std::hash::Hash {
    type Item: std::hash::Hash + Default;

    fn flat_get(&self, idx: usize) -> Option<&Self::Item>;
    fn flat_get_mut(&mut self, idx: usize) -> Option<&mut Self::Item>;
    fn get_dims(&self) -> &EraVarDims;
    fn get_flags(&self) -> &EraValueFlags;
    fn get_vals(&self) -> &[Self::Item];
    fn get_vals_mut(&mut self) -> &mut [Self::Item];

    fn flat_get_val(&self, idx: usize) -> Option<Self::Item>;
    fn flat_set_val(&mut self, idx: usize, val: Self::Item) -> Option<Self::Item>;

    fn hash_value<H: std::hash::Hasher>(&self, state: &mut H) {
        use std::hash::Hash;

        self.get_vals().hash(state);
        self.get_dims().hash(state);
        // self.get_flags().hash(state);
    }
    fn mem_usage(&self) -> usize;
    fn is_default(&self) -> bool;
    fn is_allocated(&self) -> bool {
        !self.get_vals().is_empty()
    }
    fn flat_get_val_safe(&self, idx: usize) -> Option<Self::Item> {
        if self.is_allocated() {
            self.flat_get_val(idx)
        } else {
            let size: usize = self.get_dims().iter().map(|&x| x as usize).product();
            (idx < size).then(|| Self::Item::default())
        }
    }
    fn get_val_safe(&self, idxs: &[u32]) -> Option<Self::Item> {
        if self.is_allocated() {
            let idx = self.calc_idx(idxs)?;
            self.flat_get_val(idx)
        } else {
            self.calc_idx(idxs).map(|_| Self::Item::default())
        }
    }
    fn calc_idx(&self, idxs: &[u32]) -> Option<usize> {
        let dims = self.get_dims();
        if idxs.len() > dims.len() {
            return None;
        }
        if idxs.iter().zip(dims.iter()).any(|(&a, &b)| a >= b) {
            return None;
        }
        let (_, idx) = dims
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
}

impl ArrayLikeValue for ArrIntValue {
    type Item = IntValue;

    fn flat_get(&self, idx: usize) -> Option<&IntValue> {
        self.vals.get(idx)
    }

    fn flat_get_mut(&mut self, idx: usize) -> Option<&mut IntValue> {
        self.vals.get_mut(idx)
    }

    fn get_dims(&self) -> &EraVarDims {
        &self.dims
    }

    fn get_flags(&self) -> &EraValueFlags {
        &self.flags
    }

    fn get_vals(&self) -> &[IntValue] {
        &self.vals
    }

    fn get_vals_mut(&mut self) -> &mut [IntValue] {
        &mut self.vals
    }

    fn flat_get_val(&self, idx: usize) -> Option<IntValue> {
        self.vals.get(idx).cloned()
    }

    fn flat_set_val(&mut self, idx: usize, val: IntValue) -> Option<IntValue> {
        self.vals.get_mut(idx).map(|x| std::mem::replace(x, val))
    }

    fn mem_usage(&self) -> usize {
        self.vals.capacity() * std::mem::size_of::<IntValue>()
    }

    fn is_default(&self) -> bool {
        self.vals.iter().all(|x| x.val == 0)
    }
}

impl ArrayLikeValue for ArrStrValue {
    type Item = StrValue;

    fn flat_get(&self, idx: usize) -> Option<&StrValue> {
        self.vals.get(idx)
    }

    fn flat_get_mut(&mut self, idx: usize) -> Option<&mut StrValue> {
        self.vals.get_mut(idx)
    }

    fn get_dims(&self) -> &EraVarDims {
        &self.dims
    }

    fn get_flags(&self) -> &EraValueFlags {
        &self.flags
    }

    fn get_vals(&self) -> &[StrValue] {
        &self.vals
    }

    fn get_vals_mut(&mut self) -> &mut [StrValue] {
        &mut self.vals
    }

    fn flat_get_val(&self, idx: usize) -> Option<StrValue> {
        self.vals.get(idx).cloned()
    }

    fn flat_set_val(&mut self, idx: usize, val: StrValue) -> Option<StrValue> {
        self.vals.get_mut(idx).map(|x| std::mem::replace(x, val))
    }

    fn mem_usage(&self) -> usize {
        self.vals.capacity() * std::mem::size_of::<StrValue>()
    }

    fn is_default(&self) -> bool {
        self.vals.iter().all(|x| x.val.is_empty())
    }
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
impl_serde_for_modular_bitfield!(EraValueFlags, u8);

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

impl RcStrValue {
    #[inline]
    pub fn new(val: RcStr) -> Self {
        RcStrValue { val }
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
    pub fn ensure_alloc(&mut self) {
        if self.vals.is_empty() {
            let size = self.dims.iter().fold(1, |acc, x| acc * (*x as usize));
            self.vals.reserve_exact(size);
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

/// Types for stack values, dedicated for VM.
#[derive_ReprC]
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, strum::Display, Serialize, Deserialize)]
pub enum StackValueKind {
    Int,
    Str,
    ArrRef,
}

impl StackValueKind {
    pub fn is_arr(&self) -> bool {
        use StackValueKind::*;
        match self {
            ArrRef => true,
            Int | Str => false,
        }
    }
    pub fn is_int(&self) -> bool {
        matches!(self, StackValueKind::Int)
    }
    pub fn is_str(&self) -> bool {
        matches!(self, StackValueKind::Str)
    }
}

/// Types for concrete values.
#[derive_ReprC]
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, strum::Display, Serialize, Deserialize)]
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
pub type StackValueInner = FlatStackValue;

/// Types for array values.
#[derive_ReprC]
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, strum::Display, Serialize, Deserialize)]
pub enum ArrayValueKind {
    ArrInt,
    ArrStr,
}

impl ArrayValueKind {
    pub fn is_int(&self) -> bool {
        matches!(self, ArrayValueKind::ArrInt)
    }
    pub fn is_str(&self) -> bool {
        matches!(self, ArrayValueKind::ArrStr)
    }
    pub fn to_scalar(&self) -> ScalarValueKind {
        use ArrayValueKind::*;
        match self {
            ArrInt => ScalarValueKind::Int,
            ArrStr => ScalarValueKind::Str,
        }
    }
    pub fn with_arr(&self) -> ValueKind {
        use ArrayValueKind::*;
        match self {
            ArrInt => ValueKind::ArrInt,
            ArrStr => ValueKind::ArrStr,
        }
    }
    pub fn without_arr(&self) -> ValueKind {
        use ArrayValueKind::*;
        match self {
            ArrInt => ValueKind::Int,
            ArrStr => ValueKind::Str,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct ArrayValue(ptr_union::Union2<Box<ArrIntValue>, Box<ArrStrValue>>);
impl Deref for ArrayValue {
    type Target = ptr_union::Union2<Box<ArrIntValue>, Box<ArrStrValue>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl DerefMut for ArrayValue {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
impl Serialize for ArrayValue {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        self.as_unpacked().serialize(serializer)
    }
}
impl<'de> Deserialize<'de> for ArrayValue {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        FlatArrayValue::deserialize(deserializer).map(Into::into)
    }
}

pub trait ArrayValueExt: Sized {
    fn new_int_arr(dims: EraVarDims, vals: Vec<IntValue>) -> Self;
    fn new_str_arr(dims: EraVarDims, vals: Vec<StrValue>) -> Self;
    fn new_int_0darr(val: i64) -> Self {
        Self::new_int_arr(smallvec::smallvec![1], vec![IntValue { val }])
    }
    fn new_str_0darr(val: ArcStr) -> Self {
        Self::new_str_arr(smallvec::smallvec![1], vec![StrValue { val }])
    }
    fn as_unpacked(&self) -> FlatArrayValueRef;
    fn as_unpacked_mut(&mut self) -> FlatArrayValueRefMut;
    unsafe fn as_unpacked_mut_unchecked(&self) -> FlatArrayValueRefMut;
    fn into_unpacked(self) -> FlatArrayValue;
    fn is_default(&self) -> bool;
    fn dims(&self) -> &EraVarDims {
        self.as_unpacked().dims()
    }
    fn dims_cnt(&self) -> usize {
        self.as_unpacked().dims().len()
    }
    fn flags(&self) -> EraValueFlags {
        self.as_unpacked().flags()
    }
    fn ensure_alloc(&mut self) {
        self.as_unpacked_mut().ensure_alloc()
    }
    fn clear_alloc(&mut self) {
        self.as_unpacked_mut().clear_alloc()
    }
    fn as_arrint(&self) -> Option<&ArrIntValue> {
        match self.as_unpacked() {
            FlatArrayValueRef::ArrInt(x) => Some(x),
            _ => None,
        }
    }
    fn as_arrint_mut(&mut self) -> Option<&mut ArrIntValue> {
        match self.as_unpacked_mut() {
            FlatArrayValueRefMut::ArrInt(x) => Some(x),
            _ => None,
        }
    }
    unsafe fn as_arrint_mut_unchecked(&self) -> Option<&mut ArrIntValue> {
        match self.as_unpacked_mut_unchecked() {
            FlatArrayValueRefMut::ArrInt(x) => Some(x),
            _ => None,
        }
    }
    fn as_arrstr(&self) -> Option<&ArrStrValue> {
        match self.as_unpacked() {
            FlatArrayValueRef::ArrStr(x) => Some(x),
            _ => None,
        }
    }
    fn as_arrstr_mut(&mut self) -> Option<&mut ArrStrValue> {
        match self.as_unpacked_mut() {
            FlatArrayValueRefMut::ArrStr(x) => Some(x),
            _ => None,
        }
    }
    unsafe fn as_arrstr_mut_unchecked(&self) -> Option<&mut ArrStrValue> {
        match self.as_unpacked_mut_unchecked() {
            FlatArrayValueRefMut::ArrStr(x) => Some(x),
            _ => None,
        }
    }
    fn kind(&self) -> ArrayValueKind {
        match self.as_unpacked() {
            FlatArrayValueRef::ArrInt(_) => ArrayValueKind::ArrInt,
            FlatArrayValueRef::ArrStr(_) => ArrayValueKind::ArrStr,
        }
    }
    fn mem_usage(&self) -> usize {
        match self.as_unpacked() {
            FlatArrayValueRef::ArrInt(x) => x.mem_usage(),
            FlatArrayValueRef::ArrStr(x) => x.mem_usage(),
        }
    }
    fn is_arrint(&self) -> bool {
        matches!(self.as_unpacked(), FlatArrayValueRef::ArrInt(_))
    }
    fn is_arrstr(&self) -> bool {
        matches!(self.as_unpacked(), FlatArrayValueRef::ArrStr(_))
    }
    fn calc_idx(&self, idxs: &[u32]) -> Option<usize> {
        self.as_unpacked().calc_idx(idxs)
    }
}

impl ArrayValueExt for ArrayValue {
    fn new_int_arr(dims: EraVarDims, vals: Vec<IntValue>) -> Self {
        ArrayValue(ARRAY_VALUE_BUILDER.a(Box::new(ArrIntValue {
            dims,
            vals,
            flags: EraValueFlags::new(),
        })))
    }

    fn new_str_arr(dims: EraVarDims, vals: Vec<StrValue>) -> Self {
        ArrayValue(ARRAY_VALUE_BUILDER.b(Box::new(ArrStrValue {
            dims,
            vals,
            flags: EraValueFlags::new(),
        })))
    }

    #[inline(always)]
    fn as_unpacked(&self) -> FlatArrayValueRef {
        use ptr_union::Enum2;
        match unsafe { self.as_deref_unchecked().unpack() } {
            Enum2::A(x) => FlatArrayValueRef::ArrInt(x),
            Enum2::B(x) => FlatArrayValueRef::ArrStr(x),
        }
    }

    #[inline(always)]
    fn as_unpacked_mut(&mut self) -> FlatArrayValueRefMut {
        // SAFETY: By mutable self reference, the produced mutable reference is guaranteed to be unique.
        unsafe { self.as_unpacked_mut_unchecked() }
    }

    /// # Safety
    ///
    /// The caller must ensure that the produced mutable reference is unique.
    #[inline(always)]
    unsafe fn as_unpacked_mut_unchecked(&self) -> FlatArrayValueRefMut {
        use crate::util::erase_lt_mut;
        use erasable::ErasablePtr;
        let erased = self.as_untagged_ptr();
        unsafe {
            if self.is_a() {
                let mut r = ManuallyDrop::new(Box::unerase(erased));
                FlatArrayValueRefMut::ArrInt(erase_lt_mut(&mut r))
            } else {
                let mut r = ManuallyDrop::new(Box::unerase(erased));
                FlatArrayValueRefMut::ArrStr(erase_lt_mut(&mut r))
            }
        }
    }

    #[inline(always)]
    fn into_unpacked(self) -> FlatArrayValue {
        use ptr_union::Enum2;
        match self.0.unpack() {
            Enum2::A(x) => FlatArrayValue::ArrInt(x),
            Enum2::B(x) => FlatArrayValue::ArrStr(x),
        }
    }

    fn is_default(&self) -> bool {
        match self.as_unpacked() {
            FlatArrayValueRef::ArrInt(x) => x.is_default(),
            FlatArrayValueRef::ArrStr(x) => x.is_default(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum FlatArrayValue {
    ArrInt(Box<ArrIntValue>),
    ArrStr(Box<ArrStrValue>),
}

const ARRAY_VALUE_BUILDER: ptr_union::Builder2<Box<ArrIntValue>, Box<ArrStrValue>> =
    unsafe { ptr_union::Builder2::new_unchecked() };

impl From<FlatArrayValue> for ArrayValue {
    fn from(value: FlatArrayValue) -> Self {
        ArrayValue(match value {
            FlatArrayValue::ArrInt(x) => ARRAY_VALUE_BUILDER.a(x),
            FlatArrayValue::ArrStr(x) => ARRAY_VALUE_BUILDER.b(x),
        })
    }
}

impl From<ArrayValue> for FlatArrayValue {
    fn from(value: ArrayValue) -> Self {
        use ptr_union::Enum2;
        match value.0.unpack() {
            Enum2::A(x) => FlatArrayValue::ArrInt(x),
            Enum2::B(x) => FlatArrayValue::ArrStr(x),
        }
    }
}

impl FlatArrayValue {
    pub fn new_int_arr(dims: EraVarDims, vals: Vec<IntValue>) -> Self {
        FlatArrayValue::ArrInt(Box::new(ArrIntValue {
            dims,
            vals,
            flags: EraValueFlags::new(),
        }))
    }

    pub fn new_str_arr(dims: EraVarDims, vals: Vec<StrValue>) -> Self {
        FlatArrayValue::ArrStr(Box::new(ArrStrValue {
            dims,
            vals,
            flags: EraValueFlags::new(),
        }))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum FlatArrayValueRef<'a> {
    ArrInt(&'a ArrIntValue),
    ArrStr(&'a ArrStrValue),
}

impl<'a> FlatArrayValueRef<'a> {
    pub fn dims(&self) -> &'a EraVarDims {
        use FlatArrayValueRef::*;
        match self {
            ArrInt(x) => &x.dims,
            ArrStr(x) => &x.dims,
        }
    }

    pub fn flags(&self) -> EraValueFlags {
        use FlatArrayValueRef::*;
        match self {
            ArrInt(x) => x.flags,
            ArrStr(x) => x.flags,
        }
    }

    pub fn calc_idx(&self, idxs: &[u32]) -> Option<usize> {
        use FlatArrayValueRef::*;
        match self {
            ArrInt(x) => x.calc_idx(idxs),
            ArrStr(x) => x.calc_idx(idxs),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Serialize)]
pub enum FlatArrayValueRefMut<'a> {
    ArrInt(&'a mut ArrIntValue),
    ArrStr(&'a mut ArrStrValue),
}

impl<'a> FlatArrayValueRefMut<'a> {
    pub fn ensure_alloc(&mut self) {
        use FlatArrayValueRefMut::*;
        match self {
            ArrInt(x) => x.ensure_alloc(),
            ArrStr(x) => x.ensure_alloc(),
        }
    }

    pub fn clear_alloc(&mut self) {
        use FlatArrayValueRefMut::*;
        match self {
            ArrInt(x) => x.clear_alloc(),
            ArrStr(x) => x.clear_alloc(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct VariablePlaceRef {
    pub is_dynamic: bool,
    /// Absolute index into the variable place.
    pub index: usize,
}

/// A stack value that can be either an integer, a string, or an reference (index) to an array.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct StackValue(StackValueInner);
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum FlatStackValue {
    Int(IntValue),
    Str(StrValue),
    ArrRef(VariablePlaceRef),
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RefFlatStackValue<'a> {
    Int(&'a IntValue),
    Str(&'a StrValue),
    ArrRef(&'a VariablePlaceRef),
}
impl RefFlatStackValue<'_> {
    pub fn kind(&self) -> StackValueKind {
        use RefFlatStackValue::*;
        match self {
            Int(_) => StackValueKind::Int,
            Str(_) => StackValueKind::Str,
            ArrRef(_) => StackValueKind::ArrRef,
        }
    }
}
#[derive(Debug, PartialEq, Eq)]
pub enum RefFlatStackValueMut<'a> {
    Int(&'a mut IntValue),
    Str(&'a mut StrValue),
    ArrRef(&'a mut VariablePlaceRef),
}
impl RefFlatStackValueMut<'_> {
    pub fn kind(&self) -> StackValueKind {
        use RefFlatStackValueMut::*;
        match self {
            Int(_) => StackValueKind::Int,
            Str(_) => StackValueKind::Str,
            ArrRef(_) => StackValueKind::ArrRef,
        }
    }
}

impl StackValue {
    #[inline]
    pub fn new_int(val: i64) -> Self {
        StackValue(StackValueInner::Int(IntValue { val }))
    }
    #[inline]
    pub fn new_str(val: ArcStr) -> Self {
        StackValue(StackValueInner::Str(StrValue { val }))
    }
    #[inline]
    pub fn new_arr_ref(val: VariablePlaceRef) -> Self {
        StackValue(StackValueInner::ArrRef(val))
    }
    pub fn into_unpacked(self) -> FlatStackValue {
        self.0
    }
    pub fn as_unpacked(&self) -> RefFlatStackValue {
        use FlatStackValue::*;
        match &self.0 {
            Int(x) => RefFlatStackValue::Int(x),
            Str(x) => RefFlatStackValue::Str(x),
            ArrRef(x) => RefFlatStackValue::ArrRef(x),
        }
    }
    pub fn as_unpacked_mut(&mut self) -> RefFlatStackValueMut {
        use FlatStackValue::*;
        match &mut self.0 {
            Int(x) => RefFlatStackValueMut::Int(x),
            Str(x) => RefFlatStackValueMut::Str(x),
            ArrRef(x) => RefFlatStackValueMut::ArrRef(x),
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
    pub fn kind(&self) -> StackValueKind {
        self.as_unpacked().kind()
    }
    pub fn is_default(&self) -> bool {
        use RefFlatStackValue::*;
        match self.as_unpacked() {
            Int(x) => x.val == 0,
            Str(x) => x.val.is_empty(),
            ArrRef(_) => false,
        }
    }
    pub fn as_int(&self) -> Option<&IntValue> {
        match self.as_unpacked() {
            RefFlatStackValue::Int(x) => Some(x),
            _ => None,
        }
    }
    pub fn as_str(&self) -> Option<&StrValue> {
        match self.as_unpacked() {
            RefFlatStackValue::Str(x) => Some(x),
            _ => None,
        }
    }
    pub fn as_arr_ref(&self) -> Option<&VariablePlaceRef> {
        match self.as_unpacked() {
            RefFlatStackValue::ArrRef(x) => Some(x),
            _ => None,
        }
    }
    pub fn as_int_mut(&mut self) -> Option<&mut IntValue> {
        match self.as_unpacked_mut() {
            RefFlatStackValueMut::Int(x) => Some(x),
            _ => None,
        }
    }
    pub fn as_str_mut(&mut self) -> Option<&mut StrValue> {
        match self.as_unpacked_mut() {
            RefFlatStackValueMut::Str(x) => Some(x),
            _ => None,
        }
    }
}
impl FlatStackValue {
    pub fn into_packed(self) -> StackValue {
        use FlatStackValue::*;
        StackValue(match self {
            Int(x) => StackValueInner::Int(x),
            Str(x) => StackValueInner::Str(x),
            ArrRef(x) => StackValueInner::ArrRef(x),
        })
    }
    pub fn deep_clone(&self) -> Self {
        use FlatStackValue::*;
        match self {
            // NOTE: We cannot mutate IntValue and StrValue, so it is safe to perform
            //       shallow copies on them
            // Int(x) => Int(x.deref().clone().into()),
            // Str(x) => Str(x.deref().clone().into()),
            Int(x) => Int(x.clone()),
            Str(x) => Str(x.clone()),
            ArrRef(x) => ArrRef(x.clone()),
        }
    }
    pub fn kind(&self) -> StackValueKind {
        match self {
            FlatStackValue::Int(_) => StackValueKind::Int,
            FlatStackValue::Str(_) => StackValueKind::Str,
            FlatStackValue::ArrRef(_) => StackValueKind::ArrRef,
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
    init_vars: Vec<(usize, ArrayValue)>,
}

impl Serialize for EraVarPool {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        use serde::ser::SerializeStruct;
        let mut state = serializer.serialize_struct("EraVarPool", 3)?;
        state.serialize_field("var_names", &self.var_names)?;
        state.serialize_field("vars", &self.vars)?;
        state.serialize_field("init_vars", &self.init_vars)?;
        state.end()
    }
}

impl<'de> Deserialize<'de> for EraVarPool {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        #[derive(Deserialize)]
        #[serde(field_identifier, rename_all = "snake_case")]
        enum Field {
            VarNames,
            Vars,
            InitVars,
        }

        struct EraVarPoolVisitor;

        impl<'de> serde::de::Visitor<'de> for EraVarPoolVisitor {
            type Value = EraVarPool;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("struct EraVarPool")
            }

            fn visit_map<V: serde::de::MapAccess<'de>>(
                self,
                mut map: V,
            ) -> Result<Self::Value, V::Error> {
                use serde::de;

                let mut var_names = None;
                let mut vars = None;
                let mut init_vars = None;
                while let Some(key) = map.next_key()? {
                    match key {
                        Field::VarNames => {
                            if var_names.is_some() {
                                return Err(de::Error::duplicate_field("var_names"));
                            }
                            var_names = Some(map.next_value()?);
                        }
                        Field::Vars => {
                            if vars.is_some() {
                                return Err(de::Error::duplicate_field("vars"));
                            }
                            vars = Some(map.next_value()?);
                        }
                        Field::InitVars => {
                            if init_vars.is_some() {
                                return Err(de::Error::duplicate_field("init_vars"));
                            }
                            init_vars = Some(map.next_value()?);
                        }
                    }
                }
                let var_names = var_names.ok_or_else(|| de::Error::missing_field("var_names"))?;
                let vars = vars.ok_or_else(|| de::Error::missing_field("vars"))?;
                let init_vars = init_vars.ok_or_else(|| de::Error::missing_field("init_vars"))?;
                let mut r = EraVarPool {
                    var_names,
                    chara_var_idxs: Vec::new(),
                    normal_var_idxs: Vec::new(),
                    vars,
                    init_vars,
                };
                r.chara_var_idxs = r
                    .vars
                    .iter()
                    .enumerate()
                    .filter_map(|(i, x)| if x.is_charadata { Some(i) } else { None })
                    .collect();
                r.normal_var_idxs = r
                    .vars
                    .iter()
                    .enumerate()
                    .filter_map(|(i, x)| if !x.is_charadata { Some(i) } else { None })
                    .collect();
                Ok(r)
            }
        }

        const FIELDS: &[&str] = &["var_names", "vars", "init_vars"];
        deserializer.deserialize_struct("EraVarPool", FIELDS, EraVarPoolVisitor)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EraVarInfo {
    pub name: Ascii<ArcStr>,
    pub val: ArrayValue,
    pub src_file: ArcStr,
    pub src_span: SrcSpan,
    // TODO: Compress with modular-bitfield
    pub is_const: bool,
    pub is_charadata: bool,
    pub is_global: bool,
    pub is_savedata: bool,
    pub never_trap: bool,
}

impl EraVarInfo {
    pub fn new(name: ArcStr, val: ArrayValue) -> Self {
        EraVarInfo {
            name: Ascii::new(name),
            val,
            src_file: ArcStr::default(),
            src_span: SrcSpan::default(),
            is_const: false,
            is_charadata: false,
            is_global: false,
            is_savedata: false,
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
            info.val = info.val.clone();
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
    pub fn add_var_force(&mut self, info: EraVarInfo) -> (usize, Option<EraVarInfo>) {
        let replaced_var;
        let (var_idx, info) = match self.var_names.entry(info.name.clone()) {
            hashbrown::hash_map::Entry::Occupied(e) => {
                // Update existing variable
                let var_idx = *e.get();
                self.init_vars.retain(|x| x.0 != var_idx);
                self.chara_var_idxs.retain(|&x| x != var_idx);
                self.normal_var_idxs.retain(|&x| x != var_idx);
                replaced_var = Some(std::mem::replace(&mut self.vars[var_idx], info));
                (var_idx, &mut self.vars[var_idx])
            }
            hashbrown::hash_map::Entry::Vacant(e) => {
                // Add new variable
                let var_idx = self.vars.len();
                e.insert(var_idx);
                self.vars.push(info);
                replaced_var = None;
                (var_idx, &mut self.vars[var_idx])
            }
        };

        if !info.is_const && !info.val.is_default() {
            // When resetting, we need to keep original variable values
            self.init_vars.push((var_idx, info.val.clone()));
            info.val = info.val.clone();
        }

        if info.is_charadata {
            self.chara_var_idxs.push(var_idx);
        } else {
            self.normal_var_idxs.push(var_idx);
        }

        (var_idx, replaced_var)
    }

    #[must_use]
    pub fn get_var(&self, name: &str) -> Option<&ArrayValue> {
        self.var_names
            .get(Ascii::new_str(name))
            .map(|x| &self.vars[*x].val)
    }

    #[must_use]
    pub fn get_var_mut(&mut self, name: &str) -> Option<&mut ArrayValue> {
        self.var_names
            .get_mut(Ascii::new_str(name))
            .map(|x| &mut self.vars[*x].val)
    }

    #[must_use]
    pub fn get_var_by_idx(&self, idx: usize) -> Option<&ArrayValue> {
        self.vars.get(idx).map(|x| &x.val)
    }

    #[must_use]
    pub fn get_var_by_idx_mut(&mut self, idx: usize) -> Option<&mut ArrayValue> {
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
    pub fn get_var_info_mut(&mut self, idx: usize) -> Option<&mut EraVarInfo> {
        self.vars.get_mut(idx)
    }

    #[must_use]
    pub fn get_var_info_by_name(&self, name: &str) -> Option<&EraVarInfo> {
        self.get_var_idx(name).and_then(|idx| self.vars.get(idx))
    }

    #[must_use]
    pub fn get_var_info_by_name_mut(&mut self, name: &str) -> Option<&mut EraVarInfo> {
        self.get_var_idx(name)
            .and_then(|idx| self.vars.get_mut(idx))
    }

    pub fn iter(&self) -> impl Iterator<Item = &EraVarInfo> {
        self.vars.iter()
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut EraVarInfo> {
        self.vars.iter_mut()
    }

    pub fn chara_vars_iter(&self) -> impl Iterator<Item = &EraVarInfo> {
        self.chara_var_idxs.iter().map(|&x| &self.vars[x])
    }

    pub fn chara_vars_iter_mut(&mut self) -> impl Iterator<Item = &mut EraVarInfo> {
        unsafe {
            let vars = &mut self.vars[..];
            (self.chara_var_idxs.iter()).map(|&x| &mut *std::ptr::addr_of_mut!(vars[x]))
        }
    }

    pub fn normal_vars_iter(&self) -> impl Iterator<Item = &EraVarInfo> {
        self.normal_var_idxs.iter().map(|&x| &self.vars[x])
    }

    pub fn normal_vars_iter_mut(&mut self) -> impl Iterator<Item = &mut EraVarInfo> {
        unsafe {
            let vars = &mut self.vars[..];
            (self.normal_var_idxs.iter()).map(|&x| &mut *std::ptr::addr_of_mut!(vars[x]))
        }
    }

    /// Reset variables to their default values. Some variables are not reset if they
    /// are meant to persist across multiple runs.
    pub fn reset_variables(&mut self) {
        for var in self.vars.iter_mut() {
            if var.is_const {
                continue;
            }
            if var.val.flags().is_trap() {
                continue;
            }

            match var.val.as_unpacked_mut() {
                FlatArrayValueRefMut::ArrInt(x) => {
                    let should_reset = var.is_charadata
                        || !(var.is_global || matches!(var.name.as_ref(), "GLOBAL" | "ITEMPRICE"));
                    if should_reset {
                        x.vals.fill(Default::default());
                    }
                }
                FlatArrayValueRefMut::ArrStr(x) => {
                    let should_reset = var.is_charadata
                        || !(var.is_global || matches!(var.name.as_ref(), "GLOBALS" | "STR"));
                    if should_reset {
                        x.vals.fill(Default::default());
                    }
                }
            }
        }
    }

    /// Reinitialize variables with their initial values if they have initializers.
    pub fn reinit_variables(&mut self) {
        for var in &self.init_vars {
            let (var, init_val) = (&mut self.vars[var.0], &var.1);
            match (var.val.as_unpacked_mut(), init_val.as_unpacked()) {
                (FlatArrayValueRefMut::ArrInt(d), FlatArrayValueRef::ArrInt(s)) => {
                    d.vals.clone_from(&s.vals);
                }
                (FlatArrayValueRefMut::ArrStr(d), FlatArrayValueRef::ArrStr(s)) => {
                    d.vals.clone_from(&s.vals);
                }
                _ => unreachable!(),
            }
        }
    }

    pub fn charas_var_capacity(&self) -> Option<u32> {
        self.chara_vars_iter().next().map(|x| x.val.dims()[0])
    }

    pub fn grow_charas_var_capacity(&mut self, additional_cap: u32) {
        if additional_cap == 0 {
            return;
        }

        for var in self.chara_vars_iter_mut() {
            var.val.ensure_alloc();
            let val = var.val.as_unpacked_mut();
            match val {
                FlatArrayValueRefMut::ArrInt(x) => {
                    let size: usize = x.dims.iter().skip(1).map(|&x| x as usize).product();
                    let add_cap = (additional_cap as usize) * size;
                    x.vals.reserve_exact(add_cap);
                    let new_cap = x.vals.len() + add_cap;
                    x.vals.resize(new_cap, Default::default());
                    x.dims[0] += additional_cap;
                }
                FlatArrayValueRefMut::ArrStr(x) => {
                    let size: usize = x.dims.iter().skip(1).map(|&x| x as usize).product();
                    let add_cap = (additional_cap as usize) * size;
                    x.vals.reserve_exact(add_cap);
                    let new_cap = x.vals.len() + add_cap;
                    x.vals.resize(new_cap, Default::default());
                    x.dims[0] += additional_cap;
                }
                _ => unreachable!(),
            }
        }
    }
}

impl EraVarPool {
    pub fn get_var_i_0(&self, name: &str) -> Option<i64> {
        self.get_var(name)
            .and_then(|x| x.as_arrint())
            .and_then(|x| x.vals.get(0))
            .map(|x| x.val)
    }

    pub fn get_var_s_0(&self, name: &str) -> Option<&ArcStr> {
        self.get_var(name)
            .and_then(|x| x.as_arrstr())
            .and_then(|x| x.vals.get(0))
            .map(|x| &x.val)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
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
#[derive(
    num_derive::FromPrimitive,
    num_derive::ToPrimitive,
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    Serialize,
    Deserialize,
)]
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
impl From<EraCharaCsvPropType> for u8 {
    fn from(value: EraCharaCsvPropType) -> Self {
        value.to_i()
    }
}
impl TryFrom<u8> for EraCharaCsvPropType {
    type Error = ();
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        Self::try_from_i(value).ok_or(())
    }
}

#[repr(u8)]
#[derive(
    num_derive::FromPrimitive,
    num_derive::ToPrimitive,
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    Serialize,
    Deserialize,
)]
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
        } else if is("TRAINNAME") {
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
impl From<EraCsvVarKind> for u8 {
    fn from(value: EraCsvVarKind) -> Self {
        value.to_i()
    }
}
impl TryFrom<u8> for EraCsvVarKind {
    type Error = ();
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        Self::try_from_i(value).ok_or(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
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

    pub fn coerce_as_int(&self) -> Option<i64> {
        match self {
            ScalarValue::Int(x) => Some(*x),
            ScalarValue::Empty => Some(0),
            _ => None,
        }
    }

    pub fn coerce_as_str(&self) -> Option<&str> {
        match self {
            ScalarValue::Str(x) => Some(x),
            ScalarValue::Empty => Some(""),
            _ => None,
        }
    }
}

#[derive_ReprC]
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, strum::Display, Serialize, Deserialize)]
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
    /// `(arr: Arr, idx: Int, val: Any) -> (val: Any)`
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
    GetCallerFuncName,
    GetCharaNum,
    CsvGetNum,
    GetRandomRange,
    GetRandomMax,
    /// Extra (fused) operations group #1. Introduced for performance reasons.
    ExtOp1,
    ExtOp2,
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
    /// `(var: Arr, flat_idx: Int, end: Int, step: Int) -> (Self, do_next: Int)`
    ///
    /// Checks current FOR loop WITHOUT stepping, and returns whether the loop should continue.
    ForLoopNoStep,
    ExtendStrToWidth,
    HtmlPrint,
    HtmlPopPrintingStr,
    HtmlGetPrintedStr,
    HtmlStringLen,
    PrintButton,
    /// `(sprite_name: Str, sprite_name_button: Str, width: Int, height: Int, ypos: Int)`
    PrintImg,
    /// `(sprite_name: Str, sprite_name_button: Str, color_matrix, width: Int, height: Int, ypos: Int)`
    PrintImgWithColorMatrix,
    PrintRect,
    PrintSpace,
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
    EvalStrForm,
    EvalIntExpr,
    EvalStrExpr,
    Await,
    VarExists,
    PlayBgm,
    StopBgm,
    PlaySound,
    StopSound,
}

#[derive_ReprC]
#[repr(u8)]
#[derive(num_derive::FromPrimitive, num_derive::ToPrimitive, Debug, Clone, Copy)]
pub enum EraExtBytecode2 {
    IntrinsicGetNextEventHandler,
}

/// Strongly typed version of Era bytecode. All integer values are encoded in native-endian.
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum EraBytecodeKind {
    FailWithMsg,
    DebugBreak,
    Quit,
    Throw,
    Nop,
    ReturnVoid,
    ReturnInt,
    ReturnStr,
    CallFun { args_cnt: u8, func_idx: u32 },
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
    RowAssign { vals_cnt: u8 },
    ForLoopStep,
    ForLoopNoStep,
    ExtendStrToWidth,
    HtmlPrint,
    HtmlPopPrintingStr,
    HtmlGetPrintedStr,
    HtmlStringLen,
    PrintButton { flags: EraPrintExtendedFlags },
    PrintImg,
    PrintImgWithColorMatrix,
    PrintRect,
    PrintSpace,
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
    EvalStrForm,
    EvalIntExpr,
    EvalStrExpr,
    Await,
    VarExists,
    PlayBgm,
    StopBgm,
    PlaySound,
    StopSound,
    // ----- ExtOp2 -----
    IntrinsicGetNextEventHandler,
}

impl EraBytecodeKind {
    pub fn from_reader<R: std::io::Read>(reader: &mut R) -> std::io::Result<Self> {
        use byteorder::{ReadBytesExt, LE};
        use EraBytecodeKind::*;
        use EraExtBytecode1 as Ext1;
        use EraExtBytecode2 as Ext2;
        use EraPriBytecode as Pri;

        trait Adhoc {
            fn ri8(&mut self) -> std::io::Result<i8>;
            fn ri16(&mut self) -> std::io::Result<i16>;
            fn ri32(&mut self) -> std::io::Result<i32>;
            fn ri64(&mut self) -> std::io::Result<i64>;
            fn ru8(&mut self) -> std::io::Result<u8>;
            fn ru16(&mut self) -> std::io::Result<u16>;
            fn ru32(&mut self) -> std::io::Result<u32>;
            fn ru64(&mut self) -> std::io::Result<u64>;
        }

        impl<R: std::io::Read> Adhoc for R {
            fn ri8(&mut self) -> std::io::Result<i8> {
                self.read_i8()
            }
            fn ri16(&mut self) -> std::io::Result<i16> {
                self.read_i16::<LE>()
            }
            fn ri32(&mut self) -> std::io::Result<i32> {
                self.read_i32::<LE>()
            }
            fn ri64(&mut self) -> std::io::Result<i64> {
                self.read_i64::<LE>()
            }
            fn ru8(&mut self) -> std::io::Result<u8> {
                self.read_u8()
            }
            fn ru16(&mut self) -> std::io::Result<u16> {
                self.read_u16::<LE>()
            }
            fn ru32(&mut self) -> std::io::Result<u32> {
                self.read_u32::<LE>()
            }
            fn ru64(&mut self) -> std::io::Result<u64> {
                self.read_u64::<LE>()
            }
        }

        match num_traits::FromPrimitive::from_u8(reader.ru8()?).ok_or_else(|| {
            std::io::Error::new(std::io::ErrorKind::InvalidData, "Invalid bytecode kind")
        })? {
            Pri::FailWithMsg => Ok(FailWithMsg),
            Pri::DebugBreak => Ok(DebugBreak),
            Pri::Quit => Ok(Quit),
            Pri::Throw => Ok(Throw),
            Pri::Nop => Ok(Nop),
            Pri::ReturnVoid => Ok(ReturnVoid),
            Pri::ReturnInt => Ok(ReturnInt),
            Pri::ReturnStr => Ok(ReturnStr),
            Pri::CallFun => Ok(CallFun {
                args_cnt: reader.ru8()?,
                func_idx: reader.ru32()?,
            }),
            Pri::TryCallFun => Ok(TryCallFun {
                args_cnt: reader.ru8()?,
            }),
            Pri::TryCallFunForce => Ok(TryCallFunForce {
                args_cnt: reader.ru8()?,
            }),
            Pri::RestartExecAtFun => Ok(RestartExecAtFun),
            Pri::JumpWW => Ok(JumpWW {
                offset: reader.ri32()?,
            }),
            Pri::JumpIfWW => Ok(JumpIfWW {
                offset: reader.ri32()?,
            }),
            Pri::JumpIfNotWW => Ok(JumpIfNotWW {
                offset: reader.ri32()?,
            }),
            Pri::LoadConstStr => Ok(LoadConstStr {
                idx: reader.ru32()?,
            }),
            Pri::LoadImm8 => Ok(LoadImm8 {
                imm: reader.ri8()? as _,
            }),
            Pri::LoadImm16 => Ok(LoadImm16 {
                imm: reader.ri16()?,
            }),
            Pri::LoadImm32 => Ok(LoadImm32 {
                imm: reader.ri32()?,
            }),
            Pri::LoadImm64 => Ok(LoadImm64 {
                imm: reader.ri64()?,
            }),
            Pri::LoadVarWW => Ok(LoadVarWW {
                idx: reader.ru32()?,
            }),
            Pri::LoadConstVarWW => Ok(LoadConstVarWW {
                idx: reader.ru32()?,
            }),
            Pri::LoadLocalVar => Ok(LoadLocalVar { idx: reader.ru8()? }),
            Pri::Pop => Ok(Pop),
            Pri::PopAllN => Ok(PopAllN {
                count: reader.ru8()?,
            }),
            Pri::PopOneN => Ok(PopOneN { idx: reader.ru8()? }),
            Pri::Swap2 => Ok(Swap2),
            Pri::Duplicate => Ok(Duplicate),
            Pri::DuplicateAllN => Ok(DuplicateAllN {
                count: reader.ru8()?,
            }),
            Pri::DuplicateOneN => Ok(DuplicateOneN { idx: reader.ru8()? }),
            Pri::AddInt => Ok(AddInt),
            Pri::SubInt => Ok(SubInt),
            Pri::MulInt => Ok(MulInt),
            Pri::DivInt => Ok(DivInt),
            Pri::ModInt => Ok(ModInt),
            Pri::NegInt => Ok(NegInt),
            Pri::BitAndInt => Ok(BitAndInt),
            Pri::BitOrInt => Ok(BitOrInt),
            Pri::BitXorInt => Ok(BitXorInt),
            Pri::BitNotInt => Ok(BitNotInt),
            Pri::ShlInt => Ok(ShlInt),
            Pri::ShrInt => Ok(ShrInt),
            Pri::CmpIntLT => Ok(CmpIntLT),
            Pri::CmpIntLEq => Ok(CmpIntLEq),
            Pri::CmpIntGT => Ok(CmpIntGT),
            Pri::CmpIntGEq => Ok(CmpIntGEq),
            Pri::CmpIntEq => Ok(CmpIntEq),
            Pri::CmpIntNEq => Ok(CmpIntNEq),
            Pri::CmpStrLT => Ok(CmpStrLT),
            Pri::CmpStrLEq => Ok(CmpStrLEq),
            Pri::CmpStrGT => Ok(CmpStrGT),
            Pri::CmpStrGEq => Ok(CmpStrGEq),
            Pri::CmpStrEq => Ok(CmpStrEq),
            Pri::CmpStrNEq => Ok(CmpStrNEq),
            Pri::LogicalNot => Ok(LogicalNot),
            Pri::MaxInt => Ok(MaxInt),
            Pri::MinInt => Ok(MinInt),
            Pri::ClampInt => Ok(ClampInt),
            Pri::InRangeInt => Ok(InRangeInt),
            Pri::InRangeStr => Ok(InRangeStr),
            Pri::GetBit => Ok(GetBit),
            Pri::SetBit => Ok(SetBit),
            Pri::ClearBit => Ok(ClearBit),
            Pri::InvertBit => Ok(InvertBit),
            Pri::BuildString => Ok(BuildString {
                count: reader.ru8()?,
            }),
            Pri::PadString => Ok(PadString {
                flags: EraPadStringFlags::from(reader.ru8()?),
            }),
            Pri::RepeatStr => Ok(RepeatStr),
            Pri::BuildArrIdxFromMD => Ok(BuildArrIdxFromMD {
                count: reader.ru8()?,
            }),
            Pri::GetArrValFlat => Ok(GetArrValFlat),
            Pri::SetArrValFlat => Ok(SetArrValFlat),
            Pri::TimesFloat => Ok(TimesFloat),
            Pri::FunExists => Ok(FunExists),
            Pri::ReplaceStr => Ok(ReplaceStr),
            Pri::SubStr => Ok(SubStr),
            Pri::SubStrU => Ok(SubStrU),
            Pri::StrFind => Ok(StrFind),
            Pri::StrFindU => Ok(StrFindU),
            Pri::StrLen => Ok(StrLen),
            Pri::StrLenU => Ok(StrLenU),
            Pri::CountSubStr => Ok(CountSubStr),
            Pri::StrCharAtU => Ok(StrCharAtU),
            Pri::IntToStr => Ok(IntToStr),
            Pri::StrToInt => Ok(StrToInt),
            Pri::FormatIntToStr => Ok(FormatIntToStr),
            Pri::StrIsValidInt => Ok(StrIsValidInt),
            Pri::StrToUpper => Ok(StrToUpper),
            Pri::StrToLower => Ok(StrToLower),
            Pri::StrToHalf => Ok(StrToHalf),
            Pri::StrToFull => Ok(StrToFull),
            Pri::BuildBarStr => Ok(BuildBarStr),
            Pri::EscapeRegexStr => Ok(EscapeRegexStr),
            Pri::EncodeToUnicode => Ok(EncodeToUnicode),
            Pri::UnicodeToStr => Ok(UnicodeToStr),
            Pri::IntToStrWithBase => Ok(IntToStrWithBase),
            Pri::HtmlTagSplit => Ok(HtmlTagSplit),
            Pri::HtmlToPlainText => Ok(HtmlToPlainText),
            Pri::HtmlEscape => Ok(HtmlEscape),
            Pri::PowerInt => Ok(PowerInt),
            Pri::SqrtInt => Ok(SqrtInt),
            Pri::CbrtInt => Ok(CbrtInt),
            Pri::LogInt => Ok(LogInt),
            Pri::Log10Int => Ok(Log10Int),
            Pri::ExponentInt => Ok(ExponentInt),
            Pri::AbsInt => Ok(AbsInt),
            Pri::SignInt => Ok(SignInt),
            Pri::GroupMatch => Ok(GroupMatch {
                count: reader.ru8()?,
            }),
            Pri::ArrayCountMatches => Ok(ArrayCountMatches),
            Pri::CArrayCountMatches => Ok(CArrayCountMatches),
            Pri::SumArray => Ok(SumArray),
            Pri::SumCArray => Ok(SumCArray),
            Pri::MaxArray => Ok(MaxArray),
            Pri::MaxCArray => Ok(MaxCArray),
            Pri::MinArray => Ok(MinArray),
            Pri::MinCArray => Ok(MinCArray),
            Pri::InRangeArray => Ok(InRangeArray),
            Pri::InRangeCArray => Ok(InRangeCArray),
            Pri::ArrayRemove => Ok(ArrayRemove),
            Pri::ArraySortAsc => Ok(ArraySortAsc),
            Pri::ArraySortDesc => Ok(ArraySortDesc),
            Pri::ArrayMSort => Ok(ArrayMSort {
                subs_cnt: reader.ru8()?,
            }),
            Pri::ArrayCopy => Ok(ArrayCopy),
            Pri::ArrayShift => Ok(ArrayShift),
            Pri::Print => Ok(Print),
            Pri::PrintLine => Ok(PrintLine),
            Pri::PrintExtended => Ok(PrintExtended {
                flags: EraPrintExtendedFlags::from(reader.ru8()?),
            }),
            Pri::ReuseLastLine => Ok(ReuseLastLine),
            Pri::ClearLine => Ok(ClearLine),
            Pri::Wait => Ok(Wait {
                flags: EraWaitFlags::from(reader.ru8()?),
            }),
            Pri::TWait => Ok(TWait),
            Pri::Input => Ok(Input {
                flags: EraInputExtendedFlags::from(reader.ru8()?),
            }),
            Pri::KbGetKeyState => Ok(KbGetKeyState),
            Pri::GetCallerFuncName => Ok(GetCallerFuncName),
            Pri::GetCharaNum => Ok(GetCharaNum),
            Pri::CsvGetNum => Ok(CsvGetNum {
                kind: EraCsvVarKind::try_from_i(reader.ru8()?).ok_or_else(|| {
                    std::io::Error::new(std::io::ErrorKind::InvalidData, "Invalid CSV var kind")
                })?,
            }),
            Pri::GetRandomRange => Ok(GetRandomRange),
            Pri::GetRandomMax => Ok(GetRandomMax),
            Pri::ExtOp1 => {
                match num_traits::FromPrimitive::from_u8(reader.ru8()?).ok_or_else(|| {
                    std::io::Error::new(std::io::ErrorKind::InvalidData, "Invalid ext op1 kind")
                })? {
                    Ext1::RowAssign => Ok(RowAssign {
                        vals_cnt: reader.ru8()?,
                    }),
                    Ext1::ForLoopStep => Ok(ForLoopStep),
                    Ext1::ForLoopNoStep => Ok(ForLoopNoStep),
                    Ext1::ExtendStrToWidth => Ok(ExtendStrToWidth),
                    Ext1::HtmlPrint => Ok(HtmlPrint),
                    Ext1::HtmlPopPrintingStr => Ok(HtmlPopPrintingStr),
                    Ext1::HtmlGetPrintedStr => Ok(HtmlGetPrintedStr),
                    Ext1::HtmlStringLen => Ok(HtmlStringLen),
                    Ext1::PrintButton => Ok(PrintButton {
                        flags: EraPrintExtendedFlags::from(reader.ru8()?),
                    }),
                    Ext1::PrintImg => Ok(PrintImg),
                    Ext1::PrintImgWithColorMatrix => Ok(PrintImgWithColorMatrix),
                    Ext1::PrintRect => Ok(PrintRect),
                    Ext1::PrintSpace => Ok(PrintSpace),
                    Ext1::SplitString => Ok(SplitString),
                    Ext1::GCreate => Ok(GCreate),
                    Ext1::GCreateFromFile => Ok(GCreateFromFile),
                    Ext1::GDispose => Ok(GDispose),
                    Ext1::GCreated => Ok(GCreated),
                    Ext1::GDrawSprite => Ok(GDrawSprite),
                    Ext1::GDrawSpriteWithColorMatrix => Ok(GDrawSpriteWithColorMatrix),
                    Ext1::GClear => Ok(GClear),
                    Ext1::SpriteCreate => Ok(SpriteCreate),
                    Ext1::SpriteDispose => Ok(SpriteDispose),
                    Ext1::SpriteCreated => Ok(SpriteCreated),
                    Ext1::SpriteAnimeCreate => Ok(SpriteAnimeCreate),
                    Ext1::SpriteAnimeAddFrame => Ok(SpriteAnimeAddFrame),
                    Ext1::SpriteWidth => Ok(SpriteWidth),
                    Ext1::SpriteHeight => Ok(SpriteHeight),
                    Ext1::CheckFont => Ok(CheckFont),
                    Ext1::SaveText => Ok(SaveText),
                    Ext1::LoadText => Ok(LoadText),
                    Ext1::FindElement => Ok(FindElement),
                    Ext1::FindLastElement => Ok(FindLastElement),
                    Ext1::FindChara => Ok(FindChara),
                    Ext1::FindLastChara => Ok(FindLastChara),
                    Ext1::VarSet => Ok(VarSet),
                    Ext1::CVarSet => Ok(CVarSet),
                    Ext1::GetVarSizeByName => Ok(GetVarSizeByName),
                    Ext1::GetVarAllSize => Ok(GetVarAllSize),
                    Ext1::GetHostTimeRaw => Ok(GetHostTimeRaw),
                    Ext1::GetHostTime => Ok(GetHostTime),
                    Ext1::GetHostTimeS => Ok(GetHostTimeS),
                    Ext1::CsvGetProp2 => Ok(CsvGetProp2 {
                        csv_kind: EraCharaCsvPropType::try_from_i(reader.ru8()?).ok_or_else(
                            || {
                                std::io::Error::new(
                                    std::io::ErrorKind::InvalidData,
                                    "Invalid CSV prop kind",
                                )
                            },
                        )?,
                    }),
                    Ext1::CharaCsvExists => Ok(CharaCsvExists),
                    Ext1::GetPalamLv => Ok(GetPalamLv),
                    Ext1::GetExpLv => Ok(GetExpLv),
                    Ext1::AddChara => Ok(AddChara),
                    Ext1::AddVoidChara => Ok(AddVoidChara),
                    Ext1::PickUpChara => Ok(PickUpChara {
                        charas_cnt: reader.ru8()?,
                    }),
                    Ext1::DeleteChara => Ok(DeleteChara {
                        charas_cnt: reader.ru8()?,
                    }),
                    Ext1::SwapChara => Ok(SwapChara),
                    Ext1::AddCopyChara => Ok(AddCopyChara),
                    Ext1::LoadData => Ok(LoadData),
                    Ext1::SaveData => Ok(SaveData),
                    Ext1::CheckData => Ok(CheckData),
                    Ext1::GetCharaRegNum => Ok(GetCharaRegNum),
                    Ext1::LoadGlobal => Ok(LoadGlobal),
                    Ext1::SaveGlobal => Ok(SaveGlobal),
                    Ext1::ResetData => Ok(ResetData),
                    Ext1::ResetCharaStain => Ok(ResetCharaStain),
                    Ext1::SaveChara => Ok(SaveChara {
                        charas_cnt: reader.ru8()?,
                    }),
                    Ext1::LoadChara => Ok(LoadChara),
                    Ext1::GetConfig => Ok(GetConfig),
                    Ext1::GetConfigS => Ok(GetConfigS),
                    Ext1::FindCharaDataFile => Ok(FindCharaDataFile),
                    Ext1::EvalStrForm => Ok(EvalStrForm),
                    Ext1::EvalIntExpr => Ok(EvalIntExpr),
                    Ext1::EvalStrExpr => Ok(EvalStrExpr),
                    Ext1::Await => Ok(Await),
                    Ext1::VarExists => Ok(VarExists),
                    Ext1::PlayBgm => Ok(PlayBgm),
                    Ext1::StopBgm => Ok(StopBgm),
                    Ext1::PlaySound => Ok(PlaySound),
                    Ext1::StopSound => Ok(StopSound),
                    // _ => Err(std::io::Error::new(
                    //     std::io::ErrorKind::InvalidData,
                    //     "Invalid ext op1 kind",
                    // )),
                }
            }
            Pri::ExtOp2 => {
                match num_traits::FromPrimitive::from_u8(reader.ru8()?).ok_or_else(|| {
                    std::io::Error::new(std::io::ErrorKind::InvalidData, "Invalid ext op1 kind")
                })? {
                    Ext2::IntrinsicGetNextEventHandler => Ok(IntrinsicGetNextEventHandler),
                    // _ => Err(std::io::Error::new(
                    //     std::io::ErrorKind::InvalidData,
                    //     "Invalid ext op2 kind",
                    // )),
                }
            }
            _ => Err(std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                "Invalid bytecode kind",
            )),
        }
    }

    pub fn from_bytes(bytes: &[u8]) -> Option<Self> {
        Self::with_len_from_bytes(bytes).map(|(kind, _)| kind)
    }

    pub fn with_len_from_bytes(bytes: &[u8]) -> Option<(Self, u8)> {
        let mut bytes_reader = bytes;
        let kind = Self::from_reader(&mut bytes_reader).ok()?;
        let len = bytes.len() - bytes_reader.len();
        Some((kind, len as u8))
    }

    pub fn to_bytes(&self) -> ArrayVec<[u8; 12]> {
        self.to_bytes_inline()
    }

    #[inline(always)]
    pub fn to_bytes_inline(&self) -> ArrayVec<[u8; 12]> {
        use EraBytecodeKind::*;
        use EraExtBytecode1 as Ext1;
        use EraExtBytecode2 as Ext2;
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
            CallFun { args_cnt, func_idx } => {
                bytes.push(Pri::CallFun as u8);
                bytes.push(args_cnt);
                bytes.extend_from_slice(&func_idx.to_ne_bytes());
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
            GetCallerFuncName => bytes.push(Pri::GetCallerFuncName as u8),
            GetCharaNum => bytes.push(Pri::GetCharaNum as u8),
            CsvGetNum { kind } => {
                bytes.push(Pri::CsvGetNum as u8);
                bytes.push(kind.to_i());
            }
            GetRandomRange => bytes.push(Pri::GetRandomRange as u8),
            GetRandomMax => bytes.push(Pri::GetRandomMax as u8),
            // ----- ExtOp1 -----
            // TODO...
            RowAssign { vals_cnt } => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::RowAssign as u8);
                bytes.push(vals_cnt);
            }
            ForLoopStep => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::ForLoopStep as u8);
            }
            ForLoopNoStep => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::ForLoopNoStep as u8);
            }
            ExtendStrToWidth => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::ExtendStrToWidth as u8);
            }
            HtmlPrint => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::HtmlPrint as u8);
            }
            HtmlPopPrintingStr => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::HtmlPopPrintingStr as u8);
            }
            HtmlGetPrintedStr => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::HtmlGetPrintedStr as u8);
            }
            HtmlStringLen => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::HtmlStringLen as u8);
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
            PrintImgWithColorMatrix => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::PrintImgWithColorMatrix as u8);
            }
            PrintRect => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::PrintRect as u8);
            }
            PrintSpace => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::PrintSpace as u8);
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
            EvalStrForm => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::EvalStrForm as u8);
            }
            EvalIntExpr => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::EvalIntExpr as u8);
            }
            EvalStrExpr => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::EvalStrExpr as u8);
            }
            Await => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::Await as u8);
            }
            VarExists => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::VarExists as u8);
            }
            PlayBgm => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::PlayBgm as u8);
            }
            StopBgm => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::StopBgm as u8);
            }
            PlaySound => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::PlaySound as u8);
            }
            StopSound => {
                bytes.push(Pri::ExtOp1 as u8);
                bytes.push(Ext1::StopSound as u8);
            }
            // ----- ExtOp2 -----
            IntrinsicGetNextEventHandler => {
                bytes.push(Pri::ExtOp2 as u8);
                bytes.push(Ext2::IntrinsicGetNextEventHandler as u8);
            }
        }
        bytes
    }

    #[inline(always)]
    pub fn bytes_len(&self) -> usize {
        self.to_bytes_inline().len()
    }

    /// Returns the number of stack slots this bytecode will push / pop.
    /// Positive value means pushing, negative value means popping.
    /// None means this bytecode performs non-trivial stack manipulation.
    pub fn stack_influence(&self) -> Option<i32> {
        use EraBytecodeKind::*;

        Some(match *self {
            // ! Interrupts execution
            FailWithMsg | DebugBreak | Quit | Throw => return None,
            Nop => 0,
            // ! Unwinds the stack
            ReturnVoid | ReturnInt | ReturnStr => return None,
            // ! Calls a function, which *may* return a value
            CallFun { .. } => return None,
            TryCallFun { args_cnt } => -(args_cnt as i32 * 2 + 1 - 1),
            TryCallFunForce { args_cnt } => -(args_cnt as i32 * 2 + 1),
            // ! Unwinds the stack
            RestartExecAtFun => return None,
            JumpWW { .. } => 0,
            JumpIfWW { .. } | JumpIfNotWW { .. } => -1,
            LoadConstStr { .. }
            | LoadImm8 { .. }
            | LoadImm16 { .. }
            | LoadImm32 { .. }
            | LoadImm64 { .. }
            | LoadVarWW { .. }
            | LoadConstVarWW { .. }
            | LoadLocalVar { .. } => 1,
            Pop => -1,
            PopAllN { count } => -(count as i32),
            PopOneN { .. } => -1,
            Swap2 => 0,
            Duplicate => 1,
            DuplicateAllN { count } => count as i32,
            DuplicateOneN { .. } => 1,
            AddInt | SubInt | MulInt | DivInt | ModInt => -1,
            NegInt => 0,
            BitAndInt | BitOrInt | BitXorInt | BitNotInt | ShlInt | ShrInt | CmpIntLT
            | CmpIntLEq | CmpIntGT | CmpIntGEq | CmpIntEq | CmpIntNEq | CmpStrLT | CmpStrLEq
            | CmpStrGT | CmpStrGEq | CmpStrEq | CmpStrNEq => -1,
            LogicalNot => 0,
            MaxInt | MinInt => -1,
            ClampInt | InRangeInt | InRangeStr => -2,
            GetBit | SetBit | ClearBit | InvertBit => -1,
            BuildString { count } => -(count as i32 - 1),
            PadString { .. } => -1,
            RepeatStr => -1,
            BuildArrIdxFromMD { count } => -(count as i32 - 1),
            GetArrValFlat => -1,
            SetArrValFlat => -2,
            TimesFloat => -1,
            FunExists => 0,
            ReplaceStr => -2,
            SubStr | SubStrU => -2,
            StrFind | StrFindU => -2,
            StrLen | StrLenU => 0,
            CountSubStr => -1,
            StrCharAtU => -1,
            IntToStr | StrToInt => 0,
            FormatIntToStr => -1,
            StrIsValidInt => 0,
            StrToUpper | StrToLower | StrToHalf | StrToFull => 0,
            BuildBarStr => -2,
            EscapeRegexStr => 0,
            EncodeToUnicode => -1,
            UnicodeToStr => 0,
            IntToStrWithBase => -1,
            HtmlTagSplit => -5,
            HtmlToPlainText | HtmlEscape => 0,
            PowerInt => -1,
            SqrtInt | CbrtInt | LogInt | Log10Int | ExponentInt | AbsInt | SignInt => 0,
            GroupMatch { count } => -(count as i32 + 1 - 1),
            ArrayCountMatches | CArrayCountMatches => -4,
            SumArray | SumCArray | MaxArray | MaxCArray | MinArray | MinCArray => -3,
            InRangeArray | InRangeCArray => -5,
            ArrayRemove => -4,
            ArraySortAsc | ArraySortDesc => -4,
            ArrayMSort { subs_cnt } => -(subs_cnt as i32 + 1),
            ArrayCopy => -2,
            ArrayShift => -6,
            // -----
            Print | PrintLine | PrintExtended { .. } | ReuseLastLine => -1,
            ClearLine => -1,
            Wait { .. } => 0,
            TWait => -2,
            Input { flags } => -match (flags.is_one(), flags.is_timed()) {
                (false, false) => 2 + flags.has_default_value() as i32,
                (false, true) => 5,
                (true, false) => flags.has_default_value() as i32,
                (true, true) => 5,
            },
            KbGetKeyState => 0,
            GetCallerFuncName => 1,
            GetCharaNum => 1,
            CsvGetNum { .. } => 0,
            GetRandomRange => -1,
            GetRandomMax => 0,
            // ----- ExtOp1 -----
            RowAssign { vals_cnt } => -(vals_cnt as i32 + 2),
            ForLoopStep => 1,
            ForLoopNoStep => 1,
            ExtendStrToWidth => -1,
            HtmlPrint => -2,
            HtmlPopPrintingStr => 1,
            HtmlGetPrintedStr => 0,
            HtmlStringLen => -1,
            PrintButton { .. } => -2,
            PrintImg => -5,
            PrintImgWithColorMatrix => -6,
            PrintRect => -4,
            PrintSpace => -1,
            SplitString => -6,
            GCreate => -2,
            GCreateFromFile => -1,
            GDispose | GCreated => 0,
            GDrawSprite => -5,
            GDrawSpriteWithColorMatrix => -6,
            GClear => -1,
            SpriteCreate => -5,
            SpriteDispose | SpriteCreated => 0,
            SpriteAnimeCreate => -2,
            SpriteAnimeAddFrame => -8,
            SpriteWidth | SpriteHeight => 0,
            CheckFont => 0,
            SaveText => -3,
            LoadText => -2,
            FindElement | FindLastElement => -5,
            FindChara | FindLastChara => -4,
            VarSet | CVarSet => -5,
            GetVarSizeByName => -1,
            GetVarAllSize => -1,
            GetHostTimeRaw | GetHostTime | GetHostTimeS => 1,
            CsvGetProp2 { .. } => -1,
            CharaCsvExists => 0,
            GetPalamLv | GetExpLv => -1,
            AddChara => -1,
            AddVoidChara => 0,
            PickUpChara { charas_cnt } | DeleteChara { charas_cnt } => -(charas_cnt as i32),
            SwapChara => -2,
            AddCopyChara => return None,
            LoadData => 0,
            SaveData => -1,
            CheckData => 0,
            GetCharaRegNum => 0,
            LoadGlobal | SaveGlobal => 1,
            ResetData => 0,
            ResetCharaStain => -1,
            SaveChara { charas_cnt } => -(charas_cnt as i32 + 2),
            LoadChara => -1,
            GetConfig | GetConfigS => 0,
            FindCharaDataFile => -1,
            EvalStrForm | EvalIntExpr | EvalStrExpr => 0,
            Await => -1,
            VarExists => 0,
            PlayBgm => -1,
            StopBgm => 0,
            PlaySound => -1,
            StopSound => 0,
            // ----- ExtOp2 -----
            IntrinsicGetNextEventHandler => -2,
        })
    }
}

#[derive(Debug, Clone)]
struct PackedSrcSpans {
    // (num_size, starts)
    starts: (u8, Vec<u8>),
    // (num_size, lens)
    lens: (u8, Vec<u8>),
    // Length of each run (instruction) of bytecodes. Used to run-length encode the spans.
    // NOTE: We rely on the fact that no instruction is longer than 15 bytes, i.e.
    //       bc_sizes can be seen as Vec<NonZero<u4>>.
    bc_sizes: Vec<u8>,
}

#[derive(Debug, Clone)]
pub struct EraBcChunk {
    pub name: ArcStr,
    bc: Vec<u8>,
    packed_src_spans: PackedSrcSpans,
}

impl EraBcChunk {
    pub fn new(name: ArcStr, bc: Vec<u8>, src_spans: Vec<(SrcSpan, u8)>) -> Self {
        let mut bc = bc;
        bc.shrink_to_fit();
        let packed_src_spans = {
            use crate::util::pack_u32_from_iter;
            let starts = pack_u32_from_iter(src_spans.iter().map(|span| span.0.start().0));
            let lens = pack_u32_from_iter(src_spans.iter().map(|span| span.0.len()));
            let mut bc_sizes = Vec::with_capacity((src_spans.len() + 1) / 2);
            let mut it = src_spans.chunks_exact(2);
            bc_sizes.extend(it.by_ref().map(|x| x[0].1 | (x[1].1 << 4)));
            if let Some(x) = it.remainder().first() {
                bc_sizes.push(x.1);
            }
            PackedSrcSpans {
                starts,
                lens,
                bc_sizes,
            }
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

    #[inline]
    pub fn get_bc_mut(&mut self) -> &mut [u8] {
        &mut self.bc
    }

    pub fn lookup_src(&self, idx: usize) -> Option<SrcSpan> {
        use crate::util::read_packed_u32;
        // Translate bytecode bytes index to instruction index
        let idx = (|| {
            let mut i = 0;
            let mut acc = 0;
            for size in self
                .packed_src_spans
                .bc_sizes
                .iter()
                .map(|&x| x as usize)
                .map(|x| [x & 0xF, x >> 4])
                .flatten()
            {
                acc += size;
                if idx < acc {
                    return Some(i);
                }
                i += 1;
            }
            None
        })()?;
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

    pub fn src_spans_iter(&self) -> impl Iterator<Item = SrcSpan> + '_ {
        use crate::util::read_packed_u32;
        let starts_num_size = self.packed_src_spans.starts.0;
        let lens_num_size = self.packed_src_spans.lens.0;
        let inst_count = self.packed_src_spans.lens.1.len() / lens_num_size as usize;
        (0..inst_count)
            .map(move |i| {
                let start = read_packed_u32(
                    starts_num_size,
                    &self.packed_src_spans.starts.1
                        [i * starts_num_size as usize..(i + 1) * starts_num_size as usize],
                );
                let len = read_packed_u32(
                    lens_num_size,
                    &self.packed_src_spans.lens.1
                        [i * lens_num_size as usize..(i + 1) * lens_num_size as usize],
                );
                // let bc_len = self.packed_src_spans.bc_sizes[i] as usize;
                let bc_len = if i % 2 == 0 {
                    self.packed_src_spans.bc_sizes[i / 2] as usize & 0xF
                } else {
                    self.packed_src_spans.bc_sizes[i / 2] as usize >> 4
                };
                std::iter::repeat(SrcSpan::new(SrcPos(start), len)).take(bc_len)
            })
            .flatten()
    }

    pub fn clear(&mut self) {
        self.bc.clear();
        self.packed_src_spans.starts.1.clear();
        self.packed_src_spans.lens.1.clear();
    }

    pub fn len(&self) -> usize {
        self.bc.len()
    }

    pub fn mem_usage(&self) -> usize {
        self.bc.capacity()
            + self.packed_src_spans.starts.1.capacity()
            + self.packed_src_spans.lens.1.capacity()
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EraFuncFrameArgInfo {
    /// The type of the argument. If it is of array type, the variable is REF-qualified.
    pub var_kind: ValueKind,
    /// The dimension count of the argument, if it is REF-qualified (i.e. var_kind is `Arr*`).
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
    /// The type of the variable.
    pub var_kind: ArrayValueKind,
    /// The dimension count of the variable.
    pub dims_cnt: u8,
}

#[derive(Debug, Clone, Default)]
pub struct EraFuncFrameInfo<'i> {
    /// Argument slots for the function.
    pub args: Vec<EraFuncFrameArgInfo>,
    /// Declared variables in the function scope.
    pub vars: FxIndexMap<&'i Ascii<str>, EraFuncFrameVarInfo>,
}

#[derive(Debug, Clone)]
pub struct EraFuncInfo<'i> {
    pub name: ArcStr,
    pub name_span: SrcSpan,
    pub frame_info: EraFuncFrameInfo<'i>,
    pub chunk_idx: u32,
    pub bc_offset: u32,
    pub bc_size: u32,
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

/// A multi-dimensional array with other dimensions masked. For iteration
/// along a single dimension.
///
/// For example, given a 3D array with dimensions `[2, 3, 4]`, `MaskedArr` can
/// be used to iterate along the second dimension (with `MaskedArr::new(arr, index, 1)`
/// or `MaskedArr::new(arr, index, -2)`), with the first and third dimensions masked.
#[derive(Debug)]
pub struct MaskedArr<T> {
    arr: T,
    index_base: usize,
    stride: usize,
    max_len: usize,
}

impl<T> MaskedArr<T> {
    pub fn len(&self) -> usize {
        self.max_len
    }

    pub fn into_inner(self) -> T {
        self.arr
    }

    pub fn iter(&self) -> MaskedArrIter<T> {
        MaskedArrIter::new(self)
    }

    pub fn iter_mut(&mut self) -> MaskedArrIterMut<T> {
        MaskedArrIterMut::new(self)
    }
}

pub struct MaskedArrIter<'a, T> {
    arr: &'a MaskedArr<T>,
    index: usize,
    max_len: usize,
}

impl<T, A> MaskedArr<T>
where
    T: Deref<Target = A>,
    A: ArrayLikeValue,
{
    pub fn try_new(arr: T, index: usize, mut dim_pos: i64) -> Option<Self> {
        let dims = arr.get_dims();

        if dim_pos < 0 {
            dim_pos = dim_pos.wrapping_add_unsigned(dims.len() as u64);
        }
        if dim_pos < 0 {
            return None;
        }
        let dim_pos = dim_pos as usize;
        if dim_pos >= dims.len() {
            return None;
        }

        let stride = dims[dim_pos + 1..].iter().map(|&x| x as usize).product();
        let max_len = dims[dim_pos] as usize;
        let index_base = index - ((index / stride) % max_len) * stride;

        if arr.flat_get(index_base).is_none() {
            // Index out of bounds
            return None;
        }

        Some(Self {
            arr,
            index_base,
            stride,
            max_len,
        })
    }

    pub fn get<'a>(&'a self, index: usize) -> Option<&'a A::Item>
    where
        A: 'a,
    {
        if index >= self.max_len {
            return None;
        }
        let index = self.index_base + index * self.stride;
        self.arr.flat_get(index)
    }

    /// Gets the masked array as a slice. Only works when the stride is 1 (i.e. the
    /// active dimension is the last dimension).
    pub fn as_slice<'a>(&'a self) -> Option<&'a [A::Item]>
    where
        A: 'a,
    {
        if self.stride != 1 {
            return None;
        }
        let start = self.index_base;
        let end = start + self.len();
        let vals = self.arr.get_vals();
        Some(&vals[start..end])
    }
}

impl<T, A> MaskedArr<T>
where
    T: DerefMut<Target = A>,
    A: ArrayLikeValue,
{
    pub fn get_mut<'a>(&'a mut self, index: usize) -> Option<&'a mut A::Item>
    where
        A: 'a,
    {
        if index >= self.max_len {
            return None;
        }
        let index = self.index_base + index * self.stride;
        self.arr.flat_get_mut(index)
    }

    /// Gets the masked array as a slice. Only works when the stride is 1 (i.e. the
    /// active dimension is the last dimension).
    pub fn as_slice_mut<'a>(&'a mut self) -> Option<&'a mut [A::Item]>
    where
        A: 'a,
    {
        if self.stride != 1 {
            return None;
        }
        let start = self.index_base;
        let end = start + self.len();
        let vals = self.arr.get_vals_mut();
        Some(&mut vals[start..end])
    }
}

impl<'a, T> MaskedArrIter<'a, T> {
    pub fn new(arr: &'a MaskedArr<T>) -> Self {
        Self {
            arr,
            index: 0,
            max_len: arr.len(),
        }
    }
}

impl<'a, T, A> Iterator for MaskedArrIter<'a, T>
where
    T: Deref<Target = A>,
    A: ArrayLikeValue + 'a,
{
    type Item = &'a A::Item;

    fn next(&mut self) -> Option<Self::Item> {
        if self.len() == 0 {
            return None;
        }
        let val = self.arr.get(self.index);
        self.index += 1;
        val
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let remaining = self.len();
        (remaining, Some(remaining))
    }
}

impl<'a, T, A> DoubleEndedIterator for MaskedArrIter<'a, T>
where
    T: Deref<Target = A>,
    A: ArrayLikeValue + 'a,
{
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.len() == 0 {
            return None;
        }
        self.max_len -= 1;
        self.arr.get(self.max_len)
    }
}

impl<'a, T, A> ExactSizeIterator for MaskedArrIter<'a, T>
where
    T: Deref<Target = A>,
    A: ArrayLikeValue + 'a,
{
    fn len(&self) -> usize {
        self.max_len - self.index
    }
}

pub struct MaskedArrIterMut<'a, T> {
    arr: &'a mut MaskedArr<T>,
    index: usize,
    max_len: usize,
}

impl<'a, T> MaskedArrIterMut<'a, T> {
    pub fn new(arr: &'a mut MaskedArr<T>) -> Self {
        let max_len = arr.len();
        Self {
            arr,
            index: 0,
            max_len,
        }
    }
}

impl<'a, T, A> Iterator for MaskedArrIterMut<'a, T>
where
    T: DerefMut<Target = A>,
    A: ArrayLikeValue + 'a,
{
    type Item = &'a mut A::Item;

    fn next(&mut self) -> Option<Self::Item> {
        if self.len() == 0 {
            return None;
        }
        let val = self.arr.get_mut(self.index);
        self.index += 1;
        // TODO: SAFETY
        unsafe { val.map(|x| &mut *(x as *mut A::Item)) }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let remaining = self.len();
        (remaining, Some(remaining))
    }
}

impl<'a, T, A> DoubleEndedIterator for MaskedArrIterMut<'a, T>
where
    T: DerefMut<Target = A>,
    A: ArrayLikeValue + 'a,
{
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.len() == 0 {
            return None;
        }
        self.max_len -= 1;
        let val = self.arr.get_mut(self.max_len);
        // TODO: SAFETY
        unsafe { val.map(|x| &mut *(x as *mut A::Item)) }
    }
}

impl<'a, T, A> ExactSizeIterator for MaskedArrIterMut<'a, T>
where
    T: DerefMut<Target = A>,
    A: ArrayLikeValue + 'a,
{
    fn len(&self) -> usize {
        self.max_len - self.index
    }
}

// TODO: Remove this; wrongly designed type
pub enum RayonExecPool<'a, 'scope> {
    SingleThreaded,
    Global,
    Scoped(&'a rayon::Scope<'scope>),
}

impl Default for RayonExecPool<'_, '_> {
    fn default() -> Self {
        Self::SingleThreaded
    }
}

impl<'a, 'scope> RayonExecPool<'a, 'scope> {
    pub fn is_single_threaded(&self) -> bool {
        matches!(self, Self::SingleThreaded)
    }

    pub fn scope<F>(&self, f: F)
    where
        F: FnOnce(Option<&rayon::Scope<'scope>>) + Send,
    {
        match *self {
            Self::SingleThreaded => f(None),
            Self::Global => rayon::scope(|s| f(Some(s))),
            Self::Scoped(scope) => f(Some(scope)),
        }
    }
}
