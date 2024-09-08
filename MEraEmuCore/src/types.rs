// TODO: Migrate constants & types to this file

use std::{
    borrow::Cow,
    cell::RefCell,
    collections::BTreeMap,
    marker::PhantomData,
    ops::{ControlFlow, Deref},
};

use cstree::syntax::{SyntaxNode, SyntaxToken};
use hashbrown::HashMap;
use rclite::Rc;
use safer_ffi::{derive_ReprC, prelude::VirtualPtr};

use crate::util::{rcstr::ArcStr, Ascii};

#[derive_ReprC]
#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SrcPos(pub u32);

impl Default for SrcPos {
    fn default() -> Self {
        SrcPos(0)
    }
}

#[derive_ReprC]
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SrcSpan {
    start: SrcPos,
    /// If the span is tagged (MSB is 1), then the len represents the tag (excluding
    /// the tag bit), which is used to locate span inside the macro expansion result.
    /// Otherwise, the len represents the length of the span.
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
    pub fn start(&self) -> SrcPos {
        self.start
    }
    pub fn len(&self) -> u32 {
        self.len
    }
    pub fn end(&self) -> SrcPos {
        SrcPos(self.start.0 + self.len)
    }
    pub fn is_tagged(&self) -> bool {
        const TAG_MASK: u32 = 0x80000000;
        (self.len & TAG_MASK) != 0
    }
    pub fn len_without_tag(&self) -> u32 {
        self.len & 0x7FFFFFFF
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
    // ----- Nodes -----
    Program,

    VarDecl,
    VarSDecl,
    LocalSizeDecl,
    LocalSSizeDecl,
    DefineDecl,
    EventKindDecl,
    FunctionDecl,  // `#FUNCTION`
    FunctionSDecl, // `#FUNCTIONS`
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
    RestartStmt,
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
    pub data: Box<str>,
}

/// A `#define` list, lexically scoped.
// pub struct EraDefineScope<'parent> {
//     parent: Option<&'parent EraDefineScope<'parent>>,
//     defines: HashMap<Box<[u8]>, Box<[u8]>>,
// }
#[derive(Debug, Default)]
pub struct EraDefineScope {
    defines: HashMap<Box<str>, EraDefineData>,
}

impl EraDefineScope {
    pub fn new() -> Self {
        EraDefineScope {
            defines: HashMap::new(),
        }
    }

    pub fn insert(&mut self, key: Box<str>, data: EraDefineData) {
        self.defines.insert(key, data);
    }

    pub fn get(&self, key: &str) -> Option<&EraDefineData> {
        self.defines.get(key)
    }
}

#[derive(Debug)]
pub struct EraSourceFile {
    pub filename: ArcStr,
    /// The root AST node of the lossless syntax tree, including untouched macros.
    pub orig_root: cstree::green::GreenNode,
    /// The list of macro substitution mappings.
    pub macro_map: HashMap<SrcSpan, cstree::green::GreenNode>,
    /// The list of `#define`'s.
    pub defines: EraDefineScope,
    /// Whether the file is a header file.
    pub is_header: bool,
}

/* TODO: Design notes:
 *
 * Macros (& replacements) can only occur and replace nodes of Expression, Statement and Item.
*/

#[derive(Debug)]
pub struct EraCompilerCtx<Callback> {
    pub callback: Callback,
    /// Replacements defined in `_Rename.csv`.
    pub global_replace: EraDefineScope,
    /// `#define`'s from erh files are considered global.
    // global_define: EraDefineScope<'static>,
    pub global_define: EraDefineScope,
    /// The source file map.
    pub source_map: Rc<HashMap<ArcStr, EraSourceFile>>,
    /// Current active source file name. Empty if no source file is active.
    pub active_source: ArcStr,
    pub variables: EraVarPool,
    /// The node cache used for CST building.
    pub node_cache: cstree::build::NodeCache<'static>,
    /// The list of character templates. Used by runtime.
    pub chara_templates: BTreeMap<u32, EraCharaInitTemplate>,
    /// The list of CSV contextual indices. Used by CSV variable access.
    pub csv_indices: HashMap<Ascii<ArcStr>, Vec<(EraCsvVarKind, u32)>>,
}

impl<Callback: EraCompilerCallback> EraCompilerCtx<Callback> {
    pub fn new(callback: Callback) -> Self {
        EraCompilerCtx {
            callback,
            global_replace: EraDefineScope::new(),
            global_define: EraDefineScope::new(),
            source_map: Default::default(),
            active_source: ArcStr::default(),
            variables: EraVarPool::new(),
            node_cache: cstree::build::NodeCache::new(),
            chara_templates: BTreeMap::new(),
            csv_indices: HashMap::new(),
        }
    }

    pub fn emit_diag(&mut self, diag: Diagnostic) {
        let provider = DiagnosticProvider::new(&diag, &self.source_map, self.node_cache.interner());
        self.callback.emit_diag(&provider);
        diag.cancel();
    }

    pub fn interner(&self) -> &cstree::interning::TokenInterner {
        self.node_cache.interner()
    }
}

pub trait EraCompilerCallback {
    // ----- Diagnostics -----
    fn emit_diag(&mut self, diag: &DiagnosticProvider);
    // ----- Virtual Machine -----
    fn on_get_rand(&mut self) -> u64;
    fn on_print(&mut self, content: &str, flags: crate::bytecode::PrintExtendedFlags);
    // TODO: Debug is a global flag inside VM?
    fn on_debugprint(&mut self, content: &str, flags: crate::bytecode::PrintExtendedFlags);
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
    fn on_print_button(
        &mut self,
        content: &str,
        value: &str,
        flags: crate::bytecode::PrintExtendedFlags,
    );
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
pub enum EraExecutionBreakReason {
    CallbackBreak,
    StopFlag,
    IllegalInstruction,
    DebugBreakInstruction,
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
    pub fn new() -> Diagnostic<'static> {
        Diagnostic {
            filename: ArcStr::default(),
            src: &[],
            entries: Vec::new(),
        }
    }

    pub fn with_file(filename: ArcStr) -> Diagnostic<'a> {
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

    // NOTE: Use empty filename to refer to `self.filename`.
    pub fn span_err(
        &mut self,
        filename: ArcStr,
        span: SrcSpan,
        message: impl Into<String>,
    ) -> &mut Self {
        let filename = self.conv_filename(filename);
        self.entries.push(DiagnosticEntry {
            level: DiagnosticLevel::Error,
            filename,
            span,
            message: message.into(),
        });
        self
    }

    pub fn span_warn(
        &mut self,
        filename: ArcStr,
        span: SrcSpan,
        message: impl Into<String>,
    ) -> &mut Self {
        let filename = self.conv_filename(filename);
        self.entries.push(DiagnosticEntry {
            level: DiagnosticLevel::Warning,
            filename,
            span,
            message: message.into(),
        });
        self
    }

    pub fn span_note(
        &mut self,
        filename: ArcStr,
        span: SrcSpan,
        message: impl Into<String>,
    ) -> &mut Self {
        let filename = self.conv_filename(filename);
        self.entries.push(DiagnosticEntry {
            level: DiagnosticLevel::Note,
            filename,
            span,
            message: message.into(),
        });
        self
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
    src_map: &'a HashMap<ArcStr, EraSourceFile>,
    resolver: &'a cstree::interning::TokenInterner,
}

impl<'a, 'src> DiagnosticProvider<'a, 'src> {
    pub fn new(
        diag: &'a Diagnostic<'src>,
        src_map: &'a HashMap<ArcStr, EraSourceFile>,
        resolver: &'a cstree::interning::TokenInterner,
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

        // return None;

        // let src_file = self.src_map.get(filename)?;
        // let src_root =
        //     cstree::syntax::SyntaxNode::<EraTokenKind>::new_root(src_file.orig_root.clone());
        // let src = src_root.display(self.resolver);
        let src = if filename.is_empty() {
            if self.diag.filename.is_empty() {
                return None;
            }
            Cow::Borrowed(self.diag.src)
        } else if std::ptr::eq(self.diag.filename.as_str(), filename) && !self.diag.src.is_empty() {
            Cow::Borrowed(self.diag.src)
        } else {
            let src_file = self.src_map.get(&ArcStr::from(filename))?;
            let src_root =
                cstree::syntax::SyntaxNode::<EraTokenKind>::new_root(src_file.orig_root.clone());
            Cow::Owned(src_root.display(self.resolver).into_bytes())
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
#[derive(Debug, Clone, Copy)]
pub struct PrintExtendedFlags {
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
pub struct PadStringFlags {
    pub left_pad: bool,
    pub right_pad: bool,
    __: modular_bitfield::specifiers::B6,
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
#[derive(Debug, Clone)]
pub struct ArrIntValue {
    pub vals: Vec<IntValue>,
    pub dims: EraVarDims,
    pub flags: EraValueFlags,
}
#[derive(Debug, Clone)]
pub struct ArrStrValue {
    pub vals: Vec<StrValue>,
    pub dims: EraVarDims,
    pub flags: EraValueFlags,
}

#[modular_bitfield::bitfield]
#[repr(u8)]
#[derive(Debug, Clone, Copy)]
pub struct EraValueFlags {
    pub is_trap: bool,
    pub is_const: bool,
    pub is_charadata: bool,
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
    #[inline]
    pub fn ensure_alloc(&mut self) {
        if self.vals.is_empty() {
            let size = self.dims.iter().fold(1, |acc, x| acc * (*x as usize));
            self.vals.resize(size, Default::default());
        }
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
    #[inline]
    pub fn ensure_alloc(&mut self) {
        if self.vals.is_empty() {
            let size = self.dims.iter().fold(1, |acc, x| acc * (*x as usize));
            self.vals.resize(size, Default::default());
        }
    }
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
}

//pub type Value = ptr_union::Union4<Box<IntValue>, Box<StrValue>, Box<ArrValue>, Box<()>>;
// pub type ValueInner = ptr_union::Union4<
//     Rc<IntValue>,
//     Rc<StrValue>,
//     Rc<RefCell<ArrIntValue>>,
//     Rc<RefCell<ArrStrValue>>,
// >;
pub type ValueInner = FlatValue;

#[derive(Debug, Clone)]
pub struct Value(ValueInner);
#[derive(Debug, Clone)]
pub enum FlatValue {
    Int(IntValue),
    Str(StrValue),
    ArrInt(Rc<RefCell<ArrIntValue>>),
    ArrStr(Rc<RefCell<ArrStrValue>>),
}
#[derive(Debug, Clone)]
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
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

impl<S: cstree::Syntax, D> CstreeNodeOrTokenExt2
    for cstree::util::NodeOrToken<SyntaxNode<S, D>, SyntaxToken<S, D>>
{
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
        match self {
            cstree::util::NodeOrToken::Node(x) => x.display(resolver),
            cstree::util::NodeOrToken::Token(x) => x.resolve_text(resolver).to_owned(),
        }
    }
}
