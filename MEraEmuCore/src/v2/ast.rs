use cstree::{
    interning::{Resolver, TokenKey},
    syntax::{SyntaxElementRef, SyntaxNode, SyntaxToken},
    util::NodeOrToken,
};

use crate::types::*;

use EraTokenKind as Token;

pub trait EraAstNode<'a>
where
    Self: 'a,
{
    fn cast(node: &'a SyntaxNode<Token>) -> Option<Self>
    where
        Self: Sized;
    fn can_cast(kind: Token) -> bool
    where
        Self: Sized;
    fn node(&self) -> &'a SyntaxNode<Token>;
}

pub trait EraAstToken<'a>
where
    Self: 'a,
{
    fn cast(node: &'a SyntaxToken<Token>) -> Option<Self>
    where
        Self: Sized;
    fn can_cast(kind: Token) -> bool
    where
        Self: Sized;
    fn token(&self) -> &'a SyntaxToken<Token>;
}

macro_rules! impl_node_simple {
    ($name:ident, $token_kind:ident) => {
        #[derive(Debug, Clone)]
        pub struct $name<'a>(&'a SyntaxNode<Token>);

        impl<'a> EraAstNode<'a> for $name<'a> {
            fn cast(node: &'a SyntaxNode<Token>) -> Option<Self> {
                if Self::can_cast(node.kind()) {
                    Some(Self(node))
                } else {
                    None
                }
            }

            fn can_cast(kind: Token) -> bool {
                kind == Token::$token_kind
            }

            fn node(&self) -> &'a SyntaxNode<Token> {
                self.0
            }
        }

        impl $name<'_> {
            pub fn src_span(&self) -> SrcSpan {
                self.node().text_range().into()
            }
        }
    };
}

macro_rules! impl_node_enum {
    ($name:ident => $kind_name:ident, $($token_kind:ident => $token_node:ident),+ $(,)?) => {
        #[derive(Debug, Clone)]
        pub struct $name<'a>(&'a SyntaxNode<Token>);

        pub enum $kind_name<'a> {
            $($token_kind($token_node<'a>),)+
        }

        impl<'a> EraAstNode<'a> for $name<'a> {
            fn cast(node: &'a SyntaxNode<Token>) -> Option<Self> {
                if Self::can_cast(node.kind()) {
                    Some(Self(node))
                } else {
                    None
                }
            }

            fn can_cast(kind: Token) -> bool {
                matches!(kind, $(Token::$token_kind)|+)
            }

            fn node(&self) -> &'a SyntaxNode<Token> {
                self.0
            }
        }

        impl $name<'_> {
            pub fn kind(&self) -> $kind_name {
                match self.node().kind() {
                    $(Token::$token_kind => $kind_name::$token_kind($token_node::cast(self.node()).unwrap()),)+
                    _ => unreachable!("invalid node kind"),
                }
            }

            pub fn src_span(&self) -> SrcSpan {
                self.node().text_range().into()
            }
        }
    };
}

macro_rules! impl_leaf_simple {
    ($name:ident, $token_kind:ident) => {
        #[derive(Debug, Clone)]
        pub struct $name<'a>(&'a SyntaxToken<Token>);

        impl<'a> EraAstToken<'a> for $name<'a> {
            fn cast(token: &'a SyntaxToken<Token>) -> Option<Self> {
                if Self::can_cast(token.kind()) {
                    Some(Self(token))
                } else {
                    None
                }
            }

            fn can_cast(kind: Token) -> bool {
                kind == Token::$token_kind
            }

            fn token(&self) -> &'a SyntaxToken<Token> {
                self.0
            }
        }

        impl $name<'_> {
            pub fn kind(&self) -> Token {
                self.token().kind()
            }

            pub fn resolve_text<'i, I>(&self, resolver: &'i I) -> &'i str
            where
                I: Resolver<TokenKey> + ?Sized,
            {
                self.token().resolve_text(resolver)
            }

            pub fn src_span(&self) -> SrcSpan {
                self.token().text_range().into()
            }
        }
    };
}

macro_rules! impl_leaf_enum {
    ($name:ident, $($token_kind:ident),+ $(,)?) => {
        #[derive(Debug, Clone)]
        pub struct $name<'a>(&'a SyntaxToken<Token>);

        impl<'a> EraAstToken<'a> for $name<'a> {
            fn cast(token: &'a SyntaxToken<Token>) -> Option<Self> {
                if Self::can_cast(token.kind()) {
                    Some(Self(token))
                } else {
                    None
                }
            }

            fn can_cast(kind: Token) -> bool {
                matches!(kind, $(Token::$token_kind)|+)
            }

            fn token(&self) -> &'a SyntaxToken<Token> {
                self.0
            }
        }

        impl $name<'_> {
            pub fn kind(&self) -> Token {
                self.token().kind()
            }

            pub fn resolve_text<'i, I>(&self, resolver: &'i I) -> &'i str
            where
                I: Resolver<TokenKey> + ?Sized,
            {
                self.token().resolve_text(resolver)
            }

            pub fn src_span(&self) -> SrcSpan {
                self.token().text_range().into()
            }
        }
    };
}

// pub struct EraProgramNode(SyntaxNode<Token>);

// impl EraAstNode for EraProgramNode {
//     fn cast(node: SyntaxNode<Token>) -> Option<Self> {
//         if node.kind() == Token::Program {
//             Some(Self(node))
//         } else {
//             None
//         }
//     }

//     fn can_cast(kind: Token) -> bool {
//         kind == Token::Program
//     }

//     fn node(&self) -> &SyntaxNode<Token> {
//         &self.0
//     }
// }

// Nodes
impl_node_simple!(EraProgramNode, Program);

impl_node_simple!(EraVarDeclNode, VarDecl);
impl_node_simple!(EraVarSDeclNode, VarSDecl);
impl_node_simple!(EraLocalSizeDeclNode, LocalSizeDecl);
impl_node_simple!(EraLocalSSizeDeclNode, LocalSSizeDecl);
impl_node_simple!(EraDefineDeclNode, DefineDecl);
impl_node_simple!(EraEventKindDeclNode, EventKindDecl);
impl_node_simple!(EraFunctionDeclNode, FunctionDecl);
impl_node_simple!(EraFunctionSDeclNode, FunctionSDecl);
impl_node_simple!(EraSharpDeclListNode, SharpDeclList);

impl_node_simple!(EraFunctionItemNode, FunctionItem);

impl_node_simple!(EraLabelStmtNode, LabelStmt);
impl_node_simple!(EraNopStmtNode, NopStmt);
impl_node_simple!(EraExprStmtNode, ExprStmt);
impl_node_simple!(EraRowAssignStmtNode, RowAssignStmt);
impl_node_simple!(EraResultCmdCallStmtNode, ResultCmdCallStmt);
impl_node_simple!(EraDebugPrintStmtNode, DebugPrintStmt);
impl_node_simple!(EraPrintStmtNode, PrintStmt);
impl_node_simple!(EraPrintDataStmtNode, PrintDataStmt);
impl_node_simple!(EraWaitStmtNode, WaitStmt);
impl_node_simple!(EraIfStmtNode, IfStmt);
impl_node_simple!(EraQuitStmtNode, QuitStmt);
impl_node_simple!(EraSelectCaseStmtNode, SelectCaseStmt);
impl_node_simple!(EraWhileStmtNode, WhileStmt);
impl_node_simple!(EraCallStmtNode, CallStmt);
impl_node_simple!(EraTryCallStmtNode, TryCallStmt);
impl_node_simple!(EraTryCCallStmtNode, TryCCallStmt);
impl_node_simple!(EraJumpStmtNode, JumpStmt);
impl_node_simple!(EraTryJumpStmtNode, TryJumpStmt);
impl_node_simple!(EraTryCJumpStmtNode, TryCJumpStmt);
impl_node_simple!(EraReturnStmtNode, ReturnStmt);
impl_node_simple!(EraContinueStmtNode, ContinueStmt);
impl_node_simple!(EraBreakStmtNode, BreakStmt);
impl_node_simple!(EraThrowStmtNode, ThrowStmt);
impl_node_simple!(EraRepeatStmtNode, RepeatStmt);
impl_node_simple!(EraGotoStmtNode, GotoStmt);
impl_node_simple!(EraForStmtNode, ForStmt);
impl_node_simple!(EraDoLoopStmtNode, DoLoopStmt);
// impl_node_simple!(EraGCreateStmtNode, GCreateStmt);
// impl_node_simple!(EraGDisposeStmtNode, GDisposeStmt);
// impl_node_simple!(EraGDrawSpriteStmtNode, GDrawSpriteStmt);
impl_node_simple!(EraSplitStmtNode, SplitStmt);
impl_node_simple!(EraTimesStmtNode, TimesStmt);
impl_node_simple!(EraSetBitStmtNode, SetBitStmt);
impl_node_simple!(EraClearBitStmtNode, ClearBitStmt);
impl_node_simple!(EraInvertBitStmtNode, InvertBitStmt);
impl_node_simple!(EraSetColorStmtNode, SetColorStmt);
impl_node_simple!(EraResetColorStmtNode, ResetColorStmt);
impl_node_simple!(EraSetBgColorStmtNode, SetBgColorStmt);
impl_node_simple!(EraResetBgColorStmtNode, ResetBgColorStmt);
impl_node_simple!(EraVarSetStmtNode, VarSetStmt);
impl_node_simple!(EraCVarSetStmtNode, CVarSetStmt);
impl_node_simple!(EraVarSizeStmtNode, VarSizeStmt);
impl_node_simple!(EraSwapStmtNode, SwapStmt);
impl_node_simple!(EraHtmlPrintStmtNode, HtmlPrintStmt);
impl_node_simple!(EraPrintButtonStmtNode, PrintButtonStmt);
impl_node_simple!(EraArrayRemoveStmtNode, ArrayRemoveStmt);
impl_node_simple!(EraArraySortStmtNode, ArraySortStmt);
impl_node_simple!(EraArrayMSortStmtNode, ArrayMSortStmt);
impl_node_simple!(EraArrayCopyStmtNode, ArrayCopyStmt);
impl_node_simple!(EraArrayShiftStmtNode, ArrayShiftStmt);
impl_node_simple!(EraInputStmtNode, InputStmt);
impl_node_simple!(EraInputSStmtNode, InputSStmt);
impl_node_simple!(EraTInputStmtNode, TInputStmt);
impl_node_simple!(EraTInputSStmtNode, TInputSStmt);
impl_node_simple!(EraOneInputStmtNode, OneInputStmt);
impl_node_simple!(EraOneInputSStmtNode, OneInputSStmt);
impl_node_simple!(EraTOneInputStmtNode, TOneInputStmt);
impl_node_simple!(EraTOneInputSStmtNode, TOneInputSStmt);
impl_node_simple!(EraReuseLastLineStmtNode, ReuseLastLineStmt);
impl_node_simple!(EraClearLineStmtNode, ClearLineStmt);
impl_node_simple!(EraDrawLineStmtNode, DrawLineStmt);
impl_node_simple!(EraCustomDrawLineStmtNode, CustomDrawLineStmt);
impl_node_simple!(EraTWaitStmtNode, TWaitStmt);
impl_node_simple!(EraFontStyleStmtNode, FontStyleStmt);
impl_node_simple!(EraFontBoldStmtNode, FontBoldStmt);
impl_node_simple!(EraFontItalicStmtNode, FontItalicStmt);
impl_node_simple!(EraFontRegularStmtNode, FontRegularStmt);
impl_node_simple!(EraSetFontStmtNode, SetFontStmt);
impl_node_simple!(EraStrDataStmtNode, StrDataStmt);
impl_node_simple!(EraPutFormStmtNode, PutFormStmt);
impl_node_simple!(EraSkipDispStmtNode, SkipDispStmt);
impl_node_simple!(EraBeginStmtNode, BeginStmt);
impl_node_simple!(EraDoTrainStmtNode, DoTrainStmt);
impl_node_simple!(EraRedrawStmtNode, RedrawStmt);
impl_node_simple!(EraStrLenStmtNode, StrLenStmt);
impl_node_simple!(EraStrLenUStmtNode, StrLenUStmt);
impl_node_simple!(EraAlignmentStmtNode, AlignmentStmt);
impl_node_simple!(EraToolTipSetDelayStmtNode, ToolTipSetDelayStmt);
impl_node_simple!(EraToolTipSetDurationStmtNode, ToolTipSetDurationStmt);
impl_node_simple!(EraRandomizeStmtNode, RandomizeStmt);
impl_node_simple!(EraDumpRandStmtNode, DumpRandStmt);
impl_node_simple!(EraInitRandStmtNode, InitRandStmt);
impl_node_simple!(EraBarStmtNode, BarStmt);
impl_node_simple!(EraAddCharaStmtNode, AddCharaStmt);
impl_node_simple!(EraPickUpCharaStmtNode, PickUpCharaStmt);
impl_node_simple!(EraDelCharaStmtNode, DelCharaStmt);
impl_node_simple!(EraSwapCharaStmtNode, SwapCharaStmt);
impl_node_simple!(EraAddCopyCharaStmtNode, AddCopyCharaStmt);
impl_node_simple!(EraResetStainStmtNode, ResetStainStmt);
impl_node_simple!(EraSaveCharaStmtNode, SaveCharaStmt);
impl_node_simple!(EraLoadCharaStmtNode, LoadCharaStmt);
impl_node_simple!(EraSetAnimeTimerStmtNode, SetAnimeTimerStmt);
impl_node_simple!(EraHtmlTagSplitStmtNode, HtmlTagSplitStmt);
impl_node_simple!(EraPowerStmtNode, PowerStmt);
impl_node_simple!(EraLoadDataStmtNode, LoadDataStmt);
impl_node_simple!(EraSaveDataStmtNode, SaveDataStmt);
// impl_node_simple!(EraCheckDataStmtNode, CheckDataStmt);
impl_node_simple!(EraRestartStmtNode, RestartStmt);
impl_node_simple!(EraGetTimeStmtNode, GetTimeStmt);
impl_node_simple!(EraLoadGlobalStmtNode, LoadGlobalStmt);
impl_node_simple!(EraSaveGlobalStmtNode, SaveGlobalStmt);
impl_node_simple!(EraLoadGameStmtNode, LoadGameStmt);
impl_node_simple!(EraSaveGameStmtNode, SaveGameStmt);
impl_node_simple!(EraDebugClearStmtNode, DebugClearStmt);
impl_node_simple!(EraResetDataStmtNode, ResetDataStmt);
impl_node_simple!(EraDataStmtNode, DataStmt);
impl_node_simple!(EraStmtListNode, StmtList);

impl_node_simple!(EraSelectCaseSingleNode, SelectCaseSingle);
impl_node_simple!(EraSelectCaseRangeNode, SelectCaseRange);
impl_node_simple!(EraSelectCaseCondNode, SelectCaseCond);

impl_node_simple!(EraPreUnaryExprNode, PreUnaryExpr);
impl_node_simple!(EraPostUnaryExprNode, PostUnaryExpr);
impl_node_simple!(EraBinaryExprNode, BinaryExpr);
impl_node_simple!(EraTernaryExprNode, TernaryExpr);
impl_node_simple!(EraFunCallExprNode, FunCallExpr);
impl_node_simple!(EraParenExprNode, ParenExpr);
impl_node_simple!(EraVarIdxExprNode, VarIdxExpr);
impl_node_simple!(EraVarNamespaceExprNode, VarNamespaceExpr);
impl_node_simple!(EraEmptyExprNode, EmptyExpr);
impl_node_simple!(EraExprListNode, ExprList);
impl_node_simple!(EraStringFormNode, StringForm);
impl_node_simple!(EraStringFormInterpPartNode, StringFormInterpPart);

impl_node_enum!(
    EraSharpDeclNode => EraSharpDeclNodeKind,
    VarDecl => EraVarDeclNode,
    VarSDecl => EraVarSDeclNode,
    LocalSizeDecl => EraLocalSizeDeclNode,
    LocalSSizeDecl => EraLocalSSizeDeclNode,
    DefineDecl => EraDefineDeclNode,
    EventKindDecl => EraEventKindDeclNode,
    FunctionDecl => EraFunctionDeclNode,
    FunctionSDecl => EraFunctionSDeclNode,
);
impl_node_enum!(
    EraDeclItemNode => EraDeclItemNodeKind,
    // SharpDecl => EraSharpDeclNode,
    VarDecl => EraVarDeclNode,
    VarSDecl => EraVarSDeclNode,
    LocalSizeDecl => EraLocalSizeDeclNode,
    LocalSSizeDecl => EraLocalSSizeDeclNode,
    DefineDecl => EraDefineDeclNode,
    EventKindDecl => EraEventKindDeclNode,
    FunctionDecl => EraFunctionDeclNode,
    FunctionSDecl => EraFunctionSDeclNode,
    FunctionItem => EraFunctionItemNode,
);
impl_node_enum!(
    EraStmtNode => EraStmtNodeKind,
    LabelStmt => EraLabelStmtNode,
    NopStmt => EraNopStmtNode,
    ExprStmt => EraExprStmtNode,
    RowAssignStmt => EraRowAssignStmtNode,
    ResultCmdCallStmt => EraResultCmdCallStmtNode,
    DebugPrintStmt => EraDebugPrintStmtNode,
    PrintStmt => EraPrintStmtNode,
    PrintDataStmt => EraPrintDataStmtNode,
    WaitStmt => EraWaitStmtNode,
    IfStmt => EraIfStmtNode,
    QuitStmt => EraQuitStmtNode,
    SelectCaseStmt => EraSelectCaseStmtNode,
    WhileStmt => EraWhileStmtNode,
    CallStmt => EraCallStmtNode,
    TryCallStmt => EraTryCallStmtNode,
    TryCCallStmt => EraTryCCallStmtNode,
    JumpStmt => EraJumpStmtNode,
    TryJumpStmt => EraTryJumpStmtNode,
    TryCJumpStmt => EraTryCJumpStmtNode,
    ReturnStmt => EraReturnStmtNode,
    ContinueStmt => EraContinueStmtNode,
    BreakStmt => EraBreakStmtNode,
    ThrowStmt => EraThrowStmtNode,
    RepeatStmt => EraRepeatStmtNode,
    GotoStmt => EraGotoStmtNode,
    ForStmt => EraForStmtNode,
    DoLoopStmt => EraDoLoopStmtNode,
    // GCreateStmt => EraGCreateStmtNode,
    // GDisposeStmt => EraGDisposeStmtNode,
    // GDrawSpriteStmt => EraGDrawSpriteStmtNode,
    SplitStmt => EraSplitStmtNode,
    TimesStmt => EraTimesStmtNode,
    SetBitStmt => EraSetBitStmtNode,
    ClearBitStmt => EraClearBitStmtNode,
    InvertBitStmt => EraInvertBitStmtNode,
    SetColorStmt => EraSetColorStmtNode,
    ResetColorStmt => EraResetColorStmtNode,
    SetBgColorStmt => EraSetBgColorStmtNode,
    ResetBgColorStmt => EraResetBgColorStmtNode,
    VarSetStmt => EraVarSetStmtNode,
    CVarSetStmt => EraCVarSetStmtNode,
    VarSizeStmt => EraVarSizeStmtNode,
    SwapStmt => EraSwapStmtNode,
    HtmlPrintStmt => EraHtmlPrintStmtNode,
    PrintButtonStmt => EraPrintButtonStmtNode,
    ArrayRemoveStmt => EraArrayRemoveStmtNode,
    ArraySortStmt => EraArraySortStmtNode,
    ArrayMSortStmt => EraArrayMSortStmtNode,
    ArrayCopyStmt => EraArrayCopyStmtNode,
    ArrayShiftStmt => EraArrayShiftStmtNode,
    InputStmt => EraInputStmtNode,
    InputSStmt => EraInputSStmtNode,
    TInputStmt => EraTInputStmtNode,
    TInputSStmt => EraTInputSStmtNode,
    OneInputStmt => EraOneInputStmtNode,
    OneInputSStmt => EraOneInputSStmtNode,
    TOneInputStmt => EraTOneInputStmtNode,
    TOneInputSStmt => EraTOneInputSStmtNode,
    ReuseLastLineStmt => EraReuseLastLineStmtNode,
    ClearLineStmt => EraClearLineStmtNode,
    DrawLineStmt => EraDrawLineStmtNode,
    CustomDrawLineStmt => EraCustomDrawLineStmtNode,
    TWaitStmt => EraTWaitStmtNode,
    FontStyleStmt => EraFontStyleStmtNode,
    FontBoldStmt => EraFontBoldStmtNode,
    FontItalicStmt => EraFontItalicStmtNode,
    FontRegularStmt => EraFontRegularStmtNode,
    SetFontStmt => EraSetFontStmtNode,
    StrDataStmt => EraStrDataStmtNode,
    PutFormStmt => EraPutFormStmtNode,
    SkipDispStmt => EraSkipDispStmtNode,
    BeginStmt => EraBeginStmtNode,
    DoTrainStmt => EraDoTrainStmtNode,
    RedrawStmt => EraRedrawStmtNode,
    StrLenStmt => EraStrLenStmtNode,
    StrLenUStmt => EraStrLenUStmtNode,
    AlignmentStmt => EraAlignmentStmtNode,
    ToolTipSetDelayStmt => EraToolTipSetDelayStmtNode,
    ToolTipSetDurationStmt => EraToolTipSetDurationStmtNode,
    RandomizeStmt => EraRandomizeStmtNode,
    DumpRandStmt => EraDumpRandStmtNode,
    InitRandStmt => EraInitRandStmtNode,
    BarStmt => EraBarStmtNode,
    AddCharaStmt => EraAddCharaStmtNode,
    PickUpCharaStmt => EraPickUpCharaStmtNode,
    DelCharaStmt => EraDelCharaStmtNode,
    SwapCharaStmt => EraSwapCharaStmtNode,
    AddCopyCharaStmt => EraAddCopyCharaStmtNode,
    ResetStainStmt => EraResetStainStmtNode,
    SaveCharaStmt => EraSaveCharaStmtNode,
    LoadCharaStmt => EraLoadCharaStmtNode,
    SetAnimeTimerStmt => EraSetAnimeTimerStmtNode,
    HtmlTagSplitStmt => EraHtmlTagSplitStmtNode,
    PowerStmt => EraPowerStmtNode,
    LoadDataStmt => EraLoadDataStmtNode,
    SaveDataStmt => EraSaveDataStmtNode,
    // CheckDataStmt => EraCheckDataStmtNode,
    RestartStmt => EraRestartStmtNode,
    GetTimeStmt => EraGetTimeStmtNode,
    LoadGlobalStmt => EraLoadGlobalStmtNode,
    SaveGlobalStmt => EraSaveGlobalStmtNode,
    LoadGameStmt => EraLoadGameStmtNode,
    SaveGameStmt => EraSaveGameStmtNode,
    DebugClearStmt => EraDebugClearStmtNode,
    ResetDataStmt => EraResetDataStmtNode,
    DataStmt => EraDataStmtNode,
);
impl_node_enum!(
    EraSelectCaseCaseNode => EraSelectCaseCaseNodeKind,
    SelectCaseSingle => EraSelectCaseSingleNode,
    SelectCaseRange => EraSelectCaseRangeNode,
    SelectCaseCond => EraSelectCaseCondNode,
);
impl_node_enum!(
    EraExprNode => EraExprNodeKind,
    PreUnaryExpr => EraPreUnaryExprNode,
    PostUnaryExpr => EraPostUnaryExprNode,
    BinaryExpr => EraBinaryExprNode,
    TernaryExpr => EraTernaryExprNode,
    FunCallExpr => EraFunCallExprNode,
    ParenExpr => EraParenExprNode,
    VarIdxExpr => EraVarIdxExprNode,
    VarNamespaceExpr => EraVarNamespaceExprNode,
    EmptyExpr => EraEmptyExprNode,
    StringForm => EraStringFormNode,
);

// Leaves
impl_leaf_simple!(EraIdentLeaf, Identifier);
impl_leaf_simple!(EraIntLiteralLeaf, IntLiteral);
// impl_leaf_simple!(EraFloatLiteralLeaf, FloatLiteral);
impl_leaf_simple!(EraStringLiteralLeaf, StringLiteral);
impl_leaf_simple!(EraPlainStringLiteralLeaf, PlainStringLiteral);

impl_leaf_enum!(
    EraOperatorLeaf,
    Plus,
    Minus,
    Multiply,
    Divide,
    Percentage,
    BitAnd,
    BitOr,
    BitXor,
    BitNot,
    BitShiftL,
    BitShiftR,
    LogicalNot,
    LogicalAnd,
    LogicalOr,
    CmpEq,
    CmpNEq,
    CmpLT,
    CmpGT,
    CmpLEq,
    CmpGEq,
    QuestionMark,
    Assign,
    ExprAssign,
    PlusAssign,
    MinusAssign,
    MultiplyAssign,
    DivideAssign,
    ModuloAssign,
    BitAndAssign,
    BitOrAssign,
    BitXorAssign,
    Increment,
    Decrement,
);
impl_leaf_enum!(
    EraExprLeaf,
    // Identifier => EraIdentLeaf,
    // IntLiteral => EraIntLiteralLeaf,
    // // FloatLiteral => EraFloatLiteralLeaf,
    // StringLiteral => EraStringLiteralLeaf,
    Identifier,
    IntLiteral,
    // FloatLiteral,
    StringLiteral,
);
impl_leaf_enum!(EraEventKindLeaf, KwOnly, KwPri, KwLater,);

// TODO: Use packed pointers
#[derive(Debug, Clone)]
pub enum EraExprNodeOrLeaf<'a> {
    Node(EraExprNode<'a>),
    Leaf(EraExprLeaf<'a>),
}

impl<'a> EraExprNodeOrLeaf<'a> {
    pub fn cast(val: SyntaxElementRef<'a, Token>) -> Option<Self> {
        Some(match val {
            SyntaxElementRef::Node(node) => Self::Node(EraExprNode::cast(node)?),
            SyntaxElementRef::Token(token) => Self::Leaf(EraExprLeaf::cast(token)?),
        })
    }

    pub fn can_cast(kind: Token) -> bool {
        EraExprNode::can_cast(kind) || EraExprLeaf::can_cast(kind)
    }

    pub fn is_node(&self) -> bool {
        matches!(self, Self::Node(_))
    }

    pub fn is_leaf(&self) -> bool {
        matches!(self, Self::Leaf(_))
    }

    pub fn as_node(&self) -> Option<&'a EraExprNode> {
        match self {
            Self::Node(node) => Some(node),
            _ => None,
        }
    }

    pub fn as_leaf(&self) -> Option<&'a EraExprLeaf> {
        match self {
            Self::Leaf(leaf) => Some(leaf),
            _ => None,
        }
    }

    pub fn src_span(&self) -> SrcSpan {
        match self {
            Self::Node(node) => node.node().text_range().into(),
            Self::Leaf(leaf) => leaf.token().text_range().into(),
        }
    }
}

// TODO: Use packed pointers
#[derive(Debug, Clone)]
pub enum EraStringFormPartNodeOrLeaf<'a> {
    Node(EraStringFormInterpPartNode<'a>),
    Leaf(EraPlainStringLiteralLeaf<'a>),
}

impl<'a> EraStringFormPartNodeOrLeaf<'a> {
    pub fn cast(val: SyntaxElementRef<'a, Token>) -> Option<Self> {
        Some(match val {
            SyntaxElementRef::Node(node) => Self::Node(EraStringFormInterpPartNode::cast(node)?),
            SyntaxElementRef::Token(token) => Self::Leaf(EraPlainStringLiteralLeaf::cast(token)?),
        })
    }

    pub fn can_cast(kind: Token) -> bool {
        EraStringFormInterpPartNode::can_cast(kind) || EraPlainStringLiteralLeaf::can_cast(kind)
    }

    pub fn is_node(&self) -> bool {
        matches!(self, Self::Node(_))
    }

    pub fn is_leaf(&self) -> bool {
        matches!(self, Self::Leaf(_))
    }

    pub fn as_node(&self) -> Option<&'a EraStringFormInterpPartNode> {
        match self {
            Self::Node(node) => Some(node),
            _ => None,
        }
    }

    pub fn as_leaf(&self) -> Option<&'a EraPlainStringLiteralLeaf> {
        match self {
            Self::Leaf(leaf) => Some(leaf),
            _ => None,
        }
    }

    pub fn src_span(&self) -> SrcSpan {
        match self {
            Self::Node(node) => node.node().text_range().into(),
            Self::Leaf(leaf) => leaf.token().text_range().into(),
        }
    }
}

// pub struct EraDeclItemNode<'a>(&'a SyntaxNode<Token>);

// impl<'a> EraAstNode<'a> for EraDeclItemNode<'a> {
//     fn cast(node: &'a SyntaxNode<Token>) -> Option<Self> {
//         if Self::can_cast(node.kind()) {
//             Some(Self(node))
//         } else {
//             None
//         }
//     }

//     fn can_cast(kind: Token) -> bool {
//         matches!(
//             kind,
//             Token::DefineDecl | Token::FunctionItem | Token::VarDecl | Token::VarSDecl
//         )
//     }

//     fn node(&self) -> &'a SyntaxNode<Token> {
//         self.0
//     }
// }

// impl EraDeclItemNode<'_> {
//     pub fn children(&self) -> impl Iterator<Item = EraDeclItemNode> {
//         self.0.children().filter_map(EraDeclItemNode::cast)
//     }
// }

impl EraProgramNode<'_> {
    pub fn children(&self) -> impl Iterator<Item = EraDeclItemNode> {
        self.0.children().filter_map(EraDeclItemNode::cast)
    }
}

impl EraVarDeclNode<'_> {
    pub fn name(&self) -> Option<EraIdentLeaf> {
        self.node()
            .children_with_tokens()
            .filter_map(|x| x.into_token())
            .find_map(EraIdentLeaf::cast)
    }

    pub fn dimensions(&self) -> Option<EraExprListNode> {
        self.node()
            .children_with_tokens()
            .take_while(|x| x.as_token().filter(|x| x.kind() == Token::Assign).is_none())
            .filter_map(|x| x.into_node())
            .find_map(EraExprListNode::cast)
    }

    pub fn initializer(&self) -> Option<EraExprListNode> {
        let mut iter = self.node().children_with_tokens();
        // Skip `=`
        iter.find_map(|x| x.into_token().filter(|x| x.kind() == Token::Assign))?;
        iter.filter_map(|x| x.into_node())
            .find_map(EraExprListNode::cast)
    }

    pub fn is_const(&self) -> bool {
        self.node().children_with_tokens().any(|x| {
            x.as_token()
                .filter(|x| x.kind() == Token::KwConst)
                .is_some()
        })
    }

    pub fn is_charadata(&self) -> bool {
        self.node().children_with_tokens().any(|x| {
            x.as_token()
                .filter(|x| x.kind() == Token::KwCharadata)
                .is_some()
        })
    }

    pub fn is_global(&self) -> bool {
        self.node().children_with_tokens().any(|x| {
            x.as_token()
                .filter(|x| x.kind() == Token::KwGlobal)
                .is_some()
        })
    }
}

impl EraVarSDeclNode<'_> {
    pub fn name(&self) -> Option<EraIdentLeaf> {
        self.node()
            .children_with_tokens()
            .filter_map(|x| x.into_token())
            .find_map(EraIdentLeaf::cast)
    }

    pub fn dimensions(&self) -> Option<EraExprListNode> {
        self.node()
            .children_with_tokens()
            .take_while(|x| x.as_token().filter(|x| x.kind() == Token::Assign).is_none())
            .filter_map(|x| x.into_node())
            .find_map(EraExprListNode::cast)
    }

    pub fn initializer(&self) -> Option<EraExprListNode> {
        let mut iter = self.node().children_with_tokens();
        // Skip `=`
        iter.find_map(|x| x.into_token().filter(|x| x.kind() == Token::Assign))?;
        iter.filter_map(|x| x.into_node())
            .find_map(EraExprListNode::cast)
    }

    pub fn is_const(&self) -> bool {
        self.node().children_with_tokens().any(|x| {
            x.as_token()
                .filter(|x| x.kind() == Token::KwConst)
                .is_some()
        })
    }

    pub fn is_charadata(&self) -> bool {
        self.node().children_with_tokens().any(|x| {
            x.as_token()
                .filter(|x| x.kind() == Token::KwCharadata)
                .is_some()
        })
    }

    pub fn is_global(&self) -> bool {
        self.node().children_with_tokens().any(|x| {
            x.as_token()
                .filter(|x| x.kind() == Token::KwGlobal)
                .is_some()
        })
    }
}

impl EraLocalSizeDeclNode<'_> {
    pub fn size(&self) -> Option<EraExprNodeOrLeaf> {
        self.node()
            .children_with_tokens()
            .find_map(EraExprNodeOrLeaf::cast)
    }
}

impl EraLocalSSizeDeclNode<'_> {
    pub fn size(&self) -> Option<EraExprNodeOrLeaf> {
        self.node()
            .children_with_tokens()
            .find_map(EraExprNodeOrLeaf::cast)
    }
}

// ...............................................
// impl EraDefineDeclNode<'_> {
//     pub fn name(&self) -> Option<EraIdentLeaf> {
//         self.node()
//             .children_with_tokens()
//             .filter_map(|x| x.into_token())
//             .find_map(EraIdentLeaf::cast)
//     }

//     pub fn value(&self) -> Option<EraExprNodeOrLeaf> {
//         self.node()
//             .children_with_tokens()
//             .find_map(EraExprNodeOrLeaf::cast)
//     }
// }

impl EraEventKindDeclNode<'_> {
    pub fn event_kind(&self) -> Option<EraEventKindLeaf> {
        self.node()
            .children_with_tokens()
            .filter_map(|x| x.into_token())
            .find_map(EraEventKindLeaf::cast)
    }
}

impl EraFunctionDeclNode<'_> {
    // No extra data
}

impl EraFunctionSDeclNode<'_> {
    // No extra data
}

impl EraSharpDeclListNode<'_> {
    pub fn children(&self) -> impl Iterator<Item = EraDeclItemNode> {
        self.node().children().filter_map(EraDeclItemNode::cast)
    }
}

impl EraFunctionItemNode<'_> {
    pub fn name(&self) -> Option<EraIdentLeaf> {
        self.node()
            .children_with_tokens()
            .filter_map(|x| x.into_token())
            .find_map(EraIdentLeaf::cast)
    }

    pub fn arguments(&self) -> Option<EraExprListNode> {
        self.node().children().find_map(EraExprListNode::cast)
    }

    pub fn sharp_declarations(&self) -> Option<EraSharpDeclListNode> {
        self.node().children().find_map(EraSharpDeclListNode::cast)
    }

    pub fn body(&self) -> Option<EraStmtListNode> {
        self.node().children().find_map(EraStmtListNode::cast)
    }
}

impl EraPreUnaryExprNode<'_> {
    pub fn operator(&self) -> Option<EraOperatorLeaf> {
        self.node()
            .children_with_tokens()
            .filter_map(|x| x.into_token())
            .find_map(EraOperatorLeaf::cast)
    }

    pub fn rhs(&self) -> Option<EraExprNodeOrLeaf> {
        self.node()
            .children_with_tokens()
            .find_map(EraExprNodeOrLeaf::cast)
    }
}

impl EraPostUnaryExprNode<'_> {
    pub fn operator(&self) -> Option<EraOperatorLeaf> {
        self.node()
            .children_with_tokens()
            .filter_map(|x| x.into_token())
            .find_map(EraOperatorLeaf::cast)
    }

    pub fn lhs(&self) -> Option<EraExprNodeOrLeaf> {
        self.node()
            .children_with_tokens()
            .find_map(EraExprNodeOrLeaf::cast)
    }
}

impl EraBinaryExprNode<'_> {
    pub fn lhs(&self) -> Option<EraExprNodeOrLeaf> {
        self.node()
            .children_with_tokens()
            .find_map(EraExprNodeOrLeaf::cast)
    }

    pub fn operator(&self) -> Option<EraOperatorLeaf> {
        self.node()
            .children_with_tokens()
            .filter_map(|x| x.into_token())
            .find_map(EraOperatorLeaf::cast)
    }

    pub fn rhs(&self) -> Option<EraExprNodeOrLeaf> {
        let mut iter = self.node().children_with_tokens();
        // Skip operator
        iter.find_map(|x| x.into_token().and_then(EraOperatorLeaf::cast))?;
        iter.find_map(EraExprNodeOrLeaf::cast)
    }
}

impl EraTernaryExprNode<'_> {
    pub fn condition(&self) -> Option<EraExprNodeOrLeaf> {
        self.node()
            .children_with_tokens()
            .find_map(EraExprNodeOrLeaf::cast)
    }

    pub fn true_expr(&self) -> Option<EraExprNodeOrLeaf> {
        let mut iter = self.node().children_with_tokens();
        // Skip `?`
        iter.find_map(|x| x.into_token().filter(|x| x.kind() == Token::QuestionMark))?;
        iter.find_map(EraExprNodeOrLeaf::cast)
    }

    pub fn false_expr(&self) -> Option<EraExprNodeOrLeaf> {
        let mut iter = self.node().children_with_tokens();
        // Skip `?` and true expr (i.e. `#`)
        iter.find_map(|x| x.into_token().filter(|x| x.kind() == Token::NumberSign))?;
        iter.find_map(EraExprNodeOrLeaf::cast)
    }
}

impl EraFunCallExprNode<'_> {
    pub fn name(&self) -> Option<EraIdentLeaf> {
        self.node()
            .children_with_tokens()
            .filter_map(|x| x.into_token())
            .find_map(EraIdentLeaf::cast)
    }

    pub fn arguments(&self) -> Option<EraExprListNode> {
        self.node().children().find_map(EraExprListNode::cast)
    }
}

impl EraParenExprNode<'_> {
    pub fn child(&self) -> Option<EraExprNodeOrLeaf> {
        self.node()
            .children_with_tokens()
            .find_map(EraExprNodeOrLeaf::cast)
    }
}

impl EraVarIdxExprNode<'_> {
    pub fn name(&self) -> Option<EraIdentLeaf> {
        self.node()
            .children_with_tokens()
            .filter_map(|x| x.into_token())
            .find_map(EraIdentLeaf::cast)
    }

    pub fn indices(&self) -> impl Iterator<Item = EraExprNodeOrLeaf> {
        self.node()
            .children_with_tokens()
            .skip_while(|x| x.as_token().filter(|x| x.kind() == Token::Colon).is_none())
            .filter_map(EraExprNodeOrLeaf::cast)
    }
}

impl EraVarNamespaceExprNode<'_> {
    pub fn name(&self) -> Option<EraIdentLeaf> {
        self.node()
            .children_with_tokens()
            .filter_map(|x| x.into_token())
            .find_map(EraIdentLeaf::cast)
    }

    pub fn namespace(&self) -> Option<EraIdentLeaf> {
        // Skip `@`
        self.node()
            .children_with_tokens()
            .skip_while(|x| x.as_token().filter(|x| x.kind() == Token::At).is_none())
            .filter_map(|x| x.into_token())
            .find_map(EraIdentLeaf::cast)
    }
}

impl EraEmptyExprNode<'_> {
    // No extra data
}

impl EraExprListNode<'_> {
    pub fn children(&self) -> impl Iterator<Item = EraExprNodeOrLeaf> {
        self.node()
            .children_with_tokens()
            .filter_map(EraExprNodeOrLeaf::cast)
    }
}

impl EraStringFormNode<'_> {
    pub fn parts(&self) -> impl Iterator<Item = EraStringFormPartNodeOrLeaf> {
        self.node()
            .children_with_tokens()
            .filter_map(EraStringFormPartNodeOrLeaf::cast)
    }
}

impl EraStringFormInterpPartNode<'_> {
    pub fn expr(&self) -> Option<EraExprNodeOrLeaf> {
        self.node()
            .children_with_tokens()
            .find_map(EraExprNodeOrLeaf::cast)
    }

    pub fn width(&self) -> Option<EraExprNodeOrLeaf> {
        // Skip first `,`
        self.node()
            .children_with_tokens()
            .skip_while(|x| x.as_token().filter(|x| x.kind() == Token::Comma).is_none())
            .find_map(EraExprNodeOrLeaf::cast)
    }

    pub fn alignment(&self) -> Option<EraIdentLeaf> {
        // Skip second `,`
        self.node()
            .children_with_tokens()
            .skip_while(|x| x.as_token().filter(|x| x.kind() == Token::Comma).is_none())
            .skip(1)
            .skip_while(|x| x.as_token().filter(|x| x.kind() == Token::Comma).is_none())
            .filter_map(|x| x.into_token())
            .find_map(EraIdentLeaf::cast)
    }
}
