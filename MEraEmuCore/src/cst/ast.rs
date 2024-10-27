use std::iter::FusedIterator;

use cstree::{
    build::{GreenNodeBuilder, NodeCache},
    green::{GreenNode, GreenToken},
    interning::{Resolver, TokenKey},
    util::NodeOrToken,
    Syntax,
};
use itertools::Itertools;
use paste::paste;

use crate::types::*;
use crate::util::syntax::*;

use EraTokenKind as Token;

// fn not_newline<D>(x: &SyntaxElementRef<'_, Token, D>) -> bool {
//     x.as_token().map_or(true, |x| x.kind() != Token::LineBreak)
// }

// fn not_comma<D>(x: &SyntaxElementRef<'_, Token, D>) -> bool {
//     x.as_token().map_or(true, |x| x.kind() != Token::Comma)
// }

fn not_newline(x: &SyntaxElementRef<'_, Token>) -> bool {
    x.as_token().map_or(true, |x| x.kind() != Token::LineBreak)
}

fn not_comma(x: &SyntaxElementRef<'_, Token>) -> bool {
    x.as_token().map_or(true, |x| x.kind() != Token::Comma)
}

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
        #[derive(Debug, Clone, Copy)]
        #[repr(transparent)]
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

        paste! {
            impl $name<'_> {
                pub fn src_span(&self) -> SrcSpan {
                    self.node().text_range().into()
                }

                pub fn to_owned(&self) -> [<Owned $name>] {
                    [<Owned $name>](self.node().clone())
                }
            }

            #[derive(Debug, Clone)]
            pub struct [<Owned $name>](SyntaxNode<Token>);

            impl [<Owned $name>] {
                pub fn as_ref(&self) -> $name {
                    $name(&self.0)
                }
            }
        }
    };
}

macro_rules! impl_node_enum {
    ($name:ident => $kind_name:ident, $($token_kind:ident => $token_node:ident),+ $(,)?) => {
        #[derive(Debug, Clone, Copy)]
        #[repr(transparent)]
        pub struct $name<'a>(&'a SyntaxNode<Token>);

        #[derive(Debug, Clone, Copy)]
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

        impl<'a> $name<'a> {
            pub fn kind(&self) -> $kind_name<'a> {
                match self.node().kind() {
                    $(Token::$token_kind => $kind_name::$token_kind($token_node::cast(self.node()).unwrap()),)+
                    _ => unreachable!("invalid node kind"),
                }
            }

            pub fn src_span(&self) -> SrcSpan {
                self.node().text_range().into()
            }
        }

        $(impl<'a> From<$token_node<'a>> for $name<'a> {
            fn from(node: $token_node<'a>) -> Self {
                Self(node.0)
            }
        })+

        paste! {
            impl $name<'_> {
                pub fn to_owned(&self) -> [<Owned $name>] {
                    [<Owned $name>](self.node().clone())
                }
            }

            #[derive(Debug, Clone)]
            pub struct [<Owned $name>](SyntaxNode<Token>);

            impl [<Owned $name>] {
                pub fn as_ref(&self) -> $name {
                    $name(&self.0)
                }
            }
        }
    };
}

macro_rules! impl_leaf_simple {
    ($name:ident, $token_kind:ident) => {
        #[derive(Debug, Clone, Copy)]
        #[repr(transparent)]
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

        paste! {
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

                pub fn to_owned(&self) -> [<Owned $name>] {
                    [<Owned $name>](self.token().clone())
                }
            }

            #[derive(Debug, Clone)]
            pub struct [<Owned $name>](SyntaxToken<Token>);

            impl [<Owned $name>] {
                pub fn as_ref(&self) -> $name {
                    $name(&self.0)
                }
            }
        }
    };
}

macro_rules! impl_leaf_enum {
    ($name:ident, $($token_kind:ident),+ $(,)?) => {
        #[derive(Debug, Clone, Copy)]
        #[repr(transparent)]
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

        paste! {
            impl $name<'_> {
                pub fn to_owned(&self) -> [<Owned $name>] {
                    [<Owned $name>](self.token().clone())
                }
            }

            #[derive(Debug, Clone)]
            pub struct [<Owned $name>](SyntaxToken<Token>);

            impl [<Owned $name>] {
                pub fn as_ref(&self) -> $name {
                    $name(&self.0)
                }
            }
        }
    };
}

// #[derive(Debug, Clone, Copy)]
// #[repr(transparent)]
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
impl_node_simple!(EraTransientDeclNode, TransientDecl);
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
impl_node_simple!(EraForceWaitStmtNode, ForceWaitStmt);
impl_node_simple!(EraWaitAnyKeyStmtNode, WaitAnyKeyStmt);
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
impl_node_simple!(EraRestartStmtNode, RestartStmt);
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
impl_node_simple!(EraPrintButtonCStmtNode, PrintButtonCStmt);
impl_node_simple!(EraPrintButtonLCStmtNode, PrintButtonLCStmt);
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
impl_node_simple!(EraBarLStmtNode, BarLStmt);
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
impl_node_simple!(EraSelectCasePredListNode, SelectCasePredList);

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
    TransientDecl => EraTransientDeclNode,
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
    TransientDecl => EraTransientDeclNode,
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
    ForceWaitStmt => EraForceWaitStmtNode,
    WaitAnyKeyStmt => EraWaitAnyKeyStmtNode,
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
    PrintButtonCStmt => EraPrintButtonCStmtNode,
    PrintButtonLCStmt => EraPrintButtonLCStmtNode,
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
    BarLStmt => EraBarLStmtNode,
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
    EraSelectCasePredNode => EraSelectCasePredNodeKind,
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
impl_leaf_simple!(EraKwIdentLeaf, KwIdent);
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

macro_rules! impl_from {
    ($to:ident, $($from:ident),+ $(,)?) => {
        $(impl<'a> From<$from<'a>> for $to<'a> {
            fn from(val: $from<'a>) -> Self {
                Self(val.0)
            }
        })+
    };
}

// Macro for homogeneous nodes (nodes with the same structure)
macro_rules! impl_homogeneous {
    ($primary:ident, $($secondary:ident),+) => {
        $(impl<'a> core::ops::Deref for $secondary<'a> {
            type Target = $primary<'a>;

            fn deref(&self) -> &Self::Target {
                unsafe { std::mem::transmute(self) }
            }
        })+

        impl_from!($primary, $($secondary),+);
    };
}

impl<'a> From<EraIdentLeaf<'a>> for EraExprLeaf<'a> {
    fn from(val: EraIdentLeaf<'a>) -> Self {
        Self(val.0)
    }
}

impl EraIdentLeaf<'_> {
    pub fn text_key(&self) -> TokenKey {
        self.token().text_key().unwrap()
    }
}

impl EraKwIdentLeaf<'_> {
    pub fn text_key(&self) -> TokenKey {
        self.token().text_key().unwrap()
    }
}

// TODO: Use packed pointers
#[derive(Debug, Clone, Copy)]
pub enum EraExprNodeOrLeaf<'a> {
    Node(EraExprNode<'a>),
    Leaf(EraExprLeaf<'a>),
}

impl<'a> TryFrom<EraExprNodeOrLeaf<'a>> for EraIdentLeaf<'a> {
    type Error = EraExprNodeOrLeaf<'a>;

    fn try_from(val: EraExprNodeOrLeaf<'a>) -> Result<Self, Self::Error> {
        let leaf = match val {
            EraExprNodeOrLeaf::Leaf(leaf) => leaf,
            _ => return Err(val),
        };
        if let Some(leaf) = EraIdentLeaf::cast(leaf.token()) {
            Ok(leaf)
        } else {
            Err(EraExprNodeOrLeaf::Leaf(leaf))
        }
    }
}

impl<'a> From<EraExprNode<'a>> for EraExprNodeOrLeaf<'a> {
    fn from(val: EraExprNode<'a>) -> Self {
        Self::Node(val)
    }
}

impl<'a> From<EraExprLeaf<'a>> for EraExprNodeOrLeaf<'a> {
    fn from(val: EraExprLeaf<'a>) -> Self {
        Self::Leaf(val)
    }
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

    pub fn inner(&self) -> SyntaxElementRef<'a, Token> {
        match self {
            Self::Node(node) => SyntaxElementRef::Node(node.node()),
            Self::Leaf(leaf) => SyntaxElementRef::Token(leaf.token()),
        }
    }
}

// TODO: Use packed pointers
#[derive(Debug, Clone, Copy)]
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

// TODO: Use packed pointers
#[derive(Debug, Clone, Copy)]
pub enum EraVarExprNodeOrLeaf<'a> {
    Node(EraVarNamespaceExprNode<'a>),
    Leaf(EraIdentLeaf<'a>),
}

impl<'a> From<EraVarExprNodeOrLeaf<'a>> for EraExprNodeOrLeaf<'a> {
    fn from(val: EraVarExprNodeOrLeaf<'a>) -> Self {
        match val {
            EraVarExprNodeOrLeaf::Node(node) => EraExprNodeOrLeaf::Node(node.into()),
            EraVarExprNodeOrLeaf::Leaf(leaf) => EraExprNodeOrLeaf::Leaf(leaf.into()),
        }
    }
}

impl<'a> From<EraIdentLeaf<'a>> for EraVarExprNodeOrLeaf<'a> {
    fn from(val: EraIdentLeaf<'a>) -> Self {
        Self::Leaf(val)
    }
}

impl<'a> From<EraVarNamespaceExprNode<'a>> for EraVarExprNodeOrLeaf<'a> {
    fn from(val: EraVarNamespaceExprNode<'a>) -> Self {
        Self::Node(val)
    }
}

impl<'a> EraVarExprNodeOrLeaf<'a> {
    pub fn cast(val: SyntaxElementRef<'a, Token>) -> Option<Self> {
        Some(match val {
            SyntaxElementRef::Node(node) => Self::Node(EraVarNamespaceExprNode::cast(node)?),
            SyntaxElementRef::Token(token) => Self::Leaf(EraIdentLeaf::cast(token)?),
        })
    }

    pub fn can_cast(kind: Token) -> bool {
        EraVarNamespaceExprNode::can_cast(kind) || EraIdentLeaf::can_cast(kind)
    }

    pub fn is_node(&self) -> bool {
        matches!(self, Self::Node(_))
    }

    pub fn is_leaf(&self) -> bool {
        matches!(self, Self::Leaf(_))
    }

    pub fn as_node(&self) -> Option<&'a EraVarNamespaceExprNode> {
        match self {
            Self::Node(node) => Some(node),
            _ => None,
        }
    }

    pub fn as_leaf(&self) -> Option<&'a EraIdentLeaf> {
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

#[derive(Debug, Clone, Copy)]
pub enum EraVarOptIdxExprConstruct<'a> {
    VarIdx(EraVarIdxExprNode<'a>),
    Var(EraVarExprNodeOrLeaf<'a>),
}

impl<'a> EraVarOptIdxExprConstruct<'a> {
    pub fn cast(val: SyntaxElementRef<'a, Token>) -> Option<Self> {
        Some(if let NodeOrToken::Node(node) = val {
            if let Some(x) = EraVarIdxExprNode::cast(node) {
                Self::VarIdx(x)
            } else {
                Self::Var(EraVarExprNodeOrLeaf::cast(val)?)
            }
        } else {
            Self::Var(EraVarExprNodeOrLeaf::cast(val)?)
        })
    }

    pub fn can_cast(kind: Token) -> bool {
        EraVarIdxExprNode::can_cast(kind) || EraVarExprNodeOrLeaf::can_cast(kind)
    }

    pub fn is_var_idx(&self) -> bool {
        matches!(self, Self::VarIdx(_))
    }

    pub fn is_var(&self) -> bool {
        matches!(self, Self::Var(_))
    }

    pub fn as_var_idx(&self) -> Option<EraVarIdxExprNode<'a>> {
        match self {
            Self::VarIdx(node) => Some(*node),
            _ => None,
        }
    }

    pub fn as_var(&self) -> Option<EraVarExprNodeOrLeaf<'a>> {
        match self {
            Self::Var(var) => Some(*var),
            _ => None,
        }
    }

    pub fn src_span(&self) -> SrcSpan {
        match self {
            Self::VarIdx(node) => node.node().text_range().into(),
            Self::Var(var) => var.src_span(),
        }
    }

    pub fn name(&self) -> Option<EraVarExprNodeOrLeaf<'a>> {
        match self {
            Self::VarIdx(node) => node.name(),
            Self::Var(var) => Some(*var),
        }
    }

    pub fn indices(&self) -> impl Iterator<Item = EraExprNodeOrLeaf<'a>> + FusedIterator {
        use itertools::Either;
        match self {
            Self::VarIdx(node) => Either::Left(node.indices()),
            Self::Var(var) => Either::Right(std::iter::empty()),
        }
        .into_iter()
    }
}

impl<'a> From<EraVarIdxExprNode<'a>> for EraVarOptIdxExprConstruct<'a> {
    fn from(val: EraVarIdxExprNode<'a>) -> Self {
        Self::VarIdx(val)
    }
}

impl<'a> From<EraVarExprNodeOrLeaf<'a>> for EraVarOptIdxExprConstruct<'a> {
    fn from(val: EraVarExprNodeOrLeaf<'a>) -> Self {
        Self::Var(val)
    }
}

impl<'a> From<EraIdentLeaf<'a>> for EraVarOptIdxExprConstruct<'a> {
    fn from(val: EraIdentLeaf<'a>) -> Self {
        Self::Var(EraVarExprNodeOrLeaf::Leaf(val))
    }
}

impl<'a> From<EraVarNamespaceExprNode<'a>> for EraVarOptIdxExprConstruct<'a> {
    fn from(val: EraVarNamespaceExprNode<'a>) -> Self {
        Self::Var(EraVarExprNodeOrLeaf::Node(val))
    }
}

impl<'a> EraProgramNode<'a> {
    pub fn children(&self) -> impl Iterator<Item = EraDeclItemNode<'a>> + FusedIterator {
        self.0.children().filter_map(EraDeclItemNode::cast)
    }
}

impl<'a> EraVarDeclNode<'a> {
    pub fn name(&self) -> Option<EraIdentLeaf<'a>> {
        self.node()
            .children_with_tokens()
            .filter_map(|x| x.into_token())
            .find_map(EraIdentLeaf::cast)
    }

    pub fn dimensions(&self) -> Option<EraExprListNode<'a>> {
        self.node()
            .children_with_tokens()
            .take_while(|x| x.as_token().filter(|x| x.kind() == Token::Assign).is_none())
            .filter_map(|x| x.into_node())
            .find_map(EraExprListNode::cast)
    }

    pub fn initializer(&self) -> Option<EraExprListNode<'a>> {
        let mut iter = self.node().children_with_tokens();
        // Skip `=`
        iter.find_map(|x| x.into_token().filter(|x| x.kind() == Token::Assign))?;
        iter.filter_map(|x| x.into_node())
            .find_map(EraExprListNode::cast)
    }

    pub fn is_ref(&self) -> bool {
        self.node()
            .children_with_tokens()
            .any(|x| x.as_token().filter(|x| x.kind() == Token::KwRef).is_some())
    }

    pub fn is_dynamic(&self) -> bool {
        self.node().children_with_tokens().any(|x| {
            x.as_token()
                .filter(|x| x.kind() == Token::KwDynamic)
                .is_some()
        })
    }

    pub fn is_savedata(&self) -> bool {
        self.node().children_with_tokens().any(|x| {
            x.as_token()
                .filter(|x| x.kind() == Token::KwSavedata)
                .is_some()
        })
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

impl<'a> EraVarSDeclNode<'a> {
    pub fn name(&self) -> Option<EraIdentLeaf<'a>> {
        self.node()
            .children_with_tokens()
            .filter_map(|x| x.into_token())
            .find_map(EraIdentLeaf::cast)
    }

    pub fn dimensions(&self) -> Option<EraExprListNode<'a>> {
        self.node()
            .children_with_tokens()
            .take_while(|x| x.as_token().filter(|x| x.kind() == Token::Assign).is_none())
            .filter_map(|x| x.into_node())
            .find_map(EraExprListNode::cast)
    }

    pub fn initializer(&self) -> Option<EraExprListNode<'a>> {
        let mut iter = self.node().children_with_tokens();
        // Skip `=`
        iter.find_map(|x| x.into_token().filter(|x| x.kind() == Token::Assign))?;
        iter.filter_map(|x| x.into_node())
            .find_map(EraExprListNode::cast)
    }

    pub fn is_ref(&self) -> bool {
        self.node()
            .children_with_tokens()
            .any(|x| x.as_token().filter(|x| x.kind() == Token::KwRef).is_some())
    }

    pub fn is_dynamic(&self) -> bool {
        self.node().children_with_tokens().any(|x| {
            x.as_token()
                .filter(|x| x.kind() == Token::KwDynamic)
                .is_some()
        })
    }

    pub fn is_savedata(&self) -> bool {
        self.node().children_with_tokens().any(|x| {
            x.as_token()
                .filter(|x| x.kind() == Token::KwSavedata)
                .is_some()
        })
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

impl<'a> EraLocalSizeDeclNode<'a> {
    pub fn size(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.node()
            .children_with_tokens()
            .find_map(EraExprNodeOrLeaf::cast)
    }
}

impl<'a> EraLocalSSizeDeclNode<'a> {
    pub fn size(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.node()
            .children_with_tokens()
            .find_map(EraExprNodeOrLeaf::cast)
    }
}

impl<'a> EraEventKindDeclNode<'a> {
    pub fn event_kind(&self) -> Option<EraEventKindLeaf<'a>> {
        self.node()
            .children_with_tokens()
            .filter_map(|x| x.into_token())
            .find_map(EraEventKindLeaf::cast)
    }
}

impl<'a> EraFunctionDeclNode<'a> {
    // No extra data
}

impl<'a> EraFunctionSDeclNode<'a> {
    // No extra data
}

impl<'a> EraSharpDeclListNode<'a> {
    pub fn children(&self) -> impl Iterator<Item = EraDeclItemNode<'a>> + FusedIterator {
        self.node().children().filter_map(EraDeclItemNode::cast)
    }
}

impl<'a> EraFunctionItemNode<'a> {
    pub fn name(&self) -> Option<EraIdentLeaf<'a>> {
        self.node()
            .children_with_tokens()
            .filter_map(|x| x.into_token())
            .find_map(EraIdentLeaf::cast)
    }

    pub fn arguments(&self) -> Option<EraExprListNode<'a>> {
        self.node().children().find_map(EraExprListNode::cast)
    }

    pub fn sharp_declarations(&self) -> Option<EraSharpDeclListNode<'a>> {
        self.node().children().find_map(EraSharpDeclListNode::cast)
    }

    pub fn body(&self) -> Option<EraStmtListNode<'a>> {
        self.node().children().find_map(EraStmtListNode::cast)
    }
}

impl<'a> EraPreUnaryExprNode<'a> {
    pub fn operator(&self) -> Option<EraOperatorLeaf<'a>> {
        self.node()
            .children_with_tokens()
            .filter_map(|x| x.into_token())
            .find_map(EraOperatorLeaf::cast)
    }

    pub fn rhs(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.node()
            .children_with_tokens()
            .find_map(EraExprNodeOrLeaf::cast)
    }
}

impl<'a> EraPostUnaryExprNode<'a> {
    pub fn operator(&self) -> Option<EraOperatorLeaf<'a>> {
        self.node()
            .children_with_tokens()
            .filter_map(|x| x.into_token())
            .find_map(EraOperatorLeaf::cast)
    }

    pub fn lhs(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.node()
            .children_with_tokens()
            .find_map(EraExprNodeOrLeaf::cast)
    }
}

impl<'a> EraBinaryExprNode<'a> {
    pub fn lhs(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.node()
            .children_with_tokens()
            .find_map(EraExprNodeOrLeaf::cast)
    }

    pub fn operator(&self) -> Option<EraOperatorLeaf<'a>> {
        self.node()
            .children_with_tokens()
            .filter_map(|x| x.into_token())
            .find_map(EraOperatorLeaf::cast)
    }

    pub fn rhs(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        let mut iter = self.node().children_with_tokens();
        // Skip operator
        iter.find_map(|x| x.into_token().and_then(EraOperatorLeaf::cast))?;
        iter.find_map(EraExprNodeOrLeaf::cast)
    }
}

impl<'a> EraTernaryExprNode<'a> {
    pub fn condition(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.node()
            .children_with_tokens()
            .find_map(EraExprNodeOrLeaf::cast)
    }

    pub fn true_expr(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        let mut iter = self.node().children_with_tokens();
        // Skip `?`
        iter.find_map(|x| x.into_token().filter(|x| x.kind() == Token::QuestionMark))?;
        iter.find_map(EraExprNodeOrLeaf::cast)
    }

    pub fn false_expr(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        let mut iter = self.node().children_with_tokens();
        // Skip `?` and true expr (i.e. `#`)
        iter.find_map(|x| x.into_token().filter(|x| x.kind() == Token::NumberSign))?;
        iter.find_map(EraExprNodeOrLeaf::cast)
    }
}

impl<'a> EraFunCallExprNode<'a> {
    pub fn name(&self) -> Option<EraIdentLeaf<'a>> {
        self.node()
            .children_with_tokens()
            .filter_map(|x| x.into_token())
            .find_map(EraIdentLeaf::cast)
    }

    pub fn arguments(&self) -> Option<EraExprListNode<'a>> {
        self.node().children().find_map(EraExprListNode::cast)
    }
}

impl<'a> EraParenExprNode<'a> {
    pub fn child(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.node()
            .children_with_tokens()
            .find_map(EraExprNodeOrLeaf::cast)
    }
}

impl<'a> EraVarIdxExprNode<'a> {
    pub fn name(&self) -> Option<EraVarExprNodeOrLeaf<'a>> {
        self.node()
            .children_with_tokens()
            .find_map(EraVarExprNodeOrLeaf::cast)
    }

    pub fn indices(&self) -> impl Iterator<Item = EraExprNodeOrLeaf<'a>> + FusedIterator {
        self.node()
            .children_with_tokens()
            .skip_while(|x| x.as_token().filter(|x| x.kind() == Token::Colon).is_none())
            .filter_map(EraExprNodeOrLeaf::cast)
    }
}

impl<'a> EraVarNamespaceExprNode<'a> {
    pub fn name(&self) -> Option<EraIdentLeaf<'a>> {
        self.node()
            .children_with_tokens()
            .filter_map(|x| x.into_token())
            .find_map(EraIdentLeaf::cast)
    }

    pub fn namespace(&self) -> Option<EraIdentLeaf<'a>> {
        // Skip `@`
        self.node()
            .children_with_tokens()
            .skip_while(|x| x.as_token().filter(|x| x.kind() == Token::At).is_none())
            .filter_map(|x| x.into_token())
            .find_map(EraIdentLeaf::cast)
    }
}

impl<'a> EraEmptyExprNode<'a> {
    // No extra data
}

impl<'a> EraExprListNode<'a> {
    // TODO: Proper span info
    // pub fn new<I>(children: I) -> OwnedEraExprListNode
    // where
    //     I: IntoIterator<Item = EraExprNodeOrLeaf<'a>>,
    //     I::IntoIter: ExactSizeIterator,
    // {
    //     let green = GreenNode::new(
    //         Token::ExprList.into_raw(),
    //         children.into_iter().map(|x| match x {
    //             EraExprNodeOrLeaf::Node(node) => NodeOrToken::Node(node.node().green().clone()),
    //             EraExprNodeOrLeaf::Leaf(leaf) => NodeOrToken::Token(leaf.token().green().clone()),
    //         }),
    //     );
    //     OwnedEraExprListNode(SyntaxNode::new_root(green))
    // }

    pub fn children(&self) -> impl Iterator<Item = EraExprNodeOrLeaf<'a>> + FusedIterator {
        self.node()
            .children_with_tokens()
            .filter_map(EraExprNodeOrLeaf::cast)
    }
}

impl<'a> EraStringFormNode<'a> {
    pub fn parts(&self) -> impl Iterator<Item = EraStringFormPartNodeOrLeaf<'a>> + FusedIterator {
        self.node()
            .children_with_tokens()
            .filter_map(EraStringFormPartNodeOrLeaf::cast)
    }
}

impl<'a> EraStringFormInterpPartNode<'a> {
    pub fn expr(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.node()
            .children_with_tokens()
            .find_map(EraExprNodeOrLeaf::cast)
    }

    pub fn width(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        // Skip first `,`
        self.node()
            .children_with_tokens()
            .skip_while(|x| x.as_token().filter(|x| x.kind() == Token::Comma).is_none())
            .find_map(EraExprNodeOrLeaf::cast)
    }

    pub fn alignment(&self) -> Option<EraIdentLeaf<'a>> {
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

impl<'a> EraSelectCasePredListNode<'a> {
    pub fn children(&self) -> impl Iterator<Item = EraSelectCasePredNode<'a>> + FusedIterator {
        self.node()
            .children()
            .filter_map(EraSelectCasePredNode::cast)
    }
}

impl<'a> EraStmtListNode<'a> {
    pub fn children(&self) -> impl Iterator<Item = EraStmtNode<'a>> + FusedIterator {
        self.node().children().filter_map(EraStmtNode::cast)
    }
}

impl<'a> EraLabelStmtNode<'a> {
    pub fn label(&self) -> Option<EraIdentLeaf<'a>> {
        self.node()
            .children_with_tokens()
            .filter_map(|x| x.into_token())
            .find_map(EraIdentLeaf::cast)
    }
}

impl<'a> EraNopStmtNode<'a> {
    // No extra data
}

impl<'a> EraExprStmtNode<'a> {
    pub fn expr(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.node()
            .children_with_tokens()
            .find_map(EraExprNodeOrLeaf::cast)
    }
}

impl<'a> EraRowAssignStmtNode<'a> {
    pub fn base_assign(&self) -> Option<EraBinaryExprNode<'a>> {
        self.node().children().find_map(EraBinaryExprNode::cast)
    }

    pub fn extra_values(&self) -> Option<EraExprListNode<'a>> {
        self.node().children().find_map(EraExprListNode::cast)
    }
}

impl<'a> EraResultCmdCallStmtNode<'a> {
    pub fn command(&self) -> Option<EraKwIdentLeaf<'a>> {
        self.node()
            .children_with_tokens()
            .filter_map(|x| x.into_token())
            .find_map(EraKwIdentLeaf::cast)
    }

    pub fn arguments(&self) -> Option<EraExprListNode<'a>> {
        self.node().children().find_map(EraExprListNode::cast)
    }
}

macro_rules! impl_cmd_args {
    ($($node:ident),+ $(,)?) => {
        $(impl<'a> $node<'a> {
            pub fn arguments(&self) -> Option<EraExprListNode<'a>> {
                self.node().children().find_map(EraExprListNode::cast)
            }
        })+
    };
}

impl_cmd_args!(EraDebugPrintStmtNode, EraPrintStmtNode);

impl<'a> EraPrintStmtNode<'a> {
    // NOTE: Used to expose flags encoded in the command
    pub fn command(&self) -> Option<EraKwIdentLeaf<'a>> {
        self.node()
            .children_with_tokens()
            .filter_map(|x| x.into_token())
            .find_map(EraKwIdentLeaf::cast)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum EraPrintDataStmtNodeChildKind<'a> {
    Single(EraStringFormNode<'a>),
    List(EraExprListNode<'a>),
}

impl<'a> EraPrintDataStmtNode<'a> {
    // NOTE: Used to expose flags encoded in the command
    pub fn command(&self) -> Option<EraKwIdentLeaf<'a>> {
        self.node()
            .children_with_tokens()
            .filter_map(|x| x.into_token())
            .find_map(EraKwIdentLeaf::cast)
    }

    // NOTE: We return Expr instead of VarExpr here because the grammar should allow
    //       any expression here, for maximum flexibility. It's the duty of codegen to
    //       ensure that the expression is a valid variable expression.
    pub fn dest(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.node()
            .children_with_tokens()
            .take_while(not_newline)
            .find_map(EraExprNodeOrLeaf::cast)
    }

    pub fn data(&self) -> impl Iterator<Item = EraPrintDataStmtNodeChildKind<'a>> + FusedIterator {
        self.node().children().filter_map(|x| {
            if let Some(x) = EraStringFormNode::cast(x) {
                Some(EraPrintDataStmtNodeChildKind::Single(x))
            } else if let Some(x) = EraExprListNode::cast(x) {
                Some(EraPrintDataStmtNodeChildKind::List(x))
            } else {
                None
            }
        })
    }
}

#[derive(Debug, Clone, Copy)]
pub enum EraIfStmtNodeChildKind<'a> {
    CondBody((EraExprNodeOrLeaf<'a>, Option<EraStmtListNode<'a>>)),
    BodyOnly(EraStmtListNode<'a>),
}

impl<'a> EraIfStmtNode<'a> {
    pub fn children(&self) -> impl Iterator<Item = EraIfStmtNodeChildKind<'a>> + FusedIterator {
        struct Iter<I: Iterator> {
            iter: std::iter::Peekable<I>,
        }

        impl<'a, I> Iterator for Iter<I>
        where
            I: Iterator<Item = SyntaxElementRef<'a, Token>> + FusedIterator,
        {
            type Item = EraIfStmtNodeChildKind<'a>;

            fn next(&mut self) -> Option<Self::Item> {
                let item = self.iter.next()?;
                if let Some(body) = item.into_node().and_then(EraStmtListNode::cast) {
                    return Some(EraIfStmtNodeChildKind::BodyOnly(body));
                }
                let Some(cond) = EraExprNodeOrLeaf::cast(item) else {
                    unreachable!();
                };
                let body = self
                    .iter
                    .next_if(|x| EraStmtListNode::can_cast(x.kind()))
                    .map(|body| body.into_node().and_then(EraStmtListNode::cast).unwrap());
                Some(EraIfStmtNodeChildKind::CondBody((cond, body)))
            }
        }

        impl<'a, I> FusedIterator for Iter<I> where
            I: Iterator<Item = SyntaxElementRef<'a, Token>> + FusedIterator
        {
        }

        Iter {
            iter: self
                .node()
                .children_with_tokens()
                .filter(|x| {
                    let kind = x.kind();
                    EraStmtListNode::can_cast(kind) || EraExprNodeOrLeaf::can_cast(kind)
                })
                .peekable(),
        }
    }
}

impl<'a> EraQuitStmtNode<'a> {
    // No extra data
}

#[derive(Debug, Clone, Copy)]
pub enum EraSelectCaseStmtNodeChildKind<'a> {
    PredBody((EraSelectCasePredListNode<'a>, Option<EraStmtListNode<'a>>)),
    BodyOnly(EraStmtListNode<'a>),
}

impl<'a> EraSelectCaseStmtNode<'a> {
    pub fn value(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.node()
            .children_with_tokens()
            .take_while(not_newline)
            .find_map(EraExprNodeOrLeaf::cast)
    }

    pub fn children(
        &self,
    ) -> impl Iterator<Item = EraSelectCaseStmtNodeChildKind<'a>> + FusedIterator {
        struct Iter<I: Iterator> {
            iter: std::iter::Peekable<I>,
        }

        impl<'a, I> Iterator for Iter<I>
        where
            I: Iterator<Item = &'a SyntaxNode<Token>> + FusedIterator,
        {
            type Item = EraSelectCaseStmtNodeChildKind<'a>;

            fn next(&mut self) -> Option<Self::Item> {
                let item = self.iter.next()?;
                if let Some(body) = EraStmtListNode::cast(item) {
                    return Some(EraSelectCaseStmtNodeChildKind::BodyOnly(body));
                }
                let Some(pred) = EraSelectCasePredListNode::cast(item) else {
                    unreachable!();
                };
                let body = self
                    .iter
                    .next_if(|x| EraStmtListNode::can_cast(x.kind()))
                    .map(|body| EraStmtListNode::cast(body).unwrap());
                Some(EraSelectCaseStmtNodeChildKind::PredBody((pred, body)))
            }
        }

        impl<'a, I> FusedIterator for Iter<I> where I: Iterator<Item = &'a SyntaxNode<Token>> + FusedIterator
        {}

        Iter {
            iter: self
                .node()
                .children()
                .filter(|x| {
                    let kind = x.kind();
                    EraStmtListNode::can_cast(kind) || EraSelectCasePredListNode::can_cast(kind)
                })
                .peekable(),
        }
    }
}

impl<'a> EraSelectCaseSingleNode<'a> {
    pub fn value(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.node()
            .children_with_tokens()
            .find_map(EraExprNodeOrLeaf::cast)
    }
}

impl<'a> EraSelectCaseRangeNode<'a> {
    pub fn start(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.node()
            .children_with_tokens()
            .find_map(EraExprNodeOrLeaf::cast)
    }

    pub fn end(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        let mut iter = self.node().children_with_tokens();
        // Skip `TO` (KwIdent)
        iter.find_map(|x| x.into_token().filter(|x| x.kind() == Token::KwIdent))?;
        iter.find_map(EraExprNodeOrLeaf::cast)
    }
}

impl<'a> EraSelectCaseCondNode<'a> {
    pub fn op(&self) -> Option<EraOperatorLeaf<'a>> {
        self.node()
            .children_with_tokens()
            .filter_map(|x| x.into_token())
            .find_map(EraOperatorLeaf::cast)
    }

    pub fn value(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.node()
            .children_with_tokens()
            .find_map(EraExprNodeOrLeaf::cast)
    }
}

impl<'a> EraWhileStmtNode<'a> {
    pub fn condition(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.node()
            .children_with_tokens()
            .find_map(EraExprNodeOrLeaf::cast)
    }

    pub fn body(&self) -> Option<EraStmtListNode<'a>> {
        self.node().children().find_map(EraStmtListNode::cast)
    }
}

impl<'a> EraCallStmtNode<'a> {
    pub fn name(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.node()
            .children_with_tokens()
            .find_map(EraExprNodeOrLeaf::cast)
    }

    pub fn arguments(&self) -> Option<EraExprListNode<'a>> {
        self.node().children().find_map(EraExprListNode::cast)
    }
}

impl<'a> EraTryCallStmtNode<'a> {
    pub fn name(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.node()
            .children_with_tokens()
            .find_map(EraExprNodeOrLeaf::cast)
    }

    pub fn arguments(&self) -> Option<EraExprListNode<'a>> {
        self.node().children().find_map(EraExprListNode::cast)
    }
}

impl<'a> EraTryCCallStmtNode<'a> {
    pub fn name(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.node()
            .children_with_tokens()
            .find_map(EraExprNodeOrLeaf::cast)
    }

    pub fn arguments(&self) -> Option<EraExprListNode<'a>> {
        self.node().children().find_map(EraExprListNode::cast)
    }

    pub fn success_body(&self) -> Option<EraStmtListNode<'a>> {
        self.node().children().find_map(EraStmtListNode::cast)
    }

    pub fn catch_body(&self) -> Option<EraStmtListNode<'a>> {
        let mut iter = self.node().children();
        // Skip success body
        iter.find_map(EraStmtListNode::cast)?;
        iter.find_map(EraStmtListNode::cast)
    }
}

impl<'a> EraJumpStmtNode<'a> {
    pub fn name(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.node()
            .children_with_tokens()
            .find_map(EraExprNodeOrLeaf::cast)
    }

    pub fn arguments(&self) -> Option<EraExprListNode<'a>> {
        self.node().children().find_map(EraExprListNode::cast)
    }
}

impl<'a> EraTryJumpStmtNode<'a> {
    pub fn name(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.node()
            .children_with_tokens()
            .find_map(EraExprNodeOrLeaf::cast)
    }

    pub fn arguments(&self) -> Option<EraExprListNode<'a>> {
        self.node().children().find_map(EraExprListNode::cast)
    }
}

impl<'a> EraTryCJumpStmtNode<'a> {
    pub fn name(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.node()
            .children_with_tokens()
            .find_map(EraExprNodeOrLeaf::cast)
    }

    pub fn arguments(&self) -> Option<EraExprListNode<'a>> {
        self.node().children().find_map(EraExprListNode::cast)
    }

    pub fn success_body(&self) -> Option<EraStmtListNode<'a>> {
        self.node().children().find_map(EraStmtListNode::cast)
    }

    pub fn catch_body(&self) -> Option<EraStmtListNode<'a>> {
        let mut iter = self.node().children();
        // Skip success body
        iter.find_map(EraStmtListNode::cast)?;
        iter.find_map(EraStmtListNode::cast)
    }
}

// HACK: This is a workaround for the fact that `EraJumpStmtNode` and `EraCallStmtNode`
//       essentially have the same structure, but are different nodes in the grammar.
impl<'a> From<EraJumpStmtNode<'a>> for EraCallStmtNode<'a> {
    fn from(val: EraJumpStmtNode<'a>) -> Self {
        Self(val.0)
    }
}
impl<'a> From<EraTryJumpStmtNode<'a>> for EraTryCallStmtNode<'a> {
    fn from(val: EraTryJumpStmtNode<'a>) -> Self {
        Self(val.0)
    }
}
impl<'a> From<EraTryCJumpStmtNode<'a>> for EraTryCCallStmtNode<'a> {
    fn from(val: EraTryCJumpStmtNode<'a>) -> Self {
        Self(val.0)
    }
}

impl<'a> EraReturnStmtNode<'a> {
    pub fn values(&self) -> Option<EraExprListNode<'a>> {
        self.node().children().find_map(EraExprListNode::cast)
    }
}

impl<'a> EraContinueStmtNode<'a> {
    // No extra data
}

impl<'a> EraBreakStmtNode<'a> {
    // No extra data
}

impl<'a> EraRestartStmtNode<'a> {
    // No extra data
}

impl_cmd_args!(EraThrowStmtNode);

impl<'a> EraThrowStmtNode<'a> {
    pub fn value(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.node()
            .children_with_tokens()
            .find_map(EraExprNodeOrLeaf::cast)
    }
}

impl<'a> EraRepeatStmtNode<'a> {
    pub fn count(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.node()
            .children_with_tokens()
            .find_map(EraExprNodeOrLeaf::cast)
    }

    pub fn body(&self) -> Option<EraStmtListNode<'a>> {
        self.node().children().find_map(EraStmtListNode::cast)
    }
}

impl_cmd_args!(EraGotoStmtNode);

impl<'a> EraGotoStmtNode<'a> {
    pub fn label(&self) -> Option<EraIdentLeaf<'a>> {
        self.arguments()
            .and_then(|x| x.children().nth(0))
            .and_then(|x| x.try_into().ok())
    }
}

impl<'a> EraForStmtNode<'a> {
    fn _get_exprs(&self) -> Option<EraExprListNode<'a>> {
        self.node().children().find_map(EraExprListNode::cast)
    }

    pub fn var(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self._get_exprs().and_then(|x| x.children().nth(0))
    }

    pub fn start(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self._get_exprs().and_then(|x| x.children().nth(1))
    }

    pub fn end(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self._get_exprs().and_then(|x| x.children().nth(2))
    }

    pub fn step(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self._get_exprs().and_then(|x| x.children().nth(3))
    }

    pub fn body(&self) -> Option<EraStmtListNode<'a>> {
        self.node().children().find_map(EraStmtListNode::cast)
    }
}

impl<'a> EraDoLoopStmtNode<'a> {
    pub fn body(&self) -> Option<EraStmtListNode<'a>> {
        self.node().children().find_map(EraStmtListNode::cast)
    }

    pub fn condition(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.node()
            .children_with_tokens()
            .find_map(EraExprNodeOrLeaf::cast)
    }
}

impl<'a> EraTimesStmtNode<'a> {
    pub fn target(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.node()
            .children_with_tokens()
            .find_map(EraExprNodeOrLeaf::cast)
    }

    pub fn factor(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.node()
            .children_with_tokens()
            .filter_map(EraExprNodeOrLeaf::cast)
            .nth(1)
    }
}

impl_cmd_args!(
    EraSplitStmtNode,
    EraSetBitStmtNode,
    EraClearBitStmtNode,
    EraInvertBitStmtNode,
    EraSetColorStmtNode,
    EraResetColorStmtNode,
    EraSetBgColorStmtNode,
    EraResetBgColorStmtNode,
    EraVarSetStmtNode,
    EraCVarSetStmtNode,
    EraVarSizeStmtNode,
    EraSwapStmtNode,
    EraHtmlPrintStmtNode,
    EraPrintButtonStmtNode,
    EraPrintButtonCStmtNode,
    EraPrintButtonLCStmtNode,
    EraArrayRemoveStmtNode,
    EraArraySortStmtNode,
    EraArrayMSortStmtNode,
    EraArrayCopyStmtNode,
    EraArrayShiftStmtNode,
    EraInputStmtNode,
    EraInputSStmtNode,
    EraTInputStmtNode,
    EraTInputSStmtNode,
    EraOneInputStmtNode,
    EraOneInputSStmtNode,
    EraTOneInputStmtNode,
    EraTOneInputSStmtNode,
    EraReuseLastLineStmtNode,
    EraClearLineStmtNode,
    EraDrawLineStmtNode,
    EraCustomDrawLineStmtNode,
    EraTWaitStmtNode,
    EraFontStyleStmtNode,
    EraFontBoldStmtNode,
    EraFontItalicStmtNode,
    EraFontRegularStmtNode,
    EraSetFontStmtNode,
    EraStrDataStmtNode,
    EraPutFormStmtNode,
    EraSkipDispStmtNode,
    EraBeginStmtNode,
    EraDoTrainStmtNode,
    EraRedrawStmtNode,
    EraStrLenStmtNode,
    EraStrLenUStmtNode,
    EraAlignmentStmtNode,
    EraToolTipSetDelayStmtNode,
    EraToolTipSetDurationStmtNode,
    EraRandomizeStmtNode,
    EraDumpRandStmtNode,
    EraInitRandStmtNode,
    EraBarStmtNode,
    EraAddCharaStmtNode,
    EraPickUpCharaStmtNode,
    EraDelCharaStmtNode,
    EraSwapCharaStmtNode,
    EraAddCopyCharaStmtNode,
    EraResetStainStmtNode,
    EraSaveCharaStmtNode,
    EraLoadCharaStmtNode,
    EraSetAnimeTimerStmtNode,
    EraHtmlTagSplitStmtNode,
    EraPowerStmtNode,
    EraLoadDataStmtNode,
    EraSaveDataStmtNode,
    // EraCheckDataStmtNode,
    EraGetTimeStmtNode,
    EraLoadGlobalStmtNode,
    EraSaveGlobalStmtNode,
    EraLoadGameStmtNode,
    EraSaveGameStmtNode,
    EraDebugClearStmtNode,
    EraResetDataStmtNode,
);

impl<'a> EraSplitStmtNode<'a> {
    pub fn input(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(0))
    }

    pub fn separator(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(1))
    }

    pub fn dest(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(2))
    }

    pub fn count_dest(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(3))
    }
}

impl<'a> EraSetBitStmtNode<'a> {
    pub fn target(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(0))
    }

    pub fn bits(&self) -> impl Iterator<Item = EraExprNodeOrLeaf<'a>> + FusedIterator {
        if let Some(args) = self.arguments() {
            itertools::Either::Left(args.children().skip(1))
        } else {
            itertools::Either::Right(std::iter::empty())
        }
    }
}

impl<'a> EraClearBitStmtNode<'a> {
    pub fn target(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(0))
    }

    pub fn bits(&self) -> impl Iterator<Item = EraExprNodeOrLeaf<'a>> + FusedIterator {
        if let Some(args) = self.arguments() {
            itertools::Either::Left(args.children().skip(1))
        } else {
            itertools::Either::Right(std::iter::empty())
        }
    }
}

impl<'a> EraInvertBitStmtNode<'a> {
    pub fn target(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(0))
    }

    pub fn bits(&self) -> impl Iterator<Item = EraExprNodeOrLeaf<'a>> + FusedIterator {
        if let Some(args) = self.arguments() {
            itertools::Either::Left(args.children().skip(1))
        } else {
            itertools::Either::Right(std::iter::empty())
        }
    }
}

impl_homogeneous!(EraSetColorStmtNode, EraSetBgColorStmtNode);
impl_homogeneous!(EraResetColorStmtNode, EraResetBgColorStmtNode);

impl<'a> EraSetColorStmtNode<'a> {
    // No extra data
    // pub fn colors(&self) -> impl Iterator<Item = EraExprNodeOrLeaf<'a>> + FusedIterator {
    //     self.arguments().into_iter().flat_map(|x| x.children())
    // }
}

impl<'a> EraResetColorStmtNode<'a> {
    // No extra data
}

impl<'a> EraVarSetStmtNode<'a> {
    pub fn target(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(0))
    }

    pub fn value(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(1))
    }

    pub fn start_index(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(2))
    }

    pub fn end_index(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(3))
    }
}

impl<'a> EraCVarSetStmtNode<'a> {
    pub fn target(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(0))
    }

    pub fn index(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(1))
    }

    pub fn value(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(2))
    }

    pub fn start_id(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(3))
    }

    pub fn end_id(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(4))
    }
}

impl<'a> EraVarSizeStmtNode<'a> {
    pub fn target(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(0))
    }
}

impl<'a> EraSwapStmtNode<'a> {
    pub fn lhs(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(0))
    }

    pub fn rhs(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(1))
    }
}

impl<'a> EraHtmlPrintStmtNode<'a> {
    pub fn text(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(0))
    }
}

impl<'a> EraPrintButtonStmtNode<'a> {
    pub fn content(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(0))
    }

    pub fn value(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(1))
    }
}

impl<'a> EraPrintButtonCStmtNode<'a> {
    pub fn content(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(0))
    }

    pub fn value(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(1))
    }
}

impl<'a> EraPrintButtonLCStmtNode<'a> {
    pub fn content(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(0))
    }

    pub fn value(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(1))
    }
}

impl_from!(
    EraPrintButtonStmtNode,
    EraPrintButtonCStmtNode,
    EraPrintButtonLCStmtNode
);

impl<'a> EraArrayRemoveStmtNode<'a> {
    pub fn target(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(0))
    }

    pub fn start_index(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(1))
    }

    pub fn count(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(2))
    }
}

impl<'a> EraArraySortStmtNode<'a> {
    pub fn target(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(0))
    }

    pub fn ordering(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(1))
    }

    pub fn start_index(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(2))
    }

    pub fn count(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(3))
    }
}

impl<'a> EraArrayMSortStmtNode<'a> {
    // No extra data
}

impl<'a> EraArrayCopyStmtNode<'a> {
    pub fn source_name(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(0))
    }

    pub fn dest_name(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(1))
    }
}

impl<'a> EraArrayShiftStmtNode<'a> {
    pub fn target(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(0))
    }

    pub fn shift_count(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(1))
    }

    pub fn value(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(2))
    }

    pub fn start_index(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(3))
    }

    pub fn target_count(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(4))
    }
}

impl_homogeneous!(EraInputStmtNode, EraInputSStmtNode);

impl<'a> EraInputStmtNode<'a> {
    pub fn default_value(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(0))
    }

    pub fn can_click(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(1))
    }

    pub fn allow_skip(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(2))
    }
}

impl_homogeneous!(EraTInputStmtNode, EraTInputSStmtNode);

impl<'a> EraTInputStmtNode<'a> {
    pub fn time_limit(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(0))
    }

    pub fn default_value(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(1))
    }

    pub fn show_prompt(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(2))
    }

    pub fn expiry_msg(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(3))
    }

    pub fn can_click(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(4))
    }
}

impl_homogeneous!(EraOneInputStmtNode, EraOneInputSStmtNode);

impl<'a> EraOneInputStmtNode<'a> {
    pub fn default_value(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(0))
    }
}

impl_homogeneous!(EraTOneInputStmtNode, EraTOneInputSStmtNode);

impl<'a> EraTOneInputStmtNode<'a> {
    pub fn time_limit(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(0))
    }

    pub fn default_value(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(1))
    }

    pub fn show_prompt(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(2))
    }

    pub fn expiry_msg(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(3))
    }

    pub fn can_click(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(4))
    }
}

impl<'a> EraReuseLastLineStmtNode<'a> {
    pub fn content(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(0))
    }
}

impl<'a> EraClearLineStmtNode<'a> {
    pub fn count(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(0))
    }
}

impl<'a> EraDrawLineStmtNode<'a> {
    // No extra data
}

impl<'a> EraCustomDrawLineStmtNode<'a> {
    pub fn content(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(0))
    }
}

impl<'a> EraTWaitStmtNode<'a> {
    pub fn duration(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(0))
    }

    pub fn force_wait(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(1))
    }
}

impl<'a> EraFontStyleStmtNode<'a> {
    pub fn style(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(0))
    }
}

impl<'a> EraSetFontStmtNode<'a> {
    pub fn font_name(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(0))
    }
}

// #[derive(Debug, Clone, Copy)]
// pub enum EraStrDataStmtNodeChildKind<'a> {
//     Single(EraStringFormNode<'a>),
//     List(EraExprListNode<'a>),
// }
type EraStrDataStmtNodeChildKind<'a> = EraPrintDataStmtNodeChildKind<'a>;

impl<'a> EraStrDataStmtNode<'a> {
    pub fn target(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.node()
            .children_with_tokens()
            .take_while(not_newline)
            .find_map(EraExprNodeOrLeaf::cast)
    }

    pub fn data(&self) -> impl Iterator<Item = EraStrDataStmtNodeChildKind<'a>> + FusedIterator {
        self.node().children().filter_map(|x| {
            if let Some(x) = EraStringFormNode::cast(x) {
                Some(EraStrDataStmtNodeChildKind::Single(x))
            } else if let Some(x) = EraExprListNode::cast(x) {
                Some(EraStrDataStmtNodeChildKind::List(x))
            } else {
                None
            }
        })
    }
}

impl<'a> EraPutFormStmtNode<'a> {
    pub fn content(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(0))
    }
}

impl<'a> EraSkipDispStmtNode<'a> {
    pub fn is_skip(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(0))
    }
}

impl<'a> EraBeginStmtNode<'a> {
    pub fn procedure(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(0))
    }
}

impl<'a> EraDoTrainStmtNode<'a> {
    pub fn number(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(0))
    }
}

impl<'a> EraRedrawStmtNode<'a> {
    pub fn arg(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(0))
    }
}

impl_homogeneous!(EraStrLenStmtNode, EraStrLenUStmtNode);

impl<'a> EraStrLenStmtNode<'a> {
    pub fn content(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(0))
    }
}

impl<'a> EraAlignmentStmtNode<'a> {
    pub fn alignment(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(0))
    }
}

impl<'a> EraToolTipSetDelayStmtNode<'a> {
    pub fn delay(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(0))
    }
}

impl<'a> EraToolTipSetDurationStmtNode<'a> {
    pub fn duration(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(0))
    }
}

impl<'a> EraRandomizeStmtNode<'a> {
    pub fn seed(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(0))
    }
}

impl_homogeneous!(EraBarStmtNode, EraBarLStmtNode);

impl<'a> EraBarStmtNode<'a> {
    pub fn value(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(0))
    }

    pub fn max_value(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(1))
    }

    pub fn length(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(2))
    }
}

impl<'a> EraSwapCharaStmtNode<'a> {
    pub fn chara1(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(0))
    }

    pub fn chara2(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(1))
    }
}

impl<'a> EraAddCopyCharaStmtNode<'a> {
    pub fn chara(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(0))
    }
}

impl<'a> EraResetStainStmtNode<'a> {
    pub fn chara(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(0))
    }
}

impl<'a> EraSaveCharaStmtNode<'a> {
    pub fn filename(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(0))
    }

    pub fn memo(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(1))
    }

    pub fn charas(&self) -> impl Iterator<Item = EraExprNodeOrLeaf<'a>> + FusedIterator {
        self.arguments()
            .into_iter()
            .flat_map(|x| x.children().skip(2))
    }
}

impl<'a> EraLoadCharaStmtNode<'a> {
    pub fn filename(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(0))
    }
}

impl<'a> EraSetAnimeTimerStmtNode<'a> {
    pub fn duration(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(0))
    }
}

impl<'a> EraHtmlTagSplitStmtNode<'a> {
    pub fn html(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(0))
    }

    pub fn var_tags(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(1))
    }

    pub fn var_count(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(2))
    }
}

impl<'a> EraPowerStmtNode<'a> {
    pub fn target(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(0))
    }

    pub fn base(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(1))
    }

    pub fn exponent(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(2))
    }
}

impl<'a> EraLoadDataStmtNode<'a> {
    pub fn save_id(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(0))
    }
}

impl<'a> EraSaveDataStmtNode<'a> {
    pub fn save_id(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(0))
    }

    pub fn save_info(&self) -> Option<EraExprNodeOrLeaf<'a>> {
        self.arguments().and_then(|x| x.children().nth(1))
    }
}
