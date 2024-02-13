use std::collections::HashMap;

use indoc::concatdoc;

use crate::bytecode::PadStringFlags;
use crate::util::*;

use crate::{
    bytecode::{PrintExtendedFlags, SourcePosInfo},
    lexer::{EraLexerMode, EraToken, EraTokenKind, EraTokenLite},
};

pub struct EraRootASTNode {
    pub decls: Vec<EraDecl>,
    pub src_info: SourcePosInfo,
}

pub enum EraDecl {
    SharpDecl(EraSharpDecl),
    FunDecl(EraFunDecl),
}

pub enum EraSharpDecl {
    VarDecl(EraVarDecl),
    LocalSizeDecl(EraLocalSizeDecl),
    LocalSSizeDecl(EraLocalSSizeDecl),
    FunctionDecl(EraSharpFunctionDecl),
}
impl EraSharpDecl {
    pub fn source_pos_info(&self) -> SourcePosInfo {
        match self {
            Self::FunctionDecl(x) => x.src_info,
            Self::LocalSizeDecl(x) => x.src_info,
            Self::LocalSSizeDecl(x) => x.src_info,
            Self::VarDecl(x) => x.src_info,
        }
    }
}

pub struct EraSharpFunctionDecl {
    pub returns_string: bool,
    pub src_info: SourcePosInfo,
}

#[derive(Debug, Clone)]
pub struct EraVarExpr {
    pub name: String,
    // Array indices. May be empty or smaller than actual array dimensions.
    pub idxs: Vec<EraExpr>,
    pub src_info: SourcePosInfo,
}

// #[repr(u8)]
// #[derive(Debug, Clone)]
// pub enum EraStrFormExprPartAlignment {
//     Left,
//     Right,
// }

#[derive(Debug, Clone)]
pub struct EraStrFormExprPartExpression {
    pub expr: EraExpr,
    pub width: Option<EraExpr>,
    pub alignment: PadStringFlags,
}
impl EraStrFormExprPartExpression {
    pub fn source_pos_info(&self) -> SourcePosInfo {
        self.expr.source_pos_info()
    }
}

#[derive(Debug, Clone)]
pub enum EraStrFormExprPart {
    Literal(String, SourcePosInfo),
    Expression(EraStrFormExprPartExpression),
}

/// The string interpolation expression.
#[derive(Debug, Clone)]
pub struct EraStrFormExpr {
    pub parts: Vec<EraStrFormExprPart>,
    pub src_info: SourcePosInfo,
}

/// The leaf expression.
#[derive(Debug, Clone)]
pub enum EraTermExpr {
    Var(EraVarExpr),
    Literal(EraLiteral),
    StrForm(EraStrFormExpr),
}
impl EraTermExpr {
    pub fn source_pos_info(&self) -> SourcePosInfo {
        match self {
            Self::Var(x) => x.src_info,
            Self::Literal(x) => x.source_pos_info(),
            Self::StrForm(x) => x.src_info,
        }
    }
}

#[derive(Debug, Clone)]
pub enum EraExpr {
    /// The leaf expression.
    Term(EraTermExpr),
    // Add(EraTermExpr, EraTermExpr),
    // Sub(EraTermExpr, EraTermExpr),
    // Mul(EraTermExpr, EraTermExpr),
    // Div(EraTermExpr, EraTermExpr),
    // Mod(EraTermExpr, EraTermExpr),
    // UnaryAdd(EraTermExpr),
    // UnarySub(EraTermExpr),
    // BitAnd(EraTermExpr, EraTermExpr),
    // BitOr(EraTermExpr, EraTermExpr),
    // BitXor(EraTermExpr, EraTermExpr),
    // LogicalAnd(EraTermExpr, EraTermExpr),
    // LogicalOr(EraTermExpr, EraTermExpr),
    // LogicalNot(EraTermExpr, EraTermExpr),
    Grouping(EraTokenLite, Box<EraExpr>, EraTokenLite),
    PreUnary(EraTokenLite, Box<EraExpr>),
    PostUnary(Box<EraExpr>, EraTokenLite),
    FunCall(Box<EraExpr>, Vec<EraExpr>),
    Binary(Box<EraExpr>, EraTokenLite, Box<EraExpr>),
    // NOTE: Erabasic only has simple assignment semantics, so we use EraVarExpr as lhs here.
    // Assign(EraVarExpr, EraTermExpr),
    Ternary(
        Box<EraExpr>,
        EraTokenLite,
        Box<EraExpr>,
        EraTokenLite,
        Box<EraExpr>,
    ),
}
impl EraExpr {
    pub fn source_pos_info(&self) -> SourcePosInfo {
        match self {
            Self::Term(x) => x.source_pos_info(),
            Self::Grouping(x, ..) => x.src_info,
            Self::PreUnary(x, ..) => x.src_info,
            Self::PostUnary(x, ..) => x.source_pos_info(),
            Self::FunCall(x, ..) => x.source_pos_info(),
            Self::Binary(x, ..) => x.source_pos_info(),
            Self::Ternary(x, ..) => x.source_pos_info(),
        }
    }
    pub fn new_int(val: i64, src_info: SourcePosInfo) -> Self {
        EraExpr::Term(EraTermExpr::Literal(EraLiteral::Integer(val, src_info)))
    }
    pub fn new_str(val: String, src_info: SourcePosInfo) -> Self {
        EraExpr::Term(EraTermExpr::Literal(EraLiteral::String(val, src_info)))
    }
    fn is_var(&self) -> bool {
        match self {
            Self::Term(EraTermExpr::Var(_)) => true,
            _ => false,
        }
    }
}

impl EraExpr {
    // TODO: Finish EraExpr::try_evaluate_constant
    pub fn try_evaluate_constant(self) -> Result<EraLiteral, EraParseErrorInfo> {
        let src_info = self.source_pos_info();

        fn make_err(
            src_info: SourcePosInfo,
            is_error: bool,
            msg: impl Into<String>,
        ) -> EraParseErrorInfo {
            EraParseErrorInfo {
                src_info,
                is_error,
                msg: msg.into(),
            }
        }

        Ok(match self {
            EraExpr::Term(x) => match x {
                EraTermExpr::Var(x) => {
                    return Err(make_err(x.src_info, true, "not a constant expression"));
                }
                EraTermExpr::StrForm(x) => {
                    return Err(make_err(x.src_info, true, "not a constant expression"));
                }
                EraTermExpr::Literal(x) => match x {
                    EraLiteral::Integer(x, _) => EraLiteral::Integer(x, src_info),
                    EraLiteral::String(x, _) => EraLiteral::String(x, src_info),
                },
            },
            EraExpr::Grouping(_, x, _) => x.try_evaluate_constant()?,
            EraExpr::PreUnary(op, x) => match (op.kind, x.try_evaluate_constant()?) {
                (EraTokenKind::Plus, EraLiteral::Integer(x, _)) => EraLiteral::Integer(x, src_info),
                (EraTokenKind::Minus, EraLiteral::Integer(x, cur_si)) => {
                    let x = match 0i64.checked_sub(x) {
                        Some(x) => x,
                        None => {
                            return Err(make_err(
                                cur_si,
                                true,
                                "overflow during constant expression evaluation",
                            ));
                        }
                    };
                    EraLiteral::Integer(x, src_info)
                }
                _ => {
                    return Err(make_err(
                        op.src_info,
                        true,
                        "invalid arithmetic during constant expression evaluation",
                    ));
                }
            },
            EraExpr::PostUnary(x, op) => match (x.try_evaluate_constant()?, op.kind) {
                _ => {
                    return Err(make_err(
                        op.src_info,
                        true,
                        "invalid arithmetic during constant expression evaluation",
                    ));
                }
            },
            EraExpr::FunCall(x, _) => {
                return Err(make_err(
                    x.source_pos_info(),
                    true,
                    "invalid arithmetic during constant expression evaluation",
                ));
            }
            EraExpr::Binary(x1, op, x2) => match (
                x1.try_evaluate_constant()?,
                op.kind,
                x2.try_evaluate_constant()?,
            ) {
                (EraLiteral::Integer(x1, _), EraTokenKind::Plus, EraLiteral::Integer(x2, _)) => {
                    let x = match x1.checked_add(x2) {
                        Some(x) => x,
                        None => {
                            return Err(make_err(
                                op.src_info,
                                true,
                                "overflow during constant expression evaluation",
                            ));
                        }
                    };
                    EraLiteral::Integer(x, src_info)
                }
                (EraLiteral::Integer(x1, _), EraTokenKind::Minus, EraLiteral::Integer(x2, _)) => {
                    let x = match x1.checked_sub(x2) {
                        Some(x) => x,
                        None => {
                            return Err(make_err(
                                op.src_info,
                                true,
                                "overflow during constant expression evaluation",
                            ));
                        }
                    };
                    EraLiteral::Integer(x, src_info)
                }
                (
                    EraLiteral::Integer(x1, _),
                    EraTokenKind::Multiply,
                    EraLiteral::Integer(x2, _),
                ) => {
                    let x = match x1.checked_mul(x2) {
                        Some(x) => x,
                        None => {
                            return Err(make_err(
                                op.src_info,
                                true,
                                "overflow during constant expression evaluation",
                            ));
                        }
                    };
                    EraLiteral::Integer(x, src_info)
                }
                (EraLiteral::Integer(x1, _), EraTokenKind::Divide, EraLiteral::Integer(x2, _)) => {
                    let x = match x1.checked_div(x2) {
                        Some(x) => x,
                        None => {
                            return Err(make_err(
                                op.src_info,
                                true,
                                "overflow during constant expression evaluation",
                            ));
                        }
                    };
                    EraLiteral::Integer(x, src_info)
                }
                _ => {
                    return Err(make_err(
                        op.src_info,
                        true,
                        "overflow during constant expression evaluation",
                    ));
                }
            },
            EraExpr::Ternary(x1, _, x2, _, x3) => {
                let x1 = x1.try_evaluate_constant()?;
                let x2 = x2.try_evaluate_constant()?;
                let x3 = x3.try_evaluate_constant()?;
                let x1 = match x1 {
                    EraLiteral::Integer(x, _) => x != 0,
                    EraLiteral::String(_, _) => true,
                };
                if x1 {
                    x2
                } else {
                    x3
                }
            }
        })
    }
}

#[derive(Debug, Clone)]
pub enum EraLiteral {
    Integer(i64, SourcePosInfo),
    String(String, SourcePosInfo),
}
impl EraLiteral {
    pub fn source_pos_info(&self) -> SourcePosInfo {
        match self {
            Self::Integer(_, i) => *i,
            Self::String(_, i) => *i,
        }
    }
}

pub struct EraVarDecl {
    pub name: String,
    pub dims: Vec<u32>,
    pub inits: Vec<EraExpr>,
    pub is_string: bool,
    pub is_ref: bool,
    pub is_const: bool,
    pub is_global: bool,
    pub is_dynamic: bool,
    pub src_info: SourcePosInfo,
}

pub struct EraLocalSizeDecl {
    pub size: u64,
    pub src_info: SourcePosInfo,
}

pub struct EraLocalSSizeDecl {
    pub size: u64,
    pub src_info: SourcePosInfo,
}

pub struct EraFunParamDecl {
    pub name: String,
    pub dims: Vec<(u32, SourcePosInfo)>,
    pub src_info: SourcePosInfo,
}

#[derive(Debug, Clone, Copy)]
pub enum EraFunKind {
    Procedure,
    Function,
    FunctionS,
}

pub struct EraFunDecl {
    pub name: String,
    // Used to describe how arguments are bound to parameters (pseudo assignment)
    pub params: Vec<EraExpr>,
    pub kind: EraFunKind,
    //pub vars: Vec<EraVarDecl>,
    pub decls: Vec<EraSharpDecl>,
    pub body: Vec<EraStmt>,
    pub src_info: SourcePosInfo,
}

pub struct EraPrintStmt {
    pub vals: Vec<EraExpr>,
    pub flags: PrintExtendedFlags,
    pub src_info: SourcePosInfo,
}

pub struct EraIfStmt {
    pub cond: EraExpr,
    pub body: Vec<EraStmt>,
    pub else_body: Vec<EraStmt>,
    pub src_info: SourcePosInfo,
}

pub struct EraWaitStmt {
    pub src_info: SourcePosInfo,
}

pub enum EraSelectCaseStmtCondition {
    Single(EraExpr),
    Range(EraExpr, EraExpr),
    Condition(EraTokenLite, EraExpr),
}
pub struct EraSelectCaseStmt {
    pub cond: EraExpr,
    pub cases: Vec<(Vec<EraSelectCaseStmtCondition>, Vec<EraStmt>)>,
    pub case_else: Vec<EraStmt>,
    pub src_info: SourcePosInfo,
}

pub struct EraQuitStmt {
    pub src_info: SourcePosInfo,
}
pub struct EraWhileStmt {
    pub cond: EraExpr,
    pub body: Vec<EraStmt>,
    pub src_info: SourcePosInfo,
}

pub struct EraCallStmt {
    pub func: EraExpr,
    pub args: Vec<EraExpr>,
    pub src_info: SourcePosInfo,
}

pub struct EraReturnStmt {
    pub vals: Vec<EraExpr>,
    pub src_info: SourcePosInfo,
}

pub struct EraContinueStmt {
    pub src_info: SourcePosInfo,
}

pub struct EraBreakStmt {
    pub src_info: SourcePosInfo,
}

pub struct EraThrowStmt {
    pub val: EraExpr,
    pub src_info: SourcePosInfo,
}

pub struct EraRepeatStmt {
    pub loop_cnt: EraExpr,
    pub body: Vec<EraStmt>,
    pub src_info: SourcePosInfo,
}

pub struct EraLabelStmt {
    pub name: String,
    pub src_info: SourcePosInfo,
}

pub struct EraGotoStmt {
    pub target: String,
    pub src_info: SourcePosInfo,
}

pub enum EraStmt {
    Expr(EraExpr),
    Command(EraCommandStmt),
    Label(EraLabelStmt),
}
impl EraStmt {
    fn source_pos_info(&self) -> SourcePosInfo {
        use EraStmt::*;
        match self {
            Expr(x) => x.source_pos_info(),
            Command(x) => x.source_pos_info(),
            Label(x) => x.src_info,
        }
    }
}

pub enum EraCommandStmt {
    DebugPrint(EraPrintStmt),
    Print(EraPrintStmt),
    Wait(EraWaitStmt),
    If(EraIfStmt),
    Quit(EraQuitStmt),
    SelectCase(EraSelectCaseStmt),
    While(EraWhileStmt),
    Call(EraCallStmt),
    Return(EraReturnStmt),
    Continue(EraContinueStmt),
    Break(EraBreakStmt),
    Throw(EraThrowStmt),
    Repeat(EraRepeatStmt),
    Goto(EraGotoStmt),
}
impl EraCommandStmt {
    pub fn source_pos_info(&self) -> SourcePosInfo {
        use EraCommandStmt::*;
        match self {
            DebugPrint(x) => x.src_info,
            Print(x) => x.src_info,
            Wait(x) => x.src_info,
            If(x) => x.src_info,
            Quit(x) => x.src_info,
            SelectCase(x) => x.src_info,
            While(x) => x.src_info,
            Call(x) => x.src_info,
            Return(x) => x.src_info,
            Continue(x) => x.src_info,
            Break(x) => x.src_info,
            Throw(x) => x.src_info,
            Repeat(x) => x.src_info,
            Goto(x) => x.src_info,
        }
    }
}

pub struct EraParser<ErrReportFn> {
    err_report_fn: ErrReportFn,
}

pub struct EraParseErrorInfo {
    pub src_info: SourcePosInfo,
    pub is_error: bool,
    pub msg: String,
}
impl EraParseErrorInfo {
    fn new_token(token: EraTokenLite, is_error: bool, msg: &str) -> Self {
        EraParseErrorInfo {
            src_info: token.src_info,
            is_error,
            msg: format!("`{}`: {}", token, msg),
        }
    }
}

impl<T: FnMut(&EraParseErrorInfo)> EraParser<T> {
    pub fn new(err_report_fn: T) -> Self {
        EraParser { err_report_fn }
    }
    pub fn parse<'b, U: FnMut(&crate::lexer::EraLexErrorInfo)>(
        &mut self,
        lexer: &'b mut crate::lexer::EraLexer<'b, U>,
        global_vars: &HashMap<CaselessString, EraParserSlimVarTypeInfo>,
    ) -> Option<EraRootASTNode> {
        EraParserImpl::new(self, lexer, global_vars).program()
    }
}

struct EraParserImpl<'a, 'b, ErrReportFn, LexErrReportFn> {
    err_report_fn: &'a mut ErrReportFn,
    lexer: &'b mut crate::lexer::EraLexer<'b, LexErrReportFn>,
    global_vars: HashMap<CaselessString, EraParserSlimVarTypeInfo>,
    local_vars: HashMap<CaselessString, EraParserSlimVarTypeInfo>,
    curly_brackets_cnt: u32,
    src_info: SourcePosInfo,
}

impl<'a, 'b, T: FnMut(&EraParseErrorInfo), U: FnMut(&crate::lexer::EraLexErrorInfo)>
    EraParserImpl<'a, 'b, T, U>
{
    fn new(
        parser: &'a mut EraParser<T>,
        lexer: &'b mut crate::lexer::EraLexer<'b, U>,
        global_vars: &HashMap<CaselessString, EraParserSlimVarTypeInfo>,
    ) -> Self {
        Self {
            err_report_fn: &mut parser.err_report_fn,
            lexer,
            global_vars: global_vars.clone(),
            local_vars: HashMap::new(),
            curly_brackets_cnt: 0,
            src_info: SourcePosInfo { line: 1, column: 1 },
        }
    }
    fn report_token_err(&mut self, token: EraTokenLite, is_error: bool, msg: &str) {
        (self.err_report_fn)(&EraParseErrorInfo::new_token(token, is_error, msg));
    }
    fn report_err<V: Into<String>>(&mut self, src_info: SourcePosInfo, is_error: bool, msg: V) {
        (self.err_report_fn)(&EraParseErrorInfo {
            src_info,
            is_error,
            msg: msg.into(),
        });
    }
    fn begin_ignore_newline(&mut self, src_info: SourcePosInfo) {
        self.curly_brackets_cnt += 1;
    }
    fn end_ignore_newline(&mut self, src_info: SourcePosInfo) {
        if self.curly_brackets_cnt == 0 {
            self.report_err(src_info, true, "too many `}`");
        } else {
            self.curly_brackets_cnt -= 1;
        }
    }
    fn is_ignoring_newline(&self) -> bool {
        self.curly_brackets_cnt > 0
    }
    fn program(&mut self) -> Option<EraRootASTNode> {
        self.skip_whitespace();
        let first_token = self.lexer.peek(EraLexerMode::Normal);
        if matches!(first_token.kind, EraTokenKind::Eof | EraTokenKind::Invalid) {
            self.report_token_err(
                first_token.into(),
                true,
                "unexpected token at start of file",
            );
            return None;
        }
        let mut root = EraRootASTNode {
            decls: Vec::new(),
            src_info: first_token.src_info,
        };
        loop {
            self.skip_whitespace();
            if self.lexer.peek(EraLexerMode::Normal).is_eof() {
                break;
            }
            let decl = self.declaration()?;
            if let EraDecl::SharpDecl(x) = &decl {
                match x {
                    EraSharpDecl::FunctionDecl(_) => (),
                    EraSharpDecl::VarDecl(x) => {
                        let info = EraParserSlimVarTypeInfo {
                            is_string: x.is_string,
                        };
                        if self
                            .global_vars
                            .insert(CaselessString::new(x.name.clone()), info)
                            .is_some()
                        {
                            self.report_err(
                                x.src_info,
                                true,
                                format!("redefinition of global variable `{}`", x.name),
                            );
                        }
                    }
                    _ => {
                        self.report_err(
                            x.source_pos_info(),
                            false,
                            "this declaration should not appear here; ignoring",
                        );
                    }
                }
            }
            root.decls.push(decl);
        }
        Some(root)
    }
    fn matches_newline(&mut self) -> Option<EraToken<'b>> {
        self.try_consume(EraLexerMode::Normal, EraTokenKind::LineBreak)
    }
    fn consume_newline(&mut self) -> Option<EraToken<'b>> {
        self.consume(EraLexerMode::Normal, EraTokenKind::LineBreak)
    }
    fn declaration(&mut self) -> Option<EraDecl> {
        self.skip_whitespace();
        if self
            .try_consume(EraLexerMode::Normal, EraTokenKind::NumberSign)
            .is_some()
        {
            Some(EraDecl::SharpDecl(self.sharp_declaration()?))
        } else {
            Some(EraDecl::FunDecl(self.func_declaration()?))
        }
    }
    fn sharp_declaration(&mut self) -> Option<EraSharpDecl> {
        let first = self.read_token(EraLexerMode::SharpDecl);
        match first.kind {
            EraTokenKind::KwDim => Some(EraSharpDecl::VarDecl(self.var_declaration(false)?)),
            EraTokenKind::KwDimS => Some(EraSharpDecl::VarDecl(self.var_declaration(true)?)),
            EraTokenKind::KwLocalSize | EraTokenKind::KwLocalSSize => {
                let size = self.expression(true)?;
                let size_si = size.source_pos_info();
                let size = match self.evaluate_expression_ast(size) {
                    Some(x) => match x {
                        EraLiteral::Integer(x, _) => x,
                        EraLiteral::String(_, cur_si) => {
                            self.synchronize();
                            self.report_token_err(first.into(), true, "invalid LOCALSIZE size");
                            return None;
                        }
                    },
                    None => {
                        self.synchronize();
                        return None;
                    }
                };
                let size = match size.try_into() {
                    Ok(x) => x,
                    _ => {
                        self.synchronize();
                        self.report_token_err(
                            first.into(),
                            true,
                            "local size size must be non-negative",
                        );
                        return None;
                    }
                };
                Some(match first.kind {
                    EraTokenKind::KwLocalSize => EraSharpDecl::LocalSizeDecl(EraLocalSizeDecl {
                        size,
                        src_info: first.src_info,
                    }),
                    EraTokenKind::KwLocalSSize => EraSharpDecl::LocalSSizeDecl(EraLocalSSizeDecl {
                        size,
                        src_info: first.src_info,
                    }),
                    _ => unreachable!(),
                })
            }
            EraTokenKind::KwFunction => Some(EraSharpDecl::FunctionDecl(EraSharpFunctionDecl {
                returns_string: false,
                src_info: first.src_info,
            })),
            EraTokenKind::KwFunctionS => Some(EraSharpDecl::FunctionDecl(EraSharpFunctionDecl {
                returns_string: true,
                src_info: first.src_info,
            })),
            _ => {
                self.synchronize_with_token(first.into());
                self.report_token_err(
                    first.into(),
                    true,
                    "unrecognized token in sharp declaration",
                );
                None
            }
        }
    }
    fn var_declaration(&mut self, is_string: bool) -> Option<EraVarDecl> {
        let mut is_ref = false;
        let mut is_const = false;
        let mut is_global = false;
        let mut is_dynamic = false;
        let mut dims = Vec::new();
        let mut inits = Vec::new();
        let name = loop {
            let token = self.read_token(EraLexerMode::SharpDecl);
            match token.kind {
                EraTokenKind::KwDynamic | EraTokenKind::KwGlobal | EraTokenKind::KwRef => {
                    if is_ref || is_global || is_dynamic {
                        self.synchronize();
                        self.report_token_err(
                            token.into(),
                            true,
                            "too many qualifiers in variable declaration",
                        );
                        return None;
                    }
                    match token.kind {
                        EraTokenKind::KwDynamic => is_dynamic = true,
                        EraTokenKind::KwGlobal => is_global = true,
                        EraTokenKind::KwRef => is_ref = true,
                        _ => unreachable!(),
                    }
                }
                EraTokenKind::KwConst => {
                    if is_const {
                        self.report_token_err(
                            token.into(),
                            false,
                            "redundant const qualifier in variable declaration",
                        );
                    }
                    is_const = true;
                }
                EraTokenKind::Identifier => break token,
                _ => {
                    self.report_token_err(
                        token.into(),
                        true,
                        "unexpected token in variable declaration",
                    );
                    return None;
                }
            }
        };
        // Array dimensions
        while self
            .try_consume(EraLexerMode::SharpDecl, EraTokenKind::Comma)
            .is_some()
        {
            let dim = self.expression_bp(
                infix_binding_power(EraTokenKind::Assign).unwrap().1 + 2,
                true,
                EraTokenKind::Eof,
            )?;
            let cur_si = dim.source_pos_info();
            let dim = self.unwrap_int_from_expression(dim)?;
            let dim = match dim.try_into() {
                Ok(x) => x,
                _ => {
                    self.report_err(cur_si, true, "invalid array dimension; assuming 1");
                    1
                }
            };
            dims.push(dim);
        }
        // Initializers
        if self
            .try_consume(EraLexerMode::SharpDecl, EraTokenKind::Assign)
            .is_some()
        {
            loop {
                let init = self.expression(true)?;
                // HACK: Handle constants only for now
                let init = self.unwrap_literal_from_expression(init)?;
                inits.push(EraExpr::Term(EraTermExpr::Literal(init)));
                if self
                    .try_consume(EraLexerMode::SharpDecl, EraTokenKind::Comma)
                    .is_none()
                {
                    break;
                }
            }
        }
        Some(EraVarDecl {
            name: String::from_utf8_lossy(name.lexeme).into_owned(),
            dims,
            inits,
            is_string,
            is_ref,
            is_const,
            is_global,
            is_dynamic,
            src_info: name.src_info,
        })
    }
    fn func_declaration(&mut self) -> Option<EraFunDecl> {
        self.consume(EraLexerMode::Normal, EraTokenKind::At)?;
        let name = self.consume(EraLexerMode::Normal, EraTokenKind::Identifier)?;
        let token = self.read_token(EraLexerMode::Normal);
        let mut params = Vec::new();
        let mut decls = Vec::new();
        let mut body = Vec::new();
        let mut kind = EraFunKind::Procedure;

        // Reset local variables
        self.local_vars.clear();

        // Parameters list
        match token.kind {
            EraTokenKind::Comma => {
                loop {
                    params.push(self.expression(true)?);
                    if self
                        .try_consume(EraLexerMode::Normal, EraTokenKind::Comma)
                        .is_none()
                    {
                        break;
                    }
                }
                self.consume(EraLexerMode::Normal, EraTokenKind::LineBreak)?;
            }
            EraTokenKind::LBracket => {
                if self
                    .try_consume(EraLexerMode::Normal, EraTokenKind::RBracket)
                    .is_none()
                {
                    loop {
                        params.push(self.expression(true)?);
                        if self
                            .try_consume(EraLexerMode::Normal, EraTokenKind::Comma)
                            .is_none()
                        {
                            break;
                        }
                    }
                    self.consume(EraLexerMode::Normal, EraTokenKind::RBracket)?;
                }
                self.consume(EraLexerMode::Normal, EraTokenKind::LineBreak)?;
            }
            EraTokenKind::LineBreak => (),
            _ => {
                self.synchronize_with_token(token.into());
                self.report_token_err(
                    token.into(),
                    true,
                    "unexpected token in function declaration",
                );
                return None;
            }
        }
        // Local declarations
        self.skip_whitespace();
        while self
            .try_consume(EraLexerMode::Normal, EraTokenKind::NumberSign)
            .is_some()
        {
            let decl = self.sharp_declaration()?;
            match &decl {
                EraSharpDecl::VarDecl(x) => {
                    let info = EraParserSlimVarTypeInfo {
                        is_string: x.is_string,
                    };
                    let name = CaselessString::new(x.name.clone());
                    if self.local_vars.insert(name, info).is_some() {
                        self.report_err(
                            x.src_info,
                            true,
                            format!("redefinition of local variable `{}`", x.name),
                        );
                    }
                }
                EraSharpDecl::FunctionDecl(x) => {
                    if !matches!(kind, EraFunKind::Procedure) {
                        self.report_err(x.src_info, true, "too many function type specifiers");
                    }
                    kind = if x.returns_string {
                        EraFunKind::FunctionS
                    } else {
                        EraFunKind::Function
                    };
                }
                _ => (),
            }
            decls.push(decl);
            self.skip_whitespace();
        }
        // Body
        loop {
            self.skip_whitespace();
            let token = self.lexer.peek(EraLexerMode::Normal);
            match token.kind {
                EraTokenKind::At | EraTokenKind::Eof => break,
                EraTokenKind::NumberSign => {
                    self.synchronize();
                    self.report_token_err(
                        token.into(),
                        false,
                        "this declaration should not appear within function body; ignoring",
                    );
                }
                _ => body.push(self.statement()?),
            }
        }

        Some(EraFunDecl {
            name: String::from_utf8_lossy(name.lexeme).into_owned(),
            params,
            kind,
            decls,
            body,
            src_info: name.src_info,
        })
    }
    fn statement(&mut self) -> Option<EraStmt> {
        let first = self.lexer.peek(EraLexerMode::Normal);
        Some(match first.kind {
            EraTokenKind::Identifier => EraStmt::Command({
                let cmd = first.lexeme.to_ascii_uppercase();
                let cmd = cmd.as_slice();

                trait Adhoc {
                    fn r(&mut self) -> &mut Self;
                }
                impl<T, U: FnMut(&crate::lexer::EraLexErrorInfo)> Adhoc for EraParserImpl<'_, '_, T, U> {
                    fn r(&mut self) -> &mut Self {
                        // Discard command token
                        let token = self.lexer.read(EraLexerMode::Normal);
                        self.src_info = token.src_info;
                        self
                    }
                }

                if let Some((arg_fmt, flags)) = Self::try_recognize_print_cmd(cmd) {
                    EraCommandStmt::Print(self.r().stmt_print(arg_fmt, flags)?)
                } else if let Some((arg_fmt, flags)) = Self::try_recognize_debugprint_cmd(cmd) {
                    EraCommandStmt::DebugPrint(self.r().stmt_print(arg_fmt, flags)?)
                } else {
                    match cmd {
                        b"IF" => EraCommandStmt::If(self.r().stmt_if()?),
                        b"SELECTCASE" => EraCommandStmt::SelectCase(self.r().stmt_selectcase()?),
                        b"SIF" => EraCommandStmt::If(self.r().stmt_sif()?),
                        b"QUIT" => EraCommandStmt::Quit(self.r().stmt_quit()?),
                        b"WAIT" => EraCommandStmt::Wait(self.r().stmt_wait()?),
                        b"WHILE" => EraCommandStmt::While(self.r().stmt_while()?),
                        b"RETURN" | b"RETURNF" => EraCommandStmt::Return(self.r().stmt_return()?),
                        b"CALL" => EraCommandStmt::Call(self.r().stmt_call()?),
                        b"CONTINUE" => EraCommandStmt::Continue(self.r().stmt_continue()?),
                        b"BREAK" => EraCommandStmt::Break(self.r().stmt_break()?),
                        b"THROW" => EraCommandStmt::Throw(self.r().stmt_throw()?),
                        b"REPEAT" => EraCommandStmt::Repeat(self.r().stmt_repeat()?),
                        b"GOTO" => EraCommandStmt::Goto(self.r().stmt_goto()?),
                        _ => return Some(EraStmt::Expr(self.expression(false)?)),
                    }
                }

                // if let Some((arg_fmt, flags)) = Self::try_recognize_print_cmd(cmd) {
                //     EraCommandStmt::Print(self.stmt_print(arg_fmt, flags)?)
                // } else {
                //     match cmd {
                //         b"WAIT" => EraCommandStmt::Wait(self.stmt_wait()?),
                //         _ => return Some(EraStmt::Expr(self.expression()?)),
                //     }
                // }
            }),
            EraTokenKind::Dollar => {
                _ = self.lexer.read(EraLexerMode::Normal);
                let token = self.consume(EraLexerMode::Normal, EraTokenKind::Identifier)?;
                self.consume_newline()?;
                EraStmt::Label(EraLabelStmt {
                    name: String::from_utf8_lossy(token.lexeme).into_owned(),
                    src_info: token.src_info,
                })
            }
            EraTokenKind::At => {
                // TODO: Add suggestion for the most recent open block
                self.report_token_err(
                    first.into(),
                    true,
                    "unexpected start of function; did you forget to close a block?",
                );
                return None;
            }
            EraTokenKind::Eof => {
                // TODO: Add suggestion for the most recent open block
                self.report_token_err(
                    first.into(),
                    true,
                    "unexpected end of file; did you forget to close a block?",
                );
                return None;
            }
            _ => EraStmt::Expr(self.expression(false)?),
        })
    }
    // TODO: Remove var_expression?
    fn var_expression(&mut self) -> Option<EraVarExpr> {
        //let name = self.consume(EraLexerMode::Normal, EraTokenKind::Identifier)?;
        let expr = self.expression(true)?;
        Some(match expr {
            EraExpr::Term(EraTermExpr::Var(x)) => x,
            _ => {
                self.report_err(
                    expr.source_pos_info(),
                    true,
                    "unexpected expression in var-expression",
                );
                return None;
            }
        })
    }
    fn expression(&mut self, pure: bool) -> Option<EraExpr> {
        self.expression_bp(0, pure, EraTokenKind::Eof)
    }
    fn expression_bp(&mut self, min_bp: u8, pure: bool, break_at: EraTokenKind) -> Option<EraExpr> {
        let first = self.lexer.read(EraLexerMode::Normal);
        let mut lhs = match first.kind {
            EraTokenKind::IntLiteral => {
                // TODO: use atoi_simd?
                let mut lexeme = first.lexeme;
                let radix =
                    if lexeme.strip_prefix_inplace(b"0x") || lexeme.strip_prefix_inplace(b"0X") {
                        16
                    } else if let Some(x) = lexeme.strip_prefix(b"0") {
                        if !x.is_empty() {
                            lexeme = x;
                        }
                        //8
                        10
                    } else {
                        10
                    };
                // TODO: Support scientific notation
                // SAFETY: Input is guaranteed to be ASCII-only bytes
                let lexeme = unsafe { std::str::from_utf8_unchecked(lexeme) };
                let val = match i64::from_str_radix(lexeme, radix) {
                    Ok(x) => x,
                    Err(_) => {
                        self.synchronize();
                        self.report_token_err(
                            first.into(),
                            true,
                            concatdoc! {"
                            invalid integer literal (if you need integer literals that can overflow ",
                            "i64, consider configuring the engine with flag use_inf_prec_int set)
                            "},
                        );
                        return None;
                    }
                };
                EraExpr::new_int(val, first.src_info)
            }
            // TODO: Handle escape characters for StringLiteral
            EraTokenKind::StringLiteral => EraExpr::new_str(
                String::from_utf8_lossy(&first.lexeme[1..(first.lexeme.len() - 1)]).into_owned(),
                first.src_info,
            ),
            EraTokenKind::StringFormStart => {
                EraExpr::Term(EraTermExpr::StrForm(self.expression_strform(first)?))
            }
            EraTokenKind::TernaryStrFormMarker => {
                self.ternary_strform()?
            }
            EraTokenKind::LBracket => {
                let lhs = self.expression(true)?;
                self.consume(EraLexerMode::Normal, EraTokenKind::RBracket)?;
                lhs
            }
            EraTokenKind::Identifier => EraExpr::Term(EraTermExpr::Var(EraVarExpr {
                name: String::from_utf8_lossy(first.lexeme).into_owned(),
                idxs: Vec::new(),
                src_info: first.src_info,
            })),
            _ => {
                // Handle prefix
                if let Some(((), r_bp)) = prefix_binding_power(first.kind) {
                    let rhs = self.expression_bp(r_bp, pure, break_at)?;
                    EraExpr::PreUnary(first.into(), rhs.into())
                } else {
                    self.synchronize_with_token(first.into());
                    self.report_token_err(first.into(), true, "unexpected token in expression");
                    return None;
                }
            }
        };

        loop {
            let token = self.peek_token(EraLexerMode::Normal);

            // Handle postfix
            if let Some((l_bp, ())) = postfix_binding_power(token.kind) {
                if l_bp < min_bp {
                    break;
                }
                self.read_token(EraLexerMode::Normal);

                lhs = match token.kind {
                    EraTokenKind::LBracket => {
                        let mut args = Vec::new();
                        // NOTE: We explicitly allows for trailing commas inside function arguments
                        if self
                            .try_consume(EraLexerMode::Normal, EraTokenKind::RBracket)
                            .is_none()
                        {
                            loop {
                                args.push(self.expression(true)?);
                                if self
                                    .try_consume(EraLexerMode::Normal, EraTokenKind::Comma)
                                    .is_none()
                                {
                                    break;
                                }
                                if matches!(
                                    self.peek_token(EraLexerMode::Normal).kind,
                                    EraTokenKind::RBracket
                                ) {
                                    break;
                                }
                            }
                            self.consume(EraLexerMode::Normal, EraTokenKind::RBracket)?;
                        }
                        EraExpr::FunCall(lhs.into(), args)
                    }
                    _ => EraExpr::PostUnary(lhs.into(), token.into()),
                };

                continue;
            }

            // Handle infix
            if let Some((l_bp, r_bp)) = infix_binding_power(token.kind) {
                if l_bp < min_bp {
                    break;
                }
                // HACK: Break at token
                if break_at == token.kind {
                    break;
                }
                self.read_token(EraLexerMode::Normal);

                lhs = match token.kind {
                    EraTokenKind::QuestionMark => {
                        let mhs = self.expression(true)?;
                        let token2 =
                            self.consume(EraLexerMode::Normal, EraTokenKind::NumberSign)?;
                        let rhs = self.expression_bp(r_bp, pure, break_at)?;
                        EraExpr::Ternary(
                            lhs.into(),
                            token.into(),
                            mhs.into(),
                            token2.into(),
                            rhs.into(),
                        )
                    }
                    EraTokenKind::Colon => match lhs {
                        EraExpr::Term(EraTermExpr::Var(mut x)) => {
                            let rhs = self.expression_bp(r_bp, pure, break_at)?;
                            x.idxs.push(rhs);
                            EraExpr::Term(EraTermExpr::Var(x))
                        }
                        _ => {
                            self.synchronize();
                            self.report_token_err(
                                token.into(),
                                true,
                                "invalid array subscripting target",
                            );
                            return None;
                        }
                    },
                    EraTokenKind::Assign => match lhs {
                        EraExpr::Term(EraTermExpr::Var(x)) => {
                            let rhs = if self.var_is_str(x.name.as_bytes()) && !pure {
                                EraExpr::Term(EraTermExpr::StrForm(self.raw_strform(true)?))
                            } else {
                                self.expression_bp(r_bp, pure, break_at)?
                            };
                            let lhs = EraExpr::Term(EraTermExpr::Var(x));
                            EraExpr::Binary(lhs.into(), token.into(), rhs.into())
                        }
                        _ => {
                            self.synchronize();
                            self.report_token_err(token.into(), true, "invalid assignment target");
                            return None;
                        }
                    },
                    EraTokenKind::At => match lhs {
                        EraExpr::Term(EraTermExpr::Var(mut x)) => {
                            // HACK: Add `@` as part of variable name
                            let rhs =
                                self.consume(EraLexerMode::Normal, EraTokenKind::Identifier)?;
                            let rhs = format!("@{}", String::from_utf8_lossy(rhs.lexeme));
                            x.name.push_str(&rhs);
                            EraExpr::Term(EraTermExpr::Var(x))
                        }
                        _ => {
                            self.synchronize();
                            self.report_token_err(token.into(), true, "invalid variable name");
                            return None;
                        }
                    },
                    _ => {
                        let rhs = self.expression_bp(r_bp, pure, break_at)?;
                        EraExpr::Binary(lhs.into(), token.into(), rhs.into())
                    }
                };

                continue;
            }

            break;
        }

        Some(lhs)
    }
    fn expression_strform(&mut self, first: EraToken<'b>) -> Option<EraStrFormExpr> {
        let mut parts = Vec::new();
        let mut token = self.lexer.read(EraLexerMode::StrForm);
        let src_info = token.src_info;
        loop {
            match token.kind {
                EraTokenKind::PlainStringLiteral => parts.push(EraStrFormExprPart::Literal(
                    String::from_utf8_lossy(token.lexeme).into_owned(),
                    token.src_info,
                )),
                // TODO: String interpolation type-check
                EraTokenKind::LCurlyBracket => {
                    parts.push(EraStrFormExprPart::Expression(
                        self.strform_expr_part_expr(EraTokenKind::Eof)?,
                    ));
                    self.consume(EraLexerMode::Normal, EraTokenKind::RCurlyBracket)?;
                }
                // TODO: String interpolation type-check
                EraTokenKind::Percentage => {
                    parts.push(EraStrFormExprPart::Expression(
                        self.strform_expr_part_expr(EraTokenKind::Percentage)?,
                    ));
                    self.consume(EraLexerMode::Normal, EraTokenKind::Percentage)?;
                }
                EraTokenKind::TernaryStrFormMarker => {
                    let part = EraStrFormExprPartExpression {
                        expr: self.ternary_strform()?,
                        width: None,
                        alignment: PadStringFlags::new(),
                    };
                    parts.push(EraStrFormExprPart::Expression(part));
                }
                EraTokenKind::DoubleQuote => {
                    break;
                }
                _ => {
                    // TODO: Synchronize here?
                    self.report_token_err(token.into(), true, "unexpected token");
                    return None;
                }
            }
            token = self.lexer.read(EraLexerMode::StrForm);
        }
        Some(EraStrFormExpr { parts, src_info })
    }
    fn ternary_strform(&mut self) -> Option<EraExpr> {
        let cond = self.expression_bp(
            infix_binding_power(EraTokenKind::QuestionMark).unwrap().1 + 2,
            true,
            EraTokenKind::Eof,
        )?;
        let mut is_first = true;
        let mut then_parts = Vec::new();
        let mut else_parts = Vec::new();
        let op1 = self.consume(EraLexerMode::Normal, EraTokenKind::QuestionMark)?;
        // Mid
        let op2 = loop {
            let token = self.lexer.read(EraLexerMode::TernaryStrForm);
            match token.kind {
                EraTokenKind::PlainStringLiteral => {
                    let mut lexeme = token.lexeme;
                    if is_first {
                        while let [b' ' | b'\t', rest @ ..] = lexeme {
                            lexeme = rest;
                        }
                    }
                    then_parts.push(EraStrFormExprPart::Literal(
                        String::from_utf8_lossy(lexeme).into_owned(),
                        token.src_info,
                    ));
                }
                // TODO: String interpolation type-check
                EraTokenKind::LCurlyBracket => {
                    then_parts.push(EraStrFormExprPart::Expression(
                        self.strform_expr_part_expr(EraTokenKind::Eof)?,
                    ));
                    self.consume(EraLexerMode::Normal, EraTokenKind::RCurlyBracket)?;
                }
                // TODO: String interpolation type-check
                EraTokenKind::Percentage => {
                    then_parts.push(EraStrFormExprPart::Expression(
                        self.strform_expr_part_expr(EraTokenKind::Percentage)?,
                    ));
                    self.consume(EraLexerMode::Normal, EraTokenKind::Percentage)?;
                }
                EraTokenKind::TernaryStrFormMarker => {
                    // NOTE: We disallow recursion here
                    // TODO: Synchronize here?
                    self.report_token_err(token.into(), true, "unexpected token, expected `#`");
                    return None;
                }
                EraTokenKind::NumberSign => {
                    if let Some(EraStrFormExprPart::Literal(x, _)) = then_parts.last_mut() {
                        x.truncate(x.trim_end_matches(&[' ', '\t']).len());
                    }
                    break token;
                },
                _ => {
                    // TODO: Synchronize here?
                    self.report_token_err(token.into(), true, "unexpected token");
                    return None;
                }
            }
            is_first = false;
        };
        // Right
        is_first = true;
        loop {
            let token = self.lexer.read(EraLexerMode::StrForm);
            match token.kind {
                EraTokenKind::PlainStringLiteral => {
                    let mut lexeme = token.lexeme;
                    if is_first {
                        while let [b' ' | b'\t', rest @ ..] = lexeme {
                            lexeme = rest;
                        }
                    }
                    else_parts.push(EraStrFormExprPart::Literal(
                        String::from_utf8_lossy(lexeme).into_owned(),
                        token.src_info,
                    ));
                }
                // TODO: String interpolation type-check
                EraTokenKind::LCurlyBracket => {
                    else_parts.push(EraStrFormExprPart::Expression(
                        self.strform_expr_part_expr(EraTokenKind::Eof)?,
                    ));
                    self.consume(EraLexerMode::Normal, EraTokenKind::RCurlyBracket)?;
                }
                // TODO: String interpolation type-check
                EraTokenKind::Percentage => {
                    else_parts.push(EraStrFormExprPart::Expression(
                        self.strform_expr_part_expr(EraTokenKind::Percentage)?,
                    ));
                    self.consume(EraLexerMode::Normal, EraTokenKind::Percentage)?;
                }
                EraTokenKind::TernaryStrFormMarker => {
                    if let Some(EraStrFormExprPart::Literal(x, _)) = else_parts.last_mut() {
                        x.truncate(x.trim_end_matches(&[' ', '\t']).len());
                    }
                    break;
                },
                _ => {
                    // TODO: Synchronize here?
                    self.report_token_err(token.into(), true, "unexpected token");
                    return None;
                }
            }
            is_first = false;
        }
        Some(EraExpr::Ternary(
            cond.into(),
            op1.into(),
            EraExpr::Term(EraTermExpr::StrForm(EraStrFormExpr {
                parts: then_parts,
                src_info: op1.src_info,
            }))
            .into(),
            op2.into(),
            EraExpr::Term(EraTermExpr::StrForm(EraStrFormExpr {
                parts: else_parts,
                src_info: op2.src_info,
            }))
            .into(),
        ))
    }
    fn raw_strform(&mut self, trim_leading_whitespace: bool) -> Option<EraStrFormExpr> {
        let mut parts = Vec::new();
        let mut token = self.lexer.read(EraLexerMode::RawStrForm);
        let mut is_first = true;
        let src_info = token.src_info;
        loop {
            match token.kind {
                EraTokenKind::PlainStringLiteral => {
                    let mut lexeme = token.lexeme;
                    if trim_leading_whitespace && is_first {
                        while let [b' ' | b'\t', rest @ ..] = lexeme {
                            lexeme = rest;
                        }
                    }
                    parts.push(EraStrFormExprPart::Literal(
                        String::from_utf8_lossy(lexeme).into_owned(),
                        token.src_info,
                    ))
                }
                // TODO: String interpolation type-check
                EraTokenKind::LCurlyBracket => {
                    parts.push(EraStrFormExprPart::Expression(
                        self.strform_expr_part_expr(EraTokenKind::Eof)?,
                    ));
                    self.consume(EraLexerMode::Normal, EraTokenKind::RCurlyBracket)?;
                }
                // TODO: String interpolation type-check
                EraTokenKind::Percentage => {
                    parts.push(EraStrFormExprPart::Expression(
                        self.strform_expr_part_expr(EraTokenKind::Percentage)?,
                    ));
                    self.consume(EraLexerMode::Normal, EraTokenKind::Percentage)?;
                }
                EraTokenKind::LineBreak => {
                    if !self.is_ignoring_newline() {
                        break;
                    }
                    parts.push(EraStrFormExprPart::Literal(" ".to_owned(), token.src_info));
                    self.handle_ignore_newline_tokens();
                }
                EraTokenKind::TernaryStrFormMarker => {
                    let part = EraStrFormExprPartExpression {
                        expr: self.ternary_strform()?,
                        width: None,
                        alignment: PadStringFlags::new(),
                    };
                    parts.push(EraStrFormExprPart::Expression(part));
                }
                _ => {
                    // TODO: Synchronize here?
                    self.report_token_err(token.into(), true, "unexpected token");
                    return None;
                }
            }
            is_first = false;
            token = self.lexer.read(EraLexerMode::RawStrForm);
        }
        Some(EraStrFormExpr { parts, src_info })
    }
    fn strform_expr_part_expr(
        &mut self,
        break_at: EraTokenKind,
    ) -> Option<EraStrFormExprPartExpression> {
        let expr = self.expression_bp(0, true, break_at)?;
        let mut width = None;
        let mut alignment = PadStringFlags::new();
        if self
            .try_consume(EraLexerMode::Normal, EraTokenKind::Comma)
            .is_some()
        {
            width = Some(self.expression_bp(0, true, break_at)?);
            if self
                .try_consume(EraLexerMode::Normal, EraTokenKind::Comma)
                .is_some()
            {
                let align = self.consume(EraLexerMode::Normal, EraTokenKind::Identifier)?;
                if align.lexeme.eq_ignore_ascii_case(b"LEFT") {
                    alignment.set_left_pad(true);
                } else if align.lexeme.eq_ignore_ascii_case(b"RIGHT") {
                    alignment.set_right_pad(true);
                } else {
                    self.report_token_err(align.into(), true, "illegal alignment specifier");
                    return None;
                }
            }
        }
        Some(EraStrFormExprPartExpression {
            expr,
            width,
            alignment,
        })
    }
    // fn command(&mut self, cmd_token: &EraToken) -> Option<EraCommandStmt> {
    //     // TODO...
    //     unimplemented!()
    // }
    fn stmt_print(
        &mut self,
        arg_fmt: EraCommandArgFmt,
        flags: PrintExtendedFlags,
    ) -> Option<EraPrintStmt> {
        let mut vals = Vec::new();
        match arg_fmt {
            // TODO: PRINT statement type-check
            EraCommandArgFmt::Expression | EraCommandArgFmt::ExpressionS => loop {
                vals.push(self.expression(true)?);
                if self
                    .try_consume(EraLexerMode::Normal, EraTokenKind::Comma)
                    .is_some()
                {
                    continue;
                }
                self.consume(EraLexerMode::Normal, EraTokenKind::LineBreak)?;
                break;
            },
            EraCommandArgFmt::RawString => {
                let mut cont = String::new();
                let mut token = self.lexer.read(EraLexerMode::RawStr);
                loop {
                    match token.kind {
                        EraTokenKind::PlainStringLiteral => {
                            cont.push_str(&String::from_utf8_lossy(token.lexeme))
                        }
                        EraTokenKind::LineBreak => {
                            if !self.is_ignoring_newline() {
                                break;
                            }
                            cont.push(' ');
                            self.handle_ignore_newline_tokens();
                        }
                        _ => {
                            // TODO: Synchronize here?
                            self.report_token_err(token.into(), true, "unexpected token");
                            return None;
                        }
                    }
                    token = self.lexer.read(EraLexerMode::RawStr);
                }
                vals.push(EraExpr::new_str(cont, token.src_info));
            }
            EraCommandArgFmt::RawStringForm => {
                vals.push(EraExpr::Term(EraTermExpr::StrForm(
                    self.raw_strform(false)?,
                )));
            }
        }
        Some(EraPrintStmt {
            vals,
            flags,
            src_info: self.src_info,
        })
    }
    fn stmt_sif(&mut self) -> Option<EraIfStmt> {
        let cond = self.expression(true)?;
        self.consume(EraLexerMode::Normal, EraTokenKind::LineBreak)?;
        self.skip_whitespace();
        // TODO: Emuera does not allow statements like if-else here, should we also
        //       disallow it?
        let body = vec![self.statement()?];
        Some(EraIfStmt {
            cond,
            body,
            else_body: Vec::new(),
            src_info: self.src_info,
        })
    }
    fn stmt_if(&mut self) -> Option<EraIfStmt> {
        let src_info = self.src_info;
        let cond = self.expression(true)?;
        self.consume(EraLexerMode::Normal, EraTokenKind::LineBreak)?;
        let mut body = Vec::new();
        let mut else_body = Vec::new();
        let mut is_at_else = false;
        loop {
            if let Some(token) = self.matches_command_end(b"ELSEIF") {
                if is_at_else {
                    self.synchronize();
                    self.report_token_err(token.into(), true, "too many ELSEIF command");
                }
                self.src_info = token.src_info;
                else_body.push(EraStmt::Command(EraCommandStmt::If(self.stmt_if()?)));
                break;
            } else if let Some(token) = self.matches_command_end(b"ELSE") {
                if is_at_else {
                    self.synchronize();
                    self.report_token_err(token.into(), true, "too many ELSE command");
                }
                is_at_else = true;
                self.consume(EraLexerMode::Normal, EraTokenKind::LineBreak)?;
            } else if let Some(token) = self.matches_command_end(b"ENDIF") {
                self.consume(EraLexerMode::Normal, EraTokenKind::LineBreak)?;
                break;
            } else {
                let stmt = self.statement()?;
                is_at_else
                    .then_some(&mut else_body)
                    .unwrap_or(&mut body)
                    .push(stmt);
            }
        }
        Some(EraIfStmt {
            cond,
            body,
            else_body,
            src_info,
        })
    }
    fn stmt_quit(&mut self) -> Option<EraQuitStmt> {
        self.consume(EraLexerMode::Normal, EraTokenKind::LineBreak)?;
        Some(EraQuitStmt {
            src_info: self.src_info,
        })
    }
    fn stmt_wait(&mut self) -> Option<EraWaitStmt> {
        self.consume(EraLexerMode::Normal, EraTokenKind::LineBreak)?;
        Some(EraWaitStmt {
            src_info: self.src_info,
        })
    }
    fn stmt_selectcase(&mut self) -> Option<EraSelectCaseStmt> {
        let src_info = self.src_info;
        let cond = self.expression(true)?;
        self.consume(EraLexerMode::Normal, EraTokenKind::LineBreak)?;
        let mut cases: Vec<(Vec<EraSelectCaseStmtCondition>, Vec<EraStmt>)> = Vec::new();
        let mut is_at_else = false;
        let mut case_else = Vec::new();
        while self.matches_command_end(b"ENDSELECT").is_none() {
            if let Some(cmd_token) = self.matches_command_end(b"CASE") {
                if is_at_else {
                    self.synchronize();
                    self.report_token_err(cmd_token.into(), true, "too many CASE command");
                    return None;
                }
                // Read conditions
                let mut conds = Vec::new();
                loop {
                    let cond;
                    if self.matches_command(b"IS").is_some() {
                        let token = self.read_token(EraLexerMode::Normal);
                        let expr = self.expression(true)?;
                        cond = EraSelectCaseStmtCondition::Condition(token.into(), expr);
                    } else {
                        let lhs = self.expression(true)?;
                        if self.matches_command(b"TO").is_some() {
                            let rhs = self.expression(true)?;
                            cond = EraSelectCaseStmtCondition::Range(lhs, rhs);
                        } else {
                            cond = EraSelectCaseStmtCondition::Single(lhs);
                        }
                    }
                    conds.push(cond);
                    if self
                        .try_consume(EraLexerMode::Normal, EraTokenKind::Comma)
                        .is_none()
                    {
                        break;
                    }
                }
                self.consume(EraLexerMode::Normal, EraTokenKind::LineBreak)?;
                cases.push((conds, Vec::new()));
                continue;
            }
            if let Some(cmd_token) = self.matches_command_end(b"CASEELSE") {
                if is_at_else {
                    self.synchronize();
                    self.report_token_err(cmd_token.into(), true, "too many CASEELSE command");
                    return None;
                }
                is_at_else = true;
                self.consume(EraLexerMode::Normal, EraTokenKind::LineBreak)?;
                continue;
            }
            let stmt = self.statement()?;
            let target_body = if is_at_else {
                &mut case_else
            } else if let Some(cur_case) = cases.last_mut() {
                &mut cur_case.1
            } else {
                self.synchronize();
                self.report_err(
                    stmt.source_pos_info(),
                    true,
                    "statement does not belong to any case",
                );
                return None;
            };
            target_body.push(stmt);
        }
        self.consume(EraLexerMode::Normal, EraTokenKind::LineBreak)?;
        Some(EraSelectCaseStmt {
            cond,
            cases,
            case_else,
            src_info,
        })
    }
    fn stmt_while(&mut self) -> Option<EraWhileStmt> {
        let src_info = self.src_info;
        let cond = self.expression(true)?;
        self.consume(EraLexerMode::Normal, EraTokenKind::LineBreak)?;
        let mut body = Vec::new();
        while self.matches_command_end(b"WEND").is_none() {
            body.push(self.statement()?);
        }
        self.consume(EraLexerMode::Normal, EraTokenKind::LineBreak)?;
        Some(EraWhileStmt {
            cond,
            body,
            src_info,
        })
    }
    fn stmt_return(&mut self) -> Option<EraReturnStmt> {
        let src_info = self.src_info;
        let mut vals = Vec::new();
        if self
            .try_consume(EraLexerMode::Normal, EraTokenKind::LineBreak)
            .is_none()
        {
            loop {
                vals.push(self.expression(true)?);
                if self
                    .try_consume(EraLexerMode::Normal, EraTokenKind::Comma)
                    .is_none()
                {
                    break;
                }
            }
            self.consume(EraLexerMode::Normal, EraTokenKind::LineBreak)?;
        }
        Some(EraReturnStmt { vals, src_info })
    }
    fn stmt_call(&mut self) -> Option<EraCallStmt> {
        // TODO: Support CALLFORM
        let src_info = self.src_info;
        let func = self.consume(EraLexerMode::Normal, EraTokenKind::Identifier)?;
        let func = EraExpr::new_str(
            String::from_utf8_lossy(func.lexeme).into_owned(),
            func.src_info,
        );
        let token = self.read_token(EraLexerMode::Normal);
        let mut args = Vec::new();
        match token.kind {
            EraTokenKind::LineBreak => (),
            EraTokenKind::Comma => loop {
                let expr = self.expression(true)?;
                args.push(expr);
                if self
                    .try_consume(EraLexerMode::Normal, EraTokenKind::Comma)
                    .is_some()
                {
                    continue;
                }
                self.consume_newline()?;
                break;
            },
            EraTokenKind::LBracket => {
                if self
                    .try_consume(EraLexerMode::Normal, EraTokenKind::RBracket)
                    .is_none()
                {
                    loop {
                        let expr = self.expression(true)?;
                        args.push(expr);
                        if self
                            .try_consume(EraLexerMode::Normal, EraTokenKind::Comma)
                            .is_none()
                        {
                            break;
                        }
                    }
                    self.consume(EraLexerMode::Normal, EraTokenKind::RBracket)?;
                }
                self.consume_newline()?;
            }
            _ => {
                self.report_token_err(token.into(), true, "unexpected token in call statement");
                return None;
            }
        }
        Some(EraCallStmt {
            func,
            args,
            src_info,
        })
    }
    fn stmt_continue(&mut self) -> Option<EraContinueStmt> {
        self.consume(EraLexerMode::Normal, EraTokenKind::LineBreak)?;
        Some(EraContinueStmt {
            src_info: self.src_info,
        })
    }
    fn stmt_break(&mut self) -> Option<EraBreakStmt> {
        self.consume(EraLexerMode::Normal, EraTokenKind::LineBreak)?;
        Some(EraBreakStmt {
            src_info: self.src_info,
        })
    }
    fn stmt_throw(&mut self) -> Option<EraThrowStmt> {
        let src_info = self.src_info;
        let val = self.raw_strform(false)?;
        let val = EraExpr::Term(EraTermExpr::StrForm(val));
        Some(EraThrowStmt { val, src_info })
    }
    fn stmt_repeat(&mut self) -> Option<EraRepeatStmt> {
        let src_info = self.src_info;
        let loop_cnt = self.expression(true)?;
        let mut body = Vec::new();
        self.consume(EraLexerMode::Normal, EraTokenKind::LineBreak)?;
        while self.matches_command_end(b"REND").is_none() {
            body.push(self.statement()?);
        }
        self.consume(EraLexerMode::Normal, EraTokenKind::LineBreak)?;
        Some(EraRepeatStmt {
            loop_cnt,
            body,
            src_info,
        })
    }
    fn stmt_goto(&mut self) -> Option<EraGotoStmt> {
        let src_info = self.src_info;
        let target = self.consume(EraLexerMode::Normal, EraTokenKind::Identifier)?;
        let target = String::from_utf8_lossy(target.lexeme).into_owned();
        self.consume_newline()?;
        Some(EraGotoStmt { target, src_info })
    }
    #[must_use]
    fn consume(
        &mut self,
        lexer_mode: EraLexerMode,
        token_kind: EraTokenKind,
    ) -> Option<EraToken<'b>> {
        let token = loop {
            let token = self.lexer.read(lexer_mode);
            if self.is_ignoring_newline() && matches!(token.kind, EraTokenKind::LineBreak) {
                continue;
            }
            break token;
        };
        if token.kind == token_kind {
            Some(token)
        } else {
            self.report_token_err(
                token.into(),
                true,
                &format!("unexpected token, expected token was `{}`", token_kind),
            );
            // If consume failed, perform synchronization automatically
            self.synchronize_with_token(token.into());
            None
        }
    }
    #[must_use]
    fn try_consume(
        &mut self,
        lexer_mode: EraLexerMode,
        token_kind: EraTokenKind,
    ) -> Option<EraToken<'b>> {
        let token = loop {
            let token = self.lexer.peek(lexer_mode);
            if self.is_ignoring_newline() && matches!(token.kind, EraTokenKind::LineBreak) {
                self.lexer.read(lexer_mode);
                continue;
            }
            break token;
        };
        if token.kind == token_kind {
            Some(self.lexer.read(lexer_mode))
        } else {
            None
        }
    }
    #[must_use]
    fn evaluate_expression_ast(&mut self, expr: EraExpr) -> Option<EraLiteral> {
        match expr.try_evaluate_constant() {
            Ok(x) => Some(x),
            Err(x) => {
                (self.err_report_fn)(&x);
                None
            }
        }
    }
    #[must_use]
    fn unwrap_int_literal(&mut self, lit: EraLiteral) -> Option<i64> {
        match lit {
            EraLiteral::Integer(x, _) => Some(x),
            EraLiteral::String(_, cur_si) => {
                self.synchronize();
                self.report_err(cur_si, true, "expected integer constant expression here");
                None
            }
        }
    }
    #[must_use]
    fn unwrap_literal_from_expression(&mut self, expr: EraExpr) -> Option<EraLiteral> {
        match self.evaluate_expression_ast(expr) {
            Some(x) => Some(x),
            None => {
                self.synchronize();
                None
            }
        }
    }
    #[must_use]
    fn unwrap_int_from_expression(&mut self, expr: EraExpr) -> Option<i64> {
        let lit = self.unwrap_literal_from_expression(expr)?;
        self.unwrap_int_literal(lit)
    }
    // Recovers from error by discarding unrecognized tokens
    fn synchronize(&mut self) {
        while self.is_ignoring_newline() {
            if self.lexer.read(EraLexerMode::Normal).is_eof() {
                return;
            }
        }
        loop {
            if matches!(
                self.lexer.read(EraLexerMode::Normal).kind,
                EraTokenKind::Eof | EraTokenKind::LineBreak
            ) {
                break;
            }
        }
    }
    fn synchronize_at_stmt_start(&mut self, peeked_token: EraTokenLite) {
        // HACK: Don't swallow @function
        if !matches!(peeked_token.kind, EraTokenKind::At) {
            self.synchronize();
        }
    }
    fn synchronize_with_token(&mut self, token: EraTokenLite) {
        if token.kind != EraTokenKind::LineBreak {
            self.synchronize();
        }
    }
    #[must_use]
    fn matches_command_end(&mut self, cmd: &[u8]) -> Option<EraToken<'b>> {
        self.skip_whitespace();
        let token = self.lexer.peek(EraLexerMode::Normal);
        if matches!(token.kind, EraTokenKind::Identifier) && token.lexeme.eq_ignore_ascii_case(cmd)
        {
            Some(self.lexer.read(EraLexerMode::Normal))
        } else {
            None
        }
    }
    #[must_use]
    fn matches_command(&mut self, cmd: &[u8]) -> Option<EraToken<'b>> {
        let token = self.peek_token(EraLexerMode::Normal);
        if matches!(token.kind, EraTokenKind::Identifier) && token.lexeme.eq_ignore_ascii_case(cmd)
        {
            Some(self.lexer.read(EraLexerMode::Normal))
        } else {
            None
        }
    }
    #[must_use]
    fn try_recognize_print_cmd(cmd: &[u8]) -> Option<(EraCommandArgFmt, PrintExtendedFlags)> {
        let cmd = cmd.to_ascii_uppercase();
        let mut cmd = cmd.as_slice();
        let arg_fmt;
        let mut flags = PrintExtendedFlags::new();
        cmd = cmd.strip_prefix(b"PRINT")?;
        // Handle PRINTPLAIN*
        if cmd.strip_prefix_inplace(b"PLAIN") {
            flags.set_force_plain(true);
            arg_fmt = if cmd.strip_prefix_inplace(b"FORM") {
                EraCommandArgFmt::RawStringForm
            } else {
                EraCommandArgFmt::RawString
            };
            return cmd.is_empty().then_some((arg_fmt, flags));
        }
        // Handle rest
        cmd.strip_prefix_inplace(b"SINGLE")
            .then(|| flags.set_is_single(true));
        if cmd.strip_prefix_inplace(b"V") {
            arg_fmt = EraCommandArgFmt::Expression;
        } else if cmd.strip_prefix_inplace(b"S") {
            arg_fmt = EraCommandArgFmt::ExpressionS;
        } else if cmd.strip_prefix_inplace(b"FORMS") {
            arg_fmt = EraCommandArgFmt::ExpressionS;
        } else if cmd.strip_prefix_inplace(b"FORM") {
            arg_fmt = EraCommandArgFmt::RawStringForm;
        } else {
            arg_fmt = EraCommandArgFmt::RawString;
        }
        if cmd.strip_prefix_inplace(b"LC") {
            flags.set_left_pad(true);
        } else if cmd.strip_prefix_inplace(b"C") {
            flags.set_right_pad(true);
        }
        cmd.strip_prefix_inplace(b"K")
            .then(|| flags.set_use_kana(true));
        cmd.strip_prefix_inplace(b"D")
            .then(|| flags.set_ignore_color(true));
        if cmd.strip_prefix_inplace(b"L") {
            flags.set_is_line(true);
        } else if cmd.strip_prefix_inplace(b"W") {
            flags.set_is_line(true);
            flags.set_is_wait(true);
        }
        cmd.is_empty().then_some((arg_fmt, flags))
    }
    #[must_use]
    fn try_recognize_debugprint_cmd(cmd: &[u8]) -> Option<(EraCommandArgFmt, PrintExtendedFlags)> {
        let cmd = cmd.to_ascii_uppercase();
        let mut cmd = cmd.as_slice();
        let arg_fmt;
        let mut flags = PrintExtendedFlags::new();
        cmd = cmd.strip_prefix(b"DEBUGPRINT")?;
        // Handle PRINTPLAIN*
        if cmd.strip_prefix_inplace(b"PLAIN") {
            flags.set_force_plain(true);
            arg_fmt = if cmd.strip_prefix_inplace(b"FORM") {
                EraCommandArgFmt::RawStringForm
            } else {
                EraCommandArgFmt::RawString
            };
            return cmd.is_empty().then_some((arg_fmt, flags));
        }
        // Handle rest
        cmd.strip_prefix_inplace(b"SINGLE")
            .then(|| flags.set_is_single(true));
        if cmd.strip_prefix_inplace(b"V") {
            arg_fmt = EraCommandArgFmt::Expression;
        } else if cmd.strip_prefix_inplace(b"S") {
            arg_fmt = EraCommandArgFmt::ExpressionS;
        } else if cmd.strip_prefix_inplace(b"FORMS") {
            arg_fmt = EraCommandArgFmt::ExpressionS;
        } else if cmd.strip_prefix_inplace(b"FORM") {
            arg_fmt = EraCommandArgFmt::RawStringForm;
        } else {
            arg_fmt = EraCommandArgFmt::RawString;
        }
        if cmd.strip_prefix_inplace(b"LC") {
            flags.set_left_pad(true);
        } else if cmd.strip_prefix_inplace(b"C") {
            flags.set_right_pad(true);
        }
        cmd.strip_prefix_inplace(b"K")
            .then(|| flags.set_use_kana(true));
        cmd.strip_prefix_inplace(b"D")
            .then(|| flags.set_ignore_color(true));
        if cmd.strip_prefix_inplace(b"L") {
            flags.set_is_line(true);
        } else if cmd.strip_prefix_inplace(b"W") {
            flags.set_is_line(true);
            flags.set_is_wait(true);
        }
        cmd.is_empty().then_some((arg_fmt, flags))
    }
    // NOTE: If you need to take special care of newline token, don't use this function.
    #[must_use]
    fn peek_token(&mut self, mode: EraLexerMode) -> EraToken<'b> {
        loop {
            let token = self.lexer.peek(mode);
            if self.is_ignoring_newline() && matches!(token.kind, EraTokenKind::LineBreak) {
                self.lexer.read(mode);
                continue;
            }
            break token;
        }
    }
    // NOTE: If you need to take special care of newline token, don't use this function.
    fn read_token(&mut self, mode: EraLexerMode) -> EraToken<'b> {
        loop {
            let token = self.lexer.read(mode);
            if self.is_ignoring_newline() && matches!(token.kind, EraTokenKind::LineBreak) {
                continue;
            }
            break token;
        }
    }
    // Ignore empty lines.
    fn skip_whitespace(&mut self) {
        loop {
            let token = self.lexer.peek(EraLexerMode::Normal);
            match token.kind {
                // HACK: Enter & leave ignore-newline mode
                EraTokenKind::LCurlyBracket => self.begin_ignore_newline(token.src_info),
                EraTokenKind::RCurlyBracket => self.end_ignore_newline(token.src_info),
                EraTokenKind::LineBreak => (),
                _ => break,
            }
            self.lexer.read(EraLexerMode::Normal);
        }
        // while matches!(
        //     self.lexer.peek(EraLexerMode::Normal).kind,
        //     EraTokenKind::LineBreak
        // ) {
        //     self.lexer.read(EraLexerMode::Normal);
        // }
    }
    fn handle_ignore_newline_tokens(&mut self) {
        let token = self.lexer.peek(EraLexerMode::Normal);
        match token.kind {
            EraTokenKind::LCurlyBracket => {
                self.begin_ignore_newline(token.src_info);
                self.lexer.read(EraLexerMode::Normal);
            }
            EraTokenKind::RCurlyBracket => {
                self.end_ignore_newline(token.src_info);
                self.lexer.read(EraLexerMode::Normal);
            }
            _ => (),
        }
    }
    fn var_is_str(&self, name: &[u8]) -> bool {
        let name = CaselessString::new(String::from_utf8_lossy(name).to_ascii_uppercase());
        if self
            .global_vars
            .get(&name)
            .map(|x| x.is_string)
            .unwrap_or(false)
            || self
                .local_vars
                .get(&name)
                .map(|x| x.is_string)
                .unwrap_or(false)
        {
            return true;
        }
        // HACK: Check for built-in string variables; use external lists in the future?
        let name = name.as_str();
        if name.starts_with("LOCALS@") || name == "LOCALS" {
            return true;
        }
        if name.starts_with("ARGS@") || name == "ARGS" {
            return true;
        }
        if name == "RESULTS" {
            return true;
        }
        false
    }
    // TODO: Remove is_builtin_command()
    fn is_builtin_command(cmd: &[u8]) -> bool {
        const BUILTIN_COMMANDS: &'static [&'static [u8]] = &[b"PRINT", b"PRINTFORM", b"WAIT"];
        BUILTIN_COMMANDS.iter().any(|x| x.eq_ignore_ascii_case(cmd))
    }
    // TODO: Remove get_command_arg_fmt()
    // Gets information about how to parse command arguments.
    fn get_command_arg_fmt(cmd: &[u8]) -> Option<EraCommandArgFmt> {
        use EraCommandArgFmt::*;
        let cmd = cmd.to_ascii_uppercase();
        // TODO...
        Some(match cmd.as_slice() {
            b"PRINT" => RawString,
            b"PRINTV" => Expression,
            b"PRINTFORM" => RawStringForm,
            _ => return None,
        })
    }
}

enum EraCommandArgFmt {
    ExpressionS,
    Expression,
    RawString,
    RawStringForm,
}

// pub struct EraPrintCommandFlags {
//     is_form: bool,
// }

#[derive(Debug, Clone, Copy)]
pub struct EraParserSlimVarTypeInfo {
    pub is_string: bool,
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
        CmpL | CmpLEq | CmpG | CmpGEq => (17, 18),
        BitShiftL | BitShiftR => (19, 20),
        Plus | Minus => (21, 22),
        Multiply | Divide | Percentage => (23, 24),
        Colon => (27, 28), // Array subscripting
        At => (29, 30),    // Special (namespaced) variable access
        _ => return None,
    })
}
fn postfix_binding_power(op: EraTokenKind) -> Option<(u8, ())> {
    use EraTokenKind::*;
    Some(match op {
        Increment | Decrement => (27, ()),
        LBracket => (27, ()),
        _ => return None,
    })
}

trait BinarySliceExt {
    fn strip_prefix_inplace(&mut self, prefix: &[u8]) -> bool;
}
impl BinarySliceExt for &[u8] {
    fn strip_prefix_inplace(&mut self, prefix: &[u8]) -> bool {
        match self.strip_prefix(prefix) {
            Some(x) => {
                *self = x;
                true
            }
            None => false,
        }
    }
}
