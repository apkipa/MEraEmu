use std::collections::{HashMap, VecDeque};
use std::rc::Rc;

use indoc::concatdoc;

use crate::bytecode::{
    EraBeginSystemProcedureKind, FlatValue, IntValue, PadStringFlags, StrValue, Value, ValueKind,
};
use crate::{routine, util::*};

use crate::vm::EraVarPool;
use crate::{
    bytecode::{PrintExtendedFlags, SourcePosInfo},
    lexer::{EraLexerMode, EraToken, EraTokenKind, EraTokenLite},
};

/// Guards statement reading unless encountered `@`, which indicates start of a new function
macro_rules! stmt_at_guard {
    ($self:expr) => {{
        match $self.statement() {
            Some(stmt) => Some(stmt),
            None => {
                $self.skip_whitespace();
                let token = $self.peek_token(EraLexerMode::Normal);
                if matches!(token.kind, EraTokenKind::At | EraTokenKind::Eof) {
                    None
                } else {
                    Some(EraStmt::Invalid(token.src_info))
                }
            }
        }
    }};
}

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
    DefineDecl(EraDefineDecl),
    EventKindDecl(EraEventKindDecl),
}
impl EraSharpDecl {
    pub fn source_pos_info(&self) -> SourcePosInfo {
        match self {
            Self::FunctionDecl(x) => x.src_info,
            Self::LocalSizeDecl(x) => x.src_info,
            Self::LocalSSizeDecl(x) => x.src_info,
            Self::VarDecl(x) => x.src_info,
            Self::DefineDecl(x) => x.src_info,
            Self::EventKindDecl(x) => x.src_info,
        }
    }
}

pub struct EraSharpFunctionDecl {
    pub returns_string: bool,
    pub src_info: SourcePosInfo,
}

pub struct EraDefineDecl {
    pub src_info: SourcePosInfo,
}

pub struct EraEventKindDecl {
    pub kind: EraEventFunKind,
    pub src_info: SourcePosInfo,
}

#[derive(Debug, Clone)]
pub struct EraVarExpr {
    pub name: arcstr::ArcStr,
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
    FunCall(Box<EraExpr>, Vec<Option<EraExpr>>),
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
    pub fn new_str(val: arcstr::ArcStr, src_info: SourcePosInfo) -> Self {
        EraExpr::Term(EraTermExpr::Literal(EraLiteral::String(val, src_info)))
    }
    pub fn new_var(name: arcstr::ArcStr, idxs: Vec<EraExpr>, src_info: SourcePosInfo) -> Self {
        EraExpr::Term(EraTermExpr::Var(EraVarExpr {
            name,
            idxs,
            src_info,
        }))
    }
    fn is_var(&self) -> bool {
        match self {
            Self::Term(EraTermExpr::Var(_)) => true,
            _ => false,
        }
    }
}

impl EraExpr {
    pub fn unwrap_int_constant(lit: EraLiteral) -> Result<i64, EraParseErrorInfo> {
        match lit {
            EraLiteral::Integer(x, _) => Ok(x),
            EraLiteral::String(_, src_info) => Err(EraParseErrorInfo {
                src_info,
                is_error: true,
                msg: "expected integer here".into(),
            }),
        }
    }
    pub fn unwrap_str_constant(lit: EraLiteral) -> Result<arcstr::ArcStr, EraParseErrorInfo> {
        match lit {
            EraLiteral::String(x, _) => Ok(x),
            EraLiteral::Integer(_, src_info) => Err(EraParseErrorInfo {
                src_info,
                is_error: true,
                msg: "expected string here".into(),
            }),
        }
    }
    // TODO: Finish EraExpr::try_evaluate_constant
    pub fn try_evaluate_constant(self, vars: &EraVarPool) -> Result<EraLiteral, EraParseErrorInfo> {
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
        fn make_overflow_err(src_info: SourcePosInfo) -> EraParseErrorInfo {
            make_err(
                src_info,
                true,
                "overflow during constant expression evaluation",
            )
        }
        fn make_invalid_arith_err(src_info: SourcePosInfo) -> EraParseErrorInfo {
            make_err(
                src_info,
                true,
                "invalid arithmetic during constant expression evaluation",
            )
        }

        enum ExprStackValue {
            Expr(EraExpr),
            Op(EraTokenLite),
        }

        // Use a dedicated stack to prevent stack overflow
        let mut expr_stack = Vec::new();
        let mut val_stack = Vec::new();

        expr_stack.push(ExprStackValue::Expr(self));
        while let Some(value) = expr_stack.pop() {
            use EraLiteral as EL;
            let expr = match value {
                ExprStackValue::Expr(x) => x,
                ExprStackValue::Op(op) => {
                    match op.kind {
                        EraTokenKind::Plus => {
                            let b = val_stack.pop().unwrap();
                            let a = val_stack.pop().unwrap();
                            match (a, b) {
                                (EL::Integer(a, a_si), EL::Integer(b, b_si)) => {
                                    let x = match a.checked_add(b) {
                                        Some(x) => x,
                                        None => {
                                            return Err(make_overflow_err(op.src_info));
                                        }
                                    };
                                    val_stack.push(EL::Integer(x, a_si));
                                }
                                (EL::String(a, a_si), EL::String(b, b_si)) => {
                                    let x = arcstr::format!("{a}{b}");
                                    val_stack.push(EL::String(x, a_si));
                                }
                                _ => {
                                    return Err(make_invalid_arith_err(op.src_info));
                                }
                            }
                        }
                        EraTokenKind::Minus => {
                            let b = val_stack.pop().unwrap();
                            let a = val_stack.pop().unwrap();
                            match (a, b) {
                                (EL::Integer(a, a_si), EL::Integer(b, b_si)) => {
                                    let x = match a.checked_sub(b) {
                                        Some(x) => x,
                                        None => {
                                            return Err(make_overflow_err(op.src_info));
                                        }
                                    };
                                    val_stack.push(EL::Integer(x, a_si));
                                }
                                _ => {
                                    return Err(make_invalid_arith_err(op.src_info));
                                }
                            }
                        }
                        EraTokenKind::Multiply => {
                            let b = val_stack.pop().unwrap();
                            let a = val_stack.pop().unwrap();
                            match (a, b) {
                                (EL::Integer(a, a_si), EL::Integer(b, b_si)) => {
                                    let x = match a.checked_mul(b) {
                                        Some(x) => x,
                                        None => {
                                            return Err(make_overflow_err(op.src_info));
                                        }
                                    };
                                    val_stack.push(EL::Integer(x, a_si));
                                }
                                _ => {
                                    return Err(make_invalid_arith_err(op.src_info));
                                }
                            }
                        }
                        EraTokenKind::Divide => {
                            let b = val_stack.pop().unwrap();
                            let a = val_stack.pop().unwrap();
                            match (a, b) {
                                (EL::Integer(a, a_si), EL::Integer(b, b_si)) => {
                                    let x = match a.checked_div(b) {
                                        Some(x) => x,
                                        None => {
                                            return Err(make_overflow_err(op.src_info));
                                        }
                                    };
                                    val_stack.push(EL::Integer(x, a_si));
                                }
                                _ => {
                                    return Err(make_invalid_arith_err(op.src_info));
                                }
                            }
                        }
                        EraTokenKind::BitShiftL => {
                            let b = val_stack.pop().unwrap();
                            let a = val_stack.pop().unwrap();
                            match (a, b) {
                                (EL::Integer(a, a_si), EL::Integer(b, b_si)) => {
                                    let x = match a.checked_shl(b.try_into().unwrap_or(u32::MAX)) {
                                        Some(x) => x,
                                        None => {
                                            return Err(make_overflow_err(op.src_info));
                                        }
                                    };
                                    val_stack.push(EL::Integer(x, a_si));
                                }
                                _ => {
                                    return Err(make_invalid_arith_err(op.src_info));
                                }
                            }
                        }
                        EraTokenKind::BitShiftR => {
                            let b = val_stack.pop().unwrap();
                            let a = val_stack.pop().unwrap();
                            match (a, b) {
                                (EL::Integer(a, a_si), EL::Integer(b, b_si)) => {
                                    let x = match a.checked_shr(b.try_into().unwrap_or(u32::MAX)) {
                                        Some(x) => x,
                                        None => {
                                            return Err(make_overflow_err(op.src_info));
                                        }
                                    };
                                    val_stack.push(EL::Integer(x, a_si));
                                }
                                _ => {
                                    return Err(make_invalid_arith_err(op.src_info));
                                }
                            }
                        }
                        _ => return Err(make_err(op.src_info, true, "not a constant expression")),
                    }
                    continue;
                }
            };
            // Expand expression
            let literal = match expr {
                EraExpr::Term(x) => match x {
                    EraTermExpr::Var(x) => match vars.get_var_info_by_name(&x.name) {
                        Some(info) if info.is_const => {
                            let idxs = x
                                .idxs
                                .into_iter()
                                .map(|x| match x.try_evaluate_constant(vars)? {
                                    EraLiteral::Integer(x, _) => Ok(x as u32),
                                    EraLiteral::String(_, src_info) => {
                                        Err(make_err(src_info, true, "expected integer here"))
                                    }
                                })
                                .collect::<Result<Vec<_>, _>>()?;
                            match info.val.clone().into_unpacked() {
                                FlatValue::ArrInt(v) => EraLiteral::Integer(
                                    v.borrow()
                                        .get(&idxs)
                                        .ok_or_else(|| {
                                            make_err(x.src_info, true, "not a constant expression")
                                        })?
                                        .val,
                                    src_info,
                                ),
                                FlatValue::ArrStr(v) => EraLiteral::String(
                                    v.borrow()
                                        .get(&idxs)
                                        .ok_or_else(|| {
                                            make_err(x.src_info, true, "not a constant expression")
                                        })?
                                        .val
                                        .clone(),
                                    src_info,
                                ),
                                _ => todo!(),
                            }
                        }
                        _ => return Err(make_err(x.src_info, true, "not a constant expression")),
                    },
                    EraTermExpr::StrForm(x) => {
                        let mut result = String::new();
                        for part in x.parts {
                            match part {
                                EraStrFormExprPart::Literal(x, _) => result.push_str(&x),
                                // FIXME: Constant string interpolation formatting
                                EraStrFormExprPart::Expression(x) => {
                                    match x.expr.try_evaluate_constant(vars)? {
                                        EraLiteral::Integer(x, _) => {
                                            result.push_str(&x.to_string())
                                        }
                                        EraLiteral::String(x, _) => result.push_str(&x),
                                    }
                                }
                            }
                        }
                        EraLiteral::String(result.into(), x.src_info)
                    }
                    EraTermExpr::Literal(x) => match x {
                        EraLiteral::Integer(x, _) => EraLiteral::Integer(x, src_info),
                        EraLiteral::String(x, _) => EraLiteral::String(x, src_info),
                    },
                },
                EraExpr::Grouping(_, x, _) => x.try_evaluate_constant(vars)?,
                EraExpr::PreUnary(op, x) => match (op.kind, x.try_evaluate_constant(vars)?) {
                    (EraTokenKind::Plus, EraLiteral::Integer(x, _)) => {
                        EraLiteral::Integer(x, src_info)
                    }
                    (EraTokenKind::Minus, EraLiteral::Integer(x, cur_si)) => {
                        let x = match 0i64.checked_sub(x) {
                            Some(x) => x,
                            None => {
                                return Err(make_overflow_err(cur_si));
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
                EraExpr::PostUnary(x, op) => match (x.try_evaluate_constant(vars)?, op.kind) {
                    _ => {
                        return Err(make_err(
                            op.src_info,
                            true,
                            "invalid arithmetic during constant expression evaluation",
                        ));
                    }
                },
                EraExpr::FunCall(fun, args) => {
                    let fun_si = fun.source_pos_info();
                    let EraExpr::Term(EraTermExpr::Var(fun)) = *fun else {
                        return Err(make_err(
                            fun_si,
                            true,
                            "invalid function call during constant expression evaluation",
                        ));
                    };
                    let upper_fun_name = fun.name.to_ascii_uppercase();
                    // HACK: Whitelist specific functions
                    match upper_fun_name.as_str() {
                        "UNICODE" => {
                            if args.len() != 1 {
                                return Err(make_err(
                                    fun_si,
                                    true,
                                    "invalid function call during constant expression evaluation",
                                ));
                            }
                            let arg = args.into_iter().next().unwrap();
                            let arg = if let Some(arg) = arg {
                                Self::unwrap_int_constant(arg.try_evaluate_constant(vars)?)?
                            } else {
                                0
                            };
                            let mut result = String::new();
                            result.push(
                                char::from_u32(arg as _).unwrap_or(char::REPLACEMENT_CHARACTER),
                            );
                            EraLiteral::String(result.into(), fun_si)
                        }
                        "VARSIZE" => {
                            if args.len() != 1 {
                                // TODO: constexpr VARSIZE
                                return Err(make_err(
                                    fun_si,
                                    true,
                                    "invalid function call during constant expression evaluation",
                                ));
                            }
                            let arg = args.into_iter().next().unwrap();
                            let arg = if let Some(arg) = arg {
                                Self::unwrap_str_constant(arg.try_evaluate_constant(vars)?)?
                            } else {
                                arcstr::ArcStr::new()
                            };
                            let Some(value) = vars.get_var(&arg) else {
                                return Err(make_err(
                                    fun_si,
                                    true,
                                    format!("variable `{arg}` does not exist"),
                                ));
                            };
                            let varsize = match value.clone().into_unpacked() {
                                FlatValue::ArrInt(x) => *x.borrow().dims.first().unwrap(),
                                FlatValue::ArrStr(x) => *x.borrow().dims.first().unwrap(),
                                _ => unreachable!(),
                            };
                            EraLiteral::Integer(varsize as _, fun_si)
                        }
                        _ => {
                            return Err(make_err(
                                fun_si,
                                true,
                                "invalid function call during constant expression evaluation",
                            ))
                        }
                    }
                }
                // EraExpr::Binary(x1, op, x2) => match (
                //     x1.try_evaluate_constant(vars)?,
                //     op.kind,
                //     x2.try_evaluate_constant(vars)?,
                // ) {
                //     (
                //         EraLiteral::Integer(x1, _),
                //         EraTokenKind::Plus,
                //         EraLiteral::Integer(x2, _),
                //     ) => {
                //         let x = match x1.checked_add(x2) {
                //             Some(x) => x,
                //             None => {
                //                 return Err(make_overflow_err(op.src_info));
                //             }
                //         };
                //         EraLiteral::Integer(x, src_info)
                //     }
                //     (EraLiteral::String(x1, _), EraTokenKind::Plus, EraLiteral::String(x2, _)) => {
                //         let x = x1 + &x2;
                //         EraLiteral::String(x, src_info)
                //     }
                //     (
                //         EraLiteral::Integer(x1, _),
                //         EraTokenKind::Minus,
                //         EraLiteral::Integer(x2, _),
                //     ) => {
                //         let x = match x1.checked_sub(x2) {
                //             Some(x) => x,
                //             None => {
                //                 return Err(make_overflow_err(op.src_info));
                //             }
                //         };
                //         EraLiteral::Integer(x, src_info)
                //     }
                //     (
                //         EraLiteral::Integer(x1, _),
                //         EraTokenKind::Multiply,
                //         EraLiteral::Integer(x2, _),
                //     ) => {
                //         let x = match x1.checked_mul(x2) {
                //             Some(x) => x,
                //             None => {
                //                 return Err(make_overflow_err(op.src_info));
                //             }
                //         };
                //         EraLiteral::Integer(x, src_info)
                //     }
                //     (
                //         EraLiteral::Integer(x1, _),
                //         EraTokenKind::Divide,
                //         EraLiteral::Integer(x2, _),
                //     ) => {
                //         let x = match x1.checked_div(x2) {
                //             Some(x) => x,
                //             None => {
                //                 return Err(make_overflow_err(op.src_info));
                //             }
                //         };
                //         EraLiteral::Integer(x, src_info)
                //     }
                //     (
                //         EraLiteral::Integer(x1, _),
                //         EraTokenKind::BitShiftL,
                //         EraLiteral::Integer(x2, _),
                //     ) => {
                //         // HACK: Overflow error report for x2 by replacing Err with u32::MAX
                //         let x = match x1.checked_shl(x2.try_into().unwrap_or(u32::MAX)) {
                //             Some(x) => x,
                //             None => {
                //                 return Err(make_overflow_err(op.src_info));
                //             }
                //         };
                //         EraLiteral::Integer(x, src_info)
                //     }
                //     (
                //         EraLiteral::Integer(x1, _),
                //         EraTokenKind::BitShiftR,
                //         EraLiteral::Integer(x2, _),
                //     ) => {
                //         let x = match x1.checked_shr(x2.try_into().unwrap_or(u32::MAX)) {
                //             Some(x) => x,
                //             None => {
                //                 return Err(make_overflow_err(op.src_info));
                //             }
                //         };
                //         EraLiteral::Integer(x, src_info)
                //     }
                //     _ => {
                //         return Err(make_err(
                //             op.src_info,
                //             true,
                //             "invalid arithmetic during constant expression evaluation",
                //         ));
                //     }
                // },
                EraExpr::Binary(x1, op, x2) => {
                    expr_stack.push(ExprStackValue::Op(op));
                    expr_stack.push(ExprStackValue::Expr(*x2));
                    expr_stack.push(ExprStackValue::Expr(*x1));
                    continue;
                }
                EraExpr::Ternary(x1, _, x2, _, x3) => {
                    let x1 = x1.try_evaluate_constant(vars)?;
                    let x2 = x2.try_evaluate_constant(vars)?;
                    let x3 = x3.try_evaluate_constant(vars)?;
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
            };
            val_stack.push(literal);
        }

        let [result] = TryInto::<[_; 1]>::try_into(val_stack).expect("stack value count is not 1");
        Ok(result)
    }
}

#[derive(Debug, Clone)]
pub enum EraLiteral {
    Integer(i64, SourcePosInfo),
    String(arcstr::ArcStr, SourcePosInfo),
}
impl EraLiteral {
    pub fn source_pos_info(&self) -> SourcePosInfo {
        match self {
            Self::Integer(_, i) => *i,
            Self::String(_, i) => *i,
        }
    }
}

#[derive(Debug, Clone)]
pub struct EraVarDecl {
    pub name: String,
    pub dims: smallvec::SmallVec<[u32; 3]>,
    pub inits: Vec<EraExpr>,
    pub is_string: bool,
    pub is_ref: bool,
    pub is_const: bool,
    pub is_global: bool,
    pub is_dynamic: bool,
    pub is_savedata: bool,
    pub is_charadata: bool,
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

#[derive(Debug, Clone, Copy)]
pub enum EraEventFunKind {
    Only,
    Pri,
    Normal,
    Later,
}

pub struct EraFunDecl {
    pub name: String,
    // Used to describe how arguments are bound to parameters (pseudo assignment)
    pub params: Vec<EraExpr>,
    pub kind: EraFunKind,
    pub event_kind: Option<EraEventFunKind>,
    //pub vars: Vec<EraVarDecl>,
    pub decls: Vec<EraSharpDecl>,
    pub body: Vec<EraStmt>,
    pub src_info: SourcePosInfo,
}

#[derive(Debug, Clone)]
pub struct EraPrintStmt {
    pub vals: Vec<EraExpr>,
    pub flags: PrintExtendedFlags,
    pub src_info: SourcePosInfo,
}

#[derive(Debug, Clone)]
pub struct EraPrintDataStmt {
    pub dest: Option<EraVarExpr>,
    pub data: Vec<Vec<EraExpr>>,
    pub flags: PrintExtendedFlags,
    pub src_info: SourcePosInfo,
}

#[derive(Debug, Clone)]
pub struct EraIfStmt {
    pub cond: EraExpr,
    pub body: Vec<EraStmt>,
    pub else_body: Vec<EraStmt>,
    pub src_info: SourcePosInfo,
}

#[derive(Debug, Clone)]
pub struct EraWaitStmt {
    pub any_key: bool,
    pub is_force: bool,
    pub src_info: SourcePosInfo,
}

#[derive(Debug, Clone)]
pub enum EraSelectCaseStmtCondition {
    Single(EraExpr),
    Range(EraExpr, EraExpr),
    Condition(EraTokenLite, EraExpr),
}
#[derive(Debug, Clone)]
pub struct EraSelectCaseStmt {
    pub cond: EraExpr,
    pub cases: Vec<(Vec<EraSelectCaseStmtCondition>, Vec<EraStmt>)>,
    pub case_else: Vec<EraStmt>,
    pub src_info: SourcePosInfo,
}

#[derive(Debug, Clone)]
pub struct EraQuitStmt {
    pub src_info: SourcePosInfo,
}
#[derive(Debug, Clone)]
pub struct EraWhileStmt {
    pub cond: EraExpr,
    pub body: Vec<EraStmt>,
    pub src_info: SourcePosInfo,
}

#[derive(Debug, Clone)]
pub struct EraCallStmt {
    pub func: EraExpr,
    pub args: Vec<Option<EraExpr>>,
    pub src_info: SourcePosInfo,
}
#[derive(Debug, Clone)]
pub struct EraTryCCallStmt {
    pub func: EraExpr,
    pub args: Vec<Option<EraExpr>>,
    pub then_body: Vec<EraStmt>,
    pub catch_body: Vec<EraStmt>,
    pub src_info: SourcePosInfo,
}

#[derive(Debug, Clone)]
pub struct EraReturnStmt {
    pub vals: Vec<EraExpr>,
    pub src_info: SourcePosInfo,
}

#[derive(Debug, Clone)]
pub struct EraContinueStmt {
    pub src_info: SourcePosInfo,
}

#[derive(Debug, Clone)]
pub struct EraBreakStmt {
    pub src_info: SourcePosInfo,
}

#[derive(Debug, Clone)]
pub struct EraThrowStmt {
    pub val: EraExpr,
    pub src_info: SourcePosInfo,
}

#[derive(Debug, Clone)]
pub struct EraRepeatStmt {
    pub loop_cnt: EraExpr,
    pub body: Vec<EraStmt>,
    pub src_info: SourcePosInfo,
}

#[derive(Debug, Clone)]
pub struct EraLabelStmt {
    pub name: String,
    pub src_info: SourcePosInfo,
}

#[derive(Debug, Clone)]
pub struct EraGotoStmt {
    pub target: String,
    pub src_info: SourcePosInfo,
}

#[derive(Debug, Clone)]
pub struct EraForStmt {
    pub var: EraVarExpr,
    pub start: EraExpr,
    pub end: EraExpr,
    pub step: EraExpr,
    pub body: Vec<EraStmt>,
    pub src_info: SourcePosInfo,
}

#[derive(Debug, Clone)]
pub struct EraDoLoopStmt {
    pub body: Vec<EraStmt>,
    pub cond: EraExpr,
    pub src_info: SourcePosInfo,
}

// pub struct EraGCreateStmt {
//     pub gid: EraExpr,
//     pub width: EraExpr,
//     pub height: EraExpr,
//     pub src_info: SourcePosInfo,
// }

// pub struct EraGDisposeStmt {
//     pub gid: EraExpr,
//     pub src_info: SourcePosInfo,
// }

// pub struct EraGDrawSpriteStmt {
//     pub gid: EraExpr,
//     pub sprite_name: EraExpr,
//     // TODO: EraGDrawSpriteStmt overloads
//     pub src_info: SourcePosInfo,
// }

#[derive(Debug, Clone)]
pub struct EraSplitStmt {
    pub input: EraExpr,
    pub separator: EraExpr,
    pub dest: EraVarExpr,
    pub dest_count: EraVarExpr,
    pub src_info: SourcePosInfo,
}

#[derive(Debug, Clone)]
pub struct EraResultCmdCallStmt {
    pub name: String,
    pub args: Vec<Option<EraExpr>>,
    pub src_info: SourcePosInfo,
}

#[derive(Debug, Clone)]
pub struct EraTimesStmt {
    pub target: EraVarExpr,
    pub factor: f64,
    pub src_info: SourcePosInfo,
}

#[derive(Debug, Clone)]
pub struct EraSetBitStmt {
    pub target: EraVarExpr,
    pub bits: Vec<EraExpr>,
    pub src_info: SourcePosInfo,
}
#[derive(Debug, Clone)]
pub struct EraClearBitStmt {
    pub target: EraVarExpr,
    pub bits: Vec<EraExpr>,
    pub src_info: SourcePosInfo,
}
#[derive(Debug, Clone)]
pub struct EraInvertBitStmt {
    pub target: EraVarExpr,
    pub bits: Vec<EraExpr>,
    pub src_info: SourcePosInfo,
}

#[derive(Debug, Clone)]
pub struct EraSetColorStmt {
    pub color: EraExpr,
    pub src_info: SourcePosInfo,
}
#[derive(Debug, Clone)]
pub struct EraResetColorStmt {
    pub src_info: SourcePosInfo,
}

#[derive(Debug, Clone)]
pub struct EraVarSetStmt {
    pub target: EraVarExpr,
    pub value: Option<EraExpr>,
    pub start_index: Option<EraExpr>,
    pub end_index: Option<EraExpr>,
    pub src_info: SourcePosInfo,
}

#[derive(Debug, Clone)]
pub struct EraCVarSetStmt {
    pub target: EraVarExpr,
    pub index: EraExpr,
    pub value: Option<EraExpr>,
    pub start_id: Option<EraExpr>,
    pub end_id: Option<EraExpr>,
    pub src_info: SourcePosInfo,
}

#[derive(Debug, Clone)]
pub struct EraVarSizeStmt {
    pub var: EraVarExpr,
    pub src_info: SourcePosInfo,
}

#[derive(Debug, Clone)]
pub struct EraSwapStmt {
    pub v1: EraVarExpr,
    pub v2: EraVarExpr,
    pub src_info: SourcePosInfo,
}

#[derive(Debug, Clone)]
pub struct EraHtmlPrintStmt {
    pub expr: EraExpr,
    pub src_info: SourcePosInfo,
}

#[derive(Debug, Clone)]
pub struct EraPrintButtonStmt {
    pub content: EraExpr,
    pub value: EraExpr,
    pub flags: PrintExtendedFlags,
    pub src_info: SourcePosInfo,
}

#[derive(Debug, Clone)]
pub struct EraArrayRemoveStmt {
    pub target: EraVarExpr,
    pub start_index: EraExpr,
    pub count: EraExpr,
    pub src_info: SourcePosInfo,
}

#[derive(Debug, Clone)]
pub struct EraArraySortStmt {
    pub target: EraVarExpr,
    pub is_ascending: bool,
    pub start_index: Option<EraExpr>,
    pub count: Option<EraExpr>,
    pub src_info: SourcePosInfo,
}

#[derive(Debug, Clone)]
pub struct EraArrayMSortStmt {
    pub primary: EraVarExpr,
    pub subs: Vec<EraVarExpr>,
    pub src_info: SourcePosInfo,
}

#[derive(Debug, Clone)]
pub struct EraArrayCopyStmt {
    pub from_name: EraExpr,
    pub to_name: EraExpr,
    pub src_info: SourcePosInfo,
}

#[derive(Debug, Clone)]
pub struct EraArrayShiftStmt {
    pub target: EraVarExpr,
    pub shift_count: EraExpr,
    pub value: EraExpr,
    pub start_index: EraExpr,
    pub target_count: EraExpr,
    pub src_info: SourcePosInfo,
}

#[derive(Debug, Clone)]
pub struct EraInputStmt {
    pub default_value: Option<EraExpr>,
    pub can_click: EraExpr,
    pub allow_skip: EraExpr,
    pub src_info: SourcePosInfo,
}

#[derive(Debug, Clone)]
pub struct EraTInputStmt {
    pub time_limit: EraExpr,
    pub default_value: EraExpr,
    pub show_prompt: EraExpr,
    pub expiry_msg: EraExpr,
    pub can_click: EraExpr,
    pub src_info: SourcePosInfo,
}

#[derive(Debug, Clone)]
pub struct EraOneInputStmt {
    pub default_value: Option<EraExpr>,
    pub src_info: SourcePosInfo,
}

#[derive(Debug, Clone)]
pub struct EraTOneInputStmt {
    pub time_limit: EraExpr,
    pub default_value: EraExpr,
    pub show_prompt: EraExpr,
    pub expiry_msg: EraExpr,
    pub can_click: EraExpr,
    pub src_info: SourcePosInfo,
}

#[derive(Debug, Clone)]
pub struct EraReuseLastLineStmt {
    pub content: EraExpr,
    pub src_info: SourcePosInfo,
}

#[derive(Debug, Clone)]
pub struct EraClearLineStmt {
    pub count: EraExpr,
    pub src_info: SourcePosInfo,
}

#[derive(Debug, Clone)]
pub struct EraCustomDrawLineStmt {
    pub content: EraExpr,
    pub src_info: SourcePosInfo,
}

#[derive(Debug, Clone)]
pub struct EraTWaitStmt {
    pub duration: EraExpr,
    pub force_wait: EraExpr,
    pub src_info: SourcePosInfo,
}

#[derive(Debug, Clone)]
pub struct EraFontStyleStmt {
    pub style: EraExpr,
    pub src_info: SourcePosInfo,
}

#[derive(Debug, Clone)]
pub struct EraSetFontStmt {
    pub font_name: EraExpr,
    pub src_info: SourcePosInfo,
}

#[derive(Debug, Clone)]
pub struct EraStrDataStmt {
    pub target: EraVarExpr,
    pub data: Vec<Vec<EraExpr>>,
    pub src_info: SourcePosInfo,
}

#[derive(Debug, Clone)]
pub struct EraPutFormStmt {
    pub cont: EraExpr,
    pub src_info: SourcePosInfo,
}

#[derive(Debug, Clone)]
pub struct EraSkipDispStmt {
    pub is_skip: EraExpr,
    pub src_info: SourcePosInfo,
}

#[derive(Debug, Clone)]
pub struct EraBeginStmt {
    pub proc: EraBeginSystemProcedureKind,
    pub src_info: SourcePosInfo,
}

#[derive(Debug, Clone)]
pub struct EraDoTrainStmt {
    pub number: EraExpr,
    pub src_info: SourcePosInfo,
}

#[derive(Debug, Clone)]
pub struct EraRedrawStmt {
    pub arg: EraExpr,
    pub src_info: SourcePosInfo,
}

#[derive(Debug, Clone)]
pub struct EraStrLenStmt {
    pub cont: EraExpr,
    pub src_info: SourcePosInfo,
}

#[repr(u8)]
#[derive(num_derive::FromPrimitive, num_derive::ToPrimitive, Debug, Clone, Copy)]
pub enum EraAlignmentKind {
    Left,
    Center,
    Right,
}

#[derive(Debug, Clone)]
pub struct EraAlignmentStmt {
    pub alignment: EraAlignmentKind,
    pub src_info: SourcePosInfo,
}

#[derive(Debug, Clone)]
pub struct EraToolTipSetDelayStmt {
    pub duration: EraExpr,
    pub src_info: SourcePosInfo,
}

#[derive(Debug, Clone)]
pub struct EraToolTipSetDurationStmt {
    pub duration: EraExpr,
    pub src_info: SourcePosInfo,
}

#[derive(Debug, Clone)]
pub struct EraRandomizeStmt {
    pub seed: EraExpr,
    pub src_info: SourcePosInfo,
}

#[derive(Debug, Clone)]
pub struct EraBarStmt {
    pub value: EraExpr,
    pub max_value: EraExpr,
    pub length: EraExpr,
    pub new_line: bool,
    pub src_info: SourcePosInfo,
}

#[derive(Debug, Clone)]
pub struct EraAddCharaStmt {
    pub charas: Vec<EraExpr>,
    pub src_info: SourcePosInfo,
}

#[derive(Debug, Clone)]
pub struct EraPickUpCharaStmt {
    pub charas: Vec<EraExpr>,
    pub src_info: SourcePosInfo,
}

#[derive(Debug, Clone)]
pub struct EraDelCharaStmt {
    pub charas: Vec<EraExpr>,
    pub src_info: SourcePosInfo,
}

#[derive(Debug, Clone)]
pub struct EraSwapCharaStmt {
    pub chara1: EraExpr,
    pub chara2: EraExpr,
    pub src_info: SourcePosInfo,
}

#[derive(Debug, Clone)]
pub struct EraAddCopyCharaStmt {
    pub chara: EraExpr,
    pub src_info: SourcePosInfo,
}

#[derive(Debug, Clone)]
pub struct EraResetStainStmt {
    pub chara: EraExpr,
    pub src_info: SourcePosInfo,
}

#[derive(Debug, Clone)]
pub struct EraSaveCharaStmt {
    pub filename: EraExpr,
    pub memo: EraExpr,
    pub charas: Vec<EraExpr>,
    pub src_info: SourcePosInfo,
}

#[derive(Debug, Clone)]
pub struct EraLoadCharaStmt {
    pub filename: EraExpr,
    pub src_info: SourcePosInfo,
}

#[derive(Debug, Clone)]
pub struct EraSetAnimeTimerStmt {
    pub duration: EraExpr,
    pub src_info: SourcePosInfo,
}

#[derive(Debug, Clone)]
pub struct EraHtmlTagSplitStmt {
    pub html: EraExpr,
    pub var_tags: EraVarExpr,
    pub var_count: EraVarExpr,
    pub src_info: SourcePosInfo,
}

#[derive(Debug, Clone)]
pub struct EraPowerStmt {
    pub target: EraVarExpr,
    pub base: EraExpr,
    pub exponent: EraExpr,
    pub src_info: SourcePosInfo,
}

#[derive(Debug, Clone)]
pub struct EraSaveDataStmt {
    pub save_id: EraExpr,
    pub save_info: EraExpr,
    pub src_info: SourcePosInfo,
}

#[derive(Debug, Clone)]
pub struct EraRowAssignStmt {
    pub dest: EraVarExpr,
    pub srcs: Vec<EraExpr>,
    pub src_info: SourcePosInfo,
}

#[derive(Debug, Clone)]
pub struct EraCmdEmptyStmt {
    pub src_info: SourcePosInfo,
}

#[derive(Debug, Clone)]
pub enum EraStmt {
    Expr(EraExpr),
    Command(EraCommandStmt),
    Label(EraLabelStmt),
    RowAssign(EraRowAssignStmt),
    Invalid(SourcePosInfo),
}
impl EraStmt {
    pub fn source_pos_info(&self) -> SourcePosInfo {
        use EraStmt::*;
        match self {
            Expr(x) => x.source_pos_info(),
            Command(x) => x.source_pos_info(),
            Label(x) => x.src_info,
            RowAssign(x) => x.src_info,
            Invalid(src_info) => *src_info,
        }
    }
}

#[derive(Debug, Clone)]
pub enum EraCommandStmt {
    Nop(EraCmdEmptyStmt),
    DebugPrint(EraPrintStmt),
    Print(EraPrintStmt),
    PrintData(EraPrintDataStmt),
    Wait(EraWaitStmt),
    If(EraIfStmt),
    Quit(EraQuitStmt),
    SelectCase(EraSelectCaseStmt),
    While(EraWhileStmt),
    Call(EraCallStmt),
    TryCall(EraCallStmt),
    TryCCall(EraTryCCallStmt),
    Jump(EraCallStmt),
    TryJump(EraCallStmt),
    TryCJump(EraTryCCallStmt),
    Return(EraReturnStmt),
    Continue(EraContinueStmt),
    Break(EraBreakStmt),
    Throw(EraThrowStmt),
    Repeat(EraRepeatStmt),
    Goto(EraGotoStmt),
    For(EraForStmt),
    DoLoop(EraDoLoopStmt),
    // GCreate(EraGCreateStmt),
    // GDispose(EraGDisposeStmt),
    // GDrawSprite(EraGDrawSpriteStmt),
    Split(EraSplitStmt),
    ResultCmdCall(EraResultCmdCallStmt),
    Times(EraTimesStmt),
    SetBit(EraSetBitStmt),
    ClearBit(EraClearBitStmt),
    InvertBit(EraInvertBitStmt),
    SetColor(EraSetColorStmt),
    ResetColor(EraResetColorStmt),
    SetBgColor(EraSetColorStmt),
    ResetBgColor(EraResetColorStmt),
    VarSet(EraVarSetStmt),
    CVarSet(EraCVarSetStmt),
    VarSize(EraVarSizeStmt),
    Swap(EraSwapStmt),
    HtmlPrint(EraHtmlPrintStmt),
    PrintButton(EraPrintButtonStmt),
    ArrayRemove(EraArrayRemoveStmt),
    ArraySort(EraArraySortStmt),
    ArrayMSort(EraArrayMSortStmt),
    ArrayCopy(EraArrayCopyStmt),
    ArrayShift(EraArrayShiftStmt),
    Input(EraInputStmt),
    InputS(EraInputStmt),
    TInput(EraTInputStmt),
    TInputS(EraTInputStmt),
    OneInput(EraOneInputStmt),
    OneInputS(EraOneInputStmt),
    TOneInput(EraTOneInputStmt),
    TOneInputS(EraTOneInputStmt),
    ReuseLastLine(EraReuseLastLineStmt),
    ClearLine(EraClearLineStmt),
    DrawLine(EraCmdEmptyStmt),
    CustomDrawLine(EraCustomDrawLineStmt),
    TWait(EraTWaitStmt),
    FontStyle(EraFontStyleStmt),
    FontBold(EraCmdEmptyStmt),
    FontItalic(EraCmdEmptyStmt),
    FontRegular(EraCmdEmptyStmt),
    SetFont(EraSetFontStmt),
    StrData(EraStrDataStmt),
    PutForm(EraPutFormStmt),
    SkipDisp(EraSkipDispStmt),
    Begin(EraBeginStmt),
    DoTrain(EraDoTrainStmt),
    Redraw(EraRedrawStmt),
    StrLen(EraStrLenStmt),
    StrLenU(EraStrLenStmt),
    Alignment(EraAlignmentStmt),
    ToolTipSetDelay(EraToolTipSetDelayStmt),
    ToolTipSetDuration(EraToolTipSetDurationStmt),
    Randomize(EraRandomizeStmt),
    DumpRand(EraCmdEmptyStmt),
    InitRand(EraCmdEmptyStmt),
    Bar(EraBarStmt),
    AddChara(EraAddCharaStmt),
    PickUpChara(EraPickUpCharaStmt),
    DelChara(EraDelCharaStmt),
    SwapChara(EraSwapCharaStmt),
    AddCopyChara(EraAddCopyCharaStmt),
    ResetStain(EraResetStainStmt),
    SaveChara(EraSaveCharaStmt),
    LoadChara(EraLoadCharaStmt),
    SetAnimeTimer(EraSetAnimeTimerStmt),
    HtmlTagSplit(EraHtmlTagSplitStmt),
    Power(EraPowerStmt),
    SaveData(EraSaveDataStmt),
    Restart(EraCmdEmptyStmt),
    GetTime(EraCmdEmptyStmt),
    LoadGlobal(EraCmdEmptyStmt),
    SaveGlobal(EraCmdEmptyStmt),
    LoadGame(EraCmdEmptyStmt),
    SaveGame(EraCmdEmptyStmt),
    DebugClear(EraCmdEmptyStmt),
    ResetData(EraCmdEmptyStmt),
}
impl EraCommandStmt {
    pub fn source_pos_info(&self) -> SourcePosInfo {
        use EraCommandStmt::*;
        match self {
            Nop(x) => x.src_info,
            DebugPrint(x) => x.src_info,
            Print(x) => x.src_info,
            PrintData(x) => x.src_info,
            Wait(x) => x.src_info,
            If(x) => x.src_info,
            Quit(x) => x.src_info,
            SelectCase(x) => x.src_info,
            While(x) => x.src_info,
            Call(x) => x.src_info,
            TryCall(x) => x.src_info,
            TryCCall(x) => x.src_info,
            Jump(x) => x.src_info,
            TryJump(x) => x.src_info,
            TryCJump(x) => x.src_info,
            Return(x) => x.src_info,
            Continue(x) => x.src_info,
            Break(x) => x.src_info,
            Throw(x) => x.src_info,
            Repeat(x) => x.src_info,
            Goto(x) => x.src_info,
            For(x) => x.src_info,
            DoLoop(x) => x.src_info,
            // GCreate(x) => x.src_info,
            // GDispose(x) => x.src_info,
            // GDrawSprite(x) => x.src_info,
            Split(x) => x.src_info,
            ResultCmdCall(x) => x.src_info,
            Times(x) => x.src_info,
            SetBit(x) => x.src_info,
            ClearBit(x) => x.src_info,
            InvertBit(x) => x.src_info,
            SetColor(x) => x.src_info,
            ResetColor(x) => x.src_info,
            SetBgColor(x) => x.src_info,
            ResetBgColor(x) => x.src_info,
            VarSet(x) => x.src_info,
            CVarSet(x) => x.src_info,
            VarSize(x) => x.src_info,
            Swap(x) => x.src_info,
            HtmlPrint(x) => x.src_info,
            PrintButton(x) => x.src_info,
            ArrayRemove(x) => x.src_info,
            ArraySort(x) => x.src_info,
            ArrayMSort(x) => x.src_info,
            ArrayCopy(x) => x.src_info,
            ArrayShift(x) => x.src_info,
            Input(x) => x.src_info,
            InputS(x) => x.src_info,
            TInput(x) => x.src_info,
            TInputS(x) => x.src_info,
            OneInput(x) => x.src_info,
            OneInputS(x) => x.src_info,
            TOneInput(x) => x.src_info,
            TOneInputS(x) => x.src_info,
            ReuseLastLine(x) => x.src_info,
            ClearLine(x) => x.src_info,
            DrawLine(x) => x.src_info,
            CustomDrawLine(x) => x.src_info,
            TWait(x) => x.src_info,
            FontStyle(x) => x.src_info,
            FontBold(x) => x.src_info,
            FontItalic(x) => x.src_info,
            FontRegular(x) => x.src_info,
            SetFont(x) => x.src_info,
            StrData(x) => x.src_info,
            PutForm(x) => x.src_info,
            SkipDisp(x) => x.src_info,
            Begin(x) => x.src_info,
            DoTrain(x) => x.src_info,
            Redraw(x) => x.src_info,
            StrLen(x) => x.src_info,
            StrLenU(x) => x.src_info,
            Alignment(x) => x.src_info,
            ToolTipSetDelay(x) => x.src_info,
            ToolTipSetDuration(x) => x.src_info,
            Randomize(x) => x.src_info,
            DumpRand(x) => x.src_info,
            InitRand(x) => x.src_info,
            Bar(x) => x.src_info,
            AddChara(x) => x.src_info,
            PickUpChara(x) => x.src_info,
            DelChara(x) => x.src_info,
            SwapChara(x) => x.src_info,
            AddCopyChara(x) => x.src_info,
            ResetStain(x) => x.src_info,
            SaveChara(x) => x.src_info,
            LoadChara(x) => x.src_info,
            SetAnimeTimer(x) => x.src_info,
            HtmlTagSplit(x) => x.src_info,
            Power(x) => x.src_info,
            SaveData(x) => x.src_info,
            Restart(x) => x.src_info,
            GetTime(x) => x.src_info,
            LoadGlobal(x) => x.src_info,
            SaveGlobal(x) => x.src_info,
            LoadGame(x) => x.src_info,
            SaveGame(x) => x.src_info,
            DebugClear(x) => x.src_info,
            ResetData(x) => x.src_info,
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
        global_vars: &mut EraVarPool,
    ) -> Option<EraRootASTNode> {
        EraParserImpl::new(self, lexer, global_vars).program()
    }
}

struct EraParserImpl<'a, 'b, ErrReportFn, LexErrReportFn> {
    err_report_fn: &'a mut ErrReportFn,
    lexer: &'b mut crate::lexer::EraLexer<'b, LexErrReportFn>,
    global_vars: &'a mut EraVarPool,
    local_vars: HashMap<CaselessString, EraParserSlimVarTypeInfo>,
    curly_brackets_cnt: u32,
    last_ate_newline: bool,
    src_info: SourcePosInfo,
    is_expression_s_mode: bool,
}

impl<'a, 'b, T: FnMut(&EraParseErrorInfo), U: FnMut(&crate::lexer::EraLexErrorInfo)>
    EraParserImpl<'a, 'b, T, U>
{
    fn new(
        parser: &'a mut EraParser<T>,
        lexer: &'b mut crate::lexer::EraLexer<'b, U>,
        global_vars: &'a mut EraVarPool,
    ) -> Self {
        Self {
            err_report_fn: &mut parser.err_report_fn,
            lexer,
            global_vars: global_vars,
            local_vars: HashMap::new(),
            curly_brackets_cnt: 0,
            last_ate_newline: false,
            src_info: SourcePosInfo { line: 1, column: 1 },
            is_expression_s_mode: false,
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
    fn evaluate_constant(&mut self, expr: EraExpr) -> Option<EraLiteral> {
        match expr.try_evaluate_constant(&self.global_vars) {
            Ok(x) => Some(x),
            Err(x) => {
                self.report_err(x.src_info, x.is_error, x.msg);
                None
            }
        }
    }
    fn materialize_var_decl(
        &mut self,
        mut decl: EraVarDecl,
    ) -> Option<(String, crate::bytecode::Value)> {
        if decl.dims.is_empty() {
            let dim = (decl.inits.len() as u32).max(1);
            decl.dims.push(dim);
        }
        // HACK: Handle CHARADATA variable dimensions
        if decl.is_charadata {
            decl.dims.insert(0, crate::engine::MAX_CHARA_COUNT);
        }
        let inits = decl
            .inits
            .into_iter()
            .map(|x| self.evaluate_constant(x))
            .collect::<Option<Vec<_>>>()?;
        Some((
            decl.name,
            if decl.is_string {
                let inits = inits
                    .into_iter()
                    .map(|x| match x {
                        EraLiteral::Integer(_, src_info) => {
                            self.report_err(src_info, true, "expected string value, found integer");
                            None
                        }
                        // TODO: Perform interning
                        EraLiteral::String(val, _) => Some(StrValue { val }),
                    })
                    .collect::<Option<Vec<_>>>()?;
                Value::new_str_arr(decl.dims, inits)
            } else {
                let inits = inits
                    .into_iter()
                    .map(|x| match x {
                        EraLiteral::Integer(val, _) => Some(IntValue { val }),
                        EraLiteral::String(_, src_info) => {
                            self.report_err(src_info, true, "expected integer value, found string");
                            None
                        }
                    })
                    .collect::<Option<Vec<_>>>()?;
                Value::new_int_arr(decl.dims, inits)
            },
        ))
    }
    fn program(&mut self) -> Option<EraRootASTNode> {
        self.skip_whitespace();
        let first_token = self.lexer.peek(EraLexerMode::Normal);
        if matches!(first_token.kind, EraTokenKind::Invalid) {
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
                        let (var_name, var_val) = self.materialize_var_decl(x.clone())?;
                        if self
                            .global_vars
                            .add_var_ex(
                                &var_name,
                                var_val,
                                x.is_const,
                                x.is_charadata,
                                x.is_global,
                                true,
                            )
                            .is_none()
                        {
                            self.report_err(
                                x.src_info,
                                true,
                                format!("redefinition of global variable `{}`", x.name),
                            );
                        }
                    }
                    EraSharpDecl::DefineDecl(_) => (),
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
    #[must_use]
    fn matches_newline(&mut self) -> Option<EraToken<'b>> {
        self.try_consume(EraLexerMode::Normal, EraTokenKind::LineBreak)
    }
    #[must_use]
    fn consume_newline(&mut self) -> Option<EraToken<'b>> {
        self.consume(EraLexerMode::Normal, EraTokenKind::LineBreak)
    }
    #[must_use]
    fn matches_comma(&mut self) -> Option<EraToken<'b>> {
        self.try_consume(EraLexerMode::Normal, EraTokenKind::Comma)
    }
    #[must_use]
    fn consume_comma(&mut self) -> Option<EraToken<'b>> {
        self.consume(EraLexerMode::Normal, EraTokenKind::Comma)
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
            EraTokenKind::KwFunction => {
                self.consume_newline()?;
                Some(EraSharpDecl::FunctionDecl(EraSharpFunctionDecl {
                    returns_string: false,
                    src_info: first.src_info,
                }))
            }
            EraTokenKind::KwFunctionS => {
                self.consume_newline()?;
                Some(EraSharpDecl::FunctionDecl(EraSharpFunctionDecl {
                    returns_string: true,
                    src_info: first.src_info,
                }))
            }
            EraTokenKind::KwOnly => {
                self.consume_newline()?;
                Some(EraSharpDecl::EventKindDecl(EraEventKindDecl {
                    kind: EraEventFunKind::Only,
                    src_info: first.src_info,
                }))
            }
            EraTokenKind::KwPri => {
                self.consume_newline()?;
                Some(EraSharpDecl::EventKindDecl(EraEventKindDecl {
                    kind: EraEventFunKind::Pri,
                    src_info: first.src_info,
                }))
            }
            EraTokenKind::KwLater => {
                self.consume_newline()?;
                Some(EraSharpDecl::EventKindDecl(EraEventKindDecl {
                    kind: EraEventFunKind::Later,
                    src_info: first.src_info,
                }))
            }
            EraTokenKind::KwDefine => {
                // HACK: We need to manually handle #DEFINE's and feed them back to lexer
                let in_def = self.consume(EraLexerMode::SharpDecl, EraTokenKind::Identifier)?;
                let out_def =
                    self.consume(EraLexerMode::RawStr, EraTokenKind::PlainStringLiteral)?;
                self.consume_newline()?;
                self.lexer
                    .push_define(in_def.lexeme.into(), out_def.lexeme.into());
                Some(EraSharpDecl::DefineDecl(EraDefineDecl {
                    src_info: first.src_info,
                }))
            }
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
        let mut is_savedata = false;
        let mut is_charadata = false;
        let mut dims = smallvec::SmallVec::new();
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
                EraTokenKind::KwSavedata => {
                    if is_savedata {
                        self.report_token_err(
                            token.into(),
                            false,
                            "redundant savedata qualifier in variable declaration",
                        );
                    }
                    is_savedata = true;
                }
                EraTokenKind::KwCharadata => {
                    if is_charadata {
                        self.report_token_err(
                            token.into(),
                            false,
                            "redundant charadata qualifier in variable declaration",
                        );
                    }
                    is_charadata = true;
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
        while self.matches_comma().is_some() {
            // HACK: Omitted dimension is 0
            if is_ref {
                match self.peek_token(EraLexerMode::Normal).kind {
                    EraTokenKind::Comma | EraTokenKind::LineBreak => {
                        dims.push(0);
                        continue;
                    }
                    _ => (),
                }
            }
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
                // Allow trailing comma
                if matches!(
                    self.peek_token(EraLexerMode::SharpDecl).kind,
                    EraTokenKind::LineBreak
                ) {
                    break;
                }
            }
            self.consume_newline()?;
        }
        Some(EraVarDecl {
            name: String::from_utf8_lossy(name.lexeme).into(),
            dims,
            inits,
            is_string,
            is_ref,
            is_const,
            is_global,
            is_dynamic,
            is_savedata,
            is_charadata,
            src_info: name.src_info,
        })
    }
    fn func_declaration(&mut self) -> Option<EraFunDecl> {
        self.consume(EraLexerMode::Normal, EraTokenKind::At)?;
        let name = self.consume(EraLexerMode::Normal, EraTokenKind::Identifier)?;
        let name_si = name.src_info;
        let name = String::from_utf8_lossy(name.lexeme).into_owned();
        let token = self.read_token(EraLexerMode::Normal);
        let mut params = Vec::new();
        let mut decls = Vec::new();
        let mut body = Vec::new();
        let mut kind = EraFunKind::Procedure;
        let mut event_kind = if routine::is_event_name(&name) {
            Some(EraEventFunKind::Normal)
        } else {
            None
        };

        // Reset local variables
        self.local_vars.clear();

        // Parameters list
        match token.kind {
            EraTokenKind::Comma => {
                loop {
                    params.push(self.expression(true)?);
                    if self.matches_comma().is_none() {
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
                        if self.matches_comma().is_none() {
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
                    let var_name = CaselessString::new(x.name.clone());
                    if self.local_vars.insert(var_name, info).is_some() {
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
                EraSharpDecl::EventKindDecl(x) => {
                    if !matches!(event_kind, Some(EraEventFunKind::Normal)) {
                        let msg = if matches!(event_kind, None) {
                            "cannot use event type specifier in non-event function"
                        } else {
                            "too many event type specifiers"
                        };
                        self.report_err(x.src_info, true, msg);
                    } else {
                        event_kind = Some(x.kind);
                    }
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
                        "sharp declaration should not appear within function body; ignoring",
                    );
                }
                _ => {
                    let stmt = self.statement().unwrap_or(EraStmt::Invalid(token.src_info));
                    body.push(stmt);
                }
            }
        }

        Some(EraFunDecl {
            name,
            params,
            kind,
            event_kind,
            decls,
            body,
            src_info: name_si,
        })
    }
    fn statement(&mut self) -> Option<EraStmt> {
        use EraCommandStmt as Cmd;

        let first = self.lexer.peek(EraLexerMode::Normal);
        Some(match first.kind {
            EraTokenKind::Identifier => EraStmt::Command({
                let cmd = first.lexeme.to_ascii_uppercase();
                let cmd = cmd.as_slice();

                trait Adhoc {
                    fn r(&mut self) -> &mut Self;
                    fn empty(&mut self) -> Option<EraCmdEmptyStmt>;
                }
                impl<T: FnMut(&EraParseErrorInfo), U: FnMut(&crate::lexer::EraLexErrorInfo)> Adhoc
                    for EraParserImpl<'_, '_, T, U>
                {
                    fn r(&mut self) -> &mut Self {
                        // Discard command token
                        let token = self.lexer.read(EraLexerMode::Normal);
                        self.src_info = token.src_info;
                        self
                    }
                    fn empty(&mut self) -> Option<EraCmdEmptyStmt> {
                        // Discard command token and finish the line
                        let token = self.lexer.read(EraLexerMode::Normal);
                        self.consume_newline()?;
                        Some(EraCmdEmptyStmt {
                            src_info: token.src_info,
                        })
                    }
                }

                if let Some((arg_fmt, flags)) = Self::try_recognize_print_cmd(cmd) {
                    Cmd::Print(self.r().stmt_print(arg_fmt, flags)?)
                } else if let Some((arg_fmt, flags)) = Self::try_recognize_debugprint_cmd(cmd) {
                    Cmd::DebugPrint(self.r().stmt_print(arg_fmt, flags)?)
                } else if let Some(flags) = Self::try_recognize_printdata_cmd(cmd) {
                    Cmd::PrintData(self.r().stmt_printdata(flags)?)
                } else {
                    match cmd {
                        b"NOP" => Cmd::Nop(self.empty()?),
                        b"IF" => Cmd::If(self.r().stmt_if()?),
                        b"SELECTCASE" => Cmd::SelectCase(self.r().stmt_selectcase()?),
                        b"SIF" => Cmd::If(self.r().stmt_sif()?),
                        b"QUIT" => Cmd::Quit(self.r().stmt_quit()?),
                        b"WAIT" => Cmd::Wait(self.r().stmt_wait(false, false)?),
                        b"FORCEWAIT" => Cmd::Wait(self.r().stmt_wait(false, true)?),
                        b"WAITANYKEY" => Cmd::Wait(self.r().stmt_wait(true, false)?),
                        b"WHILE" => Cmd::While(self.r().stmt_while()?),
                        b"RETURN" | b"RETURNF" => Cmd::Return(self.r().stmt_return()?),
                        b"CALL" | b"CALLF" => Cmd::Call(self.r().stmt_call()?),
                        b"CALLFORM" | b"CALLFORMF" => Cmd::Call(self.r().stmt_callform()?),
                        b"TRYCALL" | b"TRYCALLF" => Cmd::TryCall(self.r().stmt_call()?),
                        b"TRYCALLFORM" | b"TRYCALLFORMF" => Cmd::TryCall(self.r().stmt_callform()?),
                        b"TRYCCALL" | b"TRYCCALLF" => Cmd::TryCCall(self.r().stmt_tryccall()?),
                        b"TRYCCALLFORM" | b"TRYCCALLFORMF" => {
                            Cmd::TryCCall(self.r().stmt_tryccallform()?)
                        }
                        b"JUMP" => Cmd::Jump(self.r().stmt_call()?),
                        b"JUMPFORM" => Cmd::Jump(self.r().stmt_callform()?),
                        b"TRYJUMP" => Cmd::TryJump(self.r().stmt_call()?),
                        b"TRYJUMPFORM" => Cmd::TryJump(self.r().stmt_callform()?),
                        b"TRYCJUMP" => Cmd::TryCJump(self.r().stmt_tryccall()?),
                        b"TRYCJUMPFORM" => Cmd::TryCJump(self.r().stmt_tryccallform()?),
                        b"CONTINUE" => Cmd::Continue(self.r().stmt_continue()?),
                        b"BREAK" => Cmd::Break(self.r().stmt_break()?),
                        b"THROW" => Cmd::Throw(self.r().stmt_throw()?),
                        b"REPEAT" => Cmd::Repeat(self.r().stmt_repeat()?),
                        b"GOTO" => Cmd::Goto(self.r().stmt_goto()?),
                        b"FOR" => Cmd::For(self.r().stmt_for()?),
                        b"DO" => Cmd::DoLoop(self.r().stmt_do_loop()?),
                        // b"GCREATE" => Cmd::GCreate(self.r().stmt_gcreate()?),
                        // b"GDISPOSE" => Cmd::GDispose(self.r().stmt_gdispose()?),
                        // b"GDRAWSPRITE" => Cmd::GDrawSprite(self.r().stmt_gdrawsprite()?),
                        b"SPLIT" => Cmd::Split(self.r().stmt_split()?),
                        b"TIMES" => Cmd::Times(self.r().stmt_times()?),
                        b"SETBIT" => Cmd::SetBit(self.r().stmt_setbit()?),
                        b"CLEARBIT" => Cmd::ClearBit(self.r().stmt_clearbit()?),
                        b"INVERTBIT" => Cmd::InvertBit(self.r().stmt_invertbit()?),
                        b"SETCOLOR" => Cmd::SetColor(self.r().stmt_setcolor()?),
                        b"RESETCOLOR" => Cmd::ResetColor(self.r().stmt_resetcolor()?),
                        b"SETBGCOLOR" => Cmd::SetBgColor(self.r().stmt_setcolor()?),
                        b"RESETBGCOLOR" => Cmd::ResetBgColor(self.r().stmt_resetcolor()?),
                        b"VARSET" => Cmd::VarSet(self.r().stmt_varset()?),
                        b"CVARSET" => Cmd::CVarSet(self.r().stmt_cvarset()?),
                        b"VARSIZE" => Cmd::VarSize(self.r().stmt_varsize()?),
                        b"SWAP" => Cmd::Swap(self.r().stmt_swap()?),
                        b"HTML_PRINT" => Cmd::HtmlPrint(self.r().stmt_html_print()?),
                        b"PRINTBUTTON" | b"PRINTBUTTONC" | b"PRINTBUTTONLC" => {
                            let mut flags = PrintExtendedFlags::new();
                            match cmd {
                                b"PRINTBUTTONC" => flags.set_right_pad(true),
                                b"PRINTBUTTONLC" => flags.set_left_pad(true),
                                _ => (),
                            }
                            Cmd::PrintButton(self.r().stmt_printbutton(flags)?)
                        }
                        b"ARRAYREMOVE" => Cmd::ArrayRemove(self.r().stmt_arrayremove()?),
                        b"ARRAYSORT" => Cmd::ArraySort(self.r().stmt_arraysort()?),
                        b"ARRAYMSORT" => Cmd::ArrayMSort(self.r().stmt_arraymsort()?),
                        b"ARRAYCOPY" => Cmd::ArrayCopy(self.r().stmt_arraycopy()?),
                        b"ARRAYSHIFT" => Cmd::ArrayShift(self.r().stmt_arrayshift()?),
                        b"INPUT" => Cmd::Input(self.r().stmt_input()?),
                        b"INPUTS" => Cmd::InputS(self.r().stmt_input()?),
                        b"TINPUT" => Cmd::TInput(self.r().stmt_tinput()?),
                        b"TINPUTS" => Cmd::TInputS(self.r().stmt_tinput()?),
                        b"ONEINPUT" => Cmd::OneInput(self.r().stmt_oneinput()?),
                        b"ONEINPUTS" => Cmd::OneInputS(self.r().stmt_oneinput()?),
                        b"TONEINPUT" => Cmd::TOneInput(self.r().stmt_toneinput()?),
                        b"TONEINPUTS" => Cmd::TOneInputS(self.r().stmt_toneinput()?),
                        b"REUSELASTLINE" => Cmd::ReuseLastLine(self.r().stmt_reuselastline()?),
                        b"CLEARLINE" => Cmd::ClearLine(self.r().stmt_clearline()?),
                        b"DRAWLINE" => Cmd::DrawLine(self.empty()?),
                        b"CUSTOMDRAWLINE" => {
                            Cmd::CustomDrawLine(self.r().stmt_customdrawline(false)?)
                        }
                        b"DRAWLINEFORM" => Cmd::CustomDrawLine(self.r().stmt_customdrawline(true)?),
                        b"TWAIT" => Cmd::TWait(self.r().stmt_twait()?),
                        b"FONTSTYLE" => Cmd::FontStyle(self.r().stmt_fontstyle()?),
                        b"FONTBOLD" => Cmd::FontBold(self.empty()?),
                        b"FONTITALIC" => Cmd::FontItalic(self.empty()?),
                        b"FONTREGULAR" => Cmd::FontRegular(self.empty()?),
                        b"SETFONT" => Cmd::SetFont(self.r().stmt_setfont()?),
                        b"STRDATA" => Cmd::StrData(self.r().stmt_strdata()?),
                        b"PUTFORM" => Cmd::PutForm(self.r().stmt_putform()?),
                        b"SKIPDISP" => Cmd::SkipDisp(self.r().stmt_skipdisp()?),
                        b"BEGIN" => Cmd::Begin(self.r().stmt_begin()?),
                        b"DOTRAIN" => Cmd::DoTrain(self.r().stmt_dotrain()?),
                        b"REDRAW" => Cmd::Redraw(self.r().stmt_redraw()?),
                        b"STRLEN" => Cmd::StrLen(self.r().stmt_strlen(false)?),
                        b"STRLENFORM" => Cmd::StrLen(self.r().stmt_strlen(true)?),
                        b"STRLENU" => Cmd::StrLenU(self.r().stmt_strlen(false)?),
                        b"STRLENFORMU" => Cmd::StrLenU(self.r().stmt_strlen(true)?),
                        b"ALIGNMENT" => Cmd::Alignment(self.r().stmt_alignment()?),
                        b"TOOLTIP_SETDELAY" => {
                            Cmd::ToolTipSetDelay(self.r().stmt_tooltip_setdelay()?)
                        }
                        b"TOOLTIP_SETDURATION" => {
                            Cmd::ToolTipSetDuration(self.r().stmt_tooltip_setduration()?)
                        }
                        b"RANDOMIZE" => Cmd::Randomize(self.r().stmt_randomize()?),
                        b"DUMPRAND" => Cmd::DumpRand(self.empty()?),
                        b"INITRAND" => Cmd::InitRand(self.empty()?),
                        b"BAR" => Cmd::Bar(self.r().stmt_bar(false)?),
                        b"BARL" => Cmd::Bar(self.r().stmt_bar(true)?),
                        b"ADDCHARA" => Cmd::AddChara(self.r().stmt_addchara()?),
                        b"PICKUPCHARA" => Cmd::PickUpChara(self.r().stmt_pickupchara()?),
                        b"DELCHARA" => Cmd::DelChara(self.r().stmt_delchara()?),
                        b"SWAPCHARA" => Cmd::SwapChara(self.r().stmt_swapchara()?),
                        b"ADDCOPYCHARA" => Cmd::AddCopyChara(self.r().stmt_addcopychara()?),
                        b"RESET_STAIN" => Cmd::ResetStain(self.r().stmt_reset_stain()?),
                        b"SAVECHARA" => Cmd::SaveChara(self.r().stmt_savechara()?),
                        b"LOADCHARA" => Cmd::LoadChara(self.r().stmt_loadchara()?),
                        b"SETANIMETIMER" => Cmd::SetAnimeTimer(self.r().stmt_setanimetimer()?),
                        b"HTML_TAGSPLIT" => Cmd::HtmlTagSplit(self.r().stmt_html_tagsplit()?),
                        b"POWER" => Cmd::Power(self.r().stmt_power()?),
                        b"SAVEDATA" => Cmd::SaveData(self.r().stmt_savedata()?),
                        b"RESTART" => Cmd::Restart(self.empty()?),
                        b"GETTIME" => Cmd::GetTime(self.empty()?),
                        b"LOADGLOBAL" => Cmd::LoadGlobal(self.empty()?),
                        b"SAVEGLOBAL" => Cmd::SaveGlobal(self.empty()?),
                        b"LOADGAME" => Cmd::LoadGame(self.empty()?),
                        b"SAVEGAME" => Cmd::SaveGame(self.empty()?),
                        b"DEBUGCLEAR" => Cmd::DebugClear(self.empty()?),
                        b"RESETDATA" => Cmd::ResetData(self.empty()?),
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
                        | b"GETKEY"
                        | b"GETKEYTRIGGERED"
                        | b"FIND_CHARADATA" => Cmd::ResultCmdCall(self.stmt_result_cmd_call()?),
                        _ => return Some(self.stmt_expression()?),
                    }
                }
            }),
            EraTokenKind::Dollar => {
                _ = self.lexer.read(EraLexerMode::Normal);
                let token = self.consume(EraLexerMode::Normal, EraTokenKind::Identifier)?;
                self.consume_newline()?;
                EraStmt::Label(EraLabelStmt {
                    name: String::from_utf8_lossy(token.lexeme).into(),
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
            _ => self.stmt_expression()?,
        })
    }
    fn var_expression(&mut self) -> Option<EraVarExpr> {
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
        let lexer_mode = if self.is_expression_s_mode {
            EraLexerMode::ExpressionS
        } else {
            EraLexerMode::Normal
        };
        let first = self.read_token(lexer_mode);
        let mut lhs = match first.kind {
            EraTokenKind::IntLiteral => {
                // TODO: use atoi_simd?
                let mut lexeme = first.lexeme;
                let radix = if lexeme.strip_prefix_inplace(b"0x")
                    || lexeme.strip_prefix_inplace(b"0X")
                {
                    16
                } else if lexeme.strip_prefix_inplace(b"0b") || lexeme.strip_prefix_inplace(b"0B") {
                    2
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
                let (num, exp) = lexeme
                    .split_once("p")
                    .or_else(|| lexeme.split_once("P"))
                    .unwrap_or((lexeme, ""));
                const INVALID_MSG: &'static str = concatdoc! {"
                    invalid integer literal (if you need integer literals that can overflow ",
                    "i64, consider configuring the engine with flag use_inf_prec_int set)
                "};
                let mut val = match i64::from_str_radix(num, radix) {
                    Ok(x) => x,
                    Err(_) => {
                        self.synchronize();
                        self.report_token_err(first.into(), true, INVALID_MSG);
                        return None;
                    }
                };
                if !exp.is_empty() {
                    let Ok(exp) = u8::from_str_radix(exp, 10) else {
                        self.synchronize();
                        self.report_token_err(first.into(), true, INVALID_MSG);
                        return None;
                    };
                    val = match val.checked_shl(exp.into()) {
                        Some(x) => x,
                        None => {
                            self.synchronize();
                            self.report_token_err(first.into(), true, INVALID_MSG);
                            return None;
                        }
                    };
                }
                EraExpr::new_int(val, first.src_info)
            }
            EraTokenKind::StringLiteral => {
                let raw_literal = &first.lexeme[1..(first.lexeme.len() - 1)];
                EraExpr::new_str(
                    self.str_with_escape(raw_literal, first.src_info).into(),
                    first.src_info,
                )
            }
            EraTokenKind::StringFormStart | EraTokenKind::DoubleQuote => {
                // TODO: Handle escape characters for StringForm (both expression and raw)
                EraExpr::Term(EraTermExpr::StrForm(self.expression_strform(first)?))
            }
            EraTokenKind::TernaryStrFormMarker => self.ternary_strform()?,
            EraTokenKind::LBracket => {
                let lhs = self.expression(true)?;
                self.consume(EraLexerMode::Normal, EraTokenKind::RBracket)?;
                lhs
            }
            EraTokenKind::Identifier => EraExpr::Term(EraTermExpr::Var(EraVarExpr {
                name: String::from_utf8_lossy(first.lexeme).into(),
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
            // HACK: Support special break_at symbols
            let peek_mode = if matches!(break_at, EraTokenKind::Percentage) {
                EraLexerMode::InlineNormal
            } else {
                EraLexerMode::Normal
            };
            let token = self.peek_token(peek_mode);

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
                                if self.matches_comma().is_some() {
                                    args.push(None);
                                    continue;
                                }
                                args.push(Some(self.expression(true)?));
                                if self.matches_comma().is_none() {
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
                                self.last_ate_newline = true;
                                EraExpr::Term(EraTermExpr::StrForm(self.raw_strform(true)?))
                            } else {
                                self.expression_bp(r_bp, pure, break_at)?
                            };
                            let lhs = EraExpr::Term(EraTermExpr::Var(x));
                            // Stop immediately as we are done with the current line
                            if self.last_ate_newline {
                                return Some(EraExpr::Binary(lhs.into(), token.into(), rhs.into()));
                            }
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
                            x.name = arcstr::format!(
                                "{}@{}",
                                x.name,
                                String::from_utf8_lossy(rhs.lexeme)
                            );
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
                    self.str_with_escape(token.lexeme, token.src_info),
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
                    self.consume(EraLexerMode::InlineNormal, EraTokenKind::Percentage)?;
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
                        self.str_with_escape(lexeme, token.src_info),
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
                    self.consume(EraLexerMode::InlineNormal, EraTokenKind::Percentage)?;
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
                }
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
                        self.str_with_escape(lexeme, token.src_info),
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
                    self.consume(EraLexerMode::InlineNormal, EraTokenKind::Percentage)?;
                }
                EraTokenKind::TernaryStrFormMarker => {
                    if let Some(EraStrFormExprPart::Literal(x, _)) = else_parts.last_mut() {
                        x.truncate(x.trim_end_matches(&[' ', '\t']).len());
                    }
                    break;
                }
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
                        self.str_with_escape(lexeme, token.src_info),
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
                    self.consume(EraLexerMode::InlineNormal, EraTokenKind::Percentage)?;
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
    fn raw_str(&mut self) -> Option<EraExpr> {
        let mut cont = String::new();
        let mut token = self.lexer.read(EraLexerMode::RawStr);
        let src_info = token.src_info;
        loop {
            match token.kind {
                EraTokenKind::PlainStringLiteral => {
                    cont.push_str(&self.str_with_escape(token.lexeme, token.src_info))
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
        Some(EraExpr::new_str(cont.into(), src_info))
    }
    fn generic_rawstr(
        &mut self,
        mode: EraLexerMode,
        break_at: &[EraTokenKind],
    ) -> Option<(EraExpr, EraToken<'b>)> {
        let mut cont = String::new();
        let mut token = self.lexer.read(mode);
        let src_info = token.src_info;
        loop {
            match token.kind {
                EraTokenKind::PlainStringLiteral => {
                    cont.push_str(&self.str_with_escape(token.lexeme, token.src_info))
                }
                EraTokenKind::LineBreak => {
                    if !self.is_ignoring_newline() {
                        break;
                    }
                    cont.push(' ');
                    self.handle_ignore_newline_tokens();
                }
                _ if break_at.contains(&token.kind) => break,
                _ => {
                    // TODO: Synchronize here?
                    self.report_token_err(token.into(), true, "unexpected token");
                    return None;
                }
            }
            token = self.lexer.read(mode);
        }
        Some((EraExpr::new_str(cont.into(), src_info), token))
    }
    fn strform_expr_part_expr(
        &mut self,
        break_at: EraTokenKind,
    ) -> Option<EraStrFormExprPartExpression> {
        let expr = self.expression_bp(0, true, break_at)?;
        let mut width = None;
        let mut alignment = PadStringFlags::new();
        if self.matches_comma().is_some() {
            // HACK: Detect empty string interpolation format args
            if self.peek_token(EraLexerMode::Normal).kind == break_at {
                return Some(EraStrFormExprPartExpression {
                    expr,
                    width,
                    alignment,
                });
            }
            width = Some(self.expression_bp(0, true, break_at)?);
            if self.matches_comma().is_some() {
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
    fn stmt_expression(&mut self) -> Option<EraStmt> {
        self.last_ate_newline = false;
        let expr = self.expression(false)?;
        if self.last_ate_newline {
            return Some(EraStmt::Expr(expr));
        }
        let stmt = if matches!(
            self.peek_token(EraLexerMode::Normal).kind,
            EraTokenKind::Comma
        ) {
            // Inspect the expression
            if let EraExpr::Binary(
                lhs,
                EraTokenLite {
                    kind: EraTokenKind::Assign | EraTokenKind::ExprAssign,
                    src_info,
                },
                rhs,
            ) = expr
            {
                let EraExpr::Term(EraTermExpr::Var(dest)) = *lhs else {
                    self.report_err(lhs.source_pos_info(), true, "invalid assignment target");
                    return None;
                };
                // Extended array range-assignment
                let src_info = dest.src_info;
                let mut srcs = vec![*rhs];
                while self.matches_comma().is_some() {
                    srcs.push(self.expression(true)?);
                }
                EraStmt::RowAssign(EraRowAssignStmt {
                    dest,
                    srcs,
                    src_info,
                })
            } else {
                // Ignore the trailing comma as we cannot handle it
                EraStmt::Expr(expr)
            }
        } else {
            EraStmt::Expr(expr)
        };
        self.consume_newline()?;
        Some(stmt)
    }
    fn stmt_print(
        &mut self,
        arg_fmt: EraCommandArgFmt,
        flags: PrintExtendedFlags,
    ) -> Option<EraPrintStmt> {
        let mut vals = Vec::new();
        match arg_fmt {
            // TODO: PRINT statement type-check
            // TODO: Fix EraCommandArgFmt::ExpressionS
            EraCommandArgFmt::Expression | EraCommandArgFmt::ExpressionS => loop {
                if self
                    .try_consume(EraLexerMode::Normal, EraTokenKind::SingleQuote)
                    .is_some()
                {
                    // HACK: Raw string inside PRINTV
                    let (val, token) =
                        self.generic_rawstr(EraLexerMode::CommaRawStr, &[EraTokenKind::Comma])?;
                    vals.push(val);
                    match token.kind {
                        EraTokenKind::Comma => continue,
                        EraTokenKind::LineBreak => break,
                        _ => unreachable!(),
                    }
                }
                self.is_expression_s_mode = true;
                let r = self.expression(true);
                self.is_expression_s_mode = false;
                vals.push(r?);
                if self.matches_comma().is_some() {
                    continue;
                }
                self.consume(EraLexerMode::Normal, EraTokenKind::LineBreak)?;
                break;
            },
            EraCommandArgFmt::RawString => {
                vals.push(self.raw_str()?);
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
    fn stmt_printdata(&mut self, flags: PrintExtendedFlags) -> Option<EraPrintDataStmt> {
        let src_info = self.src_info;
        let dest = if self.matches_newline().is_none() {
            Some(self.var_expression()?)
        } else {
            None
        };
        let data = self.stmt_printdata_data()?;
        Some(EraPrintDataStmt {
            dest,
            data,
            flags,
            src_info,
        })
    }
    fn stmt_printdata_data(&mut self) -> Option<Vec<Vec<EraExpr>>> {
        let mut data = Vec::new();
        while self.matches_command_end(b"ENDDATA").is_none() {
            if self.matches_command_end(b"DATA").is_some() {
                data.push(vec![self.raw_str()?]);
            } else if self.matches_command_end(b"DATAFORM").is_some() {
                data.push(vec![EraExpr::Term(EraTermExpr::StrForm(
                    self.raw_strform(false)?,
                ))]);
            } else if self.matches_command_end(b"DATALIST").is_some() {
                let mut data_list = Vec::new();
                self.consume_newline()?;
                while self.matches_command_end(b"ENDLIST").is_none() {
                    if self.matches_command_end(b"DATA").is_some() {
                        data_list.push(self.raw_str()?);
                    } else if self.matches_command_end(b"DATAFORM").is_some() {
                        data_list.push(EraExpr::Term(EraTermExpr::StrForm(
                            self.raw_strform(false)?,
                        )));
                    } else {
                        let token = self.lexer.read(EraLexerMode::Normal).into();
                        self.report_token_err(token, true, "unknown command inside DATALIST");
                        return None;
                    }
                }
                self.consume_newline()?;
                data.push(data_list);
            } else {
                let token = self.lexer.read(EraLexerMode::Normal).into();
                self.report_token_err(token, true, "unknown command inside PRINTDATA");
                return None;
            }
        }
        self.consume_newline()?;
        Some(data)
    }
    fn stmt_sif(&mut self) -> Option<EraIfStmt> {
        let cond = self.expression(true)?;
        self.consume(EraLexerMode::Normal, EraTokenKind::LineBreak)?;
        self.skip_whitespace();
        // TODO: Emuera does not allow statements like if-else here, should we also
        //       disallow it?
        let body = vec![stmt_at_guard!(self)?];
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
        {
            // STRENGTHENED: Prevent stack overflow in debug builds.
            let mut body = &mut body;
            let mut else_body = &mut else_body;
            loop {
                if let Some(token) = self.matches_command_end(b"ELSEIF") {
                    if is_at_else {
                        self.synchronize();
                        self.report_token_err(token.into(), true, "too many ELSEIF command");
                    }
                    self.src_info = token.src_info;
                    else_body.push(EraStmt::Command(EraCommandStmt::If(EraIfStmt {
                        cond: self.expression(true)?,
                        body: Vec::new(),
                        else_body: Vec::new(),
                        src_info: self.src_info,
                    })));
                    (body, else_body) = match else_body.last_mut().unwrap() {
                        EraStmt::Command(EraCommandStmt::If(x)) => (&mut x.body, &mut x.else_body),
                        _ => unreachable!(),
                    };
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
                    let stmt = stmt_at_guard!(self)?;
                    is_at_else
                        .then_some(&mut else_body)
                        .unwrap_or(&mut body)
                        .push(stmt);
                }
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
    fn stmt_wait(&mut self, any_key: bool, is_force: bool) -> Option<EraWaitStmt> {
        self.consume_newline()?;
        Some(EraWaitStmt {
            any_key,
            is_force,
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
                    let mut last_comma =
                        match self.try_consume(EraLexerMode::Normal, EraTokenKind::Comma) {
                            Some(x) => x,
                            None => break,
                        };
                    while let Some(comma) =
                        self.try_consume(EraLexerMode::Normal, EraTokenKind::Comma)
                    {
                        self.report_token_err(last_comma.into(), false, "too many commas in case");
                        last_comma = comma;
                    }
                    let token = self.peek_token(EraLexerMode::Normal);
                    if matches!(token.kind, EraTokenKind::LineBreak) {
                        self.report_token_err(last_comma.into(), false, "too many commas in case");
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
            let stmt = stmt_at_guard!(self)?;
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
            body.push(stmt_at_guard!(self)?);
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
    fn stmt_call_args(&mut self, first: EraToken) -> Option<Vec<Option<EraExpr>>> {
        let token = first;
        let mut args = Vec::new();
        match token.kind {
            EraTokenKind::LineBreak => (),
            EraTokenKind::Comma => loop {
                if self.matches_comma().is_some() {
                    args.push(None);
                    continue;
                }
                if self.matches_newline().is_some() {
                    break;
                }
                let expr = self.expression(true)?;
                args.push(Some(expr));
                if self.matches_comma().is_some() {
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
                        if self.matches_comma().is_some() {
                            args.push(None);
                            continue;
                        }
                        if matches!(
                            self.peek_token(EraLexerMode::Normal).kind,
                            EraTokenKind::RBracket
                        ) {
                            break;
                        }
                        let expr = self.expression(true)?;
                        args.push(Some(expr));
                        if self.matches_comma().is_none() {
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
        Some(args)
    }
    fn stmt_call(&mut self) -> Option<EraCallStmt> {
        let src_info = self.src_info;
        let func = self.consume(EraLexerMode::Normal, EraTokenKind::Identifier)?;
        let func = EraExpr::new_str(String::from_utf8_lossy(func.lexeme).into(), func.src_info);
        let token = self.read_token(EraLexerMode::Normal);
        let args = self.stmt_call_args(token)?;
        Some(EraCallStmt {
            func,
            args,
            src_info,
        })
    }
    fn generic_strform(
        &mut self,
        mode: EraLexerMode,
        break_at: &[EraTokenKind],
        trim_start: bool,
        trim_end: bool,
    ) -> Option<(EraStrFormExpr, EraToken<'b>)> {
        let mut parts = Vec::new();
        let mut token = self.lexer.read(mode);
        let mut is_first = true;
        let src_info = token.src_info;
        loop {
            match token.kind {
                EraTokenKind::PlainStringLiteral => {
                    let mut lexeme = token.lexeme;
                    if trim_start && is_first {
                        while let [b' ' | b'\t', rest @ ..] = lexeme {
                            lexeme = rest;
                        }
                    }
                    parts.push(EraStrFormExprPart::Literal(
                        String::from_utf8_lossy(lexeme).into(),
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
                    self.consume(EraLexerMode::InlineNormal, EraTokenKind::Percentage)?;
                }
                EraTokenKind::LineBreak => {
                    if !self.is_ignoring_newline() {
                        // Force break
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
                _ if break_at.contains(&token.kind) => {
                    break;
                }
                _ => {
                    // TODO: Synchronize here?
                    self.report_token_err(token.into(), true, "unexpected token");
                    return None;
                }
            }
            is_first = false;
            token = self.lexer.read(mode);
        }
        if trim_end {
            if let Some(EraStrFormExprPart::Literal(x, _)) = parts.last_mut() {
                x.truncate(x.trim_end_matches(&[' ', '\t']).len());
            }
        }
        Some((EraStrFormExpr { parts, src_info }, token))
    }
    fn stmt_callform(&mut self) -> Option<EraCallStmt> {
        let src_info = self.src_info;
        let (func, token) = self.generic_strform(
            EraLexerMode::CallForm,
            &[EraTokenKind::Comma, EraTokenKind::LBracket],
            true,
            true,
        )?;
        let func = EraExpr::Term(EraTermExpr::StrForm(func));
        let args = self.stmt_call_args(token)?;
        Some(EraCallStmt {
            func,
            args,
            src_info,
        })
    }
    fn stmt_tryccall(&mut self) -> Option<EraTryCCallStmt> {
        let EraCallStmt {
            func,
            args,
            src_info,
        } = self.stmt_callform()?;
        let mut then_body = Vec::new();
        let mut catch_body = Vec::new();
        while self.matches_command_end(b"CATCH").is_none() {
            then_body.push(stmt_at_guard!(self)?);
        }
        self.consume_newline()?;
        while self.matches_command_end(b"ENDCATCH").is_none() {
            catch_body.push(stmt_at_guard!(self)?);
        }
        self.consume_newline()?;
        Some(EraTryCCallStmt {
            func,
            args,
            then_body,
            catch_body,
            src_info,
        })
    }
    fn stmt_tryccallform(&mut self) -> Option<EraTryCCallStmt> {
        self.stmt_tryccall()
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
            body.push(stmt_at_guard!(self)?);
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
        let target = String::from_utf8_lossy(target.lexeme).into();
        self.consume_newline()?;
        Some(EraGotoStmt { target, src_info })
    }
    fn stmt_for(&mut self) -> Option<EraForStmt> {
        let src_info = self.src_info;
        let var = self.expression(true)?;
        let EraExpr::Term(EraTermExpr::Var(var)) = var else {
            self.report_err(var.source_pos_info(), true, "expected a variable here");
            self.synchronize();
            return None;
        };
        self.consume(EraLexerMode::Normal, EraTokenKind::Comma)?;
        let start = self.expression(true)?;
        self.consume(EraLexerMode::Normal, EraTokenKind::Comma)?;
        let end = self.expression(true)?;
        let step = if self
            .try_consume(EraLexerMode::Normal, EraTokenKind::Comma)
            .is_some()
        {
            self.expression(true)?
        } else {
            EraExpr::Term(EraTermExpr::Literal(EraLiteral::Integer(1, src_info)))
        };
        let mut body = Vec::new();
        self.consume_newline()?;
        while self.matches_command_end(b"NEXT").is_none() {
            body.push(stmt_at_guard!(self)?);
        }
        self.consume_newline()?;
        Some(EraForStmt {
            var,
            start,
            end,
            step,
            body,
            src_info,
        })
    }
    fn stmt_do_loop(&mut self) -> Option<EraDoLoopStmt> {
        let src_info = self.src_info;
        self.consume_newline()?;
        let mut body = Vec::new();
        while self.matches_command_end(b"LOOP").is_none() {
            body.push(stmt_at_guard!(self)?);
        }
        let cond = self.expression(true)?;
        Some(EraDoLoopStmt {
            body,
            cond,
            src_info,
        })
    }
    // fn stmt_gcreate(&mut self) -> Option<EraGCreateStmt> {
    //     let src_info = self.src_info;
    //     let gid = self.expression(true)?;
    //     self.consume_comma()?;
    //     let width = self.expression(true)?;
    //     self.consume_comma()?;
    //     let height = self.expression(true)?;
    //     Some(EraGCreateStmt {
    //         gid,
    //         width,
    //         height,
    //         src_info,
    //     })
    // }
    // fn stmt_gdispose(&mut self) -> Option<EraGDisposeStmt> {
    //     let src_info = self.src_info;
    //     let gid = self.expression(true)?;
    //     Some(EraGDisposeStmt { gid, src_info })
    // }
    // fn stmt_gdrawsprite(&mut self) -> Option<EraGDrawSpriteStmt> {
    //     let src_info = self.src_info;
    //     let gid = self.expression(true)?;
    //     self.consume_comma()?;
    //     let sprite_name = self.expression(true)?;
    //     Some(EraGDrawSpriteStmt {
    //         gid,
    //         sprite_name,
    //         src_info,
    //     })
    // }
    fn stmt_split(&mut self) -> Option<EraSplitStmt> {
        let src_info = self.src_info;
        let input = self.expression(true)?;
        self.consume_comma()?;
        let separator = self.expression(true)?;
        self.consume_comma()?;
        let dest = self.var_expression()?;
        let dest_count = if self.matches_comma().is_some() {
            self.var_expression()?
        } else {
            EraVarExpr {
                name: arcstr::literal!("RESULT"),
                idxs: Vec::new(),
                src_info,
            }
        };
        self.consume_newline()?;
        Some(EraSplitStmt {
            input,
            separator,
            dest,
            dest_count,
            src_info,
        })
    }
    fn stmt_result_cmd_call(&mut self) -> Option<EraResultCmdCallStmt> {
        let name = self.lexer.read(EraLexerMode::Normal);
        let src_info = name.src_info;
        let name = String::from_utf8_lossy(name.lexeme).into();
        let mut args = Vec::new();
        if self.matches_newline().is_some() {
            return Some(EraResultCmdCallStmt {
                name,
                args,
                src_info,
            });
        }
        loop {
            args.push(Some(self.expression(true)?));
            if self.matches_comma().is_none() {
                break;
            }
        }
        self.consume_newline()?;
        Some(EraResultCmdCallStmt {
            name,
            args,
            src_info,
        })
    }
    fn stmt_times(&mut self) -> Option<EraTimesStmt> {
        let src_info = self.src_info;
        let target = self.var_expression()?;
        self.consume_comma()?;
        let factor = self.consume(EraLexerMode::RawStr, EraTokenKind::PlainStringLiteral)?;
        let Ok(factor) = String::from_utf8_lossy(factor.lexeme).trim().parse() else {
            self.report_err(factor.src_info, true, "expected a float here");
            self.synchronize();
            return None;
        };
        self.consume_newline()?;
        Some(EraTimesStmt {
            target,
            factor,
            src_info,
        })
    }
    fn stmt_setbit(&mut self) -> Option<EraSetBitStmt> {
        let src_info = self.src_info;
        let target = self.var_expression()?;
        self.consume_comma()?;
        let mut bits = Vec::new();
        loop {
            bits.push(self.expression(true)?);
            if self
                .try_consume(EraLexerMode::Normal, EraTokenKind::Comma)
                .is_none()
            {
                break;
            }
        }
        self.consume_newline()?;
        Some(EraSetBitStmt {
            target,
            bits,
            src_info,
        })
    }
    fn stmt_clearbit(&mut self) -> Option<EraClearBitStmt> {
        let src_info = self.src_info;
        let target = self.var_expression()?;
        self.consume_comma()?;
        let mut bits = Vec::new();
        loop {
            bits.push(self.expression(true)?);
            if self
                .try_consume(EraLexerMode::Normal, EraTokenKind::Comma)
                .is_none()
            {
                break;
            }
        }
        self.consume_newline()?;
        Some(EraClearBitStmt {
            target,
            bits,
            src_info,
        })
    }
    fn stmt_invertbit(&mut self) -> Option<EraInvertBitStmt> {
        let src_info = self.src_info;
        let target = self.var_expression()?;
        self.consume_comma()?;
        let mut bits = Vec::new();
        loop {
            bits.push(self.expression(true)?);
            if self
                .try_consume(EraLexerMode::Normal, EraTokenKind::Comma)
                .is_none()
            {
                break;
            }
        }
        self.consume_newline()?;
        Some(EraInvertBitStmt {
            target,
            bits,
            src_info,
        })
    }
    fn stmt_setcolor(&mut self) -> Option<EraSetColorStmt> {
        let src_info = self.src_info;
        let color;
        let a1 = self.expression(true)?;
        if let Some(comma1) = self.matches_comma() {
            let a2 = self.expression(true)?;
            let comma2 = self.consume_comma()?;
            let a3 = self.expression(true)?;
            let a1_si = a1.source_pos_info();
            let a2_si = a2.source_pos_info();
            let a3_si = a3.source_pos_info();
            // TODO: Check argument range [0, 255]
            let add_fn = |a: EraExpr, si, b: EraExpr| {
                EraExpr::Binary(
                    a.into(),
                    EraTokenLite {
                        kind: EraTokenKind::Plus,
                        src_info: si,
                    },
                    b.into(),
                )
            };
            let shift_fn = |a: EraExpr, si, b: i64| {
                EraExpr::Binary(
                    a.into(),
                    EraTokenLite {
                        kind: EraTokenKind::BitShiftL,
                        src_info: si,
                    },
                    EraExpr::new_int(b, si).into(),
                )
            };
            color = add_fn(
                add_fn(
                    shift_fn(a1, a1_si, 16),
                    comma1.src_info,
                    shift_fn(a2, a2_si, 8),
                ),
                comma2.src_info,
                a3,
            );
        } else {
            color = a1;
        }
        self.consume_newline()?;
        Some(EraSetColorStmt { color, src_info })
    }
    fn stmt_resetcolor(&mut self) -> Option<EraResetColorStmt> {
        let src_info = self.src_info;
        self.consume_newline()?;
        Some(EraResetColorStmt { src_info })
    }
    fn stmt_varset(&mut self) -> Option<EraVarSetStmt> {
        let src_info = self.src_info;
        let target = self.var_expression()?;
        let mut value = None;
        let mut start_index = None;
        let mut end_index = None;
        if self.matches_comma().is_some() {
            if self.matches_newline().is_some() {
                value = None;
            } else {
                value = Some(self.expression(true)?);
                if self.matches_comma().is_some() {
                    start_index = Some(self.expression(true)?);
                    if self.matches_comma().is_some() {
                        end_index = Some(self.expression(true)?);
                    }
                }
                self.consume_newline()?;
            }
        }
        Some(EraVarSetStmt {
            target,
            value,
            start_index,
            end_index,
            src_info,
        })
    }
    fn stmt_cvarset(&mut self) -> Option<EraCVarSetStmt> {
        let src_info = self.src_info;
        let target = self.var_expression()?;
        let index = if self.matches_comma().is_some() {
            self.expression(true)?
        } else {
            EraExpr::new_int(0, src_info)
        };
        let mut value = None;
        let mut start_id = None;
        let mut end_id = None;
        if self.matches_comma().is_some() {
            value = Some(self.expression(true)?);
            if self.matches_comma().is_some() {
                start_id = Some(self.expression(true)?);
                if self.matches_comma().is_some() {
                    end_id = Some(self.expression(true)?);
                }
            }
        }
        self.consume_newline()?;
        Some(EraCVarSetStmt {
            target,
            index,
            value,
            start_id,
            end_id,
            src_info,
        })
    }
    fn stmt_varsize(&mut self) -> Option<EraVarSizeStmt> {
        let src_info = self.src_info;
        let var = self.var_expression()?;
        self.consume_newline()?;
        Some(EraVarSizeStmt { var, src_info })
    }
    fn stmt_swap(&mut self) -> Option<EraSwapStmt> {
        let src_info = self.src_info;
        let v1 = self.var_expression()?;
        self.consume_comma()?;
        let v2 = self.var_expression()?;
        self.consume_newline()?;
        Some(EraSwapStmt { v1, v2, src_info })
    }
    fn stmt_html_print(&mut self) -> Option<EraHtmlPrintStmt> {
        let src_info = self.src_info;
        let expr = self.expression(true)?;
        self.consume_newline()?;
        Some(EraHtmlPrintStmt { expr, src_info })
    }
    fn stmt_printbutton(&mut self, flags: PrintExtendedFlags) -> Option<EraPrintButtonStmt> {
        let src_info = self.src_info;
        let content = self.expression(true)?;
        self.consume_comma()?;
        let value = self.expression(true)?;
        self.consume_newline()?;
        Some(EraPrintButtonStmt {
            content,
            value,
            flags,
            src_info,
        })
    }
    fn stmt_arrayremove(&mut self) -> Option<EraArrayRemoveStmt> {
        let src_info = self.src_info;
        let target = self.var_expression()?;
        self.consume_comma()?;
        let start_index = self.expression(true)?;
        self.consume_comma()?;
        let count = self.expression(true)?;
        self.consume_newline()?;
        Some(EraArrayRemoveStmt {
            target,
            start_index,
            count,
            src_info,
        })
    }
    fn stmt_arraysort(&mut self) -> Option<EraArraySortStmt> {
        let src_info = self.src_info;
        let target = self.var_expression()?;
        let mut is_ascending = true;
        let mut start_index = None;
        let mut count = None;
        if self.matches_comma().is_some() {
            let token = self.read_token(EraLexerMode::Normal);
            if token.lexeme.eq_ignore_ascii_case(b"FORWARD") {
                is_ascending = true;
            } else if token.lexeme.eq_ignore_ascii_case(b"BACK") {
                is_ascending = false;
            } else {
                self.report_token_err(token.into(), true, "expected `FORWARD` or `BACK`");
                return None;
            }
            if self.matches_comma().is_some() {
                start_index = Some(self.expression(true)?);
                if self.matches_comma().is_some() {
                    count = Some(self.expression(true)?);
                }
            }
        }
        self.consume_newline()?;
        Some(EraArraySortStmt {
            target,
            is_ascending,
            start_index,
            count,
            src_info,
        })
    }
    fn stmt_arraymsort(&mut self) -> Option<EraArrayMSortStmt> {
        let src_info = self.src_info;
        let primary = self.var_expression()?;
        let mut subs = Vec::new();
        while self.matches_comma().is_some() {
            subs.push(self.var_expression()?);
        }
        self.consume_newline()?;
        Some(EraArrayMSortStmt {
            primary,
            subs,
            src_info,
        })
    }
    fn stmt_arraycopy(&mut self) -> Option<EraArrayCopyStmt> {
        let src_info = self.src_info;
        let from_name = self.expression(true)?;
        self.consume_comma()?;
        let to_name = self.expression(true)?;
        self.consume_newline()?;
        Some(EraArrayCopyStmt {
            from_name,
            to_name,
            src_info,
        })
    }
    fn stmt_arrayshift(&mut self) -> Option<EraArrayShiftStmt> {
        let src_info = self.src_info;
        let target = self.var_expression()?;
        self.consume_comma()?;
        let shift_count = self.expression(true)?;
        self.consume_comma()?;
        let value = self.expression(true)?;
        let start_index = if self.matches_comma().is_some() {
            self.expression(true)?
        } else {
            EraExpr::new_int(0, src_info)
        };
        let target_count = if self.matches_comma().is_some() {
            self.expression(true)?
        } else {
            EraExpr::new_int(-1, src_info)
        };
        self.consume_newline()?;
        Some(EraArrayShiftStmt {
            target,
            shift_count,
            value,
            start_index,
            target_count,
            src_info,
        })
    }
    fn stmt_input(&mut self) -> Option<EraInputStmt> {
        let src_info = self.src_info;
        let mut default_value = None;
        let mut can_click = EraExpr::new_int(0, src_info);
        let mut allow_skip = EraExpr::new_int(0, src_info);
        if self.matches_newline().is_none() {
            default_value = Some(self.expression(true)?);
            if self.matches_comma().is_some() {
                can_click = self.expression(true)?;
                if self.matches_comma().is_some() {
                    allow_skip = self.expression(true)?;
                }
            }
            self.consume_newline()?;
        }
        Some(EraInputStmt {
            default_value,
            can_click,
            allow_skip,
            src_info,
        })
    }
    fn stmt_tinput(&mut self) -> Option<EraTInputStmt> {
        let src_info = self.src_info;
        let time_limit = self.expression(true)?;
        self.consume_comma()?;
        let default_value = self.expression(true)?;
        let mut show_prompt = EraExpr::new_int(1, src_info);
        let mut expiry_msg = EraExpr::new_str(arcstr::literal!("時間切れ"), src_info);
        let mut can_click = EraExpr::new_int(0, src_info);
        if self.matches_comma().is_some() {
            show_prompt = self.expression(true)?;
            if self.matches_comma().is_some() {
                expiry_msg = self.expression(true)?;
                if self.matches_comma().is_some() {
                    can_click = self.expression(true)?;
                }
            }
        }
        self.consume_newline()?;
        Some(EraTInputStmt {
            time_limit,
            default_value,
            show_prompt,
            expiry_msg,
            can_click,
            src_info,
        })
    }
    fn stmt_oneinput(&mut self) -> Option<EraOneInputStmt> {
        let src_info = self.src_info;
        let mut default_value = None;
        if self.matches_newline().is_none() {
            default_value = Some(self.expression(true)?);
            self.consume_newline()?;
        }
        Some(EraOneInputStmt {
            default_value,
            src_info,
        })
    }
    fn stmt_toneinput(&mut self) -> Option<EraTOneInputStmt> {
        let src_info = self.src_info;
        let time_limit = self.expression(true)?;
        self.consume_comma()?;
        let default_value = self.expression(true)?;
        let mut show_prompt = EraExpr::new_int(1, src_info);
        let mut expiry_msg = EraExpr::new_str(arcstr::ArcStr::new(), src_info);
        let mut can_click = EraExpr::new_int(0, src_info);
        if self.matches_comma().is_some() {
            show_prompt = self.expression(true)?;
            if self.matches_comma().is_some() {
                expiry_msg = self.expression(true)?;
                if self.matches_comma().is_some() {
                    can_click = self.expression(true)?;
                }
            }
        }
        self.consume_newline()?;
        Some(EraTOneInputStmt {
            time_limit,
            default_value,
            show_prompt,
            expiry_msg,
            can_click,
            src_info,
        })
    }
    fn stmt_reuselastline(&mut self) -> Option<EraReuseLastLineStmt> {
        let src_info = self.src_info;
        let content = EraExpr::Term(EraTermExpr::StrForm(self.raw_strform(false)?));
        Some(EraReuseLastLineStmt { content, src_info })
    }
    fn stmt_clearline(&mut self) -> Option<EraClearLineStmt> {
        let src_info = self.src_info;
        let count = self.expression(true)?;
        self.consume_newline()?;
        Some(EraClearLineStmt { count, src_info })
    }
    fn stmt_customdrawline(&mut self, is_form: bool) -> Option<EraCustomDrawLineStmt> {
        let src_info = self.src_info;
        let content = if is_form {
            EraExpr::Term(EraTermExpr::StrForm(self.raw_strform(false)?))
        } else {
            self.raw_str()?
        };
        Some(EraCustomDrawLineStmt { content, src_info })
    }
    fn stmt_twait(&mut self) -> Option<EraTWaitStmt> {
        let src_info = self.src_info;
        let duration = self.expression(true)?;
        self.consume_comma()?;
        let force_wait = self.expression(true)?;
        self.consume_newline()?;
        Some(EraTWaitStmt {
            duration,
            force_wait,
            src_info,
        })
    }
    fn stmt_fontstyle(&mut self) -> Option<EraFontStyleStmt> {
        let src_info = self.src_info;
        let style = if self.matches_newline().is_some() {
            EraExpr::new_int(0, src_info)
        } else {
            let r = self.expression(true)?;
            self.consume_newline()?;
            r
        };
        Some(EraFontStyleStmt { style, src_info })
    }
    fn stmt_setfont(&mut self) -> Option<EraSetFontStmt> {
        let src_info = self.src_info;
        let font_name = if self.matches_newline().is_some() {
            EraExpr::new_str(arcstr::ArcStr::new(), src_info)
        } else {
            let r = self.expression(true)?;
            self.consume_newline()?;
            r
        };
        Some(EraSetFontStmt {
            font_name,
            src_info,
        })
    }
    fn stmt_strdata(&mut self) -> Option<EraStrDataStmt> {
        let src_info = self.src_info;
        let target = if self.matches_newline().is_some() {
            EraVarExpr {
                name: arcstr::literal!("RESULTS"),
                idxs: Vec::new(),
                src_info,
            }
        } else {
            let r = self.var_expression()?;
            self.consume_newline()?;
            r
        };
        let data = self.stmt_printdata_data()?;
        Some(EraStrDataStmt {
            target,
            data,
            src_info,
        })
    }
    fn stmt_putform(&mut self) -> Option<EraPutFormStmt> {
        let src_info = self.src_info;
        let cont = EraExpr::Term(EraTermExpr::StrForm(self.raw_strform(false)?));
        Some(EraPutFormStmt { cont, src_info })
    }
    fn stmt_skipdisp(&mut self) -> Option<EraSkipDispStmt> {
        let src_info = self.src_info;
        let is_skip = self.expression(true)?;
        self.consume_newline()?;
        Some(EraSkipDispStmt { is_skip, src_info })
    }
    fn stmt_begin(&mut self) -> Option<EraBeginStmt> {
        let src_info = self.src_info;
        let token = self.read_token(EraLexerMode::Normal);
        let lexeme = token.lexeme;
        let proc = if lexeme.eq_ignore_ascii_case(b"FIRST") {
            EraBeginSystemProcedureKind::First
        } else if lexeme.eq_ignore_ascii_case(b"TITLE") {
            EraBeginSystemProcedureKind::Title
        } else if lexeme.eq_ignore_ascii_case(b"TRAIN") {
            EraBeginSystemProcedureKind::Train
        } else if lexeme.eq_ignore_ascii_case(b"AFTERTRAIN") {
            EraBeginSystemProcedureKind::AfterTrain
        } else if lexeme.eq_ignore_ascii_case(b"ABLUP") {
            EraBeginSystemProcedureKind::AblUp
        } else if lexeme.eq_ignore_ascii_case(b"TURNEND") {
            EraBeginSystemProcedureKind::TurnEnd
        } else if lexeme.eq_ignore_ascii_case(b"SHOP") {
            EraBeginSystemProcedureKind::Shop
        } else {
            self.synchronize();
            self.report_token_err(token.into(), true, "invalid procedure for BEGIN command");
            return None;
        };
        self.consume_newline()?;
        Some(EraBeginStmt { proc, src_info })
    }
    fn stmt_dotrain(&mut self) -> Option<EraDoTrainStmt> {
        let src_info = self.src_info;
        let number = self.expression(true)?;
        self.consume_newline()?;
        Some(EraDoTrainStmt { number, src_info })
    }
    fn stmt_redraw(&mut self) -> Option<EraRedrawStmt> {
        let src_info = self.src_info;
        let arg = self.expression(true)?;
        self.consume_newline()?;
        Some(EraRedrawStmt { arg, src_info })
    }
    fn stmt_strlen(&mut self, is_form: bool) -> Option<EraStrLenStmt> {
        let src_info = self.src_info;
        let cont = if is_form {
            EraExpr::Term(EraTermExpr::StrForm(self.raw_strform(false)?))
        } else {
            self.raw_str()?
        };
        Some(EraStrLenStmt { cont, src_info })
    }
    fn stmt_alignment(&mut self) -> Option<EraAlignmentStmt> {
        let src_info = self.src_info;
        let token = self.read_token(EraLexerMode::Normal);
        let lexeme = token.lexeme;
        self.consume_newline()?;
        let alignment = if lexeme.eq_ignore_ascii_case(b"LEFT") {
            EraAlignmentKind::Left
        } else if lexeme.eq_ignore_ascii_case(b"CENTER") {
            EraAlignmentKind::Center
        } else if lexeme.eq_ignore_ascii_case(b"RIGHT") {
            EraAlignmentKind::Right
        } else {
            self.synchronize();
            self.report_token_err(
                token.into(),
                true,
                "invalid alignment for ALIGNMENT command",
            );
            return None;
        };
        Some(EraAlignmentStmt {
            alignment,
            src_info,
        })
    }
    fn stmt_tooltip_setdelay(&mut self) -> Option<EraToolTipSetDelayStmt> {
        let src_info = self.src_info;
        let duration = self.expression(true)?;
        self.consume_newline()?;
        Some(EraToolTipSetDelayStmt { duration, src_info })
    }
    fn stmt_tooltip_setduration(&mut self) -> Option<EraToolTipSetDurationStmt> {
        let src_info = self.src_info;
        let duration = self.expression(true)?;
        self.consume_newline()?;
        Some(EraToolTipSetDurationStmt { duration, src_info })
    }
    fn stmt_randomize(&mut self) -> Option<EraRandomizeStmt> {
        let src_info = self.src_info;
        let seed = self.expression(true)?;
        self.consume_newline()?;
        Some(EraRandomizeStmt { seed, src_info })
    }
    fn stmt_bar(&mut self, new_line: bool) -> Option<EraBarStmt> {
        let src_info = self.src_info;
        let value = self.expression(true)?;
        self.consume_comma()?;
        let max_value = self.expression(true)?;
        self.consume_comma()?;
        let length = self.expression(true)?;
        self.consume_newline()?;
        Some(EraBarStmt {
            value,
            max_value,
            length,
            new_line,
            src_info,
        })
    }
    fn stmt_addchara(&mut self) -> Option<EraAddCharaStmt> {
        let src_info = self.src_info;
        let mut charas = vec![self.expression(true)?];
        while self.matches_comma().is_some() {
            charas.push(self.expression(true)?);
        }
        self.consume_newline()?;
        Some(EraAddCharaStmt { charas, src_info })
    }
    fn stmt_pickupchara(&mut self) -> Option<EraPickUpCharaStmt> {
        let src_info = self.src_info;
        let mut charas = vec![self.expression(true)?];
        while self.matches_comma().is_some() {
            charas.push(self.expression(true)?);
        }
        self.consume_newline()?;
        Some(EraPickUpCharaStmt { charas, src_info })
    }
    fn stmt_delchara(&mut self) -> Option<EraDelCharaStmt> {
        let src_info = self.src_info;
        let mut charas = vec![self.expression(true)?];
        while self.matches_comma().is_some() {
            charas.push(self.expression(true)?);
        }
        self.consume_newline()?;
        Some(EraDelCharaStmt { charas, src_info })
    }
    fn stmt_swapchara(&mut self) -> Option<EraSwapCharaStmt> {
        let src_info = self.src_info;
        let chara1 = self.expression(true)?;
        self.consume_comma()?;
        let chara2 = self.expression(true)?;
        self.consume_newline()?;
        Some(EraSwapCharaStmt {
            chara1,
            chara2,
            src_info,
        })
    }
    fn stmt_addcopychara(&mut self) -> Option<EraAddCopyCharaStmt> {
        let src_info = self.src_info;
        let chara = self.expression(true)?;
        self.consume_newline()?;
        Some(EraAddCopyCharaStmt { chara, src_info })
    }
    fn stmt_reset_stain(&mut self) -> Option<EraResetStainStmt> {
        let src_info = self.src_info;
        let chara = self.expression(true)?;
        self.consume_newline()?;
        Some(EraResetStainStmt { chara, src_info })
    }
    fn stmt_savechara(&mut self) -> Option<EraSaveCharaStmt> {
        let src_info = self.src_info;
        let filename = self.expression(true)?;
        self.consume_comma()?;
        let memo = self.expression(true)?;
        self.consume_comma()?;
        let mut charas = vec![self.expression(true)?];
        while self.matches_comma().is_some() {
            charas.push(self.expression(true)?);
        }
        self.consume_newline()?;
        Some(EraSaveCharaStmt {
            filename,
            memo,
            charas,
            src_info,
        })
    }
    fn stmt_loadchara(&mut self) -> Option<EraLoadCharaStmt> {
        let src_info = self.src_info;
        let filename = self.expression(true)?;
        self.consume_newline()?;
        Some(EraLoadCharaStmt { filename, src_info })
    }
    fn stmt_setanimetimer(&mut self) -> Option<EraSetAnimeTimerStmt> {
        let src_info = self.src_info;
        let duration = self.expression(true)?;
        self.consume_newline()?;
        Some(EraSetAnimeTimerStmt { duration, src_info })
    }
    fn stmt_html_tagsplit(&mut self) -> Option<EraHtmlTagSplitStmt> {
        let src_info = self.src_info;
        let html = self.expression(true)?;
        let var_tags = if self.matches_comma().is_some() {
            self.var_expression()?
        } else {
            EraVarExpr {
                name: arcstr::literal!("RESULTS"),
                idxs: Vec::new(),
                src_info,
            }
        };
        let var_count = if self.matches_comma().is_some() {
            self.var_expression()?
        } else {
            EraVarExpr {
                name: arcstr::literal!("RESULT"),
                idxs: Vec::new(),
                src_info,
            }
        };
        self.consume_newline()?;
        Some(EraHtmlTagSplitStmt {
            html,
            var_count,
            var_tags,
            src_info,
        })
    }
    fn stmt_power(&mut self) -> Option<EraPowerStmt> {
        let src_info = self.src_info;
        let target = self.var_expression()?;
        self.consume_comma()?;
        let base = self.expression(true)?;
        self.consume_comma()?;
        let exponent = self.expression(true)?;
        self.consume_newline()?;
        Some(EraPowerStmt {
            target,
            base,
            exponent,
            src_info,
        })
    }
    fn stmt_savedata(&mut self) -> Option<EraSaveDataStmt> {
        let src_info = self.src_info;
        let save_id = self.expression(true)?;
        self.consume_comma()?;
        let save_info = self.expression(true)?;
        self.consume_newline()?;
        Some(EraSaveDataStmt {
            save_id,
            save_info,
            src_info,
        })
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
                self.handle_ignore_newline_tokens();
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
                self.handle_ignore_newline_tokens();
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
        match expr.try_evaluate_constant(&self.global_vars) {
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
    fn unwrap_str_literal(&mut self, lit: EraLiteral) -> Option<arcstr::ArcStr> {
        match lit {
            EraLiteral::Integer(_, cur_si) => {
                self.synchronize();
                self.report_err(cur_si, true, "expected string constant expression here");
                None
            }
            EraLiteral::String(x, _) => Some(x),
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
    #[must_use]
    fn unwrap_str_from_expression(&mut self, expr: EraExpr) -> Option<arcstr::ArcStr> {
        let lit = self.unwrap_literal_from_expression(expr)?;
        self.unwrap_str_literal(lit)
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
    #[must_use]
    fn try_recognize_printdata_cmd(cmd: &[u8]) -> Option<PrintExtendedFlags> {
        let cmd = cmd.to_ascii_uppercase();
        let mut cmd = cmd.as_slice();
        let mut flags = PrintExtendedFlags::new();
        cmd = cmd.strip_prefix(b"PRINTDATA")?;
        // Handle rest
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
        cmd.is_empty().then_some(flags)
    }
    // NOTE: If you need to take special care of newline token, don't use this function.
    #[must_use]
    fn peek_token(&mut self, mode: EraLexerMode) -> EraToken<'b> {
        loop {
            let token = self.lexer.peek(mode);
            if self.is_ignoring_newline() && matches!(token.kind, EraTokenKind::LineBreak) {
                self.lexer.read(mode);
                self.handle_ignore_newline_tokens();
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
                self.handle_ignore_newline_tokens();
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
        let name = String::from_utf8_lossy(name);
        if self
            .global_vars
            .get_var(&name)
            .map(|x| matches!(x.kind(), ValueKind::ArrStr | ValueKind::Str))
            .unwrap_or(false)
            || self
                .local_vars
                .get(CaselessStr::new(&name))
                .map(|x| x.is_string)
                .unwrap_or(false)
        {
            return true;
        }
        // HACK: Check for built-in string variables; use external lists in the future?
        let name = name.to_ascii_uppercase();
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
    fn str_with_escape(&mut self, raw_str: &[u8], src_info: SourcePosInfo) -> String {
        let mut unescaped = Vec::with_capacity(raw_str.len());
        let mut it = raw_str.into_iter();
        let mut has_redundant_escape = false;
        while let Some(&ch) = it.next() {
            if ch != b'\\' {
                unescaped.push(ch);
                continue;
            }
            // Handle escape
            let Some(&next_ch) = it.next() else {
                self.report_err(src_info, true, "invalid escape in string literal");
                unescaped.push(ch);
                break;
            };
            match next_ch {
                b'n' => unescaped.push(b'\n'),
                b'\\' => unescaped.push(b'\\'),
                b'"' => unescaped.push(b'"'),
                _ => {
                    has_redundant_escape = true;
                    unescaped.push(next_ch);
                }
            }
        }
        if has_redundant_escape {
            // self.report_token_err(
            //     first.into(),
            //     false,
            //     "unnecessary escape in string literal",
            // );
        }
        match String::from_utf8_lossy(&unescaped) {
            // SAFETY: We just checked that the string is valid UTF-8
            std::borrow::Cow::Borrowed(x) => unsafe { String::from_utf8_unchecked(unescaped) },
            std::borrow::Cow::Owned(x) => x,
        }
    }
}

enum EraCommandArgFmt {
    ExpressionS,
    Expression,
    RawString,
    RawStringForm,
}

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
        CmpL | CmpLEq | CmpG | CmpGEq => (17, 18),
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
        LBracket => (29, ()),
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
