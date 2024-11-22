use bstr::ByteVec;
use cstree::interning::{Resolver, TokenKey};
use itertools::Itertools;
use smallvec::smallvec;

use super::parser::{
    EraNode, EraNodeArena, EraNodeDeclVarHomo, EraNodeExprTernary, EraNodeListExpr, EraNodeRef,
    EraNodeStringFormInterpPart,
};
use crate::v2::routines;
use crate::{
    types::*,
    util::{rcstr::ArcStr, Ascii},
};

use EraTokenKind as Token;

pub enum EraInterpretError {
    VarNotFound(String, SrcSpan),
    Others,
}

type InterpretResult<T> = Result<T, EraInterpretError>;

pub struct EraInterpretedVarDecl {
    pub name_key: TokenKey,
    pub name_span: SrcSpan,
    pub var_info: EraVarInfo,
    pub is_ref: bool,
    pub is_const: bool,
    pub is_dynamic: bool,
}

pub struct EraInterpreter<'ctx, 'i, 'n, Callback> {
    ctx: &'ctx mut EraCompilerCtx<'i, Callback>,
    node_arena: &'n EraNodeArena,
    is_const: bool,
}

impl<'ctx, 'i, 'n, Callback: EraCompilerCallback> EraInterpreter<'ctx, 'i, 'n, Callback> {
    pub fn new(
        ctx: &'ctx mut EraCompilerCtx<'i, Callback>,
        node_arena: &'n EraNodeArena,
        is_const: bool,
    ) -> Self {
        Self {
            ctx,
            node_arena,
            is_const,
        }
    }

    pub fn get_ctx(&self) -> &EraCompilerCtx<'i, Callback> {
        self.ctx
    }

    pub fn get_ctx_mut(&mut self) -> &mut EraCompilerCtx<'i, Callback> {
        self.ctx
    }

    /// Interpret an expression node, with global context.
    pub fn interpret_expression(&mut self, expr: EraNodeRef) -> InterpretResult<ScalarValue> {
        if !self.is_const {
            todo!("non-const expression evaluation is not supported yet");
        }

        let expr_span = self.node_arena.get_node_span(expr);

        Ok(match self.node_arena.get_node(expr) {
            EraNode::LiteralInt(x) => ScalarValue::Int(x.into()),
            EraNode::LiteralStr(x) => {
                ScalarValue::Str(self.ctx.node_cache.interner().resolve(x).to_owned())
            }
            EraNode::Identifier(_) | EraNode::ExprVarNamespace(..) => {
                self.resolve_var_node_with_idx(expr, &[])?
            }
            EraNode::ExprPreUnary(op, rhs) => {
                let rhs_span = self.node_arena.get_node_span(rhs);
                let rhs = self.interpret_expression(rhs)?;
                match op {
                    Token::Plus => {
                        let rhs = self.unwrap_int(rhs, rhs_span)?;
                        ScalarValue::Int(rhs)
                    }
                    Token::Minus => {
                        let rhs = self.unwrap_int(rhs, rhs_span)?;
                        ScalarValue::Int(rhs.wrapping_neg())
                    }
                    Token::LogicalNot => {
                        let rhs = self.unwrap_int(rhs, rhs_span)?;
                        ScalarValue::Int((rhs == 0) as _)
                    }
                    Token::BitNot => {
                        let rhs = self.unwrap_int(rhs, rhs_span)?;
                        ScalarValue::Int(!rhs)
                    }
                    _ => {
                        let op_span = self.node_arena.get_node_token_span(expr);
                        let mut diag = self.make_diag();
                        diag.span_err(Default::default(), op_span, "invalid unary operator");
                        self.ctx.emit_diag(diag);
                        return Err(EraInterpretError::Others);
                    }
                }
            }
            EraNode::ExprPostUnary(lhs, op) => {
                let lhs_span = self.node_arena.get_node_span(lhs);
                let lhs = self.interpret_expression(lhs)?;
                match op {
                    _ => {
                        let op_span = self.node_arena.get_node_token_span(expr);
                        let mut diag = self.make_diag();
                        diag.span_err(Default::default(), op_span, "invalid unary operator");
                        self.ctx.emit_diag(diag);
                        return Err(EraInterpretError::Others);
                    }
                }
            }
            EraNode::ExprBinary(lhs, op, rhs) => {
                let op = op.into();
                let op_span = self.node_arena.get_node_token_span(expr);
                let lhs_span = self.node_arena.get_node_span(lhs);
                let rhs_span = self.node_arena.get_node_span(rhs);
                let lhs = self.interpret_expression(lhs)?;
                let rhs = self.interpret_expression(rhs)?;
                match op {
                    Token::Plus => match (lhs, rhs) {
                        (ScalarValue::Int(lhs), ScalarValue::Int(rhs)) => {
                            ScalarValue::Int(lhs.wrapping_add(rhs))
                        }
                        (ScalarValue::Str(lhs), ScalarValue::Str(rhs)) => {
                            ScalarValue::Str(lhs + &rhs)
                        }
                        _ => {
                            let mut diag = self.make_diag();
                            diag.span_err(
                                Default::default(),
                                op_span,
                                "invalid operand types for binary operator",
                            );
                            self.ctx.emit_diag(diag);
                            return Err(EraInterpretError::Others);
                        }
                    },
                    Token::Minus => {
                        let lhs = self.unwrap_int(lhs, lhs_span)?;
                        let rhs = self.unwrap_int(rhs, rhs_span)?;
                        ScalarValue::Int(lhs.wrapping_sub(rhs))
                    }
                    Token::Multiply => match (lhs, rhs) {
                        (ScalarValue::Int(lhs), ScalarValue::Int(rhs)) => {
                            ScalarValue::Int(lhs.wrapping_mul(rhs))
                        }
                        (ScalarValue::Int(lhs), ScalarValue::Str(rhs)) => {
                            if lhs < 0 {
                                let mut diag = self.make_diag();
                                diag.span_err(
                                    Default::default(),
                                    lhs_span,
                                    "negative integer cannot be used as string multiplier",
                                );
                                self.ctx.emit_diag(diag);
                                return Err(EraInterpretError::Others);
                            }
                            ScalarValue::Str(rhs.repeat(lhs as _))
                        }
                        (ScalarValue::Str(lhs), ScalarValue::Int(rhs)) => {
                            if rhs < 0 {
                                let mut diag = self.make_diag();
                                diag.span_err(
                                    Default::default(),
                                    rhs_span,
                                    "negative integer cannot be used as string multiplier",
                                );
                                self.ctx.emit_diag(diag);
                                return Err(EraInterpretError::Others);
                            }
                            ScalarValue::Str(lhs.repeat(rhs as _))
                        }
                        _ => {
                            let mut diag = self.make_diag();
                            diag.span_err(
                                Default::default(),
                                op_span,
                                "invalid operand types for binary operator",
                            );
                            self.ctx.emit_diag(diag);
                            return Err(EraInterpretError::Others);
                        }
                    },
                    Token::Divide => {
                        let lhs = self.unwrap_int(lhs, lhs_span)?;
                        let rhs = self.unwrap_int(rhs, rhs_span)?;
                        if rhs == 0 {
                            let mut diag = self.make_diag();
                            diag.span_err(Default::default(), rhs_span, "division by zero");
                            self.ctx.emit_diag(diag);
                            return Err(EraInterpretError::Others);
                        }
                        ScalarValue::Int(lhs.wrapping_div(rhs))
                    }
                    Token::Percentage => {
                        let lhs = self.unwrap_int(lhs, lhs_span)?;
                        let rhs = self.unwrap_int(rhs, rhs_span)?;
                        if rhs == 0 {
                            let mut diag = self.make_diag();
                            diag.span_err(Default::default(), rhs_span, "division by zero");
                            self.ctx.emit_diag(diag);
                            return Err(EraInterpretError::Others);
                        }
                        ScalarValue::Int(lhs.wrapping_rem(rhs))
                    }
                    Token::BitAnd => {
                        let lhs = self.unwrap_int(lhs, lhs_span)?;
                        let rhs = self.unwrap_int(rhs, rhs_span)?;
                        ScalarValue::Int(lhs & rhs)
                    }
                    Token::BitOr => {
                        let lhs = self.unwrap_int(lhs, lhs_span)?;
                        let rhs = self.unwrap_int(rhs, rhs_span)?;
                        ScalarValue::Int(lhs | rhs)
                    }
                    Token::BitXor => {
                        let lhs = self.unwrap_int(lhs, lhs_span)?;
                        let rhs = self.unwrap_int(rhs, rhs_span)?;
                        ScalarValue::Int(lhs ^ rhs)
                    }
                    Token::LogicalAnd => {
                        let lhs = self.unwrap_int(lhs, lhs_span)?;
                        let rhs = self.unwrap_int(rhs, rhs_span)?;
                        ScalarValue::Int((lhs != 0 && rhs != 0) as _)
                    }
                    Token::LogicalOr => {
                        let lhs = self.unwrap_int(lhs, lhs_span)?;
                        let rhs = self.unwrap_int(rhs, rhs_span)?;
                        ScalarValue::Int((lhs != 0 || rhs != 0) as _)
                    }
                    Token::CmpEq => {
                        let lhs = self.unwrap_int(lhs, lhs_span)?;
                        let rhs = self.unwrap_int(rhs, rhs_span)?;
                        ScalarValue::Int((lhs == rhs) as _)
                    }
                    Token::CmpNEq => {
                        let lhs = self.unwrap_int(lhs, lhs_span)?;
                        let rhs = self.unwrap_int(rhs, rhs_span)?;
                        ScalarValue::Int((lhs != rhs) as _)
                    }
                    Token::CmpLT => {
                        let lhs = self.unwrap_int(lhs, lhs_span)?;
                        let rhs = self.unwrap_int(rhs, rhs_span)?;
                        ScalarValue::Int((lhs < rhs) as _)
                    }
                    Token::CmpLEq => {
                        let lhs = self.unwrap_int(lhs, lhs_span)?;
                        let rhs = self.unwrap_int(rhs, rhs_span)?;
                        ScalarValue::Int((lhs <= rhs) as _)
                    }
                    Token::CmpGT => {
                        let lhs = self.unwrap_int(lhs, lhs_span)?;
                        let rhs = self.unwrap_int(rhs, rhs_span)?;
                        ScalarValue::Int((lhs > rhs) as _)
                    }
                    Token::CmpGEq => {
                        let lhs = self.unwrap_int(lhs, lhs_span)?;
                        let rhs = self.unwrap_int(rhs, rhs_span)?;
                        ScalarValue::Int((lhs >= rhs) as _)
                    }
                    _ => {
                        let mut diag = self.make_diag();
                        diag.span_err(Default::default(), op_span, "invalid binary operator");
                        self.ctx.emit_diag(diag);
                        return Err(EraInterpretError::Others);
                    }
                }
            }
            EraNode::ExprTernary(..) => {
                let EraNodeExprTernary {
                    cond,
                    then_expr,
                    else_expr,
                } = EraNodeExprTernary::get_from(self.node_arena, expr);
                let cond_span = self.node_arena.get_node_span(cond);
                let cond = self.interpret_expression(cond)?;
                let cond = self.unwrap_int(cond, cond_span)?;
                let then_expr_span = self.node_arena.get_node_span(then_expr);
                let else_expr_span = self.node_arena.get_node_span(else_expr);
                let then_expr = self.interpret_expression(then_expr)?;
                let else_expr = self.interpret_expression(else_expr)?;
                if then_expr.kind() != else_expr.kind() {
                    let mut diag = self.make_diag();
                    diag.span_err(
                        Default::default(),
                        expr_span,
                        "operands to ternary operator have incompatible types",
                    );
                    diag.span_note(
                        Default::default(),
                        then_expr_span,
                        format!("true branch is of type {}", then_expr.kind()),
                    );
                    diag.span_note(
                        Default::default(),
                        else_expr_span,
                        format!("false branch is of type {}", else_expr.kind()),
                    );
                    self.ctx.emit_diag(diag);
                    return Err(EraInterpretError::Others);
                }
                if cond != 0 {
                    then_expr
                } else {
                    else_expr
                }
            }
            EraNode::ExprFunCall(name, args_list) => {
                // Evaluate arguments first
                let EraNodeListExpr {
                    children: args_list,
                } = EraNodeListExpr::get_from(self.node_arena, args_list);
                let args = args_list
                    .iter()
                    .map(|&x| {
                        let x = EraNodeRef(x);
                        let span = self.node_arena.get_node_span(x);
                        self.interpret_expression(x).map(|x| (x, span))
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                let name_span = self.node_arena.get_node_span(name);
                let EraNode::Identifier(name) = self.node_arena.get_node(name) else {
                    let mut diag = self.make_diag();
                    diag.span_err(Default::default(), name_span, "invalid function name");
                    self.ctx.emit_diag(diag);
                    return Err(EraInterpretError::Others);
                };
                let name = self.ctx.node_cache.interner().resolve(name);
                let args_count = args.len();
                match name.to_ascii_uppercase().as_str() {
                    "UNICODE" if args_count == 1 => {
                        let (arg0, arg0_span) = args.into_iter().next().unwrap();
                        let arg0 = self.unwrap_int(arg0, arg0_span)?;
                        ScalarValue::Str(routines::int_to_char(arg0).into())
                    }
                    "VARSIZE" if (1..2).contains(&args_count) => {
                        let mut args = args.into_iter();
                        let (var_name, var_name_span) = args.next().unwrap();
                        let var_name = self.unwrap_str(var_name, var_name_span)?;
                        let Some(var_val) = self.ctx.variables.get_var(&var_name) else {
                            let mut diag = self.make_diag();
                            diag.span_err(Default::default(), var_name_span, "variable not found");
                            self.ctx.emit_diag(diag);
                            return Err(EraInterpretError::VarNotFound(var_name, var_name_span));
                        };
                        let result = if let Some((arg, arg_span)) = args.next() {
                            let dims = var_val.dims().unwrap();
                            let var_dim = self.unwrap_int(arg, arg_span)?;
                            let var_dim = var_dim as usize;
                            match dims.get(var_dim) {
                                Some(x) => *x as _,
                                None => {
                                    let mut diag = self.make_diag();
                                    diag.span_err(
                                        Default::default(),
                                        arg_span,
                                        "variable dimension out of bounds",
                                    );
                                    self.ctx.emit_diag(diag);
                                    return Err(EraInterpretError::Others);
                                }
                            }
                        } else {
                            *var_val.dims().unwrap().last().unwrap() as _
                        };
                        ScalarValue::Int(result)
                    }
                    _ => {
                        let mut diag = self.make_diag();
                        diag.span_err(
                            Default::default(),
                            name_span,
                            format!(
                                "function `{name}` is not defined or has no matching overloads"
                            ),
                        );
                        self.ctx.emit_diag(diag);
                        return Err(EraInterpretError::Others);
                    }
                }
            }
            EraNode::ExprParen(x) => self.interpret_expression(x)?,
            EraNode::ExprVarIdx(..) => todo!(),
            EraNode::Empty => ScalarValue::Empty,
            EraNode::StringForm(x) => {
                let parts = self.node_arena.get_small_extra_data_view(x);
                let mut result = String::new();
                for part in parts.iter().map(|x| EraNodeRef(*x)) {
                    let part_span = self.node_arena.get_node_span(part);
                    let part = if let Some(part) =
                        EraNodeStringFormInterpPart::try_get_from(self.node_arena, part)
                    {
                        // TODO: width, alignment support
                        if part.width.is_some() || part.alignment.is_some() {
                            let mut diag = self.make_diag();
                            diag.span_err(
                                Default::default(),
                                part_span,
                                "width and alignment are not supported yet",
                            );
                            self.ctx.emit_diag(diag);
                            return Err(EraInterpretError::Others);
                        }
                        part.expr
                    } else {
                        part
                    };
                    let part_span = self.node_arena.get_node_span(part);
                    match self.interpret_expression(part)? {
                        ScalarValue::Int(x) => result.push_str(&x.to_string()),
                        ScalarValue::Str(x) => result.push_str(&x),
                        x => {
                            let mut diag = self.make_diag();
                            diag.span_err(
                                Default::default(),
                                part_span,
                                format!("invalid string part value: {:?}", x),
                            );
                            self.ctx.emit_diag(diag);
                            return Err(EraInterpretError::Others);
                        }
                    }
                }
                ScalarValue::Str(result)
            }
            _ => {
                let mut diag = self.make_diag();
                diag.span_err(
                    Default::default(),
                    expr_span,
                    format!("invalid expression node: {:?}", expr),
                );
                self.ctx.emit_diag(diag);
                return Err(EraInterpretError::Others);
            }
        })
    }

    pub fn interpret_expr_ok(&mut self, expr: EraNodeRef) -> Option<ScalarValue> {
        match self.interpret_expression(expr) {
            Ok(x) => Some(x),
            Err(EraInterpretError::VarNotFound(var_name, var_span)) => {
                let mut diag = self.make_diag();
                diag.span_err(
                    Default::default(),
                    var_span,
                    format!("undefined variable `{var_name}`"),
                );
                self.ctx.emit_diag(diag);
                None
            }
            _ => None,
        }
    }

    pub fn interpret_int_expr(&mut self, expr: EraNodeRef) -> InterpretResult<i64> {
        let span = self.node_arena.get_node_span(expr);
        let val = self.interpret_expression(expr)?.coerce_int();
        self.unwrap_int(val, span)
    }

    pub fn interpret_str_expr(&mut self, expr: EraNodeRef) -> InterpretResult<String> {
        let span = self.node_arena.get_node_span(expr);
        let val = self.interpret_expression(expr)?.coerce_str();
        self.unwrap_str(val, span)
    }

    pub fn interpret_int_expr_ok(&mut self, expr: EraNodeRef) -> Option<i64> {
        let span = self.node_arena.get_node_span(expr);
        let val = self.interpret_expr_ok(expr)?.coerce_int();
        self.unwrap_int(val, span).ok()
    }

    pub fn interpret_str_expr_ok(&mut self, expr: EraNodeRef) -> Option<String> {
        let span = self.node_arena.get_node_span(expr);
        let val = self.interpret_expr_ok(expr)?.coerce_str();
        self.unwrap_str(val, span).ok()
    }

    /// Interpret a variable declaration node, with global context. If the declaration is not
    /// a variable declaration, this function will will be a no-op and return an error
    /// `EraInterpretError::Others`.
    pub fn interpret_var_decl(
        &mut self,
        decl: EraNodeRef,
    ) -> InterpretResult<EraInterpretedVarDecl> {
        let decl_span = self.node_arena.get_node_span(decl);
        let Some(EraNodeDeclVarHomo {
            is_string,
            name,
            qualifiers,
            dimensions,
            initializers,
        }) = EraNodeDeclVarHomo::try_get_from(self.node_arena, decl)
        else {
            let mut diag = self.make_diag();
            diag.span_err(Default::default(), decl_span, "not a variable declaration");
            self.ctx.emit_diag(diag);
            return Err(EraInterpretError::Others);
        };

        // Name
        let name_span = self.node_arena.get_node_span(name);
        let EraNode::Identifier(name_key) = self.node_arena.get_node(name) else {
            unreachable!("invalid variable name node");
        };

        // Qualifiers
        let mut is_ref = false;
        let mut is_const = false;
        let mut is_global = false;
        let mut is_dynamic = false;
        let mut is_savedata = false;
        let mut is_charadata = false;
        for qualifier in qualifiers.iter().map(|&x| EraNodeRef(x)) {
            let qualifier_span = self.node_arena.get_node_span(qualifier);
            let EraNode::VarModifier(qualifier) = self.node_arena.get_node(qualifier) else {
                let mut diag = self.make_diag();
                diag.span_err(
                    Default::default(),
                    qualifier_span,
                    "invalid variable qualifier",
                );
                self.ctx.emit_diag(diag);
                return Err(EraInterpretError::Others);
            };
            match qualifier {
                Token::KwRef => {
                    is_ref = true;
                }
                Token::KwConst => {
                    is_const = true;
                }
                Token::KwGlobal => {
                    is_global = true;
                }
                Token::KwDynamic => {
                    is_dynamic = true;
                }
                Token::KwSavedata => {
                    is_savedata = true;
                }
                Token::KwCharadata => {
                    is_charadata = true;
                }
                _ => {
                    let mut diag = self.make_diag();
                    diag.span_err(
                        Default::default(),
                        qualifier_span,
                        "invalid variable qualifier",
                    );
                    self.ctx.emit_diag(diag);
                    return Err(EraInterpretError::Others);
                }
            }
        }

        // Dimensions
        let EraNode::ListExpr(dims) = self.node_arena.get_node(dimensions) else {
            unreachable!("invalid variable dimensions node");
        };
        let dims = self.node_arena.get_extra_data_view(dims);
        let mut dims = dims
            .iter()
            .map(|&x| EraNodeRef(x))
            .map(|x| {
                let span = self.node_arena.get_node_span(x);
                self.interpret_int_expr(x).and_then(|x| {
                    if is_ref && x != 0 {
                        let mut diag = self.make_diag();
                        diag.span_err(
                            Default::default(),
                            span,
                            "REF variable dimension must be zero or omitted",
                        );
                        self.ctx.emit_diag(diag);
                        return Err(EraInterpretError::Others);
                    }
                    if !is_ref && x == 0 {
                        let mut diag = self.make_diag();
                        diag.span_err(
                            Default::default(),
                            span,
                            "non-REF variable dimension cannot be zero or omitted",
                        );
                        self.ctx.emit_diag(diag);
                        return Err(EraInterpretError::Others);
                    }
                    x.try_into().map_err(|_| {
                        let mut diag = self.make_diag();
                        diag.span_err(
                            Default::default(),
                            span,
                            "expression does not evaluate to a valid variable dimension",
                        );
                        self.ctx.emit_diag(diag);
                        EraInterpretError::Others
                    })
                })
            })
            .collect::<Result<EraVarDims, _>>()?;
        let omitted_dims = dims.is_empty();
        if omitted_dims {
            dims.push(if is_ref { 0 } else { 1 });
        }
        if is_ref {
            if is_dynamic {
                let mut diag = self.make_diag();
                diag.span_err(
                    Default::default(),
                    decl_span,
                    "REF variable cannot be DYNAMIC",
                );
                self.ctx.emit_diag(diag);
                return Err(EraInterpretError::Others);
            }
            if is_charadata {
                let mut diag = self.make_diag();
                diag.span_err(
                    Default::default(),
                    decl_span,
                    "REF variable cannot be CHARADATA",
                );
                self.ctx.emit_diag(diag);
                return Err(EraInterpretError::Others);
            }
        }
        // HACK: Handle CHARADATA variable dimensions
        if is_charadata {
            dims.insert(0, crate::v2::engine::MAX_CHARA_COUNT);
        }

        // Initializers (& value)
        let EraNode::ListExpr(inits) = self.node_arena.get_node(initializers) else {
            unreachable!("invalid variable initializers node");
        };
        let var_val = if is_string {
            let inits: Vec<_> = self
                .node_arena
                .get_extra_data_view(inits)
                .iter()
                .map(|&x| EraNodeRef(x))
                .map(|x| self.interpret_str_expr(x).map(|x| StrValue::new(x.into())))
                .try_collect()?;
            if inits.len() > 0 {
                if is_ref {
                    let mut diag = self.make_diag();
                    diag.span_err(
                        Default::default(),
                        decl_span,
                        "REF variable cannot have initializers",
                    );
                    self.ctx.emit_diag(diag);
                    return Err(EraInterpretError::Others);
                }
                if omitted_dims {
                    *dims.last_mut().unwrap() = inits.len().max(1) as _;
                }
            }
            Value::new_str_arr(dims, inits)
        } else {
            let inits: Vec<_> = self
                .node_arena
                .get_extra_data_view(inits)
                .iter()
                .map(|&x| EraNodeRef(x))
                .map(|x| self.interpret_int_expr(x).map(IntValue::new))
                .try_collect()?;
            if inits.len() > 0 {
                if is_ref {
                    let mut diag = self.make_diag();
                    diag.span_err(
                        Default::default(),
                        decl_span,
                        "REF variable cannot have initializers",
                    );
                    self.ctx.emit_diag(diag);
                    return Err(EraInterpretError::Others);
                }
                if omitted_dims {
                    *dims.last_mut().unwrap() = inits.len().max(1) as _;
                }
            }
            Value::new_int_arr(dims, inits)
        };

        // Create variable
        Ok(EraInterpretedVarDecl {
            name_key,
            name_span,
            var_info: EraVarInfo {
                name: Ascii::new(self.ctx.node_cache.interner().resolve(name_key).into()),
                val: var_val,
                is_const,
                is_charadata,
                is_global,
                never_trap: true,
            },
            is_ref,
            is_const,
            is_dynamic,
        })
    }

    pub fn make_diag(&self) -> Diagnostic<'static> {
        Diagnostic::with_file(self.ctx.active_source.clone())
    }

    fn unwrap_int(&mut self, val: ScalarValue, span: SrcSpan) -> InterpretResult<i64> {
        match val {
            ScalarValue::Int(x) => Ok(x),
            _ => {
                let mut diag = self.make_diag();
                diag.span_err(
                    Default::default(),
                    span,
                    "expression does not evaluate to integer",
                );
                self.ctx.emit_diag(diag);
                Err(EraInterpretError::Others)
            }
        }
    }

    fn unwrap_str(&mut self, val: ScalarValue, span: SrcSpan) -> InterpretResult<String> {
        match val {
            ScalarValue::Str(x) => Ok(x),
            _ => {
                let mut diag = self.make_diag();
                diag.span_err(
                    Default::default(),
                    span,
                    "expression does not evaluate to string",
                );
                self.ctx.emit_diag(diag);
                Err(EraInterpretError::Others)
            }
        }
    }

    fn resolve_variable_node(&mut self, name: EraNodeRef) -> InterpretResult<Value> {
        let name_span = self.node_arena.get_node_span(name);
        let name_token = match self.node_arena.get_node(name) {
            EraNode::Identifier(x) => x,
            EraNode::ExprVarNamespace(..) => {
                let mut diag = self.make_diag();
                diag.span_err(
                    Default::default(),
                    name_span,
                    "namespaced variable not yet supported",
                );
                self.ctx.emit_diag(diag);
                return Err(EraInterpretError::Others);
            }
            _ => {
                let mut diag = self.make_diag();
                diag.span_err(Default::default(), name_span, "invalid variable name node");
                self.ctx.emit_diag(diag);
                return Err(EraInterpretError::Others);
            }
        };
        let name = self.ctx.node_cache.interner().resolve(name_token);

        let Some(var_info) = self.ctx.variables.get_var_info_by_name(name) else {
            // NOTE: Don't emit diagnostics here; let the caller decide and retry
            // let mut diag = self.make_diag();
            // diag.span_err(
            //     Default::default(),
            //     token.text_range().into(),
            //     format!("variable `{}` not found", name),
            // );
            // self.ctx.emit_diag(diag);
            return Err(EraInterpretError::VarNotFound(name.into(), name_span));
        };

        var_info.val.ensure_alloc();

        if self.is_const && !var_info.is_const {
            let mut diag = self.make_diag();
            diag.span_err(
                Default::default(),
                name_span,
                format!("variable `{name}` cannot be used in constant context"),
            );
            self.ctx.emit_diag(diag);
            return Err(EraInterpretError::Others);
        }

        Ok(var_info.val.clone())
    }

    fn resolve_var_node_with_idx(
        &mut self,
        name: EraNodeRef,
        idxs: &[u32],
    ) -> InterpretResult<ScalarValue> {
        let name_span = self.node_arena.get_node_span(name);

        match self.resolve_variable_node(name)?.as_unpacked() {
            RefFlatValue::ArrInt(x) => {
                let x = x.borrow();
                let Some(x) = x.get(idxs) else {
                    let mut diag = self.make_diag();
                    diag.span_err(Default::default(), name_span, "index out of bounds");
                    self.ctx.emit_diag(diag);
                    return Err(EraInterpretError::Others);
                };
                Ok(ScalarValue::Int(x.val))
            }
            RefFlatValue::ArrStr(x) => {
                let x = x.borrow();
                let Some(x) = x.get(idxs) else {
                    let mut diag = self.make_diag();
                    diag.span_err(Default::default(), name_span, "index out of bounds");
                    self.ctx.emit_diag(diag);
                    return Err(EraInterpretError::Others);
                };
                Ok(ScalarValue::Str(x.val.as_str().to_owned()))
            }
            _ => unreachable!("invalid variable value type"),
        }
    }
}
