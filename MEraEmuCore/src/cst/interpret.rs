use bstr::ByteVec;
use cstree::interning::{Resolver, TokenKey};
use smallvec::smallvec;

use super::ast::*;
use crate::util::syntax::*;
use crate::v2::routines;
use crate::{
    types::*,
    util::{rcstr::ArcStr, Ascii},
};

use EraTokenKind as Token;

pub enum EraInterpretError<'a> {
    VarNotFound(SyntaxElementRef<'a, Token>),
    Others,
}

type InterpretResult<'a, T> = Result<T, EraInterpretError<'a>>;

pub struct EraInterpretedVarDecl {
    pub name_key: TokenKey,
    pub name_span: SrcSpan,
    pub var_info: EraVarInfo,
    pub is_ref: bool,
    pub is_const: bool,
    pub is_dynamic: bool,
}

pub struct EraInterpreter<'ctx, 'i, Callback> {
    ctx: &'ctx mut EraCompilerCtx<'i, Callback>,
    is_const: bool,
}

impl<'ctx, 'i, Callback: EraCompilerCallback> EraInterpreter<'ctx, 'i, Callback> {
    pub fn new(ctx: &'ctx mut EraCompilerCtx<'i, Callback>, is_const: bool) -> Self {
        Self { ctx, is_const }
    }

    pub fn get_ctx(&self) -> &EraCompilerCtx<'i, Callback> {
        self.ctx
    }

    pub fn get_ctx_mut(&mut self) -> &mut EraCompilerCtx<'i, Callback> {
        self.ctx
    }

    /// Interpret an expression node, with global context.
    pub fn interpret_expression<'a>(
        &mut self,
        expr: EraExprNodeOrLeaf<'a>,
    ) -> InterpretResult<'a, ScalarValue> {
        if !self.is_const {
            todo!("non-const expression evaluation is not supported yet");
        }

        let node = match expr {
            EraExprNodeOrLeaf::Node(node) => node,
            EraExprNodeOrLeaf::Leaf(leaf) => {
                return Ok({
                    let token = leaf.token();
                    let text = token.resolve_text(self.ctx.node_cache.interner());
                    match leaf.kind() {
                        Token::IntLiteral => {
                            let Some(val) = routines::parse_int_literal(text.as_bytes()) else {
                                let mut diag = self.make_diag();
                                diag.span_err(
                                    Default::default(),
                                    token.text_range().into(),
                                    "invalid integer literal",
                                );
                                self.ctx.emit_diag(diag);
                                return Err(EraInterpretError::Others);
                            };
                            ScalarValue::Int(val)
                        }
                        Token::StringLiteral => {
                            // if !matches!(text.as_bytes(), [b'"', .., b'"']) {
                            //     // Invalid string literal; this should be caught by the lexer instead
                            //     unreachable!("invalid string literal: {:?}", text);
                            // }
                            // let text = &text[1..text.len() - 1];
                            // let val = Vec::unescape_bytes(text).into_string_lossy();
                            // ScalarValue::Str(val)
                            ScalarValue::Str(routines::unwrap_str_literal(text))
                        }
                        Token::Identifier => {
                            self.resolve_var_node_with_idx(EraIdentLeaf::cast(token).unwrap(), &[])?
                        }
                        _ => unreachable!("invalid leaf kind: {:?}", leaf.kind()),
                    }
                });
            }
        };

        Ok(match node.kind() {
            EraExprNodeKind::PreUnaryExpr(x) => {
                let op = x.operator().ok_or(EraInterpretError::Others)?;
                let rhs = x.rhs().ok_or(EraInterpretError::Others)?;
                let rhs_span = rhs.src_span();
                let rhs = self.interpret_expression(rhs)?;
                match op.kind() {
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
                        let mut diag = self.make_diag();
                        diag.span_err(Default::default(), op.src_span(), "invalid unary operator");
                        self.ctx.emit_diag(diag);
                        return Err(EraInterpretError::Others);
                    }
                }
            }
            EraExprNodeKind::PostUnaryExpr(x) => {
                let op = x.operator().ok_or(EraInterpretError::Others)?;
                let lhs = x.lhs().ok_or(EraInterpretError::Others)?;
                let lhs_span = lhs.src_span();
                let lhs = self.interpret_expression(lhs)?;
                match op.kind() {
                    _ => {
                        let mut diag = self.make_diag();
                        diag.span_err(Default::default(), op.src_span(), "invalid unary operator");
                        self.ctx.emit_diag(diag);
                        return Err(EraInterpretError::Others);
                    }
                }
            }
            EraExprNodeKind::BinaryExpr(x) => {
                let op = x.operator().ok_or(EraInterpretError::Others)?;
                let lhs = x.lhs().ok_or(EraInterpretError::Others)?;
                let rhs = x.rhs().ok_or(EraInterpretError::Others)?;
                let lhs_span = lhs.src_span();
                let rhs_span = rhs.src_span();
                // NOTE: It is safe to evaluate early since we are in a const context
                // TODO: Implement short-circuiting for logical operators
                let lhs = self.interpret_expression(lhs)?;
                let rhs = self.interpret_expression(rhs)?;
                match op.kind() {
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
                                op.src_span(),
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
                                op.src_span(),
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
                        diag.span_err(Default::default(), op.src_span(), "invalid binary operator");
                        self.ctx.emit_diag(diag);
                        return Err(EraInterpretError::Others);
                    }
                }
            }
            EraExprNodeKind::TernaryExpr(x) => {
                let cond = x.condition().ok_or(EraInterpretError::Others)?;
                let true_expr = x.true_expr().ok_or(EraInterpretError::Others)?;
                let false_expr = x.false_expr().ok_or(EraInterpretError::Others)?;
                let cond_span = cond.src_span();
                let cond = self.interpret_expression(cond)?;
                let cond = self.unwrap_int(cond, cond_span)?;
                let true_expr_span = true_expr.src_span();
                let false_expr_span = false_expr.src_span();
                let true_expr = self.interpret_expression(true_expr)?;
                let false_expr = self.interpret_expression(false_expr)?;
                if true_expr.kind() != false_expr.kind() {
                    let mut diag = self.make_diag();
                    diag.span_err(
                        Default::default(),
                        x.src_span(),
                        "operands to ternary operator have incompatible types",
                    );
                    diag.span_note(
                        Default::default(),
                        true_expr_span,
                        format!("true branch is of type {}", true_expr.kind()),
                    );
                    diag.span_note(
                        Default::default(),
                        false_expr_span,
                        format!("false branch is of type {}", false_expr.kind()),
                    );
                    self.ctx.emit_diag(diag);
                    return Err(EraInterpretError::Others);
                }
                if cond != 0 {
                    true_expr
                } else {
                    false_expr
                }
            }
            EraExprNodeKind::FunCallExpr(x) => {
                let args_list = x.arguments().ok_or(EraInterpretError::Others)?;
                let args = args_list
                    .children()
                    .map(|x| {
                        let span = x.src_span();
                        self.interpret_expression(x).map(|x| (x, span))
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                let name = x.name().ok_or(EraInterpretError::Others)?;
                let name = name.resolve_text(self.ctx.node_cache.interner());
                match name.to_ascii_uppercase().as_str() {
                    "UNICODE" => {
                        if args.len() > 1 {
                            let mut diag = self.make_diag();
                            diag.span_warn(
                                Default::default(),
                                x.src_span(),
                                "too many arguments to function `UNICODE`",
                            );
                            self.ctx.emit_diag(diag);
                        }
                        let Ok([arg0]) = TryInto::<[_; 1]>::try_into(args) else {
                            let mut diag = self.make_diag();
                            diag.span_err(
                                Default::default(),
                                x.src_span(),
                                "missing argument to function `UNICODE`",
                            );
                            self.ctx.emit_diag(diag);
                            return Err(EraInterpretError::Others);
                        };
                        let arg0 = self.unwrap_int(arg0.0.coerce_int(), arg0.1)?;
                        ScalarValue::Str(routines::int_to_char(arg0).into())
                    }
                    "VARSIZE" => {
                        if args.len() > 2 {
                            let mut diag = self.make_diag();
                            diag.span_warn(
                                Default::default(),
                                x.src_span(),
                                "too many arguments to function `VARSIZE`",
                            );
                            self.ctx.emit_diag(diag);
                        }
                        let mut args = args.into_iter();
                        let Some(var_name) = args.next() else {
                            let mut diag = self.make_diag();
                            diag.span_err(
                                Default::default(),
                                x.src_span(),
                                "missing argument to function `VARSIZE`",
                            );
                            self.ctx.emit_diag(diag);
                            return Err(EraInterpretError::Others);
                        };
                        let var_name = self.unwrap_str(var_name.0.coerce_str(), var_name.1)?;
                        let Some(var_val) = self.ctx.variables.get_var(&var_name) else {
                            return Err(EraInterpretError::VarNotFound(
                                args_list.children().nth(0).unwrap().inner(),
                            ));
                        };
                        let result = if let Some(arg) = args.next() {
                            let dims = var_val.dims().unwrap();
                            let var_dim = self.unwrap_int(arg.0.coerce_int(), arg.1)?;
                            let var_dim = var_dim as usize;
                            match dims.get(var_dim) {
                                Some(x) => *x as _,
                                None => {
                                    let mut diag = self.make_diag();
                                    diag.span_err(
                                        Default::default(),
                                        arg.1,
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
                            x.src_span(),
                            format!("function `{name}` not defined"),
                        );
                        self.ctx.emit_diag(diag);
                        return Err(EraInterpretError::Others);
                    }
                }
            }
            EraExprNodeKind::ParenExpr(x) => {
                let expr = x.child().ok_or(EraInterpretError::Others)?;
                self.interpret_expression(expr)?
            }
            EraExprNodeKind::VarIdxExpr(x) => todo!(),
            EraExprNodeKind::VarNamespaceExpr(x) => todo!(),
            EraExprNodeKind::EmptyExpr(_) => ScalarValue::Empty,
            EraExprNodeKind::StringForm(x) => {
                let mut result = String::new();
                for part in x.parts() {
                    match part {
                        EraStringFormPartNodeOrLeaf::Node(x) => {
                            let expr = x.expr().ok_or(EraInterpretError::Others)?;
                            let expr_span = expr.src_span();
                            // TODO: width, alignment support
                            if x.width().is_some() || x.alignment().is_some() {
                                todo!("width and alignment support in string form");
                            }
                            let expr = self.interpret_expression(expr)?;
                            match expr {
                                ScalarValue::Int(x) => {
                                    result += itoa::Buffer::new().format(x);
                                }
                                ScalarValue::Str(x) => {
                                    result += &x;
                                }
                                _ => {
                                    let mut diag = self.make_diag();
                                    diag.span_err(
                                        Default::default(),
                                        expr_span,
                                        "invalid expression type in string form",
                                    );
                                    self.ctx.emit_diag(diag);
                                    return Err(EraInterpretError::Others);
                                }
                            }
                        }
                        EraStringFormPartNodeOrLeaf::Leaf(x) => {
                            let text = x.resolve_text(self.ctx.node_cache.interner());
                            let text = Vec::unescape_bytes(text).into_string_lossy();
                            result += &text;
                        }
                    }
                }
                ScalarValue::Str(result)
            }
        })
    }

    pub fn interpret_expr_ok(&mut self, expr: EraExprNodeOrLeaf) -> Option<ScalarValue> {
        match self.interpret_expression(expr) {
            Ok(x) => Some(x),
            Err(EraInterpretError::VarNotFound(var)) => {
                let var_name = var.resolve_text(self.ctx.node_cache.interner());
                let mut diag = self.make_diag();
                diag.span_err(
                    Default::default(),
                    var.src_span(),
                    format!("undefined variable `{var_name}`"),
                );
                self.ctx.emit_diag(diag);
                None
            }
            _ => None,
        }
    }

    pub fn interpret_int_expr<'a>(
        &mut self,
        expr: EraExprNodeOrLeaf<'a>,
    ) -> InterpretResult<'a, i64> {
        let span = expr.src_span();
        let val = self.interpret_expression(expr)?.coerce_int();
        self.unwrap_int(val, span)
    }

    pub fn interpret_str_expr<'a>(
        &mut self,
        expr: EraExprNodeOrLeaf<'a>,
    ) -> InterpretResult<'a, String> {
        let span = expr.src_span();
        let val = self.interpret_expression(expr)?.coerce_str();
        self.unwrap_str(val, span)
    }

    /// Interpret a variable declaration node, with global context. If the declaration is not
    /// a variable declaration, this function will will be a no-op and return an error
    /// `EraInterpretError::Others`.
    pub fn interpret_var_decl<'a>(
        &mut self,
        decl: EraDeclItemNode<'a>,
    ) -> InterpretResult<'a, EraInterpretedVarDecl> {
        let name_key;
        let name_span;
        let (is_ref, is_const, is_global, is_dynamic, is_savedata, is_charadata);

        let var_val = match decl.kind() {
            EraDeclItemNodeKind::VarDecl(decl) => {
                (name_key, name_span) = decl
                    .name()
                    .map(|x| (x.token().text_key().unwrap(), x.src_span()))
                    .ok_or(EraInterpretError::Others)?;
                (
                    is_ref,
                    is_const,
                    is_global,
                    is_dynamic,
                    is_savedata,
                    is_charadata,
                ) = (
                    decl.is_ref(),
                    decl.is_const(),
                    decl.is_global(),
                    decl.is_dynamic(),
                    decl.is_savedata(),
                    decl.is_charadata(),
                );
                let mut dims = if let Some(dims) = decl.dimensions() {
                    let dims = dims
                        .children()
                        .map(|x| {
                            let span = x.src_span();
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
                    dims
                } else {
                    smallvec![if is_ref { 0 } else { 1 }]
                };
                let inits = if let Some(inits) = decl.initializer() {
                    inits
                        .children()
                        .map(|x| self.interpret_int_expr(x).map(IntValue::new))
                        .collect::<Result<Vec<_>, _>>()?
                } else {
                    Vec::new()
                };

                // HACK: Handle CHARADATA variable dimensions
                if is_charadata {
                    dims.insert(0, crate::v2::engine::MAX_CHARA_COUNT);
                }

                Value::new_int_arr(dims, inits)
            }
            EraDeclItemNodeKind::VarSDecl(decl) => {
                (name_key, name_span) = decl
                    .name()
                    .map(|x| (x.token().text_key().unwrap(), x.src_span()))
                    .ok_or(EraInterpretError::Others)?;
                (
                    is_ref,
                    is_const,
                    is_global,
                    is_dynamic,
                    is_savedata,
                    is_charadata,
                ) = (
                    decl.is_ref(),
                    decl.is_const(),
                    decl.is_global(),
                    decl.is_dynamic(),
                    decl.is_savedata(),
                    decl.is_charadata(),
                );
                let mut dims = if let Some(dims) = decl.dimensions() {
                    let dims = dims
                        .children()
                        .map(|x| {
                            let span = x.src_span();
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
                    dims
                } else {
                    smallvec![if is_ref { 0 } else { 1 }]
                };
                let inits = if let Some(inits) = decl.initializer() {
                    inits
                        .children()
                        .map(|x| self.interpret_str_expr(x).map(|x| StrValue::new(x.into())))
                        .collect::<Result<Vec<_>, _>>()?
                } else {
                    Vec::new()
                };

                // HACK: Handle CHARADATA variable dimensions
                if is_charadata {
                    dims.insert(0, crate::v2::engine::MAX_CHARA_COUNT);
                }

                Value::new_str_arr(dims, inits)
            }
            // Ignore other kinds of declarations
            _ => return Err(EraInterpretError::Others),
        };

        if is_ref {
            if is_dynamic {
                let mut diag = self.make_diag();
                diag.span_err(
                    Default::default(),
                    decl.src_span(),
                    "REF variable cannot be DYNAMIC",
                );
                self.ctx.emit_diag(diag);
                return Err(EraInterpretError::Others);
            }
            if is_charadata {
                let mut diag = self.make_diag();
                diag.span_err(
                    Default::default(),
                    decl.src_span(),
                    "REF variable cannot be CHARADATA",
                );
                self.ctx.emit_diag(diag);
                return Err(EraInterpretError::Others);
            }
        }

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

    fn unwrap_int(&mut self, val: ScalarValue, span: SrcSpan) -> InterpretResult<'static, i64> {
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

    fn unwrap_str(&mut self, val: ScalarValue, span: SrcSpan) -> InterpretResult<'static, String> {
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

    fn resolve_variable_node<'a>(&mut self, name: EraIdentLeaf<'a>) -> InterpretResult<'a, Value> {
        let token = name.token();
        let name = name.resolve_text(self.ctx.node_cache.interner());

        let Some(var_info) = self.ctx.variables.get_var_info_by_name(name) else {
            // NOTE: Don't emit diagnostics here; let the caller decide and retry
            // let mut diag = self.make_diag();
            // diag.span_err(
            //     Default::default(),
            //     token.text_range().into(),
            //     format!("variable `{}` not found", name),
            // );
            // self.ctx.emit_diag(diag);
            return Err(EraInterpretError::VarNotFound(token.into()));
        };

        var_info.val.ensure_alloc();

        if self.is_const && !var_info.is_const {
            let mut diag = self.make_diag();
            diag.span_err(
                Default::default(),
                token.text_range().into(),
                format!("variable `{name}` cannot be used in constant context"),
            );
            self.ctx.emit_diag(diag);
            return Err(EraInterpretError::Others);
        }

        Ok(var_info.val.clone())
    }

    fn resolve_var_node_with_idx<'a>(
        &mut self,
        name: EraIdentLeaf<'a>,
        idxs: &[u32],
    ) -> InterpretResult<'a, ScalarValue> {
        let token = name.token();

        match self.resolve_variable_node(name)?.as_unpacked() {
            RefFlatValue::ArrInt(x) => {
                let x = x.borrow();
                let Some(x) = x.get(idxs) else {
                    let mut diag = self.make_diag();
                    diag.span_err(
                        Default::default(),
                        token.text_range().into(),
                        "index out of bounds",
                    );
                    self.ctx.emit_diag(diag);
                    return Err(EraInterpretError::Others);
                };
                Ok(ScalarValue::Int(x.val))
            }
            RefFlatValue::ArrStr(x) => {
                let x = x.borrow();
                let Some(x) = x.get(idxs) else {
                    let mut diag = self.make_diag();
                    diag.span_err(
                        Default::default(),
                        token.text_range().into(),
                        "index out of bounds",
                    );
                    self.ctx.emit_diag(diag);
                    return Err(EraInterpretError::Others);
                };
                Ok(ScalarValue::Str(x.val.as_str().to_owned()))
            }
            _ => unreachable!("invalid variable value type"),
        }
    }
}
