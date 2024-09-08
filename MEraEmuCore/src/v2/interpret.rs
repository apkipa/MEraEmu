use bstr::ByteVec;
use cstree::syntax::{SyntaxElement, SyntaxElementRef};

use super::{ast::*, routines};
use crate::types::*;

use EraTokenKind as Token;

pub enum EraInterpretError {
    VarNotFound(SyntaxElement<Token>),
    Others,
}

pub struct EraInterpreter<'ctx, Callback> {
    ctx: &'ctx mut EraCompilerCtx<Callback>,
    is_const: bool,
}

impl<'ctx, Callback: EraCompilerCallback> EraInterpreter<'ctx, Callback> {
    pub fn new(ctx: &'ctx mut EraCompilerCtx<Callback>, is_const: bool) -> Self {
        Self { ctx, is_const }
    }

    pub fn get_ctx_mut(&mut self) -> &mut EraCompilerCtx<Callback> {
        self.ctx
    }

    pub fn interpret_expression(
        &mut self,
        expr: EraExprNodeOrLeaf,
    ) -> Result<ScalarValue, EraInterpretError> {
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
                            if !matches!(text.as_bytes(), [b'"', .., b'"']) {
                                // Invalid string literal; this should be caught by the lexer instead
                                unreachable!("invalid string literal: {:?}", text);
                            }
                            let text = &text[1..text.len() - 1];
                            let val = Vec::unescape_bytes(text).into_string_lossy();
                            ScalarValue::Str(val)
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
                        format!("true branch is of type {:?}", true_expr.kind()),
                    );
                    diag.span_note(
                        Default::default(),
                        false_expr_span,
                        format!("false branch is of type {:?}", false_expr.kind()),
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
                let args = x.arguments().ok_or(EraInterpretError::Others)?;
                let args = args
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
                        ScalarValue::Str(
                            std::char::from_u32(arg0 as _)
                                .unwrap_or(char::REPLACEMENT_CHARACTER)
                                .into(),
                        )
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

    pub fn interpret_int_expr(
        &mut self,
        expr: EraExprNodeOrLeaf,
    ) -> Result<i64, EraInterpretError> {
        let span = expr.src_span();
        let val = self.interpret_expression(expr)?.coerce_int();
        self.unwrap_int(val, span)
    }

    pub fn interpret_str_expr(
        &mut self,
        expr: EraExprNodeOrLeaf,
    ) -> Result<String, EraInterpretError> {
        let span = expr.src_span();
        let val = self.interpret_expression(expr)?.coerce_str();
        self.unwrap_str(val, span)
    }

    pub fn make_diag(&self) -> Diagnostic<'static> {
        Diagnostic::with_file(self.ctx.active_source.clone())
    }

    fn unwrap_int(&mut self, val: ScalarValue, span: SrcSpan) -> Result<i64, EraInterpretError> {
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

    fn unwrap_str(&mut self, val: ScalarValue, span: SrcSpan) -> Result<String, EraInterpretError> {
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

    fn resolve_variable_node(&mut self, name: EraIdentLeaf) -> Result<Value, EraInterpretError> {
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
            return Err(EraInterpretError::VarNotFound(token.clone().into()));
        };

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

    fn resolve_var_node_with_idx(
        &mut self,
        name: EraIdentLeaf,
        idxs: &[u32],
    ) -> Result<ScalarValue, EraInterpretError> {
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
