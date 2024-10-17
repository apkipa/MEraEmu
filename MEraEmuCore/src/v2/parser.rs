use std::ops::{ControlFlow, DerefMut};

use super::lexer::{EraLexer, EraLexerMode, EraLexerNextResult};
use crate::{
    types::*,
    util::{
        rcstr::{self, ArcStr},
        Ascii,
    },
    v2::routines,
};
use bstr::ByteSlice;
use cstree::{
    build::{Checkpoint, GreenNodeBuilder, NodeCache},
    green::GreenNode,
    interning::Interner,
    Syntax,
};
use hashbrown::{HashMap, HashSet};
use indexmap::IndexMap;
use itertools::Itertools;
use rustc_hash::FxBuildHasher;
use EraLexerMode as Mode;
use EraTokenKind as Token;

type FxHashMap<K, V> = HashMap<K, V, FxBuildHasher>;
type FxIndexMap<K, V> = IndexMap<K, V, FxBuildHasher>;

type ParseResult<T> = Result<T, ()>;

pub struct EraParser<'a, 'b, 'ctx, 'i, Callback> {
    lexer: &'b mut EraLexer<'a, 'ctx, Callback>,
    node_cache: &'b mut NodeCache<'i>,
    is_header: bool,
}

impl<'a, 'b, 'ctx, 'i, Callback: EraCompilerCallback> EraParser<'a, 'b, 'ctx, 'i, Callback> {
    pub fn new(
        lexer: &'b mut EraLexer<'a, 'ctx, Callback>,
        node_cache: &'b mut NodeCache<'i>,
        is_header: bool,
    ) -> Self {
        EraParser {
            lexer,
            node_cache,
            is_header,
        }
    }

    pub fn parse_program(&mut self) -> (GreenNode, EraMacroMap) {
        let builder = GreenNodeBuilder::with_cache(self.node_cache);
        let mut site = EraParserSite::new(self.lexer, builder, self.is_header);
        // NOTE: We use error-tolerant parsing here, so theoretically this should never fail.
        site.program().unwrap();
        let (builder, macro_map) = site.into_inner();
        (builder.finish().0, macro_map)
    }
}

struct EraParserOuter<'a, 'b, 'ctx, 'cache, 'i, Callback> {
    // lexer
    l: &'b mut EraLexer<'a, 'ctx, Callback>,
    // builder
    b: GreenNodeBuilder<'cache, 'i, EraTokenKind>,
    is_header: bool,
    is_panicking: bool,
    macro_place: EraParserMacroPlace<'a>,
}

// ((macro_span, covering_span, start_pos), macro_map, src)
#[derive(Debug, Default)]
struct EraParserMacroPlace<'src>(Option<(SrcSpan, SrcSpan, u32)>, EraMacroMap, &'src str);

impl EraParserMacroPlace<'_> {
    fn handle_macro_token(&mut self, result: &EraLexerNextResult, start_pos: u32, end_pos: u32) {
        if !result.is_replaced && self.0.is_none() {
            return;
        }

        // Extend macro place
        let token_span = result.token.span;
        let extend_fn = |span: SrcSpan| {
            let start = span.start().min(token_span.start());
            let end = span.end().max(token_span.end());
            SrcSpan::with_ends(start, end)
        };
        if let Some((macro_span, covering_span, _)) = self.0.as_mut() {
            if result.is_replaced {
                *macro_span = extend_fn(*macro_span);
            }
            *covering_span = extend_fn(*covering_span);
        } else {
            self.0 = Some((token_span, token_span, start_pos));
        }

        // Close over macro if needed
        self.close_over_macro(end_pos);
    }

    fn has_macro(&self) -> bool {
        self.0.is_some()
    }

    fn is_closed_over(&self) -> bool {
        self.0.map_or(false, |(macro_span, covering_span, _)| {
            // Ensure that we have left the macro region (avoids repeated macro tokens)
            // NOTE: No equal here because then we won't be able to tell if we have
            //       left the macro region.
            covering_span.end() > macro_span.end()
        })
    }

    fn close_over_macro(&mut self, end_pos: u32) -> bool {
        if !self.is_closed_over() {
            return false;
        }

        let (macro_span, covering_span, start_pos) = self.0.take().unwrap();
        let in_span = SrcSpan::with_ends(SrcPos(start_pos), SrcPos(end_pos));
        let text = &self.2[covering_span.start().0 as usize..covering_span.end().0 as usize];
        self.1.push(in_span, text.into());

        true
    }
}

impl<'a, 'b, 'ctx, 'cache, 'i, Callback: EraCompilerCallback>
    EraParserOuter<'a, 'b, 'ctx, 'cache, 'i, Callback>
{
    fn new(
        l: &'b mut EraLexer<'a, 'ctx, Callback>,
        b: GreenNodeBuilder<'cache, 'i, EraTokenKind>,
        is_header: bool,
    ) -> Self {
        let mut macro_place = EraParserMacroPlace::default();
        macro_place.2 = l.get_src();
        EraParserOuter {
            l,
            b,
            is_header,
            is_panicking: false,
            macro_place,
        }
    }

    fn into_inner(self) -> (GreenNodeBuilder<'cache, 'i, EraTokenKind>, EraMacroMap) {
        (self.b, self.macro_place.1)
    }

    fn set_is_panicking(&mut self, is_panicking: bool) {
        self.is_panicking = is_panicking;
    }

    fn emit_diag(&mut self, diag: Diagnostic<'a>) {
        if self.is_panicking {
            diag.cancel();
            return;
        }
        self.l.emit_diag(diag);
    }

    fn next_token(&mut self, mode: EraLexerMode) -> EraLexerNextResult {
        // SAFETY: This requires the Polonius borrow checker.
        let result: EraLexerNextResult = unsafe { std::mem::transmute(self.peek_token(mode)) };
        if result.token.kind != Token::LineBreak {
            self.next_token_with_newline(mode)
        } else {
            result
        }
    }

    fn next_token_with_newline(&mut self, mode: EraLexerMode) -> EraLexerNextResult {
        loop {
            // SAFETY: This requires the Polonius borrow checker.
            let result: EraLexerNextResult = unsafe { std::mem::transmute(self.l.read(mode)) };
            let start_pos = self.b.document_len().into();
            self.b.token(result.token.kind, result.lexeme);
            let end_pos = self.b.document_len().into();
            self.macro_place
                .handle_macro_token(&result, start_pos, end_pos);
            if !matches!(result.token.kind, Token::WhiteSpace | Token::Comment) {
                break result;
            }
        }
    }

    fn peek_token(&mut self, mode: EraLexerMode) -> EraLexerNextResult {
        loop {
            // SAFETY: This requires the Polonius borrow checker.
            let result: EraLexerNextResult = unsafe { std::mem::transmute(self.l.peek(mode)) };
            if !matches!(result.token.kind, Token::WhiteSpace | Token::Comment) {
                break result;
            }
            let result = self.l.read(mode);
            let start_pos = self.b.document_len().into();
            self.b.token(result.token.kind, result.lexeme);
            let end_pos = self.b.document_len().into();
            self.macro_place
                .handle_macro_token(&result, start_pos, end_pos);
        }
    }

    fn bump(&mut self) -> EraLexerNextResult {
        let result = self.l.bump();
        let start_pos = self.b.document_len().into();
        self.b.token(result.token.kind, result.lexeme);
        let end_pos = self.b.document_len().into();
        self.macro_place
            .handle_macro_token(&result, start_pos, end_pos);
        result
    }

    fn bump_as(&mut self, kind: Token) -> EraLexerNextResult {
        let result = self.l.bump();
        let start_pos = self.b.document_len().into();
        self.b.token(kind, result.lexeme);
        let end_pos = self.b.document_len().into();
        self.macro_place
            .handle_macro_token(&result, start_pos, end_pos);
        result
    }

    fn previous_token(&self) -> Token {
        self.l.previous_token()
    }

    // fn close_over_macro(&mut self) -> bool {
    //     if !self.macro_place.is_closed_over() {
    //         return false;
    //     }
    //     let Some((macro_span, covering_span)) = self.macro_place.0.take() else {
    //         return false;
    //     };

    //     // Replace current node with a macro node
    //     let node = self.b.pop_last_child().unwrap().into_node().unwrap();
    //     let start_pos = self.b.document_len().into();
    //     // TODO: Should be handled by parent covering node
    //     assert!(
    //         covering_span.start() >= start_pos,
    //         "macro covering span starts before node"
    //     );
    //     let covering_span = SrcSpan::with_ends(SrcPos(start_pos), covering_span.end());
    //     if node.kind() == Token::Invalid.into_raw() {
    //         self.b.start_node(Token::Invalid);
    //     } else {
    //         self.b.start_node(Token::MacroNode);
    //     }
    //     self.b
    //         .token(Token::MacroNode, self.l.get_src_span(covering_span));
    //     self.b.finish_node();

    //     // Insert macro node into map
    //     self.macro_map.insert(covering_span, node);

    //     true
    // }
}

struct EraParserSite<'a, 'b, 'ctx, 'cache, 'i, Callback> {
    o: EraParserOuter<'a, 'b, 'ctx, 'cache, 'i, Callback>,
    base_diag: Diagnostic<'a>,
    // eat_syncs: Vec<(Mode, Token)>,
    local_str_vars: HashSet<Ascii<ArcStr>, FxBuildHasher>,
}

impl<'a, 'b, 'ctx, 'cache, 'i, Callback: EraCompilerCallback>
    EraParserSite<'a, 'b, 'ctx, 'cache, 'i, Callback>
{
    fn new(
        l: &'b mut EraLexer<'a, 'ctx, Callback>,
        b: GreenNodeBuilder<'cache, 'i, EraTokenKind>,
        is_header: bool,
    ) -> Self {
        let base_diag = l.make_diag();
        let o = EraParserOuter::new(l, b, is_header);
        EraParserSite {
            o,
            base_diag,
            // eat_syncs: Vec::new(),
            local_str_vars: HashSet::default(),
        }
    }

    fn into_inner(self) -> (GreenNodeBuilder<'cache, 'i, EraTokenKind>, EraMacroMap) {
        self.o.into_inner()
    }

    /// `{NODE OWN}`
    fn program(&mut self) -> ParseResult<()> {
        self.o.b.start_node(Token::Program);
        self.skip_newline();
        while self.o.peek_token(Mode::Normal).token.kind != Token::Eof {
            // Design note:
            //
            // If `declaration` fails, it has failed to produce a node, and we need to wrap
            // everything in an Invalid node. If successful, it has already produced a
            // valid node (potentially containing Invalid nodes), and no action is needed.
            let cp = self.o.b.checkpoint();
            if self.declaration(cp).is_err() {
                _ = self.sync_cp_to(Terminal::LineBreak.into(), cp);
            } else {
                // After a successful declaration, a newline is expected.
                _ = self.expect_sync_to_newline();
                // let token = self.o.peek_token(Mode::Normal).token;
                // if token.kind != Token::LineBreak {
                //     let mut diag = self.base_diag.clone();
                //     diag.span_err(
                //         Default::default(),
                //         token.span,
                //         format!("expected newline, found {:?}", token.kind),
                //     );
                //     self.o.emit_diag(diag);
                //     _ = self.sync_to(Terminal::LineBreak.into());
                // }
            }
            self.skip_newline();
            self.o.set_is_panicking(false);
        }
        self.o.b.finish_node();
        // TODO: Force close over macros at EOF?
        // self.o.close_over_macro();
        Ok(())
    }

    /// `{NODE OWN}`
    fn declaration(&mut self, cp: Checkpoint) -> ParseResult<()> {
        if self.o.peek_token(Mode::Normal).token.kind == Token::NumberSign {
            self.sharp_declaration(cp)
        } else {
            self.func_declaration(cp)
        }
    }

    /// `{NODE OWN}`
    fn sharp_declaration(&mut self, cp: Checkpoint) -> ParseResult<()> {
        self.eat(Mode::Normal, Token::NumberSign)?;

        let token = self.o.peek_token(Mode::SharpDecl).token;
        let result = match token.kind {
            Token::KwDim => {
                self.o.bump();
                self.o.b.start_node_at(cp, Token::VarDecl);
                self.var_declaration(false)
            }
            Token::KwDimS => {
                self.o.bump();
                self.o.b.start_node_at(cp, Token::VarSDecl);
                self.var_declaration(true)
            }
            Token::KwLocalSize => {
                self.o.bump();
                self.o.b.start_node_at(cp, Token::LocalSizeDecl);
                self.safe_expression(true, Terminal::LineBreak.into())
                    .map(|_| ())
            }
            Token::KwLocalSSize => {
                self.o.bump();
                self.o.b.start_node_at(cp, Token::LocalSSizeDecl);
                self.safe_expression(true, Terminal::LineBreak.into())
                    .map(|_| ())
            }
            Token::KwFunction => {
                self.o.bump();
                self.o.b.start_node_at(cp, Token::FunctionDecl);
                Ok(())
            }
            Token::KwFunctionS => {
                self.o.bump();
                self.o.b.start_node_at(cp, Token::FunctionSDecl);
                Ok(())
            }
            Token::KwOnly | Token::KwPri | Token::KwLater => {
                self.o.bump();
                self.o.b.start_node_at(cp, Token::EventKindDecl);
                Ok(())
            }
            Token::KwDefine => {
                self.o.bump();
                self.o.b.start_node_at(cp, Token::DefineDecl);
                self.define_declaration()
            }
            Token::KwTransient => {
                self.o.bump();
                self.o.b.start_node_at(cp, Token::TransientDecl);
                Ok(())
            }
            _ => {
                let mut diag = self.base_diag.clone();
                diag.span_err(
                    Default::default(),
                    token.span,
                    format!("expected declaration keyword, found {:?}", token.kind),
                );
                self.o.emit_diag(diag);
                return Err(());
            }
        };

        // Design note:
        //
        // `var_declaration` borrows our started node, so we are responsible for finishing it,
        // no matter whether it succeeded or failed.
        if result.is_err() {
            _ = self.sync_to(Terminal::LineBreak.into());
        }
        self.o.b.finish_node();

        Ok(())
    }

    /// `{NODE BORROW}`
    fn var_declaration(&mut self, is_str: bool) -> ParseResult<()> {
        let (name, name_span) = loop {
            let token = self.o.peek_token(Mode::SharpDecl).token;
            match token.kind {
                Token::KwDynamic | Token::KwGlobal | Token::KwRef => _ = self.o.bump(),
                Token::KwConst => _ = self.o.bump(),
                Token::KwSavedata => _ = self.o.bump(),
                Token::KwCharadata => _ = self.o.bump(),
                Token::Identifier => {
                    let token = self.o.bump();
                    break (token.lexeme, token.token.span);
                }
                _ => {
                    let mut diag = self.base_diag.clone();
                    diag.span_err(
                        Default::default(),
                        token.span,
                        format!("expected identifier, found {:?}", token.kind),
                    );
                    self.o.emit_diag(diag);
                    return Err(());
                }
            }
        };

        // HACK: For string assignment check
        if is_str {
            self.local_str_vars.insert(Ascii::new(ArcStr::from(name)));
        }

        // Array dimensions
        if self.try_eat(Mode::Normal, Token::Comma).is_some() {
            self.o.b.start_node(Token::ExprList);
            // Break at `=`
            let min_bp = infix_binding_power(Token::Assign).unwrap().1 + 2;
            if self.comma_expr_list(min_bp).is_err() {
                _ = self.sync_to(Terminal::LineBreak.into());
            }
            self.o.b.finish_node();
        }

        // Initializer
        if self.try_eat(Mode::Normal, Token::Assign).is_some() {
            self.o.b.start_node(Token::ExprList);
            if self.comma_expr_list(0).is_err() {
                _ = self.sync_to(Terminal::LineBreak.into());
            }
            self.o.b.finish_node();
        }

        Ok(())
    }

    fn define_declaration(&mut self) -> ParseResult<()> {
        let in_def = self.eat(Mode::Normal, Token::Identifier)?;
        let (in_def, in_def_span) = (in_def.lexeme.to_owned().into_boxed_str(), in_def.token.span);
        let out_def = self
            .eat(Mode::RawStr, Token::PlainStringLiteral)?
            .lexeme
            .to_owned()
            .into_boxed_str();
        let define_data = EraDefineData {
            filename: self.o.l.get_ctx().active_source.clone(),
            span: in_def_span,
            data: out_def,
        };
        self.o.l.push_define(in_def, define_data);

        Ok(())
    }

    /// `{NODE OWN}`
    fn func_declaration(&mut self, cp: Checkpoint) -> ParseResult<()> {
        self.eat(Mode::Normal, Token::At)?;

        // function name
        self.eat(Mode::Normal, Token::Identifier)?;

        self.o.b.start_node_at(cp, Token::FunctionItem);

        // Local string vars
        self.local_str_vars.clear();
        self.local_str_vars
            .insert(Ascii::new(rcstr::literal!("LOCALS")));
        self.local_str_vars
            .insert(Ascii::new(rcstr::literal!("ARGS")));

        let token = self.o.peek_token(Mode::Normal).token;
        match token.kind {
            Token::LParen => {
                self.o.b.start_node(Token::ExprList);
                _ = self.o.bump();
                if self.paren_expr_list().is_err() {
                    _ = self.sync_to(Terminal::RParen | Terminal::LineBreak);
                }
                self.o.b.finish_node();
            }
            Token::Comma => {
                self.o.b.start_node(Token::ExprList);
                _ = self.o.bump();
                if self.comma_expr_list(0).is_err() {
                    _ = self.sync_to(Terminal::LineBreak.into());
                }
                self.o.b.finish_node();
            }
            Token::LineBreak => (),
            _ => {
                let mut diag = self.base_diag.clone();
                diag.span_err(
                    Default::default(),
                    self.o.peek_token(Mode::Normal).token.span,
                    format!("expected `(` or `,`, found {:?}", token.kind),
                );
                self.o.emit_diag(diag);
                return Err(());
            }
        }

        // Function body
        if self.eat(Mode::Normal, Token::LineBreak).is_err() {
            _ = self.sync_to(Terminal::LineBreak.into());
        }
        self.skip_newline();
        // Local declarations (appears before statements)
        self.o.b.start_node(Token::SharpDeclList);
        while self.o.peek_token(Mode::Normal).token.kind == Token::NumberSign {
            let cp = self.o.b.checkpoint();
            if self.sharp_declaration(cp).is_err() {
                _ = self.sync_cp_to(Terminal::LineBreak.into(), cp);
            } else {
                // After a successful declaration, a newline is expected.
                // let token = self.o.peek_token(Mode::Normal).token;
                // if token.kind != Token::LineBreak {
                //     let mut diag = self.base_diag.clone();
                //     diag.span_err(
                //         Default::default(),
                //         token.span,
                //         format!("expected newline, found {:?}", token.kind),
                //     );
                //     self.o.emit_diag(diag);
                //     _ = self.sync_to(Terminal::LineBreak.into());
                // }
                _ = self.expect_sync_to_newline();
            }
            self.skip_newline();
        }
        self.o.b.finish_node();
        // Statements
        self.o.b.start_node(Token::StmtList);
        loop {
            match self.safe_statement() {
                ControlFlow::Break(()) => break,
                ControlFlow::Continue(()) => self.skip_newline(),
            }
        }
        self.o.b.finish_node();

        // Finish function item
        self.o.b.finish_node();

        Ok(())
    }

    /// `{NODE OWN}`
    ///
    /// Parses a statement or syncs to end of line, with avoidance of `@` and `<EOF>`.
    fn safe_statement(&mut self) -> ControlFlow<()> {
        let token = self.o.peek_token(Mode::Normal).token;
        match token.kind {
            Token::At | Token::Eof => return ControlFlow::Break(()),
            Token::NumberSign => {
                let mut diag = self.base_diag.clone();
                diag.span_warn(
                    Default::default(),
                    token.span,
                    "sharp declaration should not appear within function body; ignoring",
                );
                self.o.emit_diag(diag);
                _ = self.sync_to(Terminal::LineBreak.into());

                return ControlFlow::Continue(());
            }
            _ => (),
        }

        let cp = self.o.b.checkpoint();
        if self.statement().is_err() {
            _ = self.sync_cp_to(Terminal::LineBreak.into(), cp);
        } else {
            // TODO: Macro
            // self.o.close_over_macro();

            // NOTE: We try to avoid consuming newlines such as `@func` unexpectedly.
            if self.o.l.previous_token() != Token::LineBreak {
                // After a successful statement, a newline is expected.
                _ = self.expect_sync_to_newline();
            }
        }

        ControlFlow::Continue(())
    }

    /// `{NODE OWN}`
    fn statement(&mut self) -> ParseResult<()> {
        use EraCmdArgFmt as CmdArg;

        let token = self.o.peek_token(Mode::Normal);
        let (token, lexeme) = (token.token, token.lexeme);

        match token.kind {
            // Maybe commands or expressions
            Token::Identifier => (),
            Token::Dollar => {
                self.o.b.start_node(Token::LabelStmt);
                _ = self.o.bump();
                if self.eat(Mode::Normal, Token::Identifier).is_err() {
                    _ = self.sync_to(Terminal::LineBreak.into());
                }
                self.o.b.finish_node();
                return Ok(());
            }
            Token::At => {
                // TODO: Add suggestion for the most recent open block
                let mut diag = self.base_diag.clone();
                diag.span_err(
                    Default::default(),
                    token.span,
                    "unexpected start of function; did you forget to close a block?",
                );
                self.o.emit_diag(diag);
                return Err(());
            }
            Token::Eof => {
                // TODO: Add suggestion for the most recent open block
                let mut diag = self.base_diag.clone();
                diag.span_err(
                    Default::default(),
                    token.span,
                    "unexpected end of file; did you forget to close a block?",
                );
                self.o.emit_diag(diag);
                return Err(());
            }
            _ => {
                let cp = self.o.b.checkpoint();
                if self.stmt_expression().is_err() {
                    _ = self.sync_cp_to(Terminal::LineBreak.into(), cp);
                }
                return Ok(());
            }
        }

        // Handle commands or expressions
        trait Adhoc {
            fn r(&mut self) -> &mut Self;
        }
        impl<Callback: EraCompilerCallback> Adhoc for EraParserSite<'_, '_, '_, '_, '_, Callback> {
            fn r(&mut self) -> &mut Self {
                _ = self.o.bump_as(Token::KwIdent);
                self
            }
        }

        macro_rules! make {
            ($node:expr) => {{
                self.o.b.start_node($node);
                self.r();
                self.o.b.finish_node();
            }};
            ($node:expr, $name:ident()) => {{
                self.o.b.start_node($node);
                if self.r().$name().is_err() {
                    _ = self.sync_to(Terminal::LineBreak.into());
                }
                self.o.b.finish_node();
            }};
            ($node:expr, $name:ident($($args:expr),+)) => {{
                self.o.b.start_node($node);
                if self.r().$name($($args),+).is_err() {
                    _ = self.sync_to(Terminal::LineBreak.into());
                }
                self.o.b.finish_node();
            }};
        }
        macro_rules! make_wo_bump {
            ($node:expr, $name:ident($($args:expr),+)) => {{
                self.o.b.start_node($node);
                if self.$name($($args),+).is_err() {
                    _ = self.sync_to(Terminal::LineBreak.into());
                }
                self.o.b.finish_node();
            }};
        }

        // NOTE: We assume that built-in commands are not too long. If that's not the case,
        //       remember to revisit and modify the `MAX_BUF_LEN` below.
        const MAX_BUF_LEN: usize = 32;
        let Some(cmd) = crate::util::inline_to_ascii_uppercase::<MAX_BUF_LEN>(lexeme.as_bytes())
        else {
            // NOTE: Cannot use `make!` because we cannot bump the token here.
            let cp = self.o.b.checkpoint();
            if self.stmt_expression().is_err() {
                _ = self.sync_cp_to(Terminal::LineBreak.into(), cp);
            }
            return Ok(());
        };
        let cmd = cmd.as_bytes();
        if let Some((arg_fmt, _)) = routines::recognize_print_cmd(cmd) {
            make!(Token::PrintStmt, command_arg(arg_fmt));
        } else if let Some((arg_fmt, _)) = routines::recognize_debugprint_cmd(cmd) {
            make!(Token::DebugPrintStmt, command_arg(arg_fmt));
        } else if let Some(_) = routines::recognize_printdata_cmd(cmd) {
            make!(Token::PrintDataStmt, stmt_printdata());
        } else {
            match cmd {
                b"STRDATA" => make!(Token::StrDataStmt, stmt_strdata()),
                b"IF" => {
                    let start_span = self.o.bump_as(Token::KwIdent).token.span;
                    make_wo_bump!(Token::IfStmt, stmt_if(start_span))
                }
                b"SIF" => make!(Token::IfStmt, stmt_sif()),
                b"SELECTCASE" => make!(Token::SelectCaseStmt, stmt_selectcase()),
                b"WHILE" => make!(Token::WhileStmt, stmt_while()),
                b"REPEAT" => make!(Token::RepeatStmt, stmt_repeat()),
                b"FOR" => make!(Token::ForStmt, stmt_for()),
                b"DO" => make!(Token::DoLoopStmt, stmt_do_loop()),
                b"CALL" | b"CALLF" => make!(Token::CallStmt, stmt_call()),
                b"CALLFORM" | b"CALLFORMF" => make!(Token::CallStmt, stmt_callform()),
                b"TRYCALL" | b"TRYCALLF" => make!(Token::TryCallStmt, stmt_call()),
                b"TRYCALLFORM" | b"TRYCALLFORMF" => make!(Token::TryCallStmt, stmt_callform()),
                b"TRYCCALL" | b"TRYCCALLF" => make!(Token::TryCCallStmt, stmt_tryccall()),
                b"TRYCCALLFORM" | b"TRYCCALLFORMF" => {
                    make!(Token::TryCCallStmt, stmt_tryccallform())
                }
                b"JUMP" => make!(Token::JumpStmt, stmt_call()),
                b"JUMPFORM" => make!(Token::JumpStmt, stmt_callform()),
                b"TRYJUMP" => make!(Token::TryJumpStmt, stmt_call()),
                b"TRYJUMPFORM" => make!(Token::TryJumpStmt, stmt_callform()),
                b"TRYCJUMP" => make!(Token::TryCJumpStmt, stmt_tryccall()),
                b"TRYCJUMPFORM" => make!(Token::TryCJumpStmt, stmt_tryccallform()),
                b"TIMES" => make!(Token::TimesStmt, stmt_times()),
                b"NOP" => make!(Token::NopStmt),
                b"CONTINUE" => make!(Token::ContinueStmt),
                b"BREAK" => make!(Token::BreakStmt),
                b"RESTART" => make!(Token::RestartStmt),
                b"RETURN" | b"RETURNF" => make!(Token::ReturnStmt, command_arg(CmdArg::Expression)),
                b"THROW" => make!(Token::ThrowStmt, command_arg(CmdArg::RawStringForm)),
                b"GOTO" => make!(Token::GotoStmt, command_arg(CmdArg::Expression)),
                b"QUIT" => make!(Token::QuitStmt),
                b"WAIT" => make!(Token::WaitStmt),
                b"FORCEWAIT" => make!(Token::ForceWaitStmt),
                b"WAITANYKEY" => make!(Token::WaitAnyKeyStmt),
                // b"GCREATE" => make!(Token::GCreateStmt, command_arg(CmdArg::Expression)),
                // b"GDISPOSE" => make!(Token::GDisposeStmt, command_arg(CmdArg::Expression)),
                // b"GDRAWSPRITE" => make!(Token::GDrawSpriteStmt, command_arg(CmdArg::Expression)),
                b"SPLIT" => make!(Token::SplitStmt, cmd_arg_limit(3, 4)),
                b"SETBIT" => make!(Token::SetBitStmt, command_arg(CmdArg::Expression)),
                b"CLEARBIT" => make!(Token::ClearBitStmt, command_arg(CmdArg::Expression)),
                b"INVERTBIT" => make!(Token::InvertBitStmt, command_arg(CmdArg::Expression)),
                b"SETCOLOR" => make!(Token::SetColorStmt, command_arg(CmdArg::Expression)),
                b"RESETCOLOR" => make!(Token::ResetColorStmt),
                b"SETBGCOLOR" => make!(Token::SetBgColorStmt, command_arg(CmdArg::Expression)),
                b"RESETBGCOLOR" => make!(Token::ResetBgColorStmt),
                b"VARSET" => make!(Token::VarSetStmt, cmd_arg_limit(1, 4)),
                b"CVARSET" => make!(Token::CVarSetStmt, cmd_arg_limit(1, 5)),
                b"VARSIZE" => make!(Token::VarSizeStmt, cmd_arg_limit(1, 1)),
                b"SWAP" => make!(Token::SwapStmt, cmd_arg_limit(2, 2)),
                b"HTML_PRINT" => make!(Token::HtmlPrintStmt, cmd_arg_limit(1, 1)),
                b"PRINTBUTTON" => make!(Token::PrintButtonStmt, cmd_arg_limit(2, 2)),
                b"PRINTBUTTONC" => make!(Token::PrintButtonCStmt, cmd_arg_limit(2, 2)),
                b"PRINTBUTTONLC" => make!(Token::PrintButtonLCStmt, cmd_arg_limit(2, 2)),
                b"ARRAYREMOVE" => make!(Token::ArrayRemoveStmt, cmd_arg_limit(3, 3)),
                b"ARRAYSORT" => make!(Token::ArraySortStmt, cmd_arg_limit(1, 4)),
                b"ARRAYMSORT" => make!(Token::ArrayMSortStmt, command_arg(CmdArg::Expression)),
                b"ARRAYCOPY" => make!(Token::ArrayCopyStmt, cmd_arg_limit(2, 2)),
                b"ARRAYSHIFT" => make!(Token::ArrayShiftStmt, cmd_arg_limit(3, 5)),
                b"INPUT" => make!(Token::InputStmt, cmd_arg_limit(0, 3)),
                b"INPUTS" => make!(Token::InputSStmt, cmd_arg_limit(0, 3)),
                b"TINPUT" => make!(Token::TInputStmt, cmd_arg_limit(2, 5)),
                b"TINPUTS" => make!(Token::TInputSStmt, cmd_arg_limit(2, 5)),
                b"ONEINPUT" => make!(Token::OneInputStmt, cmd_arg_limit(0, 1)),
                b"ONEINPUTS" => make!(Token::OneInputSStmt, cmd_arg_limit(0, 1)),
                b"TONEINPUT" => make!(Token::TOneInputStmt, cmd_arg_limit(2, 5)),
                b"TONEINPUTS" => make!(Token::TOneInputSStmt, cmd_arg_limit(2, 5)),
                b"REUSELASTLINE" => {
                    make!(Token::ReuseLastLineStmt, command_arg(CmdArg::RawStringForm))
                }
                b"CLEARLINE" => make!(Token::ClearLineStmt, cmd_arg_limit(1, 1)),
                b"DRAWLINE" => make!(Token::DrawLineStmt),
                b"CUSTOMDRAWLINE" => {
                    make!(Token::CustomDrawLineStmt, command_arg(CmdArg::RawString))
                }
                b"DRAWLINEFORM" => make!(
                    Token::CustomDrawLineStmt,
                    command_arg(CmdArg::RawStringForm)
                ),
                b"TWAIT" => make!(Token::TWaitStmt, cmd_arg_limit(2, 2)),
                b"FONTSTYLE" => make!(Token::FontStyleStmt, cmd_arg_limit(1, 1)),
                b"FONTBOLD" => make!(Token::FontBoldStmt),
                b"FONTITALIC" => make!(Token::FontItalicStmt),
                b"FONTREGULAR" => make!(Token::FontRegularStmt),
                b"SETFONT" => make!(Token::SetFontStmt, cmd_arg_limit(1, 1)),
                b"PUTFORM" => make!(Token::PutFormStmt, command_arg(CmdArg::RawStringForm)),
                b"SKIPDISP" => make!(Token::SkipDispStmt, cmd_arg_limit(1, 1)),
                b"BEGIN" => make!(Token::BeginStmt, cmd_arg_limit(1, 1)),
                b"DOTRAIN" => make!(Token::DoTrainStmt, cmd_arg_limit(1, 1)),
                b"REDRAW" => make!(Token::RedrawStmt, cmd_arg_limit(1, 1)),
                b"STRLEN" => make!(Token::StrLenStmt, command_arg(CmdArg::RawString)),
                b"STRLENFORM" => make!(Token::StrLenStmt, command_arg(CmdArg::RawStringForm)),
                b"STRLENU" => make!(Token::StrLenUStmt, command_arg(CmdArg::RawString)),
                b"STRLENFORMU" => make!(Token::StrLenUStmt, command_arg(CmdArg::RawStringForm)),
                b"ALIGNMENT" => make!(Token::AlignmentStmt, cmd_arg_limit(1, 1)),
                b"TOOLTIP_SETDELAY" => {
                    make!(Token::ToolTipSetDelayStmt, cmd_arg_limit(1, 1))
                }
                b"TOOLTIP_SETDURATION" => {
                    make!(Token::ToolTipSetDurationStmt, cmd_arg_limit(1, 1))
                }
                b"RANDOMIZE" => make!(Token::RandomizeStmt, cmd_arg_limit(1, 1)),
                b"DUMPRAND" => make!(Token::DumpRandStmt),
                b"INITRAND" => make!(Token::InitRandStmt),
                b"BAR" => make!(Token::BarStmt, cmd_arg_limit(3, 3)),
                b"BARL" => make!(Token::BarLStmt, cmd_arg_limit(3, 3)),
                b"ADDCHARA" => make!(Token::AddCharaStmt, command_arg(CmdArg::Expression)),
                b"PICKUPCHARA" => make!(Token::PickUpCharaStmt, command_arg(CmdArg::Expression)),
                b"DELCHARA" => make!(Token::DelCharaStmt, command_arg(CmdArg::Expression)),
                b"SWAPCHARA" => make!(Token::SwapCharaStmt, cmd_arg_limit(2, 2)),
                b"ADDCOPYCHARA" => make!(Token::AddCopyCharaStmt, cmd_arg_limit(1, 1)),
                b"RESET_STAIN" => make!(Token::ResetStainStmt, cmd_arg_limit(1, 1)),
                b"SAVECHARA" => make!(Token::SaveCharaStmt, cmd_arg_limit(3, u64::MAX)),
                b"LOADCHARA" => make!(Token::LoadCharaStmt, cmd_arg_limit(1, 1)),
                b"SETANIMETIMER" => {
                    make!(Token::SetAnimeTimerStmt, cmd_arg_limit(1, 1))
                }
                b"HTML_TAGSPLIT" => make!(Token::HtmlTagSplitStmt, cmd_arg_limit(1, 3)),
                b"POWER" => make!(Token::PowerStmt, cmd_arg_limit(3, 3)),
                b"LOADDATA" => make!(Token::LoadDataStmt, cmd_arg_limit(1, 1)),
                b"SAVEDATA" => make!(Token::SaveDataStmt, cmd_arg_limit(2, 2)),
                // b"CHKDATA" => Cmd::CheckData(self.r().stmt_chkdata()?),
                b"GETTIME" => make!(Token::GetTimeStmt),
                b"LOADGLOBAL" => make!(Token::LoadGlobalStmt),
                b"SAVEGLOBAL" => make!(Token::SaveGlobalStmt),
                b"LOADGAME" => make!(Token::LoadGameStmt),
                b"SAVEGAME" => make!(Token::SaveGameStmt),
                b"DEBUGCLEAR" => make!(Token::DebugClearStmt),
                b"RESETDATA" => make!(Token::ResetDataStmt),
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
                | b"HTML_ESCAPE"
                | b"GETKEY"
                | b"GETKEYTRIGGERED"
                | b"FIND_CHARADATA"
                | b"CHKDATA" => make!(Token::ResultCmdCallStmt, command_arg(CmdArg::Expression)),
                _ => {
                    // NOTE: Cannot use `make!` because we cannot bump the token here.
                    let cp = self.o.b.checkpoint();
                    if self.stmt_expression().is_err() {
                        _ = self.sync_cp_to(Terminal::LineBreak.into(), cp);
                    }
                }
            }
        }

        Ok(())
    }

    /// `{NODE BORROW}`
    fn stmt_strdata(&mut self) -> ParseResult<()> {
        if self.try_eat(Mode::Normal, Token::LineBreak).is_none() {
            // var expression
            if self
                .safe_expression(true, Terminal::LineBreak.into())
                .is_err()
            {
                return Ok(());
            }
            _ = self.expect_sync_to_newline();
        }

        self.skip_newline();
        self.stmt_printdata_data();

        Ok(())
    }

    /// `{NODE BORROW}`
    fn stmt_printdata(&mut self) -> ParseResult<()> {
        if self.try_eat(Mode::Normal, Token::LineBreak).is_none() {
            // var expression
            if self
                .safe_expression(true, Terminal::LineBreak.into())
                .is_err()
            {
                return Ok(());
            }
            _ = self.expect_sync_to_newline();
        }

        self.skip_newline();
        self.stmt_printdata_data();

        Ok(())
    }

    /// `{NODE BORROW}`
    fn stmt_printdata_data(&mut self) {
        self.o.b.start_node(Token::StmtList);
        loop {
            if self.try_match_command("ENDDATA") {
                self.o.b.finish_node();
                _ = self.o.bump_as(Token::KwIdent);
                break;
            } else if self.try_match_command("DATA") {
                _ = self.o.bump_as(Token::KwIdent);
                self.o.b.start_node(Token::StringForm);
                if self.raw_string().is_err() {
                    _ = self.sync_to(Terminal::LineBreak.into());
                }
                self.o.b.finish_node();
            } else if self.try_match_command("DATAFORM") {
                _ = self.o.bump_as(Token::KwIdent);
                let cp = self.o.b.checkpoint();
                if self.raw_strform().is_err() {
                    _ = self.sync_cp_to(Terminal::LineBreak.into(), cp);
                }
            } else if self.try_match_command("DATALIST") {
                _ = self.o.bump_as(Token::KwIdent);

                _ = self.expect_sync_to_newline();
                self.skip_newline();

                self.o.b.start_node(Token::ExprList);
                loop {
                    if self.try_match_command("ENDLIST") {
                        self.o.b.finish_node();
                        _ = self.o.bump_as(Token::KwIdent);
                        break;
                    } else if self.try_match_command("DATA") {
                        _ = self.o.bump_as(Token::KwIdent);
                        self.o.b.start_node(Token::StringForm);
                        if self.raw_string().is_err() {
                            _ = self.sync_to(Terminal::LineBreak.into());
                        }
                        self.o.b.finish_node();
                    } else if self.try_match_command("DATAFORM") {
                        _ = self.o.bump_as(Token::KwIdent);
                        let cp = self.o.b.checkpoint();
                        if self.raw_strform().is_err() {
                            _ = self.sync_cp_to(Terminal::LineBreak.into(), cp);
                        }
                    } else {
                        let mut diag = self.base_diag.clone();
                        diag.span_err(
                            Default::default(),
                            self.o.peek_token(Mode::Normal).token.span,
                            "unknown command inside DATALIST",
                        );
                        self.o.emit_diag(diag);
                        _ = self.sync_to(Terminal::LineBreak.into());
                        // End early
                        self.o.b.finish_node();
                        break;
                    }

                    _ = self.expect_sync_to_newline();
                    self.skip_newline();
                }
            } else {
                let mut diag = self.base_diag.clone();
                diag.span_err(
                    Default::default(),
                    self.o.peek_token(Mode::Normal).token.span,
                    "unknown command inside PRINTDATA",
                );
                self.o.emit_diag(diag);
                _ = self.sync_to(Terminal::LineBreak.into());
                // End early
                self.o.b.finish_node();
                break;
            }

            _ = self.expect_sync_to_newline();
            self.skip_newline();
        }
    }

    /// `{NODE BORROW}`
    fn stmt_if(&mut self, start_span: SrcSpan) -> ParseResult<()> {
        let mut can_else = true;

        // Condition
        _ = self.safe_expression(true, Terminal::LineBreak.into());

        // Body
        _ = self.expect_sync_to_newline();
        self.skip_newline();
        self.o.b.start_node(Token::StmtList);
        loop {
            if can_else && self.try_match_command("ELSEIF") {
                // Transition to next condition
                self.o.b.finish_node();
                _ = self.o.bump_as(Token::KwIdent);
                _ = self.safe_expression(true, Terminal::LineBreak.into());
                _ = self.expect_sync_to_newline();
                self.skip_newline();
                self.o.b.start_node(Token::StmtList);
                continue;
            }
            if can_else && self.try_match_command("ELSE") {
                // Transition to else block
                can_else = false;

                self.o.b.finish_node();
                _ = self.o.bump_as(Token::KwIdent);
                _ = self.expect_sync_to_newline();
                self.skip_newline();
                self.o.b.start_node(Token::StmtList);
                continue;
            }
            if self.try_match_command("ENDIF") {
                // End of if block
                self.o.b.finish_node();
                _ = self.o.bump_as(Token::KwIdent);
                break;
            }

            match self.safe_statement() {
                ControlFlow::Break(()) => {
                    let mut diag = self.base_diag.clone();
                    diag.span_err(
                        Default::default(),
                        self.o.peek_token(Mode::Normal).token.span,
                        "unexpected end of block in IF; did you forget to close it?",
                    );
                    diag.span_note(Default::default(), start_span, "block started here");
                    self.o.emit_diag(diag);

                    self.o.b.finish_node();
                    break;
                }
                ControlFlow::Continue(()) => self.skip_newline(),
            }
        }

        Ok(())
    }

    /// `{NODE BORROW}`
    fn stmt_sif(&mut self) -> ParseResult<()> {
        // Condition
        _ = self.safe_expression(true, Terminal::LineBreak.into());

        // Body
        _ = self.expect_sync_to_newline();
        self.skip_newline();
        self.o.b.start_node(Token::StmtList);
        // NOTE: `SIF` has only one statement as its body
        if self.safe_statement().is_break() {
            let mut diag = self.base_diag.clone();
            diag.span_err(
                Default::default(),
                self.o.peek_token(Mode::Normal).token.span,
                "unexpected end of block in SIF; did you forget to close it?",
            );
            self.o.emit_diag(diag);
        }
        self.o.b.finish_node();

        Ok(())
    }

    /// `{NODE BORROW}`
    fn stmt_selectcase(&mut self) -> ParseResult<()> {
        let mut can_else = true;

        // Expression
        _ = self.safe_expression(true, Terminal::LineBreak.into());

        // Body
        _ = self.expect_sync_to_newline();
        self.skip_newline();
        self.o.b.start_node(Token::StmtList);
        loop {
            if self.try_match_command("ENDSELECT") {
                // End of select statement
                self.o.b.finish_node();
                _ = self.o.bump_as(Token::KwIdent);
                break;
            }

            if can_else && self.try_match_command("CASE") {
                // Transition to next case
                self.o.b.finish_node();
                _ = self.o.bump_as(Token::KwIdent);

                self.o.b.start_node(Token::SelectCasePredList);
                if self.stmt_selectcase_case().is_err() {
                    _ = self.sync_to(Terminal::LineBreak.into());
                }
                self.o.b.finish_node();

                _ = self.expect_sync_to_newline();
                self.skip_newline();
                self.o.b.start_node(Token::StmtList);
                continue;
            }
            if can_else && self.try_match_command("CASEELSE") {
                // Transition to else block
                can_else = false;

                self.o.b.finish_node();
                _ = self.o.bump_as(Token::KwIdent);
                _ = self.expect_sync_to_newline();
                self.skip_newline();
                self.o.b.start_node(Token::StmtList);
                continue;
            }

            match self.safe_statement() {
                ControlFlow::Break(()) => {
                    let mut diag = self.base_diag.clone();
                    diag.span_err(
                        Default::default(),
                        self.o.peek_token(Mode::Normal).token.span,
                        "unexpected end of block in SELECTCASE; did you forget to close it?",
                    );
                    self.o.emit_diag(diag);

                    self.o.b.finish_node();
                    break;
                }
                ControlFlow::Continue(()) => self.skip_newline(),
            }
        }

        Ok(())
    }

    /// `{NODE BORROW}`
    fn stmt_selectcase_case(&mut self) -> ParseResult<()> {
        if self.o.peek_token(Mode::Normal).token.kind == Token::LineBreak {
            return Ok(());
        }

        loop {
            // if self.o.peek_token(Mode::Normal).token.kind == Token::Comma {
            //     self.o.b.start_node(Token::EmptyExpr);
            //     self.o.b.finish_node();
            //     _ = self.o.bump();
            //     continue;
            // }
            match self.o.peek_token(Mode::Normal).token.kind {
                Token::Comma => {
                    self.o.b.start_node(Token::EmptyExpr);
                    self.o.b.finish_node();
                    _ = self.o.bump();
                    continue;
                }
                Token::LineBreak => break,
                _ => (),
            }

            // Condition
            if self.try_match_command("IS") {
                // `IS <op> <expr>`
                self.o.b.start_node(Token::SelectCaseCond);
                _ = self.o.bump_as(Token::KwIdent); // `IS`
                let result = (|| -> ParseResult<()> {
                    if !matches!(
                        self.o.peek_token(Mode::Normal).token.kind,
                        Token::CmpEq
                            | Token::CmpNEq
                            | Token::CmpLT
                            | Token::CmpGT
                            | Token::CmpLEq
                            | Token::CmpGEq
                            | Token::BitAnd
                    ) {
                        let mut diag = self.base_diag.clone();
                        diag.span_err(
                            Default::default(),
                            self.o.peek_token(Mode::Normal).token.span,
                            "expected comparison operator in SELECTCASE",
                        );
                        self.o.emit_diag(diag);
                        _ = self.sync_to(Terminal::Comma | Terminal::LineBreak);
                        return Err(());
                    }
                    _ = self.o.bump();
                    self.safe_expression(true, Terminal::Comma | Terminal::LineBreak)?;
                    Ok(())
                })();
                self.o.b.finish_node();
                if result.is_err() {
                    if self.o.previous_token() == Token::Comma {
                        // Reached comma
                        continue;
                    } else {
                        // Reached newline
                        return Ok(());
                    }
                }
            } else {
                // Single or range
                let cp = self.o.b.checkpoint();
                let result = (|| -> ParseResult<()> {
                    self.safe_expression(true, Terminal::Comma | Terminal::LineBreak)?;
                    if self.try_match_command("TO") {
                        // Range
                        self.o.b.start_node_at(cp, Token::SelectCaseRange);
                        _ = self.o.bump_as(Token::KwIdent); // `TO`
                        if self
                            .safe_expression(true, Terminal::Comma | Terminal::LineBreak)
                            .is_err()
                        {
                            self.o.b.finish_node();
                            return Err(());
                        }
                        self.o.b.finish_node();
                    } else {
                        // Single
                        self.o.b.start_node_at(cp, Token::SelectCaseSingle);
                        self.o.b.finish_node();
                    }
                    Ok(())
                })();
                if result.is_err() {
                    if self.o.previous_token() == Token::Comma {
                        // Reached comma
                        continue;
                    } else {
                        // Reached newline
                        return Ok(());
                    }
                }
            }

            // If no comma, must be end of expression list
            if self.try_eat(Mode::Normal, Token::Comma).is_none() {
                break;
            }
        }

        Ok(())
    }

    /// `{NODE BORROW}`
    fn stmt_while(&mut self) -> ParseResult<()> {
        // Condition
        _ = self.safe_expression(true, Terminal::LineBreak.into());

        // Body
        _ = self.expect_sync_to_newline();
        self.skip_newline();
        self.o.b.start_node(Token::StmtList);
        loop {
            if self.try_match_command("WEND") {
                // End of while block
                self.o.b.finish_node();
                _ = self.o.bump_as(Token::KwIdent);
                break;
            }

            match self.safe_statement() {
                ControlFlow::Break(()) => {
                    let mut diag = self.base_diag.clone();
                    diag.span_err(
                        Default::default(),
                        self.o.peek_token(Mode::Normal).token.span,
                        "unexpected end of block in WHILE; did you forget to close it?",
                    );
                    self.o.emit_diag(diag);

                    self.o.b.finish_node();
                    break;
                }
                ControlFlow::Continue(()) => self.skip_newline(),
            }
        }

        Ok(())
    }

    /// `{NODE BORROW}`
    fn stmt_repeat(&mut self) -> ParseResult<()> {
        // Count
        _ = self.safe_expression(true, Terminal::LineBreak.into());

        // Body
        _ = self.expect_sync_to_newline();
        self.skip_newline();
        self.o.b.start_node(Token::StmtList);
        loop {
            if self.try_match_command("REND") {
                // End of repeat block
                self.o.b.finish_node();
                _ = self.o.bump_as(Token::KwIdent);
                break;
            }

            match self.safe_statement() {
                ControlFlow::Break(()) => {
                    let mut diag = self.base_diag.clone();
                    diag.span_err(
                        Default::default(),
                        self.o.peek_token(Mode::Normal).token.span,
                        "unexpected end of block in REPEAT; did you forget to close it?",
                    );
                    self.o.emit_diag(diag);

                    self.o.b.finish_node();
                    break;
                }
                ControlFlow::Continue(()) => self.skip_newline(),
            }
        }

        Ok(())
    }

    /// `{NODE BORROW}`
    fn stmt_for(&mut self) -> ParseResult<()> {
        // <var, start, end, step>
        self.o.b.start_node(Token::ExprList);
        if self.comma_expr_list_limit(0, 4).is_err() {
            _ = self.sync_to(Terminal::LineBreak.into());
        }
        self.o.b.finish_node();

        // Body
        _ = self.expect_sync_to_newline();
        self.skip_newline();
        self.o.b.start_node(Token::StmtList);
        loop {
            if self.try_match_command("NEXT") {
                // End of for block
                self.o.b.finish_node();
                _ = self.o.bump_as(Token::KwIdent);
                break;
            }

            match self.safe_statement() {
                ControlFlow::Break(()) => {
                    let mut diag = self.base_diag.clone();
                    diag.span_err(
                        Default::default(),
                        self.o.peek_token(Mode::Normal).token.span,
                        "unexpected end of block in FOR; did you forget to close it?",
                    );
                    self.o.emit_diag(diag);

                    self.o.b.finish_node();
                    break;
                }
                ControlFlow::Continue(()) => self.skip_newline(),
            }
        }

        Ok(())
    }

    /// `{NODE BORROW}`
    fn stmt_do_loop(&mut self) -> ParseResult<()> {
        // Body
        _ = self.expect_sync_to_newline();
        self.skip_newline();
        self.o.b.start_node(Token::StmtList);
        loop {
            if self.try_match_command("LOOP") {
                // End of do loop block
                self.o.b.finish_node();
                _ = self.o.bump_as(Token::KwIdent);
                break;
            }

            match self.safe_statement() {
                ControlFlow::Break(()) => {
                    let mut diag = self.base_diag.clone();
                    diag.span_err(
                        Default::default(),
                        self.o.peek_token(Mode::Normal).token.span,
                        "unexpected end of block in DO LOOP; did you forget to close it?",
                    );
                    self.o.emit_diag(diag);

                    self.o.b.finish_node();
                    return Ok(()); // End early
                }
                ControlFlow::Continue(()) => self.skip_newline(),
            }
        }

        // Condition
        _ = self.safe_expression(true, Terminal::LineBreak.into());

        Ok(())
    }

    /// `{NODE OWN}`
    fn stmt_call_args(&mut self) {
        self.o.b.start_node(Token::ExprList);
        let token = self.o.peek_token(Mode::Normal).token.kind;
        if token == Token::Comma {
            _ = self.o.bump();
            if self.comma_expr_list(0).is_err() {
                _ = self.sync_to(Terminal::LineBreak.into());
            }
        } else if token == Token::LParen {
            _ = self.o.bump();
            if self.paren_expr_list().is_err() {
                _ = self.sync_to(Terminal::LineBreak.into());
            }
        }
        self.o.b.finish_node();
    }

    /// `{NODE BORROW}`
    fn stmt_call_nameform(&mut self) -> ParseResult<()> {
        // Function name
        loop {
            let token = self.o.peek_token(Mode::CallForm).token;
            match token.kind {
                Token::PlainStringLiteral => _ = self.o.bump(),
                Token::LBrace => {
                    self.o.b.start_node(Token::StringFormInterpPart);
                    _ = self.o.bump();
                    if self.strform_interp_part(Terminal::RBrace).is_err() {
                        _ = self.sync_to(Terminal::RBrace | Terminal::LineBreak);
                    }
                    self.o.b.finish_node();
                }
                Token::Percentage => {
                    self.o.b.start_node(Token::StringFormInterpPart);
                    _ = self.o.bump();
                    if self.strform_interp_part(Terminal::Percentage).is_err() {
                        _ = self.sync_to(Terminal::Percentage | Terminal::LineBreak);
                    }
                    self.o.b.finish_node();
                }
                Token::TernaryStrFormMarker => {
                    self.o.b.start_node(Token::StringFormInterpPart);
                    self.o.b.start_node(Token::TernaryExpr);
                    _ = self.o.bump();
                    if self.ternary_strform().is_err() {
                        _ = self.sync_to(Terminal::TernaryStrFormMarker | Terminal::LineBreak);
                    }
                    self.o.b.finish_node();
                    self.o.b.finish_node();
                }
                Token::LParen | Token::Comma | Token::LineBreak => break,
                _ => {
                    let mut diag = self.base_diag.clone();
                    diag.span_err(
                        Default::default(),
                        token.span,
                        format!("unexpected token in string form: {:?}", token.kind),
                    );
                    self.o.emit_diag(diag);
                    return Err(());
                }
            }
        }
        Ok(())
    }

    /// `{NODE BORROW}`
    fn stmt_call(&mut self) -> ParseResult<()> {
        // Function name
        if self.eat(Mode::Normal, Token::Identifier).is_err() {
            _ = self.sync_to(Terminal::LineBreak.into());
        }

        // Arguments
        self.stmt_call_args();

        Ok(())
    }

    /// `{NODE BORROW}`
    fn stmt_callform(&mut self) -> ParseResult<()> {
        // Function name
        self.o.b.start_node(Token::StringForm);
        if self.stmt_call_nameform().is_err() {
            _ = self.sync_to(Terminal::LineBreak.into());
        }
        self.o.b.finish_node();

        // Arguments
        self.stmt_call_args();

        Ok(())
    }

    /// `{NODE BORROW}`
    fn stmt_tryccall(&mut self) -> ParseResult<()> {
        let mut can_catch = true;

        // Function name
        if self.eat(Mode::Normal, Token::Identifier).is_err() {
            _ = self.sync_to(Terminal::LineBreak.into());
        }

        // Arguments
        self.stmt_call_args();

        // Then & catch body
        _ = self.expect_sync_to_newline();
        self.skip_newline();
        self.o.b.start_node(Token::StmtList);
        loop {
            if can_catch && self.try_match_command("CATCH") {
                // Transition to catch block
                can_catch = false;

                self.o.b.finish_node();
                _ = self.o.bump_as(Token::KwIdent);
                _ = self.expect_sync_to_newline();
                self.skip_newline();
                self.o.b.start_node(Token::StmtList);
                continue;
            }
            if self.try_match_command("ENDCATCH") {
                // End of try block
                self.o.b.finish_node();
                _ = self.o.bump_as(Token::KwIdent);
                break;
            }

            match self.safe_statement() {
                ControlFlow::Break(()) => {
                    let mut diag = self.base_diag.clone();
                    diag.span_err(
                        Default::default(),
                        self.o.peek_token(Mode::Normal).token.span,
                        "unexpected end of block in TRYCCALL; did you forget to close it?",
                    );
                    self.o.emit_diag(diag);

                    self.o.b.finish_node();
                    break;
                }
                ControlFlow::Continue(()) => self.skip_newline(),
            }
        }

        Ok(())
    }

    /// `{NODE BORROW}`
    fn stmt_tryccallform(&mut self) -> ParseResult<()> {
        let mut can_catch = true;

        // Function name
        self.o.b.start_node(Token::StringForm);
        if self.stmt_call_nameform().is_err() {
            _ = self.sync_to(Terminal::LineBreak.into());
        }
        self.o.b.finish_node();

        // Arguments
        self.stmt_call_args();

        // Then & catch body
        _ = self.expect_sync_to_newline();
        self.skip_newline();
        self.o.b.start_node(Token::StmtList);
        loop {
            if can_catch && self.try_match_command("CATCH") {
                // Transition to catch block
                can_catch = false;

                self.o.b.finish_node();
                _ = self.o.bump_as(Token::KwIdent);
                _ = self.expect_sync_to_newline();
                self.skip_newline();
                self.o.b.start_node(Token::StmtList);
                continue;
            }
            if self.try_match_command("ENDCATCH") {
                // End of try block
                self.o.b.finish_node();
                _ = self.o.bump_as(Token::KwIdent);
                break;
            }

            match self.safe_statement() {
                ControlFlow::Break(()) => {
                    let mut diag = self.base_diag.clone();
                    diag.span_err(
                        Default::default(),
                        self.o.peek_token(Mode::Normal).token.span,
                        "unexpected end of block in TRYCCALLFORM; did you forget to close it?",
                    );
                    self.o.emit_diag(diag);

                    self.o.b.finish_node();
                    break;
                }
                ControlFlow::Continue(()) => self.skip_newline(),
            }
        }

        Ok(())
    }

    /// `{NODE BORROW}`
    fn stmt_times(&mut self) -> ParseResult<()> {
        if self
            .safe_expression(true, Terminal::LineBreak.into())
            .is_err()
        {
            return Ok(());
        }

        if self.eat(Mode::Normal, Token::Comma).is_err() {
            _ = self.sync_to(Terminal::LineBreak.into());
        }

        // Factor (floating point, read as raw string instead)
        self.o.b.start_node(Token::StringForm);
        if self.raw_string().is_err() {
            _ = self.sync_to(Terminal::LineBreak.into());
        }
        self.o.b.finish_node();

        Ok(())
    }

    /// `{NODE OWN}`
    ///
    /// Generates a node of type `ExprList` containing the arguments of a command.
    fn command_arg(&mut self, arg_fmt: EraCmdArgFmt) -> ParseResult<()> {
        self.o.b.start_node(Token::ExprList);
        match arg_fmt {
            EraCmdArgFmt::Expression
            | EraCmdArgFmt::ExpressionS
            | EraCmdArgFmt::ExpressionSForm => {
                if self.comma_expr_list(0).is_err() {
                    _ = self.sync_to(Terminal::LineBreak.into());
                }
            }
            EraCmdArgFmt::RawStringForm => {
                let cp = self.o.b.checkpoint();
                if self.raw_strform().is_err() {
                    _ = self.sync_cp_to(Terminal::LineBreak.into(), cp);
                }
            }
            EraCmdArgFmt::RawString => {
                self.o.b.start_node(Token::StringForm);
                if self.raw_string().is_err() {
                    _ = self.sync_to(Terminal::LineBreak.into());
                }
                self.o.b.finish_node();
            }
        }
        self.o.b.finish_node();
        Ok(())
    }

    fn cmd_arg_limit(&mut self, lower_limit: u64, upper_limit: u64) -> ParseResult<()> {
        self.o.b.start_node(Token::ExprList);
        match self.comma_expr_list_limit(0, upper_limit) {
            Ok(count) => {
                if count < lower_limit {
                    let mut diag = self.base_diag.clone();
                    diag.span_err(
                        Default::default(),
                        self.o.peek_token(Mode::Normal).token.span,
                        format!("expected at least {} arguments", lower_limit),
                    );
                    self.o.emit_diag(diag);
                } else if count > upper_limit {
                    // No need to emit a diagnostic here, it is handled by the caller
                }
            }
            Err(_) => {
                _ = self.sync_to(Terminal::LineBreak.into());
            }
        }
        self.o.b.finish_node();
        Ok(())
    }

    /// `{NODE BORROW}`
    fn raw_string(&mut self) -> ParseResult<()> {
        loop {
            let token = self.o.peek_token(Mode::RawStr).token;
            match token.kind {
                EraTokenKind::PlainStringLiteral => _ = self.o.bump(),
                EraTokenKind::LineBreak => break,
                _ => {
                    let mut diag = self.base_diag.clone();
                    diag.span_err(
                        Default::default(),
                        token.span,
                        format!("unexpected token in raw string: {:?}", token.kind),
                    );
                    self.o.emit_diag(diag);
                    return Err(());
                }
            }
        }
        Ok(())
    }

    /// `{NODE OWN}`
    fn stmt_expression(&mut self) -> ParseResult<()> {
        let cp = self.o.b.checkpoint();

        let Ok(last_token) = self.safe_expression(false, Terminal::LineBreak.into()) else {
            return Ok(());
        };

        // Handle special case: row assignment (e.g. `v = 1, 2, 3`)
        if matches!(last_token, Token::Assign | Token::ExprAssign) {
            if self.o.peek_token(Mode::Normal).token.kind == Token::Comma {
                self.o.b.start_node_at(cp, Token::RowAssignStmt);
                self.o.b.start_node(Token::ExprList);
                _ = self.o.bump();
                if self.comma_expr_list(0).is_err() {
                    _ = self.sync_to(Terminal::LineBreak.into());
                }
                self.o.b.finish_node();
                self.o.b.finish_node();
                return Ok(());
            }
        }

        self.o.b.start_node_at(cp, Token::ExprStmt);
        self.o.b.finish_node();

        Ok(())
    }

    /// `{NODE OWN}`
    fn expression(&mut self, pure: bool) -> ParseResult<Token> {
        self.expression_bp(0, pure, Terminal::LineBreak.into())
    }

    /// `{NODE OWN}`
    ///
    /// Like `expression`, but guarantees that an Invalid node is created if it fails.
    fn safe_expression(
        &mut self,
        pure: bool,
        terminals: enumset::EnumSet<EraTerminalTokenKind>,
    ) -> ParseResult<Token> {
        let cp = self.o.b.checkpoint();
        let Ok(token) = self.expression(pure) else {
            _ = self.sync_cp_to(terminals, cp);
            return Err(());
        };
        Ok(token)
    }

    /// `{NODE OWN}`
    ///
    /// Like `expression_bp`, but guarantees that an Invalid node is created if it fails.
    fn safe_expression_bp(
        &mut self,
        min_bp: u8,
        pure: bool,
        terminals: enumset::EnumSet<EraTerminalTokenKind>,
    ) -> ParseResult<Token> {
        let cp = self.o.b.checkpoint();
        let Ok(token) = self.expression_bp(min_bp, pure, terminals) else {
            _ = self.sync_cp_to(terminals, cp);
            return Err(());
        };
        Ok(token)
    }

    /// `{NODE OWN}`
    fn expression_bp(
        &mut self,
        min_bp: u8,
        pure: bool,
        terminals: enumset::EnumSet<EraTerminalTokenKind>,
    ) -> ParseResult<Token> {
        // let lexer_mode = if self.is_expression_s_mode {
        //     EraLexerMode::ExpressionS
        // } else {
        //     EraLexerMode::Normal
        // };

        let mut last_processed_token = Token::Eof;

        // HACK: Support special break_at symbols
        let lexer_mode = if terminals.contains(Terminal::Percentage) {
            // HACK: Do not parse `%value%=2` as `%value` and `%=2`
            Mode::InlineNormal
        } else {
            Mode::Normal
        };
        let cp = self.o.b.checkpoint();
        let first = self.o.peek_token(lexer_mode);
        let (first, first_lexeme) = (first.token, first.lexeme);
        let mut lhs_is_str_var = false;
        // Read lhs
        match first.kind {
            Token::IntLiteral => _ = self.o.bump(),
            Token::StringLiteral => _ = self.o.bump(),
            Token::StringFormStart => {
                // TODO: Handle escape characters for StringForm (both expression and raw)
                self.o.b.start_node(Token::StringForm);
                _ = self.o.bump();
                if self.expression_strform().is_err() {
                    _ = self.sync_to(terminals | Terminal::DoubleQuote);
                }
                self.o.b.finish_node();
            }
            Token::DoubleQuote => {
                // TODO: Handle escape characters for StringForm (both expression and raw)
                self.o.b.start_node(Token::StringForm);
                _ = self.o.bump();
                if self.plain_expression_strform().is_err() {
                    _ = self.sync_to(terminals | Terminal::DoubleQuote);
                }
                self.o.b.finish_node();
            }
            Token::SingleQuote => {
                self.o.b.start_node(Token::StringForm);
                _ = self.o.bump();
                if self.quote_raw_strform().is_err() {
                    _ = self.sync_to(terminals);
                }
                self.o.b.finish_node();
            }
            Token::TernaryStrFormMarker => {
                self.o.b.start_node(Token::TernaryExpr);
                _ = self.o.bump();
                if self.ternary_strform().is_err() {
                    _ = self.sync_to(terminals | Terminal::TernaryStrFormMarker);
                }
                self.o.b.finish_node();
            }
            Token::LParen => {
                self.o.b.start_node(Token::ParenExpr);
                _ = self.o.bump();
                // let cp = self.o.b.checkpoint();
                // if self.expression_paren().is_err() {
                //     self.synchronize_cp_to_n(&(terminals | Terminal::RParen).to_tokens_vec(), cp);
                // }
                _ = (|| -> ParseResult<()> {
                    let terminals = terminals | Terminal::RParen;
                    self.safe_expression(true, terminals)?;
                    self.eat_or_sync(Mode::Normal, Token::RParen, terminals)?;
                    Ok(())
                })();
                self.o.b.finish_node();
            }
            Token::Identifier => {
                let first_lexeme = ArcStr::from(first_lexeme);
                _ = self.o.bump();
                if self.is_var_str(&first_lexeme) {
                    lhs_is_str_var = true;
                }
            }
            _ => {
                // Handle prefix
                if let Some(((), r_bp)) = prefix_binding_power(first.kind) {
                    self.o.b.start_node(Token::PreUnaryExpr);
                    _ = self.o.bump();
                    _ = self.safe_expression_bp(r_bp, pure, terminals);
                    self.o.b.finish_node();
                } else {
                    let mut diag = self.base_diag.clone();
                    diag.span_err(
                        Default::default(),
                        first.span,
                        format!("unexpected token in expression: {:?}", first.kind),
                    );
                    self.o.emit_diag(diag);
                    return Err(());
                }
            }
        };

        loop {
            let peek_mode = lexer_mode;
            let token = self.o.peek_token(peek_mode).token;

            // HACK: Break at token
            if let Ok(token) = token.kind.try_into() {
                if terminals.contains(token) {
                    break;
                }
            }

            // Handle postfix
            if let Some((l_bp, ())) = postfix_binding_power(token.kind) {
                if l_bp < min_bp {
                    break;
                }

                match token.kind {
                    Token::LParen => {
                        self.o.b.start_node_at(cp, Token::FunCallExpr);
                        self.o.b.start_node(Token::ExprList);
                        _ = self.o.bump();
                        if self.paren_expr_list().is_err() {
                            _ = self.sync_to(terminals | Terminal::RParen);
                        }
                        self.o.b.finish_node();
                        self.o.b.finish_node();
                    }
                    _ => {
                        self.o.b.start_node_at(cp, Token::PostUnaryExpr);
                        _ = self.o.bump();
                        self.o.b.finish_node();
                    }
                };

                continue;
            }

            // Handle infix
            if let Some((l_bp, r_bp)) = infix_binding_power(token.kind) {
                if l_bp < min_bp {
                    break;
                }

                last_processed_token = token.kind;

                match token.kind {
                    Token::QuestionMark => {
                        self.o.b.start_node_at(cp, Token::TernaryExpr);
                        _ = self.o.bump();
                        _ = (|| -> ParseResult<()> {
                            // let terminals = terminals | Terminal::NumberSign;
                            self.safe_expression(true, terminals)?;
                            self.eat_or_sync(Mode::Normal, Token::NumberSign, terminals)?;
                            self.safe_expression_bp(r_bp, pure, terminals)?;
                            Ok(())
                        })();
                        self.o.b.finish_node();
                    }
                    Token::Colon => {
                        self.o.b.start_node_at(cp, Token::VarIdxExpr);
                        _ = self.o.bump();
                        _ = (|| -> ParseResult<()> {
                            // let terminals = terminals | Terminal::Colon;
                            self.safe_expression_bp(r_bp, pure, terminals)?;
                            while self.try_eat(Mode::Normal, Token::Colon).is_some() {
                                self.safe_expression_bp(r_bp, pure, terminals)?;
                            }
                            Ok(())
                        })();
                        self.o.b.finish_node();
                    }
                    Token::Assign => {
                        self.o.b.start_node_at(cp, Token::BinaryExpr);
                        _ = self.o.bump();
                        let cp = self.o.b.checkpoint();
                        if lhs_is_str_var && !pure {
                            if self.raw_strform().is_err() {
                                _ = self.sync_cp_to(terminals, cp);
                            }
                        } else {
                            if self.expression_bp(r_bp, pure, terminals).is_err() {
                                _ = self.sync_cp_to(terminals, cp);
                            }
                        }
                        self.o.b.finish_node();
                    }
                    Token::At => {
                        // HACK: Should add `@` as part of variable name
                        self.o.b.start_node_at(cp, Token::VarNamespaceExpr);
                        _ = self.o.bump();
                        if self.eat(Mode::Normal, Token::Identifier).is_err() {
                            _ = self.sync_to(terminals);
                        }
                        self.o.b.finish_node();
                    }
                    _ => {
                        self.o.b.start_node_at(cp, Token::BinaryExpr);
                        _ = self.o.bump();
                        let cp = self.o.b.checkpoint();
                        if self.expression_bp(r_bp, pure, terminals).is_err() {
                            _ = self.sync_cp_to(terminals, cp);
                        }
                        self.o.b.finish_node();
                    }
                };

                continue;
            }

            break;
        }

        Ok(last_processed_token)
    }

    /// `{NODE BORROW}`
    fn expression_paren(&mut self) -> ParseResult<()> {
        self.expression(true)?;
        self.eat(Mode::Normal, Token::RParen)?;
        Ok(())
    }

    /// `{NODE BORROW}`
    ///
    /// Parses residual of expressions like `(arg1, arg2)`.
    fn paren_expr_list(&mut self) -> ParseResult<()> {
        if self.try_eat(Mode::Normal, Token::RParen).is_some() {
            // Empty expression list
            return Ok(());
        }

        // NOTE: It is possible to omit expressions, i.e. `(, arg2)` is valid.
        loop {
            match self.o.peek_token(Mode::Normal).token.kind {
                Token::Comma => {
                    // Empty expression
                    self.o.b.start_node(Token::EmptyExpr);
                    self.o.b.finish_node();
                    _ = self.o.bump();
                    continue;
                }
                Token::RParen => break,
                _ => (),
            }

            let cp = self.o.b.checkpoint();
            if self.expression(true).is_err() {
                if self
                    .sync_cp_to(Terminal::Comma | Terminal::RParen | Terminal::LineBreak, cp)
                    .is_err()
                {
                    return Ok(());
                }
                if self.o.l.previous_token() == Token::RParen {
                    // Synced to end of expression list
                    return Ok(());
                }
                // Otherwise, synced to comma
            } else {
                // If no comma, must be end of expression list
                if self.try_eat(Mode::Normal, Token::Comma).is_none() {
                    break;
                }
            }

            // TODO: Remove this?
            // // Need to handle things like `(arg1, arg2,)`
            // if self.o.peek_token(Mode::Normal).token.kind == Token::RParen {
            //     break;
            // }
        }

        self.eat(Mode::Normal, Token::RParen)?;

        Ok(())
    }

    /// `{NODE BORROW}`
    ///
    /// Parses residual of expressions like `, var1, var2`.
    fn comma_expr_list_limit(&mut self, min_bp: u8, size_limit: u64) -> ParseResult<u64> {
        assert!(size_limit > 0, "size limit must be greater than 0");
        let mut count = 0;
        loop {
            // Handle min_bp
            if min_bp != 0 {
                let token = self.o.peek_token(Mode::Normal).token;
                if let Some((l_bp, _)) = infix_binding_power(token.kind) {
                    if l_bp < min_bp {
                        break;
                    }
                }
            }

            match self.o.peek_token(Mode::Normal).token.kind {
                Token::Comma => {
                    // Empty expression
                    self.o.b.start_node(Token::EmptyExpr);
                    self.o.b.finish_node();
                    _ = self.o.bump();

                    // Check limit
                    count += 1;
                    if count >= size_limit {
                        // Too many expressions, exit early
                        break;
                    }

                    continue;
                }
                Token::LineBreak => break,
                _ => (),
            }

            let cp = self.o.b.checkpoint();
            if self
                .expression_bp(min_bp, true, Terminal::Comma | Terminal::LineBreak)
                .is_err()
            {
                if self
                    .sync_cp_to(Terminal::Comma | Terminal::LineBreak, cp)
                    .is_err()
                {
                    return Ok(count);
                }
                // Otherwise, synced to comma

                // Check limit
                count += 1;
                if count >= size_limit {
                    // Too many expressions, exit early
                    break;
                }
            } else {
                // Check limit
                count += 1;
                if count >= size_limit {
                    // Too many expressions, exit early
                    break;
                }

                // If no comma, must be end of expression list
                if self.try_eat(Mode::Normal, Token::Comma).is_none() {
                    return Ok(count);
                }
            }
        }

        let previous_is_comma = count > 0;
        if count < size_limit && previous_is_comma {
            // We want more expressions, and the last one was a comma
            self.o.b.start_node(Token::EmptyExpr);
            self.o.b.finish_node();
            count += 1;
        }

        Ok(count)
    }

    /// `{NODE BORROW}`
    ///
    /// Parses residual of expressions like `, var1, var2`.
    fn comma_expr_list(&mut self, min_bp: u8) -> ParseResult<u64> {
        self.comma_expr_list_limit(min_bp, u64::MAX)
    }

    /// `{NODE BORROW}`
    ///
    /// Parses residual of expressions like `'string content`. May break at comma.
    fn quote_raw_strform(&mut self) -> ParseResult<()> {
        loop {
            let token = self.o.peek_token(Mode::RawStrForm).token;
            match token.kind {
                Token::PlainStringLiteral => _ = self.o.bump(),
                Token::LBrace => {
                    self.o.b.start_node(Token::StringFormInterpPart);
                    _ = self.o.bump();
                    if self.strform_interp_part(Terminal::RBrace).is_err() {
                        _ = self.sync_to(Terminal::RBrace | Terminal::LineBreak);
                    }
                    self.o.b.finish_node();
                }
                Token::Percentage => {
                    self.o.b.start_node(Token::StringFormInterpPart);
                    _ = self.o.bump();
                    if self.strform_interp_part(Terminal::Percentage).is_err() {
                        _ = self.sync_to(Terminal::Percentage | Terminal::LineBreak);
                    }
                    self.o.b.finish_node();
                }
                Token::TernaryStrFormMarker => {
                    self.o.b.start_node(Token::StringFormInterpPart);
                    self.o.b.start_node(Token::TernaryExpr);
                    _ = self.o.bump();
                    if self.ternary_strform().is_err() {
                        _ = self.sync_to(Terminal::TernaryStrFormMarker | Terminal::LineBreak);
                    }
                    self.o.b.finish_node();
                    self.o.b.finish_node();
                }
                Token::Comma | Token::LineBreak => break,
                _ => {
                    let mut diag = self.base_diag.clone();
                    diag.span_err(
                        Default::default(),
                        token.span,
                        format!("unexpected token in string form: {:?}", token.kind),
                    );
                    self.o.emit_diag(diag);
                    return Err(());
                }
            }
        }
        Ok(())
    }

    /// `{NODE OWN}`
    fn raw_strform(&mut self) -> ParseResult<()> {
        self.o.b.start_node(Token::StringForm);
        if self.raw_strform_inner().is_err() {
            _ = self.sync_to(Terminal::LineBreak.into());
        }
        self.o.b.finish_node();
        Ok(())
    }

    /// `{NODE BORROW}`
    fn raw_strform_inner(&mut self) -> ParseResult<()> {
        loop {
            let token = self.o.peek_token(Mode::RawStrForm).token;
            match token.kind {
                Token::PlainStringLiteral => _ = self.o.bump(),
                Token::LBrace => {
                    self.o.b.start_node(Token::StringFormInterpPart);
                    _ = self.o.bump();
                    if self.strform_interp_part(Terminal::RBrace).is_err() {
                        _ = self.sync_to(Terminal::RBrace | Terminal::LineBreak);
                    }
                    self.o.b.finish_node();
                }
                Token::Percentage => {
                    self.o.b.start_node(Token::StringFormInterpPart);
                    _ = self.o.bump();
                    if self.strform_interp_part(Terminal::Percentage).is_err() {
                        _ = self.sync_to(Terminal::Percentage | Terminal::LineBreak);
                    }
                    self.o.b.finish_node();
                }
                Token::TernaryStrFormMarker => {
                    self.o.b.start_node(Token::StringFormInterpPart);
                    self.o.b.start_node(Token::TernaryExpr);
                    _ = self.o.bump();
                    if self.ternary_strform().is_err() {
                        _ = self.sync_to(Terminal::TernaryStrFormMarker | Terminal::LineBreak);
                    }
                    self.o.b.finish_node();
                    self.o.b.finish_node();
                }
                Token::LineBreak => break,
                _ => {
                    let mut diag = self.base_diag.clone();
                    diag.span_err(
                        Default::default(),
                        token.span,
                        format!("unexpected token in string form: {:?}", token.kind),
                    );
                    self.o.emit_diag(diag);
                    return Err(());
                }
            }
        }
        Ok(())
    }

    /// `{NODE BORROW}`
    fn expression_strform(&mut self) -> ParseResult<()> {
        self.expression_strform_inner(Mode::StrForm)
    }

    /// `{NODE BORROW}`
    fn plain_expression_strform(&mut self) -> ParseResult<()> {
        self.expression_strform_inner(Mode::PlainStr)
    }

    /// `{NODE BORROW}`
    fn expression_strform_inner(&mut self, mode: Mode) -> ParseResult<()> {
        loop {
            let token = self.o.peek_token(mode).token;
            match token.kind {
                Token::PlainStringLiteral => _ = self.o.bump(),
                Token::LBrace => {
                    self.o.b.start_node(Token::StringFormInterpPart);
                    _ = self.o.bump();
                    if self.strform_interp_part(Terminal::RBrace).is_err() {
                        _ = self.sync_to(Terminal::RBrace | Terminal::LineBreak);
                    }
                    self.o.b.finish_node();
                }
                Token::Percentage => {
                    self.o.b.start_node(Token::StringFormInterpPart);
                    _ = self.o.bump();
                    if self.strform_interp_part(Terminal::Percentage).is_err() {
                        _ = self.sync_to(Terminal::Percentage | Terminal::LineBreak);
                    }
                    self.o.b.finish_node();
                }
                Token::TernaryStrFormMarker => {
                    self.o.b.start_node(Token::StringFormInterpPart);
                    self.o.b.start_node(Token::TernaryExpr);
                    _ = self.o.bump();
                    if self.ternary_strform().is_err() {
                        _ = self.sync_to(Terminal::TernaryStrFormMarker | Terminal::LineBreak);
                    }
                    self.o.b.finish_node();
                    self.o.b.finish_node();
                }
                Token::DoubleQuote => {
                    _ = self.o.bump();
                    break;
                }
                _ => {
                    let mut diag = self.base_diag.clone();
                    diag.span_err(
                        Default::default(),
                        token.span,
                        format!("unexpected token in string form: {:?}", token.kind),
                    );
                    self.o.emit_diag(diag);
                    return Err(());
                }
            }
        }
        Ok(())
    }

    /// `{NODE BORROW}`
    fn ternary_strform(&mut self) -> ParseResult<()> {
        // cond
        let cp = self.o.b.checkpoint();
        if self
            .expression_bp(
                infix_binding_power(EraTokenKind::QuestionMark).unwrap().1 + 2,
                true,
                Terminal::TernaryStrFormMarker | Terminal::LineBreak,
            )
            .is_err()
        {
            _ = self.sync_cp_to(Terminal::TernaryStrFormMarker | Terminal::LineBreak, cp);
            return Ok(());
        }
        self.eat(Mode::Normal, Token::QuestionMark)?;

        trait Adhoc {
            fn parse_mid(&mut self) -> ParseResult<()>;
            fn parse_right(&mut self) -> ParseResult<()>;
        }
        impl<Callback: EraCompilerCallback> Adhoc for EraParserSite<'_, '_, '_, '_, '_, Callback> {
            fn parse_mid(&mut self) -> ParseResult<()> {
                loop {
                    let token = self.o.peek_token(Mode::TernaryStrForm).token;
                    match token.kind {
                        Token::PlainStringLiteral => _ = self.o.bump(),
                        Token::LBrace => {
                            self.o.b.start_node(Token::StringFormInterpPart);
                            _ = self.o.bump();
                            if self.strform_interp_part(Terminal::RBrace).is_err() {
                                _ = self.sync_to(Terminal::RBrace | Terminal::LineBreak);
                            }
                            self.o.b.finish_node();
                        }
                        Token::Percentage => {
                            self.o.b.start_node(Token::StringFormInterpPart);
                            _ = self.o.bump();
                            if self.strform_interp_part(Terminal::Percentage).is_err() {
                                _ = self.sync_to(Terminal::Percentage | Terminal::LineBreak);
                            }
                            self.o.b.finish_node();
                        }
                        // Token::TernaryStrFormMarker => {
                        //     _ = self.o.next_token(Mode::RawStrForm);
                        //     self.o.b.start_node_at(cp, Token::StringFormInterpPart);
                        //     self.o.b.start_node(Token::TernaryExpr);
                        //     let cp = self.o.b.checkpoint();
                        //     if self.ternary_strform().is_err() {
                        //         self.synchronize_cp_to_n(&[Token::LineBreak], cp);
                        //     }
                        //     self.o.b.finish_node();
                        //     self.o.b.finish_node();
                        // }
                        Token::NumberSign => break,
                        _ => {
                            let mut diag = self.base_diag.clone();
                            diag.span_err(
                                Default::default(),
                                token.span,
                                format!("unexpected token in string form: {:?}", token.kind),
                            );
                            self.o.emit_diag(diag);
                            return Err(());
                        }
                    }
                }
                Ok(())
            }

            fn parse_right(&mut self) -> ParseResult<()> {
                loop {
                    let token = self.o.peek_token(Mode::RawStrForm).token;
                    match token.kind {
                        Token::PlainStringLiteral => _ = self.o.bump(),
                        Token::LBrace => {
                            self.o.b.start_node(Token::StringFormInterpPart);
                            _ = self.o.bump();
                            if self.strform_interp_part(Terminal::RBrace).is_err() {
                                _ = self.sync_to(Terminal::RBrace | Terminal::LineBreak);
                            }
                            self.o.b.finish_node();
                        }
                        Token::Percentage => {
                            self.o.b.start_node(Token::StringFormInterpPart);
                            _ = self.o.bump();
                            if self.strform_interp_part(Terminal::Percentage).is_err() {
                                _ = self.sync_to(Terminal::Percentage | Terminal::LineBreak);
                            }
                            self.o.b.finish_node();
                        }
                        Token::TernaryStrFormMarker => break,
                        _ => {
                            let mut diag = self.base_diag.clone();
                            diag.span_err(
                                Default::default(),
                                token.span,
                                format!("unexpected token in string form: {:?}", token.kind),
                            );
                            self.o.emit_diag(diag);
                            return Err(());
                        }
                    }
                }
                Ok(())
            }
        }

        // mid
        self.o.b.start_node(Token::StringForm);
        if self.parse_mid().is_err() {
            _ = self.sync_to(Terminal::TernaryStrFormMarker | Terminal::LineBreak);
            self.o.b.finish_node();
            return Ok(());
        }
        self.o.b.finish_node();
        self.eat(Mode::TernaryStrForm, Token::NumberSign)?;
        // right
        self.o.b.start_node(Token::StringForm);
        if self.parse_right().is_err() {
            _ = self.sync_to(Terminal::TernaryStrFormMarker | Terminal::LineBreak);
            self.o.b.finish_node();
            return Ok(());
        }
        self.o.b.finish_node();
        self.eat(Mode::RawStrForm, Token::TernaryStrFormMarker)?;

        Ok(())
    }

    /// `{NODE BORROW}`
    fn strform_interp_part(&mut self, terminal: EraTerminalTokenKind) -> ParseResult<()> {
        let mode = if terminal == Terminal::Percentage {
            Mode::InlineNormal
        } else {
            Mode::Normal
        };
        // expr
        self.expression_bp(0, true, terminal.into())?;
        if self.try_eat(Mode::Normal, Token::Comma).is_some() {
            // width
            self.expression_bp(0, true, terminal.into())?;
        }
        if self.try_eat(Mode::Normal, Token::Comma).is_some() {
            // alignment
            self.eat(Mode::Normal, Token::Identifier)?;
        }
        self.eat(mode, terminal.into())?;
        Ok(())
    }

    fn skip_newline(&mut self) {
        while self.o.peek_token(Mode::Normal).token.kind == Token::LineBreak {
            // _ = self.o.next_token_with_newline(Mode::Normal);
            _ = self.o.bump();
        }
        self.o.set_is_panicking(false);
    }

    fn is_var_str(&self, name: &str) -> bool {
        self.local_str_vars.contains(Ascii::new_str(name))
            || self
                .o
                .l
                .get_ctx()
                .variables
                .get_var(name)
                .map_or(false, |x| x.kind().is_str())
    }

    // fn synchronize_to(&mut self, token: Token) {
    //     // self.o.b.start_node(Token::Invalid);
    //     // loop {
    //     //     let next = self.o.peek_token(Mode::Normal).token.kind;
    //     //     if next == token || next == Token::Eof {
    //     //         break;
    //     //     }
    //     //     _ = self.o.next_token(Mode::Normal);
    //     // }
    //     // self.o.b.finish_node();
    //     self.synchronize_to_n(&[token]);
    // }

    // fn synchronize_to2(&mut self, token1: Token, token2: Token) {
    //     self.synchronize_to_n(&[token1, token2]);
    // }

    // fn synchronize_to3(&mut self, token1: Token, token2: Token, token3: Token) {
    //     self.synchronize_to_n(&[token1, token2, token3]);
    // }

    // fn synchronize_to4(&mut self, token1: Token, token2: Token, token3: Token, token4: Token) {
    //     self.synchronize_to_n(&[token1, token2, token3, token4]);
    // }

    // fn synchronize_to_n(&mut self, tokens: &[Token]) {
    //     self.o.b.start_node(Token::Invalid);
    //     // NOTE: No overread check since we cannot compensate for the overread token here.
    //     //       The caller should ensure that the tokens are not overread.
    //     loop {
    //         let next = self.o.peek_token(Mode::Normal).token.kind;
    //         if tokens.contains(&next) || next == Token::Eof {
    //             break;
    //         }
    //         _ = self.o.next_token(Mode::Normal);
    //     }
    //     self.o.b.finish_node();
    // }

    // fn synchronize_cp_to_n(&mut self, tokens: &[Token], checkpoint: cstree::build::Checkpoint) {
    //     self.o.b.start_node_at(checkpoint, Token::Invalid);

    //     // Remember to check the previous token because we may have
    //     // overread the token that caused the error.
    //     let prev = self.o.l.previous_token();
    //     // NOTE: `)` should be consumed per synchronization (consider the case of parsing `5+(3+(4]))`,
    //     //       where the first `)`, if not consumed, will cause the parser to leave out the second `)`.
    //     if tokens
    //         .iter()
    //         .filter(|x| !matches!(x, Token::RParen))
    //         .contains(&prev)
    //     {
    //         self.o.b.finish_node();
    //         return;
    //     }

    //     loop {
    //         let next = self.o.peek_token(Mode::Normal).token.kind;
    //         if tokens.contains(&next) || next == Token::Eof {
    //             break;
    //         }
    //         _ = self.o.next_token(Mode::Normal);
    //     }
    //     self.o.b.finish_node();
    // }

    /// Synchronize to the next terminal token. If encountered LineBreak, Err is returned.
    fn sync_to(&mut self, terminals: enumset::EnumSet<EraTerminalTokenKind>) -> ParseResult<()> {
        // // Remember to check the previous token because we may have
        // // overread the token that caused the error.
        // if terminals.contains(Terminal::LineBreak) && self.o.l.previous_token() == Token::LineBreak
        // {
        //     return Err(());
        // }

        let cp = self.o.b.checkpoint();
        let mut need_build_node = false;
        let result = loop {
            let next = self.o.peek_token(Mode::Normal).token.kind;
            if next == Token::Eof {
                break Err(());
            }
            let Ok(next) = next.try_into() else {
                _ = self.o.bump();
                need_build_node = true;
                continue;
            };
            if terminals.contains(next) {
                if next == Terminal::LineBreak {
                    break Err(());
                }
                // _ = self.o.bump();
                // need_build_node = true;
                break Ok(());
            }
            _ = self.o.bump();
            need_build_node = true;
        };

        if need_build_node {
            self.o.b.start_node_at(cp, Token::Invalid);
            self.o.b.finish_node();
        }

        // Move terminal token outside the Invalid node
        if result.is_ok() {
            _ = self.o.bump();
        }

        result
    }

    fn sync_to_finish(
        &mut self,
        terminals: enumset::EnumSet<EraTerminalTokenKind>,
    ) -> ParseResult<()> {
        let result = self.sync_to(terminals);
        if result.is_err() {
            self.o.b.finish_node();
        }
        result
    }

    fn sync_cp_to(
        &mut self,
        terminals: enumset::EnumSet<EraTerminalTokenKind>,
        cp: Checkpoint,
    ) -> ParseResult<()> {
        self.o.b.start_node_at(cp, Token::Invalid);
        let result = self.sync_to(terminals);
        self.o.b.finish_node();
        result
    }

    fn expect_sync_to_newline(&mut self) -> ParseResult<()> {
        let token = self.o.peek_token(Mode::Normal).token;
        if token.kind != Token::LineBreak && !self.o.l.at_start_of_line() {
            // if token.kind != Token::LineBreak {
            let mut diag = self.base_diag.clone();
            diag.span_err(
                Default::default(),
                token.span,
                format!("expected newline, found {:?}", token.kind),
            );
            self.o.emit_diag(diag);
            _ = self.sync_to(Terminal::LineBreak.into());
            Err(())
        } else {
            Ok(())
        }
    }

    // fn synchronize_eat_sync(&mut self) {
    //     self.o.b.start_node(Token::Invalid);
    //     'outer: loop {
    //         for &(mode, token) in self.eat_syncs.iter().rev() {
    //             if [Token::Eof, token].contains(&self.o.peek_token(mode).token.kind) {
    //                 while let Some((_, sync_token)) = self.eat_syncs.pop() {
    //                     if sync_token == token {
    //                         break;
    //                     }
    //                 }
    //                 break 'outer;
    //             }
    //         }
    //         _ = self.o.next_token(Mode::Normal);
    //     }
    //     self.o.b.finish_node();
    // }

    // fn synchronize_eat_sync_cp(&mut self, checkpoint: cstree::build::Checkpoint) {
    //     self.o.b.start_node_at(checkpoint, Token::Invalid);
    //     'outer: loop {
    //         for &(mode, token) in self.eat_syncs.iter().rev() {
    //             if [Token::Eof, token].contains(&self.o.peek_token(mode).token.kind) {
    //                 while let Some((_, sync_token)) = self.eat_syncs.pop() {
    //                     if sync_token == token {
    //                         break;
    //                     }
    //                 }
    //                 break 'outer;
    //             }
    //         }
    //         _ = self.o.next_token(Mode::Normal);
    //     }
    //     self.o.b.finish_node();
    // }

    fn consume_raw(&mut self, mode: Mode, token: Token) -> ParseResult<EraLexerNextResult> {
        let next = self.o.peek_token(mode);
        if next.token.kind == token {
            Ok(self.o.bump())
        } else {
            let mut diag = self.base_diag.clone();
            diag.span_err(
                Default::default(),
                next.token.span,
                format!("expected {:?}, found {:?}", token, next.token.kind),
            );
            self.o.emit_diag(diag);
            Err(())
        }
    }

    fn try_consume_raw(&mut self, mode: Mode, token: Token) -> Option<EraLexerNextResult> {
        let next = self.o.peek_token(mode);
        if next.token.kind == token {
            Some(self.o.next_token(mode))
        } else {
            None
        }
    }

    // fn consume(&mut self, mode: Mode, token: Token) -> ParseResult<EraLexerNextResult> {
    //     if let Ok(next) = self.consume_raw(Mode::Normal, token) {
    //         Ok(next)
    //     } else {
    //         self.synchronize_to(Token::LineBreak);
    //         Err(())
    //     }
    // }

    // fn eat_push_sync(
    //     &mut self,
    //     eat_mode: Mode,
    //     eat_token: Token,
    //     end_mode: Mode,
    //     end_token: Token,
    // ) -> ParseResult<()> {
    //     self.eat(eat_mode, eat_token)?;
    //     self.eat_syncs.push((end_mode, end_token));
    //     Ok(())
    // }

    // fn push_eat_sync(&mut self, mode: Mode, token: Token) {
    //     self.eat_syncs.push((mode, token));
    // }

    // fn end_eat_sync(&mut self) -> ParseResult<()> {
    //     let (mode, token) = self
    //         .eat_syncs
    //         .last()
    //         .copied()
    //         .unwrap_or((Mode::Normal, Token::Eof));
    //     if self.eat(mode, token).is_err() {
    //         self.synchronize_eat_sync();
    //         Err(())
    //     } else {
    //         Ok(())
    //     }
    // }

    // /// Consume a token. If failed, synchronize to the next token, producing an node
    // /// containing the Invalid token.
    // fn eat(&mut self, mode: Mode, token: Token) -> ParseResult<EraLexerNextResult> {
    //     // SAFETY: This requires the Polonius borrow checker.
    //     let result: ParseResult<EraLexerNextResult> =
    //         unsafe { std::mem::transmute(self.consume_raw(mode, token)) };
    //     if let Ok(next) = result {
    //         return Ok(next);
    //     } else {
    //         self.synchronize_eat_sync();
    //         self.o.b.finish_node();
    //         Err(())
    //     }
    // }

    // /// Same as [`Self::eat`], except that `finish_node` is not called.
    // fn eat_wo_node(
    //     &mut self,
    //     mode: Mode,
    //     token: Token,
    //     cp: cstree::build::Checkpoint,
    // ) -> ParseResult<EraLexerNextResult> {
    //     // SAFETY: This requires the Polonius borrow checker.
    //     let result: ParseResult<EraLexerNextResult> =
    //         unsafe { std::mem::transmute(self.consume_raw(mode, token)) };
    //     if let Ok(next) = result {
    //         return Ok(next);
    //     } else {
    //         self.synchronize_eat_sync_cp(cp);
    //         Err(())
    //     }
    // }

    fn eat(&mut self, mode: Mode, token: Token) -> ParseResult<EraLexerNextResult> {
        self.consume_raw(mode, token)
    }

    fn try_eat(&mut self, mode: Mode, token: Token) -> Option<EraLexerNextResult> {
        self.try_consume_raw(mode, token)
    }

    fn eat_or_sync(
        &mut self,
        mode: Mode,
        token: Token,
        terminals: enumset::EnumSet<EraTerminalTokenKind>,
    ) -> ParseResult<EraLexerNextResult> {
        // SAFETY: This requires the Polonius borrow checker.
        let result: ParseResult<EraLexerNextResult> =
            unsafe { std::mem::transmute(self.eat(mode, token)) };
        let Ok(result) = result else {
            _ = self.sync_to(terminals);
            return Err(());
        };
        Ok(result)
    }

    fn try_match_command(&mut self, cmd: &str) -> bool {
        let token = self.o.peek_token(Mode::Normal);
        token.token.kind == Token::Identifier && token.lexeme.eq_ignore_ascii_case(cmd)
    }

    fn try_eat_command(&mut self, cmd: &str) -> bool {
        let result = self.try_match_command(cmd);
        if result {
            _ = self.o.bump();
        }
        result
    }

    // /// Consume a token. If failed, synchronize to the next token, producing an Invalid
    // /// node based on the given checkpoint.
    // fn eat_or_sync_cp(
    //     &mut self,
    //     mode: Mode,
    //     token: Token,
    //     cp: cstree::build::Checkpoint,
    // ) -> ParseResult<EraLexerNextResult> {
    //     // SAFETY: This requires the Polonius borrow checker.
    //     let result: ParseResult<EraLexerNextResult> =
    //         unsafe { std::mem::transmute(self.consume_raw(mode, token)) };
    //     if let Ok(next) = result {
    //         Ok(next)
    //     } else {
    //         self.o.b.finish_node();
    //         self.synchronize_eat_sync_cp(cp);
    //         Err(())
    //     }
    // }
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
        CmpLT | CmpLEq | CmpGT | CmpGEq => (17, 18),
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
        LParen => (29, ()),
        _ => return None,
    })
}

#[derive(Debug, enumset::EnumSetType)]
enum EraTerminalTokenKind {
    LineBreak,
    Percentage,
    RParen,
    RBrace,
    Comma,
    TernaryStrFormMarker,
    DoubleQuote,
}

use EraTerminalTokenKind as Terminal;

impl TryFrom<EraTokenKind> for EraTerminalTokenKind {
    type Error = ();

    fn try_from(kind: EraTokenKind) -> Result<Self, Self::Error> {
        match kind {
            EraTokenKind::LineBreak => Ok(Self::LineBreak),
            EraTokenKind::Percentage => Ok(Self::Percentage),
            EraTokenKind::RParen => Ok(Self::RParen),
            EraTokenKind::RBrace => Ok(Self::RBrace),
            EraTokenKind::Comma => Ok(Self::Comma),
            EraTokenKind::TernaryStrFormMarker => Ok(Self::TernaryStrFormMarker),
            EraTokenKind::DoubleQuote => Ok(Self::DoubleQuote),
            _ => Err(()),
        }
    }
}

impl From<EraTerminalTokenKind> for EraTokenKind {
    fn from(kind: EraTerminalTokenKind) -> Self {
        match kind {
            EraTerminalTokenKind::LineBreak => Self::LineBreak,
            EraTerminalTokenKind::Percentage => Self::Percentage,
            EraTerminalTokenKind::RParen => Self::RParen,
            EraTerminalTokenKind::RBrace => Self::RBrace,
            EraTerminalTokenKind::Comma => Self::Comma,
            EraTerminalTokenKind::TernaryStrFormMarker => Self::TernaryStrFormMarker,
            EraTerminalTokenKind::DoubleQuote => Self::DoubleQuote,
        }
    }
}

trait EraTerminalTokenKindAdhoc {
    fn to_tokens_vec(&self) -> Vec<EraTokenKind>;
}

impl EraTerminalTokenKindAdhoc for enumset::EnumSet<EraTerminalTokenKind> {
    fn to_tokens_vec(&self) -> Vec<EraTokenKind> {
        self.iter().map(Into::into).collect()
    }
}
