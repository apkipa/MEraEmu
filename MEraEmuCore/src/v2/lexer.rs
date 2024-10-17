use std::{hint::unreachable_unchecked, result};

use bstr::ByteSlice;
use rclite::Rc;

use crate::types::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EraLexerMode {
    /// The most basic mode.
    ///
    /// Examples: `PRINTFORM ...`, `a += 42`
    Normal,
    /// The mode for parsing inline expressions, usually inside strforms.
    ///
    /// Examples: `@"Hello, {name + "!"}"`
    InlineNormal,
    /// The mode for parsing sharp declarations, which start with `#`.
    ///
    /// Examples: `#DIM x = 42`
    SharpDecl,
    /// The mode for parsing string forms, which start with `@"` (i.e. quoted).
    StrForm, // Quoted string form expression
    /// The mode for parsing raw string forms, which are unquoted.
    RawStrForm, // Unquoted string form
    /// The mode for parsing raw strings, which are unquoted.
    RawStr, // Unquoted string
    PlainStr, // Quoted string
    // TODO: Reject comments inside StrForm properly by introducing a new EraLexerMode
    /// The mode for parsing ternary string forms.
    ///
    /// Examples: `PRINTFORML Eat \@ IS_APPLE == 1 ? apple # banana \@ to recover health.`
    TernaryStrForm,
    /// The mode for parsing call forms.
    ///
    /// Examples: `CALLFORM DynFunc{funcId}("hello!")`
    CallForm,
    /// The mode for parsing raw strings inside PRINTV, which starts with `'`.
    ///
    /// Examples: `PRINTV 'You have , numItems, ' items.`
    CommaRawStr,
    // TODO: Document this mode
    ExpressionS,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct EraLexerNextResult<'a> {
    pub token: EraToken,
    /// Whether the token comes from a macro or replacement. When true,
    /// token span refers to the original text containing the macro.
    /// It is possible for multiple tokens to share the same span when
    /// they are from the same macro.
    pub is_replaced: bool,
    pub lexeme: &'a str,
}

#[derive(Debug)]
pub struct EraLexer<'a, 'ctx, Callback> {
    o: EraLexerOuterState<'a, 'ctx, Callback>,
    i: EraLexerInnerState<'ctx>,
    // NOTE: 'static is used to avoid lifetime issues with self-referential structs.
    next_i: Option<(
        EraLexerMode,
        EraLexerNextResult<'static>,
        EraLexerInnerState<'ctx>,
    )>,
    // HACK: For StableDeref of lexeme
    cart: Vec<u8>,
}

#[derive(Debug)]
struct EraLexerOuterState<'a, 'ctx, Callback> {
    src: &'a [u8],
    ctx: &'ctx mut EraCompilerCtx<Callback>,
}

impl<'a, 'ctx, Callback: EraCompilerCallback> EraLexerOuterState<'a, 'ctx, Callback> {
    pub fn new_diag(&self) -> Diagnostic<'a> {
        Diagnostic::with_src(self.ctx.active_source.clone(), self.src)
    }
}

impl<'a, 'ctx, Callback: EraCompilerCallback> EraLexer<'a, 'ctx, Callback> {
    pub fn new(ctx: &'ctx mut EraCompilerCtx<Callback>, src: &'a str) -> Self {
        Self {
            o: EraLexerOuterState {
                src: src.as_bytes(),
                ctx,
            },
            i: EraLexerInnerState::new(),
            next_i: None,
            cart: Vec::new(),
        }
    }

    pub fn get_src(&self) -> &'a str {
        // SAFETY: The source is always valid UTF-8
        unsafe { std::str::from_utf8_unchecked(self.o.src) }
    }

    pub fn get_src_span(&self, span: SrcSpan) -> &'a str {
        let src = self.get_src();
        let start = span.start().0 as usize;
        let end = span.end().0 as usize;
        &src[start..end]
    }

    // HACK: expose ctx for now
    pub fn get_ctx(&self) -> &EraCompilerCtx<Callback> {
        self.o.ctx
    }

    /// ## Safety
    ///
    /// The returned context must not be mutated in a way that causes dangling references.
    /// For example, it is forbidden to remove an entry from the `global_replace` map.
    pub unsafe fn get_ctx_mut(&mut self) -> &mut EraCompilerCtx<Callback> {
        self.o.ctx
    }

    pub fn make_diag(&self) -> Diagnostic<'a> {
        self.o.new_diag()
    }

    pub fn emit_diag(&mut self, diag: Diagnostic<'a>) {
        self.o.ctx.emit_diag(diag);
    }

    pub fn push_define(&mut self, name: Box<str>, data: EraDefineData) {
        let source_map = Rc::get_mut(&mut self.o.ctx.source_map).unwrap();
        source_map
            .get_mut(&self.o.ctx.active_source)
            .map(|x| &mut x.defines)
            .unwrap_or(&mut self.o.ctx.global_define)
            .insert(name, data);
    }

    pub fn peek(&mut self, mode: EraLexerMode) -> EraLexerNextResult {
        match &self.next_i {
            Some((nmode, ..)) if *nmode == mode => {
                // Return the cached result
                return self.next_i.as_ref().unwrap().1;
            }
            _ => {}
        }

        // Load new token
        let mut next_inner = self.i.clone();
        let result = unsafe { next_inner.next_token(mode, &mut self.o, &mut self.cart) };
        self.next_i = Some((mode, result, next_inner));

        self.next_i.as_ref().unwrap().1
    }

    pub fn read(&mut self, mode: EraLexerMode) -> EraLexerNextResult {
        _ = self.peek(mode);
        let (_, result, i) = self.next_i.take().unwrap();
        self.i = i;
        result
    }

    pub fn bump(&mut self) -> EraLexerNextResult {
        let (_, result, i) = self
            .next_i
            .take()
            .expect("cannot bump token without peeking first");
        self.i = i;
        result
    }

    pub fn previous_token(&self) -> EraTokenKind {
        self.i.last_token
    }

    pub fn at_start_of_line(&self) -> bool {
        self.i.last_is_newline
    }

    /// Returns the positions of all newlines (start of line) in the source.
    pub fn newline_positions(&self) -> Vec<SrcPos> {
        let mut positions = vec![SrcPos(0)];
        let mut src = self.o.src;
        let mut offset = 0;
        while let Some(pos) = memchr::memchr(b'\n', src) {
            offset += pos + 1;
            positions.push(SrcPos(offset as _));
            src = &src[pos + 1..];
        }
        positions
    }
}

#[derive(Debug, Clone)]
struct EraLexerInnerState<'ctx> {
    src_pos: usize,
    replace_len: usize,
    alternate_src: &'ctx [u8],
    loc: SrcLoc,
    last_token: EraTokenKind,
    last_is_newline: bool,
    /// The number of pushed newline suppressions, introduced by `{` and ended by `}`.
    suppress_newline_cnt: u32,
}

impl<'ctx> EraLexerInnerState<'ctx> {
    fn new() -> Self {
        Self {
            src_pos: 0,
            replace_len: 0,
            alternate_src: b"",
            loc: SrcLoc { line: 1, col: 0 },
            last_token: EraTokenKind::LineBreak,
            last_is_newline: true,
            suppress_newline_cnt: 0,
        }
    }

    // SAFETY: The returned token has its lifetime erased, possibly attached to the cart
    //         (outer.cart) satisfying StableDeref. Special care is required for lifetime issues.
    unsafe fn next_token<Callback: EraCompilerCallback>(
        &mut self,
        mode: EraLexerMode,
        outer: &mut EraLexerOuterState<'_, 'ctx, Callback>,
        cart: &mut Vec<u8>,
    ) -> EraLexerNextResult<'static> {
        cart.clear();

        let mut site = EraLexerInnerSite {
            o: outer,
            i: self,
            cart,
            cur_lexeme_start: std::ptr::null(),
            cur_lexeme_len: 0,
            has_replaced: false,
        };

        let pos_start = if site.has_replacement() {
            site.i.src_pos - site.i.replace_len
        } else {
            site.i.src_pos
        };

        let token = {
            let mut token = site.next_token(mode);
            if token != EraTokenKind::WhiteSpace {
                site.i.last_is_newline = matches!(token, EraTokenKind::LineBreak);
            }
            if site.i.last_is_newline && site.is_suppress_newline() {
                token = EraTokenKind::WhiteSpace;
            }
            token
        };
        site.i.last_token = token;

        let pos_end = site.i.src_pos;
        let current_lexeme = site.current_lexeme();
        debug_assert!(
            current_lexeme.is_utf8(),
            "lexeme is not valid UTF-8: {:?}\nSource file: {}\nOffset: {}-{}",
            current_lexeme,
            site.o.ctx.active_source,
            pos_start,
            pos_end
        );
        let result = EraLexerNextResult {
            token: EraToken {
                kind: token,
                span: SrcSpan::new(SrcPos(pos_start as _), (pos_end - pos_start) as _),
            },
            is_replaced: site.has_replaced,
            // lexeme: if site.cur_lexeme_start.is_null() {
            //     if site.cur_lexeme_len == 0 {
            //         // No lexeme
            //         b""
            //     } else {
            //         // HACK: Get composed lexeme from the cart
            //         let lexeme = &cart[..];
            //         std::mem::transmute(lexeme)
            //     }
            // } else {
            //     let lexeme = std::slice::from_raw_parts(site.cur_lexeme_start, site.cur_lexeme_len);
            //     lexeme
            // },
            lexeme: std::mem::transmute::<_, &[u8]>(current_lexeme).to_str_unchecked(),
        };

        result
    }
}

#[derive(Debug)]
struct EraLexerInnerSite<'a, 'ctx, 'c, Callback> {
    o: &'c mut EraLexerOuterState<'a, 'ctx, Callback>,
    i: &'c mut EraLexerInnerState<'ctx>,
    cart: &'c mut Vec<u8>,
    cur_lexeme_start: *const u8,
    cur_lexeme_len: usize,
    has_replaced: bool,
}

impl<'a, 'ctx, 'c, Callback: EraCompilerCallback> EraLexerInnerSite<'a, 'ctx, 'c, Callback> {
    pub fn next_token(&mut self, mode: EraLexerMode) -> EraTokenKind {
        use crate::util::bmatch_caseless;
        use EraLexerMode as Mode;
        use EraTokenKind::*;

        let initial_src_pos = if self.has_replacement() {
            self.i.src_pos - self.i.replace_len
        } else {
            self.i.src_pos
        };

        let Some(ch) = self.next_char() else {
            return if !matches!(self.i.last_token, LineBreak | Eof) {
                // HACK: Ensure newline so that parser can handle it correctly
                LineBreak
            } else {
                Eof
            };
        };

        trait Adhoc {
            fn match_token2(
                &mut self,
                seconds: &[(u8, EraTokenKind)],
                first: EraTokenKind,
            ) -> EraTokenKind;
            fn read_newline_with_space(&mut self, starts_with_newline: bool) -> EraTokenKind;
        }
        impl<Callback: EraCompilerCallback> Adhoc for EraLexerInnerSite<'_, '_, '_, Callback> {
            fn match_token2(
                &mut self,
                seconds: &[(u8, EraTokenKind)],
                first: EraTokenKind,
            ) -> EraTokenKind {
                let ch = self.peek_char();
                if let Some(&(_, kind)) = seconds.iter().find(|(c, _)| Some(*c) == ch) {
                    self.next_char();
                    return kind;
                }
                first
            }
            fn read_newline_with_space(&mut self, starts_with_newline: bool) -> EraTokenKind {
                let mut has_newline = starts_with_newline;
                while let Some(ch) = self.peek_char() {
                    match ch {
                        b'\r' | b'\n' => {
                            has_newline = true;
                        }
                        _ if is_whitespace(ch) => (),
                        _ => break,
                    }
                    self.next_char();
                }

                // if self.is_suppress_newline() {
                //     has_newline = false;
                // }

                if has_newline {
                    LineBreak
                } else {
                    WhiteSpace
                }
            }
        }

        if self.i.last_token == Identifier
            && matches!(mode, Mode::RawStr | Mode::CommaRawStr | Mode::RawStrForm)
        {
            // HACK: For statements like PRINTFORM, remove first whitespace only
            match ch {
                _ if is_whitespace(ch) => {
                    return WhiteSpace;
                }
                b'\r' if self.is_suppress_newline() => {
                    if let Some(b'\n') = self.peek_char() {
                        self.next_char();
                    }
                    return WhiteSpace;
                }
                b'\n' if self.is_suppress_newline() => {
                    return WhiteSpace;
                }
                _ => (),
            }
        }

        // Handle newline suppression
        if self.i.last_is_newline {
            match ch {
                b'{' => {
                    self.push_suppress_newline();
                    return WhiteSpace;
                }
                b'}' => {
                    if self.is_suppress_newline() {
                        self.pop_suppress_newline();
                        return WhiteSpace;
                    } else {
                        let mut diag = self.o.new_diag();
                        diag.span_err(
                            Default::default(),
                            SrcSpan::new(SrcPos(initial_src_pos as _), 1),
                            "unexpected `}`",
                        );
                        self.o.ctx.emit_diag(diag);
                        return Invalid;
                    }
                }
                _ => (),
            }
        }

        let token_kind = match mode {
            Mode::Normal | Mode::SharpDecl | Mode::InlineNormal | Mode::ExpressionS => match ch {
                _ if is_whitespace(ch) => {
                    // Skip whitespace
                    while let Some(ch) = self.peek_char() {
                        match ch {
                            _ if is_whitespace(ch) => (),
                            _ => break,
                        }
                        self.next_char();
                    }

                    WhiteSpace
                }
                b'\r' | b'\n' => {
                    while let Some(ch) = self.peek_char() {
                        match ch {
                            b'\r' | b'\n' => (),
                            _ => break,
                        }
                        self.next_char();
                    }

                    LineBreak
                }
                b'@' => self.match_token2(&[(b'"', StringFormStart)], At),
                b';' => {
                    // Comments
                    self.skip_char_while(|x| !matches!(x, b'\r' | b'\n'));
                    Comment
                }
                b'#' => NumberSign,
                b',' => Comma,
                b':' => Colon,
                b'$' => Dollar,
                b'~' => BitNot,
                b'+' => self.match_token2(&[(b'=', PlusAssign), (b'+', Increment)], Plus),
                b'-' => self.match_token2(&[(b'=', MinusAssign), (b'-', Decrement)], Minus),
                b'*' => self.match_token2(&[(b'=', MultiplyAssign)], Multiply),
                b'/' => self.match_token2(&[(b'=', DivideAssign)], Divide),
                b'%' => {
                    if let Mode::InlineNormal = mode {
                        Percentage
                    } else {
                        self.match_token2(&[(b'=', ModuloAssign)], Percentage)
                    }
                }
                b'?' => QuestionMark,
                b'{' => LBrace,
                b'}' => RBrace,
                b'(' => LParen,
                b')' => RParen,
                b'&' => self.match_token2(&[(b'=', BitAndAssign), (b'&', LogicalAnd)], BitAnd),
                b'|' => self.match_token2(&[(b'=', BitOrAssign), (b'|', LogicalOr)], BitOr),
                b'^' => self.match_token2(&[(b'=', BitXorAssign)], BitXor),
                b'!' => self.match_token2(&[(b'=', CmpNEq)], LogicalNot),
                b'=' => self.match_token2(&[(b'=', CmpEq)], Assign),
                b'<' => self.match_token2(&[(b'=', CmpLEq), (b'<', BitShiftL)], CmpLT),
                b'>' => self.match_token2(&[(b'=', CmpGEq), (b'>', BitShiftR)], CmpGT),
                b'\'' => self.match_token2(&[(b'=', ExprAssign)], SingleQuote),
                b'\\' if self.peek_char() == Some(b'@') => {
                    self.next_char();
                    TernaryStrFormMarker
                }
                _ if is_initial_ident_char(ch) => {
                    // Handle identifier & command
                    while self.peek_char().map(is_ident_char).unwrap_or(false) {
                        self.next_char();
                    }
                    // SAFETY: Lexeme is valid UTF-8
                    let lexeme = unsafe { self.current_lexeme().to_str_unchecked() };

                    // Try handle `#DEFINE` replacements
                    if !self.has_replacement() {
                        // TODO: Lookup local define in the future
                        if let Some(define) = self.o.ctx.global_define.get(lexeme) {
                            // SAFETY: We never remove entries from define_list, so we
                            //         can transmute lifetimes without UB
                            let replace =
                                unsafe { std::mem::transmute::<_, &str>(&define.data[..]) };
                            self.apply_replacement(replace.as_bytes(), lexeme.len());
                            // Restart read via recursion (ensures correct lexeme)
                            self.reset_lexeme();
                            return self.next_token(mode);
                        }
                    }

                    // Handle specific keywords
                    if let Mode::SharpDecl = mode {
                        let matcher = bmatch_caseless! {
                            b"DIM" => KwDim,
                            b"DIMS" => KwDimS,
                            b"GLOBAL" => KwGlobal,
                            b"DYNAMIC" => KwDynamic,
                            b"REF" => KwRef,
                            b"CONST" => KwConst,
                            b"SAVEDATA" => KwSavedata,
                            b"CHARADATA" => KwCharadata,
                            b"LOCALSIZE" => KwLocalSize,
                            b"LOCALSSIZE" => KwLocalSSize,
                            b"FUNCTION" => KwFunction,
                            b"FUNCTIONS" => KwFunctionS,
                            b"DEFINE" => KwDefine,
                            b"ONLY" => KwOnly,
                            b"PRI" => KwPri,
                            b"LATER" => KwLater,
                            b"SINGLE" => KwSingle,
                            b"TRANSIENT" => KwTransient,
                            _ => Identifier,
                        };
                        matcher(lexeme.as_bytes())
                    } else {
                        Identifier
                    }
                }
                b'0' => {
                    // Handle special integer literals
                    match self.peek_char() {
                        Some(b'x' | b'X') => {
                            // Hex
                            _ = self.next_char();
                            self.skip_char_while(|x| x.is_ascii_hexdigit());
                        }
                        Some(b'b' | b'B') => {
                            // Bin
                            _ = self.next_char();
                            self.skip_char_while(|x| matches!(x, b'0' | b'1'));
                        }
                        /*
                        Some(b'0'..=b'7') => {
                            // Octal
                            self.skip_char_while(|x| matches!(x, b'0'..=b'7'));
                        }
                        _ => {
                            // Plain zero
                        }
                        */
                        Some(b'0'..=b'9') => {
                            // Decimal
                            self.skip_char_while(|x| matches!(x, b'0'..=b'9'));
                        }
                        _ => {
                            // Plain zero
                        }
                    }
                    if matches!(self.peek_char(), Some(b'p' | b'P')) {
                        _ = self.next_char();
                        self.skip_char_while(|x| matches!(x, b'0'..=b'9'));
                    }
                    IntLiteral
                }
                b'1'..=b'9' => {
                    // Handle decimal integer literal
                    // TODO: Support scientific notation
                    self.skip_char_while(|x| x.is_ascii_digit());
                    if matches!(self.peek_char(), Some(b'p' | b'P')) {
                        _ = self.next_char();
                        self.skip_char_while(|x| matches!(x, b'0'..=b'9'));
                    }
                    IntLiteral
                }
                // b'"' => {
                //     if matches!(mode, EraLexerMode::ExpressionS) {
                //         DoubleQuote
                //     } else {
                //         // Normal string literal
                //         let mut warned_newline = false;
                //         loop {
                //             let Some(ch) = self.next_char() else {
                //                 let mut diag = self.o.new_diag();
                //                 diag.span_err(
                //                     Default::default(),
                //                     SrcSpan::new(
                //                         SrcPos(initial_src_pos as _),
                //                         (self.i.src_pos - initial_src_pos) as _,
                //                     ),
                //                     "unterminated string literal",
                //                 );
                //                 self.o.ctx.emit_diag(diag);
                //                 break Invalid;
                //             };
                //             if ch == b'"' {
                //                 break StringLiteral;
                //             }
                //             if ch == b'\\' {
                //                 self.next_char();
                //             }
                //             if ch == b'\n' && !warned_newline {
                //                 warned_newline = true;
                //                 let mut diag = self.o.new_diag();
                //                 diag.span_warn(
                //                     Default::default(),
                //                     SrcSpan::new(SrcPos((self.i.src_pos - 1) as _), 1),
                //                     "newline in string literal",
                //                 );
                //                 self.o.ctx.emit_diag(diag);
                //             }
                //         }
                //     }
                // }
                b'"' => DoubleQuote,
                b'[' if self.i.last_is_newline => {
                    // Special preprocessing (macro?)

                    trait Adhoc {
                        fn read_fn(&mut self) -> Option<Box<[u8]>>;
                    }
                    impl<Callback: EraCompilerCallback> Adhoc for EraLexerInnerSite<'_, '_, '_, Callback> {
                        fn read_fn(&mut self) -> Option<Box<[u8]>> {
                            let lexeme_start = self.cur_lexeme_len;
                            loop {
                                if let None | Some(b']' | b'\r' | b'\n') = self.next_char() {
                                    break;
                                }
                            }
                            let lexeme = &self.current_lexeme()[lexeme_start..];
                            let [lexeme @ .., b']'] = lexeme else {
                                return None;
                            };
                            let mut lexeme: Box<[u8]> = lexeme.into();
                            lexeme.make_ascii_uppercase();
                            Some(lexeme)
                        }
                    }

                    let lexeme_pos_start = initial_src_pos;
                    let lexeme_len = self.i.src_pos - lexeme_pos_start;
                    let lexeme_span = SrcSpan::new(SrcPos(lexeme_pos_start as _), lexeme_len as _);

                    let Some(lexeme) = self.read_fn() else {
                        let mut diag = self.o.new_diag();
                        diag.span_err(
                            Default::default(),
                            lexeme_span,
                            "expected `]` to close the macro line",
                        );
                        self.o.ctx.emit_diag(diag);
                        return Invalid;
                    };

                    const IS_DEBUG: bool = false;
                    let (should_skip, end_name) = (bmatch_caseless! {
                        b"SKIPSTART" => (true, b"SKIPEND".as_slice()),
                        b"IF_DEBUG" => (!IS_DEBUG, b"ENDIF".as_slice()),
                        b"IF DEBUG" => (!IS_DEBUG, b"ENDIF".as_slice()),
                        _ => (false, b"".as_slice()),
                    })(&lexeme);

                    if end_name.is_empty() {
                        let mut diag = self.o.new_diag();
                        diag.span_err(
                            Default::default(),
                            lexeme_span,
                            format!("unknown macro line `[{}]`", lexeme.as_bstr()),
                        );
                        self.o.ctx.emit_diag(diag);
                        return Invalid;
                    }

                    if should_skip {
                        // Scan until found `[end_name]`
                        let mut last_is_newline = false;
                        loop {
                            let Some(ch) = self.next_char() else {
                                let mut diag = self.o.new_diag();
                                diag.span_err(
                                    Default::default(),
                                    SrcSpan::new(SrcPos(self.i.src_pos as _), 0),
                                    format!(
                                        "expected `[{}]` to close the macro block",
                                        end_name.as_bstr()
                                    ),
                                );
                                diag.span_note(
                                    Default::default(),
                                    lexeme_span,
                                    "macro block started here",
                                );
                                self.o.ctx.emit_diag(diag);

                                break;
                            };

                            match ch {
                                b'\r' | b'\n' => last_is_newline = true,
                                b'[' if last_is_newline => {
                                    if let Some(name) = self.read_fn() {
                                        if *name == *end_name {
                                            break;
                                        }
                                    }
                                    last_is_newline = false;
                                }
                                _ => {
                                    if !is_whitespace(ch) {
                                        last_is_newline = false;
                                    }
                                }
                            }
                        }

                        WhiteSpace
                    } else {
                        todo!()
                    }
                }
                _ => {
                    // Bad character
                    let mut diag = self.o.new_diag();
                    diag.span_err(
                        Default::default(),
                        SrcSpan::new(SrcPos(initial_src_pos as _), 1),
                        format!("illegal character `{}`", std::ascii::escape_default(ch)),
                    );
                    self.o.ctx.emit_diag(diag);
                    Invalid
                }
            },
            Mode::RawStr => match ch {
                // b'\r' | b'\n' if !self.is_suppress_newline() => {
                b'\r' | b'\n' => {
                    // if self.is_suppress_newline() {
                    //     // TODO: Suppress newline, emit diagnostic for now
                    //     let mut diag = self.o.new_diag();
                    //     diag.span_err(
                    //         Default::default(),
                    //         SrcSpan::new(SrcPos(initial_src_pos as _), 1),
                    //         "newline is not yet supported in raw string",
                    //     );
                    //     self.o.ctx.emit_diag(diag);
                    // }
                    self.read_newline_with_space(true)
                }
                _ => {
                    self.skip_char_while(|x| !matches!(x, b'\r' | b'\n'));
                    PlainStringLiteral
                }
            },
            Mode::CommaRawStr => match ch {
                b'\r' | b'\n' => {
                    // if self.is_suppress_newline() {
                    //     // TODO: Suppress newline, emit diagnostic for now
                    //     let mut diag = self.o.new_diag();
                    //     diag.span_err(
                    //         Default::default(),
                    //         SrcSpan::new(SrcPos(initial_src_pos as _), 1),
                    //         "newline is not yet supported in raw string",
                    //     );
                    //     self.o.ctx.emit_diag(diag);
                    // }
                    self.read_newline_with_space(true)
                }
                b',' => Comma,
                _ => {
                    self.skip_char_while(|x| !matches!(x, b'\r' | b'\n' | b','));
                    PlainStringLiteral
                }
            },
            Mode::PlainStr => match ch {
                b'"' => DoubleQuote,
                _ => {
                    if ch == b'\\' {
                        self.next_utf8_char();
                    } else {
                        self.skip_char_while(|x| !matches!(x, b'"' | b'\\'));
                    }
                    PlainStringLiteral
                }
            },
            Mode::RawStrForm => match ch {
                b'\r' | b'\n' => {
                    // if self.is_suppress_newline() {
                    //     // TODO: Suppress newline, emit diagnostic for now
                    //     let mut diag = self.o.new_diag();
                    //     diag.span_err(
                    //         Default::default(),
                    //         SrcSpan::new(SrcPos(initial_src_pos as _), 1),
                    //         "newline is not yet supported in raw string form",
                    //     );
                    //     self.o.ctx.emit_diag(diag);
                    // }
                    self.read_newline_with_space(true)
                }
                b'{' => LBrace,
                b'%' => Percentage,
                b'\\' if self.peek_char() == Some(b'@') => {
                    self.next_char();
                    TernaryStrFormMarker
                }
                _ => {
                    if ch == b'\\' {
                        self.next_char();
                    }
                    self.skip_char_while(|x| !matches!(x, b'\r' | b'\n' | b'{' | b'%' | b'\\'));
                    PlainStringLiteral
                }
            },
            Mode::StrForm => match ch {
                b'{' => LBrace,
                b'"' => DoubleQuote,
                b'%' => Percentage,
                b'\\' if self.peek_char() == Some(b'@') => {
                    self.next_char();
                    TernaryStrFormMarker
                }
                _ => {
                    if ch == b'\\' {
                        self.next_char();
                    }
                    self.skip_char_while(|x| !matches!(x, b'{' | b'"' | b'%' | b'\\'));
                    PlainStringLiteral
                }
            },
            Mode::TernaryStrForm => match ch {
                b'{' => LBrace,
                b'%' => Percentage,
                b'#' => NumberSign,
                b'\\' if self.peek_char() == Some(b'@') => {
                    self.next_char();
                    TernaryStrFormMarker
                }
                _ => {
                    if ch == b'\\' {
                        self.next_char();
                    }
                    self.skip_char_while(|x| !matches!(x, b'{' | b'%' | b'#' | b'\\'));
                    PlainStringLiteral
                }
            },
            Mode::CallForm => match ch {
                b'\r' | b'\n' => {
                    // if self.is_suppress_newline() {
                    //     // TODO: Suppress newline, emit diagnostic for now
                    //     let mut diag = self.o.new_diag();
                    //     diag.span_err(
                    //         Default::default(),
                    //         SrcSpan::new(SrcPos(initial_src_pos as _), 1),
                    //         "newline is not yet supported in raw string form",
                    //     );
                    //     self.o.ctx.emit_diag(diag);
                    // }
                    self.read_newline_with_space(true)
                }
                b'{' => LBrace,
                b'%' => Percentage,
                b'(' => LParen,
                b',' => Comma,
                b'\\' if self.peek_char() == Some(b'@') => {
                    self.next_char();
                    TernaryStrFormMarker
                }
                _ => {
                    if ch == b'\\' {
                        self.next_char();
                    }
                    self.skip_char_while(|x| {
                        !matches!(x, b'\r' | b'\n' | b'{' | b'%' | b'(' | b',' | b'\\')
                    });
                    PlainStringLiteral
                }
            },
        };

        token_kind
    }

    pub fn peek_char(&mut self) -> Option<u8> {
        self.handle_replacement();
        if self.has_replacement() {
            // NOTE: We assume that the replacement does not contain a newline.
            return self.i.alternate_src.first().copied();
        }
        self.src().first().copied()
    }

    pub fn next_char(&mut self) -> Option<u8> {
        self.handle_replacement();

        // TODO: Increment self.i.loc

        // Handle alternate source
        if let [ch, rest @ ..] = self.i.alternate_src {
            self.has_replaced = true;
            self.append_lexeme(self.i.alternate_src.as_ptr());
            self.i.alternate_src = rest;
            return Some(*ch);
        }

        // Handle normal
        if let [ch, rest @ ..] = self.src() {
            self.append_lexeme(self.src().as_ptr());
            self.i.src_pos += 1;
            return Some(*ch);
        }

        None
    }

    pub fn next_utf8_char(&mut self) -> Option<char> {
        self.handle_replacement();

        // TODO: Increment self.i.loc

        // Handle alternate source
        if !self.i.alternate_src.is_empty() {
            let (Some(ch), size) = bstr::decode_utf8(self.i.alternate_src) else {
                unsafe { unreachable_unchecked() };
            };
            self.has_replaced = true;
            for i in 0..size {
                self.append_lexeme(self.i.alternate_src.as_ptr());
                self.i.alternate_src = &self.i.alternate_src[1..];
            }
            return Some(ch);
        }

        // Handle normal
        if !self.src().is_empty() {
            let (Some(ch), size) = bstr::decode_utf8(self.src()) else {
                unsafe { unreachable_unchecked() };
            };
            for i in 0..size {
                self.append_lexeme(self.src().as_ptr());
                self.i.src_pos += 1;
            }
            return Some(ch);
        }

        None
    }

    #[inline]
    fn src(&self) -> &'a [u8] {
        &self.o.src[self.i.src_pos..]
    }

    fn current_lexeme(&self) -> &[u8] {
        if self.cur_lexeme_start.is_null() {
            // HACK: Get composed lexeme from the cart
            return &self.cart[..];
        }

        // SAFETY: The lexeme is guaranteed to be valid, since it is always continuous.
        unsafe { std::slice::from_raw_parts(self.cur_lexeme_start, self.cur_lexeme_len) }
        // // Make Miri happy
        // let src = self.o.src;
        // unsafe {
        //     let offset = self.cur_lexeme_start.offset_from(src.as_ptr()) as usize;
        //     let Some(x) = src.get(offset..(offset + self.cur_lexeme_len)) else {
        //         panic!(
        //             "invalid lexeme. src = {:?}, cur_lexeme_start = {:?}, cur_lexeme_len = {}",
        //             src.as_ptr(),
        //             self.cur_lexeme_start,
        //             self.cur_lexeme_len
        //         );
        //     };
        //     x
        //     // src.get_unchecked(offset..(offset + self.cur_lexeme_len))
        // }
    }

    // NOTE: We use `*const u8` instead of `&u8` in order to make Miri happy.
    fn append_lexeme(&mut self, ch: *const u8) {
        if self.cur_lexeme_start.is_null() && self.cur_lexeme_len == 0 {
            // Initial state
            self.cur_lexeme_start = ch;
            self.cur_lexeme_len += 1;

            return;
        }

        if !self.cur_lexeme_start.is_null() {
            // Verify if the lexeme can still stay continuous
            if unsafe { self.cur_lexeme_start.add(self.cur_lexeme_len) } != ch {
                // Lexeme is no longer continuous, must use cart to guarantee continuity
                let old_lexeme = unsafe {
                    std::slice::from_raw_parts(self.cur_lexeme_start, self.cur_lexeme_len)
                };
                self.cur_lexeme_start = std::ptr::null();
                self.cart.extend_from_slice(old_lexeme);
            }
        }

        if self.cur_lexeme_start.is_null() {
            // Empty lexeme, need to modify cart
            self.cart.push(unsafe { *ch });
        }

        self.cur_lexeme_len += 1;
    }

    fn reset_lexeme(&mut self) {
        self.cart.clear();
        self.cur_lexeme_start = std::ptr::null();
        self.cur_lexeme_len = 0;
        self.has_replaced = false;
    }

    fn handle_replacement(&mut self) {
        if self.has_replacement() {
            return;
        }

        // TODO: Increment self.i.loc

        fn split_replacement_start(src: &[u8]) -> Option<&[u8]> {
            if src.len() < 2 {
                return None;
            }
            let s: [u8; 2] = src[..2].try_into().unwrap();
            (s == [b'[', b'[']).then_some(&src[2..])
        }

        if let Some(rest) = split_replacement_start(self.src()) {
            let search_len = memchr::memchr2(b'\r', b'\n', rest).unwrap_or(rest.len());
            let Some(end_pos) = memchr::memmem::find(&rest[..search_len], b"]]") else {
                // Closing not found; treat as if it were not a replace, without emitting errors
                return;
            };
            // SAFETY: Source is valid UTF-8
            let in_replace = unsafe { rest[..end_pos].to_str_unchecked() };
            // self.i.src = &rest[(end_pos + 2)..];
            let replace_len = 2 + end_pos + 2;
            let old_src_pos = self.i.src_pos;
            self.i.src_pos += replace_len;
            let Some(out_replace) = self.o.ctx.global_replace.get(in_replace) else {
                // Replacement not found; skip this replacement, without emitting errors
                self.has_replaced = true;

                let mut diag = self.o.new_diag();
                diag.span_warn(
                    Default::default(),
                    SrcSpan::new(SrcPos(old_src_pos as _), replace_len as _),
                    format!("replacement `{}` not found", in_replace),
                );
                self.o.ctx.emit_diag(diag);
                return;
            };
            // SAFETY: The data is guaranteed to be valid, since removal of a replacement is forbidden in safe Rust
            //         according to the API contract.
            unsafe {
                let data = &out_replace.data[..];
                self.apply_replacement(std::mem::transmute(data), replace_len);
            }
        }
    }

    fn has_replacement(&self) -> bool {
        !self.i.alternate_src.is_empty()
    }

    fn apply_replacement(&mut self, replace: &'ctx [u8], replace_len: usize) {
        assert!(!self.has_replacement(), "replacement already exists");
        self.i.alternate_src = replace;
        self.i.replace_len = replace_len;
    }

    fn is_suppress_newline(&self) -> bool {
        self.i.suppress_newline_cnt > 0
    }

    fn push_suppress_newline(&mut self) {
        self.i.suppress_newline_cnt += 1;
    }

    fn pop_suppress_newline(&mut self) {
        assert!(self.is_suppress_newline(), "no suppress newline to pop");
        self.i.suppress_newline_cnt -= 1;
    }

    fn skip_char_while(&mut self, mut predicate: impl FnMut(u8) -> bool) {
        while let Some(ch) = self.peek_char() {
            if !predicate(ch) {
                break;
            }
            self.next_char();
        }
    }
}

#[inline]
fn is_whitespace(c: u8) -> bool {
    matches!(c, b' ' | b'\t')
}

// NOTE: Assuming input is UTF-8
#[inline]
fn is_ident_char(ch: u8) -> bool {
    ch.is_ascii_alphanumeric() || matches!(ch, b'_' | b'$') || !ch.is_ascii()
}

#[inline]
fn is_initial_ident_char(ch: u8) -> bool {
    ch.is_ascii_alphabetic() || ch == b'_' || !ch.is_ascii()
}
