use crate::bytecode::SourcePosInfo;

pub struct EraLexer<'a, ErrReportFn> {
    err_report_fn: ErrReportFn,
    src: &'a [u8],
    offset: u64,
    cur_line: u32,
    cur_column: u32,
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EraTokenKind {
    Invalid = 0,
    Eof,
    Plus,
    Minus,
    Multiply,
    Divide,
    Percentage,
    BitAnd,
    BitOr,
    BitXor,
    BitShiftL,
    BitShiftR,
    LogicalNot,
    LogicalAnd,
    LogicalOr,
    CmpEq,
    CmpNEq,
    CmpL,
    CmpG,
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
    Comma,
    // TODO: Remove SemiColon enum?
    SemiColon,
    Colon,
    LBracket,
    RBracket,
    LCurlyBracket,
    RCurlyBracket,
    At,
    NumberSign,
    IntLiteral,
    StringLiteral,
    PlainStringLiteral,
    StringFormStart, // Caller should manually enter StrForm mode on encountering this
    SingleComment,
    Identifier, // Even built-in commands are recognized as identifiers first
    DoubleQuote,
    LineBreak,
    // -----
    KwDim = 101,
    KwDimS,
    KwGlobal,
    KwDynamic,
    KwRef,
    KwConst,
    KwLocalSize,
    KwFunction,
    KwFunctionS,
}

#[derive(Debug, Clone, Copy)]
pub struct EraToken<'a> {
    pub kind: EraTokenKind,
    pub lexeme: &'a [u8],
    pub src_info: crate::bytecode::SourcePosInfo,
}

impl EraToken<'_> {
    pub fn is_eof(&self) -> bool {
        matches!(self.kind, EraTokenKind::Eof)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct EraTokenLite {
    pub kind: EraTokenKind,
    pub src_info: crate::bytecode::SourcePosInfo,
}
impl From<EraToken<'_>> for EraTokenLite {
    fn from(value: EraToken<'_>) -> Self {
        EraTokenLite {
            kind: value.kind,
            src_info: value.src_info,
        }
    }
}
impl std::fmt::Display for EraTokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use EraTokenKind::*;
        write!(
            f,
            "{}",
            match self {
                Eof => "<eof>",
                Plus => "+",
                Minus => "-",
                Multiply => "*",
                Divide => "/",
                Percentage => "%",
                BitAnd => "&",
                BitOr => "|",
                BitXor => "^",
                BitShiftL => "<<",
                BitShiftR => ">>",
                LogicalNot => "!",
                LogicalAnd => "&&",
                LogicalOr => "||",
                CmpEq => "==",
                CmpNEq => "!=",
                CmpL => "<",
                CmpLEq => "<=",
                CmpG => ">",
                CmpGEq => ">=",
                QuestionMark => "?",
                Assign => "=",
                ExprAssign => "'=",
                PlusAssign => "+=",
                MinusAssign => "-=",
                MultiplyAssign => "*=",
                DivideAssign => "/=",
                ModuloAssign => "%=",
                BitAndAssign => "&=",
                BitOrAssign => "|=",
                BitXorAssign => "^=",
                Increment => "++",
                Decrement => "--",
                Comma => ",",
                SemiColon => ";",
                Colon => ":",
                LBracket => "(",
                RBracket => ")",
                LCurlyBracket => "{",
                RCurlyBracket => "}",
                At => "@",
                NumberSign => "#",
                IntLiteral => "<integer literal>",
                StringLiteral => "<string literal>",
                PlainStringLiteral => "<plain string literal>",
                StringFormStart => "@\"",
                SingleComment => "<single-line comment>",
                Identifier => "<identifier>",
                DoubleQuote => "\"",
                LineBreak => "<newline>",
                // -----
                KwDim => "DIM",
                KwDimS => "DIMS",
                KwGlobal => "GLOBAL",
                KwDynamic => "DYNAMIC",
                KwRef => "REF",
                KwConst => "CONST",
                KwLocalSize => "LOCALSIZE",
                KwFunction => "FUNCTION",
                KwFunctionS => "FUNCTIONS",
                Invalid | _ => "<invalid>",
            }
        )
    }
}
impl std::fmt::Display for EraTokenLite {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.kind.fmt(f)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum EraLexerMode {
    Normal,
    SharpDecl,  // Such as `#DIM x = 42`
    StrForm,    // Quoted string form expression
    RawStrForm, // Unquoted string form
    RawStr,     // Unquoted string
                // TODO: Reject comments inside StrForm properly by introducing a new EraLexerMode
}

pub struct EraLexErrorInfo {
    pub src_info: SourcePosInfo,
    pub is_error: bool,
    pub msg: String,
}

impl<'a, T: FnMut(&EraLexErrorInfo)> EraLexer<'a, T> {
    pub fn new(src: &'a [u8], err_report_fn: T) -> Self {
        EraLexer {
            err_report_fn,
            src,
            offset: 0,
            cur_line: 1,
            cur_column: 1,
        }
    }

    pub fn peek(&mut self, mode: EraLexerMode) -> EraToken<'a> {
        // TODO: Omit mut in `&mut self` by creating a new EraLexer?
        let src = self.src;
        let (offset, cur_line, cur_column) = (self.offset, self.cur_line, self.cur_column);
        let token = self.read(mode);
        self.src = src;
        (self.offset, self.cur_line, self.cur_column) = (offset, cur_line, cur_column);
        token
    }
    pub fn read(&mut self, mode: EraLexerMode) -> EraToken<'a> {
        // Normal mode should have leading whitespaces removed
        if let EraLexerMode::Normal | EraLexerMode::SharpDecl = mode {
            self.skip_whitespace();
        }

        let old_src = self.src;
        let src_info = self.make_src_info();

        // SAFETY: src and old_src always refer to the same object
        let get_token_lexeme_fn = |this: &Self| unsafe {
            let distance = this.src.as_ptr().offset_from(old_src.as_ptr());
            &old_src[..distance as _]
        };
        let make_token_fn = |this: &Self, kind| EraToken {
            kind,
            lexeme: get_token_lexeme_fn(this),
            src_info,
        };
        let make_link2_token_fn =
            |this: &mut Self, links: &[(u8, EraTokenKind)], nonlink_kind| -> EraToken<'a> {
                let peeked_ch = this.peek_char();
                for &(link_ch, link_kind) in links {
                    if Some(link_ch) == peeked_ch {
                        this.advance_char();
                        return make_token_fn(this, link_kind);
                    }
                }
                make_token_fn(this, nonlink_kind)
            };

        let token = match mode {
            EraLexerMode::Normal | EraLexerMode::SharpDecl => {
                //use EraTokenKind as TKind;
                use EraTokenKind::*;

                // We always advance at least one char to prevent possible dead loops
                let ch = match self.advance_char() {
                    Some(ch) => ch,
                    None => return self.make_eof_token(),
                };

                match ch {
                    b'\n' => make_token_fn(self, EraTokenKind::LineBreak),
                    b'@' => {
                        if let Some(b'"') = self.peek_char() {
                            self.advance_char();
                            make_token_fn(self, EraTokenKind::StringFormStart)
                        } else {
                            make_token_fn(self, EraTokenKind::At)
                        }
                    }
                    b';' => {
                        // Swallow comments, don't return as tokens
                        loop {
                            if let None | Some(b'\n') = self.peek_char() {
                                break;
                            }
                            self.advance_char();
                        }
                        // Restart read via recursion (ensures correct lexeme)
                        self.read(mode)
                    }
                    b'#' => make_token_fn(self, NumberSign),
                    b',' => make_token_fn(self, Comma),
                    b':' => make_token_fn(self, Colon),
                    b'+' => {
                        make_link2_token_fn(self, &[(b'=', PlusAssign), (b'+', Increment)], Plus)
                    }
                    b'-' => {
                        make_link2_token_fn(self, &[(b'=', MinusAssign), (b'-', Decrement)], Minus)
                    }
                    b'*' => make_link2_token_fn(self, &[(b'=', MultiplyAssign)], Multiply),
                    b'/' => make_link2_token_fn(self, &[(b'=', DivideAssign)], Divide),
                    b'%' => make_link2_token_fn(self, &[(b'=', ModuloAssign)], Percentage),
                    b'?' => make_token_fn(self, QuestionMark),
                    b'{' => make_token_fn(self, LCurlyBracket),
                    b'}' => make_token_fn(self, RCurlyBracket),
                    b'(' => make_token_fn(self, LBracket),
                    b')' => make_token_fn(self, RBracket),
                    b'&' => make_link2_token_fn(
                        self,
                        &[(b'=', BitAndAssign), (b'&', LogicalAnd)],
                        BitAnd,
                    ),
                    b'|' => {
                        make_link2_token_fn(self, &[(b'=', BitOrAssign), (b'|', LogicalOr)], BitOr)
                    }
                    b'^' => make_link2_token_fn(self, &[(b'=', BitXorAssign)], BitXor),
                    b'!' => make_link2_token_fn(self, &[(b'=', CmpNEq)], LogicalNot),
                    b'=' => make_link2_token_fn(self, &[(b'=', CmpEq)], Assign),
                    b'<' => make_link2_token_fn(self, &[(b'<', BitShiftL), (b'=', CmpLEq)], CmpL),
                    b'>' => make_link2_token_fn(self, &[(b'>', BitShiftR), (b'=', CmpGEq)], CmpG),
                    b'\'' if matches!(self.peek_char(), Some(b'=')) => {
                        self.advance_char();
                        make_token_fn(self, ExprAssign)
                    }
                    _ if ch.is_ascii_alphabetic() || ch == b'_' || !ch.is_ascii() => {
                        // Handle identifier & command
                        while self.peek_char().map(Self::is_ident_char).unwrap_or(false) {
                            self.advance_char();
                        }
                        let mut token = make_token_fn(self, Identifier);
                        // HACK: Some commands (such as PRINT) expects one whitespace to be eaten
                        if self.peek_char().map(Self::is_whitespace).unwrap_or(false) {
                            self.advance_char();
                        }
                        // Handle specific keywords
                        if let EraLexerMode::SharpDecl = mode {
                            token.kind = if token.lexeme.eq_ignore_ascii_case(b"DIM") {
                                KwDim
                            } else if token.lexeme.eq_ignore_ascii_case(b"DIMS") {
                                KwDimS
                            } else if token.lexeme.eq_ignore_ascii_case(b"GLOBAL") {
                                KwGlobal
                            } else if token.lexeme.eq_ignore_ascii_case(b"DYNAMIC") {
                                KwDynamic
                            } else if token.lexeme.eq_ignore_ascii_case(b"REF") {
                                KwRef
                            } else if token.lexeme.eq_ignore_ascii_case(b"CONST") {
                                KwConst
                            } else if token.lexeme.eq_ignore_ascii_case(b"LOCALSIZE") {
                                KwLocalSize
                            } else if token.lexeme.eq_ignore_ascii_case(b"FUNCTION") {
                                KwFunction
                            } else if token.lexeme.eq_ignore_ascii_case(b"FUNCTIONS") {
                                KwFunctionS
                            } else {
                                token.kind
                            };
                        }
                        token
                    }
                    b'0' => {
                        // Handle special integer literal
                        match self.peek_char() {
                            Some(b'x' | b'X') => {
                                // Hex
                                self.skip_char_while(|x| x.is_ascii_hexdigit());
                            }
                            Some(b'0'..=b'7') => {
                                // Octal
                                self.skip_char_while(|x| matches!(x, b'0'..=b'7'));
                            }
                            _ => {
                                // Plain zero
                            }
                        }
                        make_token_fn(self, IntLiteral)
                    }
                    b'1'..=b'9' => {
                        // Handle decimal integer literal
                        // TODO: Support scientific notation
                        self.skip_char_while(|x| x.is_ascii_digit());
                        make_token_fn(self, IntLiteral)
                    }
                    b'"' => {
                        // Normal string literal
                        while let Some(ch) = self.advance_char() {
                            // TODO: Properly handle raw newline in string literal?
                            if ch == b'"' {
                                break;
                            }
                            if ch == b'\\' {
                                self.advance_char();
                            }
                        }
                        make_token_fn(self, StringLiteral)
                    }
                    _ => {
                        (self.err_report_fn)(&EraLexErrorInfo {
                            src_info: src_info,
                            is_error: true,
                            msg: format!("`{ch}`: unrecognized token kind"),
                        });
                        make_token_fn(self, EraTokenKind::Invalid)
                    }
                }
            }
            EraLexerMode::RawStr => {
                let ch = match self.advance_char() {
                    Some(ch) => ch,
                    None => return self.make_eof_token(),
                };

                match ch {
                    b'\n' => make_token_fn(self, EraTokenKind::LineBreak),
                    _ => {
                        self.skip_char_while(|x| x != b'\n');
                        make_token_fn(self, EraTokenKind::PlainStringLiteral)
                    }
                }
            }
            EraLexerMode::RawStrForm => {
                let ch = match self.advance_char() {
                    Some(ch) => ch,
                    None => return self.make_eof_token(),
                };

                match ch {
                    b'{' => make_token_fn(self, EraTokenKind::LCurlyBracket),
                    b'\n' => make_token_fn(self, EraTokenKind::LineBreak),
                    b'%' => make_token_fn(self, EraTokenKind::Percentage),
                    _ => {
                        self.skip_char_while(|x| !matches!(x, b'{' | b'\n' | b'%'));
                        make_token_fn(self, EraTokenKind::PlainStringLiteral)
                    }
                }
            }
            EraLexerMode::StrForm => {
                let ch = match self.advance_char() {
                    Some(ch) => ch,
                    None => return self.make_eof_token(),
                };

                match ch {
                    b'{' => make_token_fn(self, EraTokenKind::LCurlyBracket),
                    b'"' => make_token_fn(self, EraTokenKind::DoubleQuote),
                    b'%' => make_token_fn(self, EraTokenKind::Percentage),
                    _ => {
                        self.skip_char_while(|x| !matches!(x, b'{' | b'"' | b'%'));
                        make_token_fn(self, EraTokenKind::PlainStringLiteral)
                    }
                }
            }
        };

        // dbg!(token)
        token
    }

    fn is_whitespace(ch: u8) -> bool {
        matches!(ch, b' ' | b'\t')
    }
    // NOTE: Assuming input is UTF-8
    fn is_ident_char(ch: u8) -> bool {
        ch.is_ascii_alphanumeric() || ch == b'_' || !ch.is_ascii()
    }
    fn is_at_end(&self) -> bool {
        self.src.is_empty()
    }
    fn skip_whitespace(&mut self) {
        while let [b' ' | b'\t', rest @ ..] = self.src {
            self.src = rest;
            self.cur_column += 1;
        }
    }
    // NOTE: Newline is handled in a specific way
    fn advance_char(&mut self) -> Option<u8> {
        // Handle newline
        if let [b'\r', b'\n', rest @ ..] | [b'\r' | b'\n', rest @ ..] = self.src {
            self.src = rest;
            (self.cur_line, self.cur_column) = (self.cur_line + 1, 1);
            return Some(b'\n');
        }
        // Handle normal
        if let [ch, rest @ ..] = self.src {
            self.src = rest;
            self.cur_column += 1;
            Some(*ch)
        } else {
            None
        }
    }
    fn peek_char(&self) -> Option<u8> {
        self.src
            .get(0)
            .copied()
            .map(|x| if x == b'\r' { b'\n' } else { x })
    }
    fn make_src_info(&self) -> SourcePosInfo {
        SourcePosInfo {
            line: self.cur_line,
            column: self.cur_column,
        }
    }
    // Precondition: self.is_at_end() == true
    fn make_eof_token(&self) -> EraToken<'a> {
        EraToken {
            kind: EraTokenKind::Eof,
            lexeme: self.src,
            src_info: self.make_src_info(),
        }
    }
    fn skip_char_while(&mut self, mut predicate: impl FnMut(u8) -> bool) {
        while let Some(ch) = self.peek_char() {
            if !predicate(ch) {
                break;
            }
            self.advance_char();
        }
    }
}
