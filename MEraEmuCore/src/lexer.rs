use std::{collections::HashMap, ptr::NonNull, rc::Rc};

use crate::bytecode::SourcePosInfo;

#[derive(Default)]
pub struct EraLexerTempStorage {
    lexemes: Vec<Box<[u8]>>,
}

struct EraLexerOuterState<'a, ErrReportFn> {
    err_report_fn: ErrReportFn,
    replace_list: &'a HashMap<Box<[u8]>, Box<[u8]>>,
    define_list: &'a mut HashMap<Box<[u8]>, Box<[u8]>>,
    temp_storage: &'a mut EraLexerTempStorage,
}
#[derive(Clone, Copy)]
struct EraLexerInnerState<'a> {
    src: &'a [u8],
    alternative_src: &'a [u8],
    offset: u64,
    cur_line: u32,
    cur_column: u32,
    replace_size: u32,
    last_is_newline: bool,
}
struct EraLexerInnerStateSite<'a, 'b, ErrReportFn> {
    o: &'a mut EraLexerOuterState<'b, ErrReportFn>,
    i: &'a mut EraLexerInnerState<'b>,
    lexeme_start: Option<NonNull<u8>>,
    lexeme_len: usize,
    lexeme_cart: Vec<u8>,
}

pub struct EraLexer<'a, ErrReportFn> {
    outer: EraLexerOuterState<'a, ErrReportFn>,
    inner: EraLexerInnerState<'a>,
    next_inner: Option<(EraLexerMode, EraToken<'static>, EraLexerInnerState<'a>)>,
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
    BitNot,
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
    Dollar,
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
    TernaryStrFormMarker,
    SingleComment,
    Identifier, // Even built-in commands are recognized as identifiers first
    SingleQuote,
    DoubleQuote,
    LineBreak,
    // -----
    KwDim = 101,
    KwDimS,
    KwGlobal,
    KwDynamic,
    KwRef,
    KwConst,
    KwSavedata,
    KwCharadata,
    KwLocalSize,
    KwLocalSSize,
    KwFunction,
    KwFunctionS,
    KwDefine,
    KwOnly,
    KwPri,
    KwLater,
    KwSingle,
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
                BitNot => "~",
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
                Dollar => "$",
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
                TernaryStrFormMarker => "\\@",
                SingleComment => "<single-line comment>",
                Identifier => "<identifier>",
                SingleQuote => "'",
                DoubleQuote => "\"",
                LineBreak => "<newline>",
                // -----
                KwDim => "DIM",
                KwDimS => "DIMS",
                KwGlobal => "GLOBAL",
                KwDynamic => "DYNAMIC",
                KwRef => "REF",
                KwConst => "CONST",
                KwSavedata => "SAVEDATA",
                KwCharadata => "CHARADATA",
                KwLocalSize => "LOCALSIZE",
                KwLocalSSize => "LOCALSSIZE",
                KwFunction => "FUNCTION",
                KwFunctionS => "FUNCTIONS",
                KwDefine => "DEFINE",
                KwOnly => "ONLY",
                KwPri => "PRI",
                KwLater => "LATER",
                KwSingle => "SINGLE",
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EraLexerMode {
    Normal,
    InlineNormal,
    SharpDecl,  // Such as `#DIM x = 42`
    StrForm,    // Quoted string form expression
    RawStrForm, // Unquoted string form
    RawStr,     // Unquoted string
    // TODO: Reject comments inside StrForm properly by introducing a new EraLexerMode
    TernaryStrForm,
    CallForm,
    CommaRawStr,
}

pub struct EraLexErrorInfo {
    pub src_info: SourcePosInfo,
    pub is_error: bool,
    pub msg: String,
}

impl<'a, T: FnMut(&EraLexErrorInfo)> EraLexer<'a, T> {
    pub fn new(
        src: &'a [u8],
        replace_list: &'a HashMap<Box<[u8]>, Box<[u8]>>,
        define_list: &'a mut HashMap<Box<[u8]>, Box<[u8]>>,
        temp_storage: &'a mut EraLexerTempStorage,
        err_report_fn: T,
    ) -> Self {
        EraLexer {
            outer: EraLexerOuterState {
                err_report_fn,
                replace_list,
                define_list,
                temp_storage,
            },
            inner: EraLexerInnerState {
                src,
                alternative_src: b"",
                offset: 0,
                cur_line: 1,
                cur_column: 1,
                replace_size: 0,
                last_is_newline: true,
            },
            next_inner: None,
        }
    }

    pub fn peek(&mut self, mode: EraLexerMode) -> EraToken<'a> {
        if let Some((lmode, ltoken, lstate)) = &self.next_inner {
            if *lmode == mode {
                return *ltoken;
            }
        }
        let mut inner = self.inner.clone();
        let token;
        unsafe {
            let cart;
            (cart, token) = inner.next_token(mode, &mut self.outer);
            if let Some(cart) = cart {
                self.outer.temp_storage.lexemes.push(cart);
            }
        }
        self.next_inner = Some((mode, token, inner));
        token
    }
    pub fn read(&mut self, mode: EraLexerMode) -> EraToken<'a> {
        if let Some((lmode, ltoken, lstate)) = self.next_inner.take() {
            if lmode == mode {
                self.inner = lstate;
                return ltoken;
            }
        }
        let token;
        unsafe {
            let cart;
            (cart, token) = self.inner.next_token(mode, &mut self.outer);
            if let Some(cart) = cart {
                self.outer.temp_storage.lexemes.push(cart);
            }
        }
        token
    }

    pub fn push_define(&mut self, key: Box<[u8]>, value: Box<[u8]>) {
        self.outer.define_list.insert(key, value);
    }
}

impl<'a> EraLexerInnerState<'a> {
    // SAFETY: The returned (<cart>, <token>) has their lifetime erased, with token possibly attached
    //         to the cart satisfying StableDeref. Special care is required for lifetime issues.
    unsafe fn next_token<T: FnMut(&EraLexErrorInfo)>(
        &mut self,
        mode: EraLexerMode,
        outer: &mut EraLexerOuterState<'a, T>,
    ) -> (Option<Box<[u8]>>, EraToken<'static>) {
        let mut site = EraLexerInnerStateSite {
            o: outer,
            i: self,
            lexeme_start: None,
            lexeme_len: 0,
            lexeme_cart: Vec::new(),
        };
        let mut token = site.next_token(mode);
        if matches!(token.kind, EraTokenKind::Eof) {
            (None, token)
        } else if let Some(ptr) = site.lexeme_start {
            token.lexeme = std::slice::from_raw_parts(ptr.as_ptr(), site.lexeme_len);
            (None, token)
        } else {
            // SAFETY: Box<[u8]> implements StableDeref
            let cart = site.lexeme_cart.into_boxed_slice();
            // TODO: Make Miri happy (?)
            token.lexeme = std::slice::from_raw_parts(cart.as_ptr(), cart.len());
            (Some(cart), token)
        }
    }
}

impl<'a, 'b, T: FnMut(&EraLexErrorInfo)> EraLexerInnerStateSite<'a, 'b, T> {
    /// Retrieves the next token without lexeme field set.
    fn next_token(&mut self, mode: EraLexerMode) -> EraToken<'static> {
        // Normal mode should have leading whitespaces removed
        if let EraLexerMode::Normal | EraLexerMode::SharpDecl | EraLexerMode::InlineNormal = mode {
            self.skip_whitespace();
        }

        let src_info = self.make_src_info();
        let last_is_newline = self.i.last_is_newline;

        let make_token_fn = |this: &Self, kind| EraToken::<'static> {
            kind,
            lexeme: b"",
            src_info,
        };
        let make_link2_token_fn =
            |this: &mut Self, links: &[(u8, EraTokenKind)], nonlink_kind| -> EraToken<'static> {
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
            EraLexerMode::Normal | EraLexerMode::SharpDecl | EraLexerMode::InlineNormal => {
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
                        self.reset_lexeme();
                        self.next_token(mode)
                    }
                    b'#' => make_token_fn(self, NumberSign),
                    b',' => make_token_fn(self, Comma),
                    b':' => make_token_fn(self, Colon),
                    b'$' => make_token_fn(self, Dollar),
                    b'~' => make_token_fn(self, BitNot),
                    b'+' => {
                        make_link2_token_fn(self, &[(b'=', PlusAssign), (b'+', Increment)], Plus)
                    }
                    b'-' => {
                        make_link2_token_fn(self, &[(b'=', MinusAssign), (b'-', Decrement)], Minus)
                    }
                    b'*' => make_link2_token_fn(self, &[(b'=', MultiplyAssign)], Multiply),
                    b'/' => make_link2_token_fn(self, &[(b'=', DivideAssign)], Divide),
                    b'%' => {
                        if let EraLexerMode::InlineNormal = mode {
                            make_token_fn(self, Percentage)
                        } else {
                            make_link2_token_fn(self, &[(b'=', ModuloAssign)], Percentage)
                        }
                    }
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
                    b'\'' => make_link2_token_fn(self, &[(b'=', ExprAssign)], SingleQuote),
                    b'\\' => make_link2_token_fn(self, &[(b'@', TernaryStrFormMarker)], Invalid),
                    _ if ch.is_ascii_alphabetic() || ch == b'_' || !ch.is_ascii() => {
                        // Handle identifier & command
                        while self.peek_char().map(Self::is_ident_char).unwrap_or(false) {
                            self.advance_char();
                        }
                        let mut token = make_token_fn(self, Identifier);
                        // Try handle #DEFINE replacements
                        if self.i.alternative_src.is_empty() {
                            let lexeme = self.get_lexeme();
                            let lexeme_len = self.lexeme_len;
                            if let Some(v) = self.o.define_list.get(lexeme) {
                                // SAFETY: We never remove entries from define_list, so we
                                //         can transmute lifetimes without UB
                                self.i.alternative_src = unsafe { std::mem::transmute(&v[..]) };
                                self.i.replace_size = lexeme_len as _;
                                (self.i.cur_column,) = (src_info.column,);
                                // Restart read via recursion (ensures correct lexeme)
                                self.reset_lexeme();
                                return self.next_token(mode);
                            }
                        }
                        // Handle specific keywords
                        if let EraLexerMode::SharpDecl = mode {
                            let lexeme = self.get_lexeme();
                            token.kind = if lexeme.eq_ignore_ascii_case(b"DIM") {
                                KwDim
                            } else if lexeme.eq_ignore_ascii_case(b"DIMS") {
                                KwDimS
                            } else if lexeme.eq_ignore_ascii_case(b"GLOBAL") {
                                KwGlobal
                            } else if lexeme.eq_ignore_ascii_case(b"DYNAMIC") {
                                KwDynamic
                            } else if lexeme.eq_ignore_ascii_case(b"REF") {
                                KwRef
                            } else if lexeme.eq_ignore_ascii_case(b"CONST") {
                                KwConst
                            } else if lexeme.eq_ignore_ascii_case(b"SAVEDATA") {
                                KwSavedata
                            } else if lexeme.eq_ignore_ascii_case(b"CHARADATA") {
                                KwCharadata
                            } else if lexeme.eq_ignore_ascii_case(b"LOCALSIZE") {
                                KwLocalSize
                            } else if lexeme.eq_ignore_ascii_case(b"LOCALSSIZE") {
                                KwLocalSSize
                            } else if lexeme.eq_ignore_ascii_case(b"FUNCTION") {
                                KwFunction
                            } else if lexeme.eq_ignore_ascii_case(b"FUNCTIONS") {
                                KwFunctionS
                            } else if lexeme.eq_ignore_ascii_case(b"DEFINE") {
                                KwDefine
                            } else if lexeme.eq_ignore_ascii_case(b"ONLY") {
                                KwOnly
                            } else if lexeme.eq_ignore_ascii_case(b"PRI") {
                                KwPri
                            } else if lexeme.eq_ignore_ascii_case(b"LATER") {
                                KwLater
                            } else if lexeme.eq_ignore_ascii_case(b"SINGLE") {
                                KwSingle
                            } else {
                                token.kind
                            };
                        }
                        // HACK: Some commands (such as PRINT) expects one whitespace to be eaten
                        if self.peek_char().map(Self::is_whitespace).unwrap_or(false) {
                            self.advance_char();
                            // HACK: Manually reduce lexeme length
                            self.lexeme_len -= 1;
                        }
                        token
                    }
                    b'0' => {
                        // Handle special integer literal
                        match self.peek_char() {
                            Some(b'x' | b'X') => {
                                // Hex
                                _ = self.advance_char();
                                self.skip_char_while(|x| x.is_ascii_hexdigit());
                            }
                            Some(b'b' | b'B') => {
                                // Bin
                                _ = self.advance_char();
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
                            _ = self.advance_char();
                            self.skip_char_while(|x| matches!(x, b'0'..=b'9'));
                        }
                        make_token_fn(self, IntLiteral)
                    }
                    b'1'..=b'9' => {
                        // Handle decimal integer literal
                        // TODO: Support scientific notation
                        self.skip_char_while(|x| x.is_ascii_digit());
                        if matches!(self.peek_char(), Some(b'p' | b'P')) {
                            _ = self.advance_char();
                            self.skip_char_while(|x| matches!(x, b'0'..=b'9'));
                        }
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
                    b'[' if last_is_newline => {
                        // Special preprocessing (macro?)
                        let read_fn = |this: &mut Self| -> Option<Box<[u8]>> {
                            loop {
                                if let None | Some(b']' | b'\n') = this.advance_char() {
                                    break;
                                }
                            }
                            let lexeme = this.get_lexeme()[..this.lexeme_len].into();
                            this.skip_whitespace();
                            if matches!(this.advance_char(), Some(b'\n')) {
                                Some(lexeme)
                            } else {
                                None
                            }
                        };
                        self.reset_lexeme();
                        let Some(lexeme) = read_fn(self) else {
                            return make_token_fn(self, EraTokenKind::Invalid);
                        };
                        if let Some(lexeme) = lexeme.strip_suffix(b"]") {
                            const IS_DEBUG: bool = false;
                            if lexeme.eq_ignore_ascii_case(b"SKIPSTART") {
                                // Scan until found `[SKIPEND]`
                                loop {
                                    loop {
                                        let last_is_newline = self.i.last_is_newline;
                                        self.skip_whitespace();
                                        let Some(ch) = self.advance_char() else {
                                            (self.o.err_report_fn)(&EraLexErrorInfo {
                                                src_info,
                                                is_error: false,
                                                msg: "`[SKIPSTART]` does not have a matching `[SKIPEND]`".to_owned(),
                                            });
                                            return self.make_eof_token();
                                        };
                                        if last_is_newline && ch == b'[' {
                                            break;
                                        }
                                    }
                                    self.reset_lexeme();
                                    let Some(lexeme) = read_fn(self) else {
                                        return make_token_fn(self, EraTokenKind::Invalid);
                                    };
                                    if lexeme.eq_ignore_ascii_case(b"SKIPEND]") {
                                        break;
                                    }
                                }
                                // Restart read via recursion (ensures correct lexeme)
                                self.reset_lexeme();
                                self.next_token(mode)
                            } else if lexeme.eq_ignore_ascii_case(b"IF_DEBUG")
                                || lexeme.eq_ignore_ascii_case(b"IF DEBUG")
                            {
                                if IS_DEBUG {
                                    todo!()
                                } else {
                                    // Scan until found `[ENDIF]`
                                    loop {
                                        loop {
                                            let last_is_newline = self.i.last_is_newline;
                                            self.skip_whitespace();
                                            let Some(ch) = self.advance_char() else {
                                                (self.o.err_report_fn)(&EraLexErrorInfo {
                                                src_info,
                                                is_error: false,
                                                msg: "`[IF_DEBUG]` does not have a matching `[ENDIF]`".to_owned(),
                                            });
                                                return self.make_eof_token();
                                            };
                                            if last_is_newline && ch == b'[' {
                                                break;
                                            }
                                        }
                                        self.reset_lexeme();
                                        let Some(lexeme) = read_fn(self) else {
                                            return make_token_fn(self, EraTokenKind::Invalid);
                                        };
                                        if lexeme.eq_ignore_ascii_case(b"ENDIF]") {
                                            break;
                                        }
                                    }
                                    // Restart read via recursion (ensures correct lexeme)
                                    self.reset_lexeme();
                                    self.next_token(mode)
                                }
                            } else {
                                let msg = format!(
                                    "`[{}]` is unsupported",
                                    String::from_utf8_lossy(lexeme)
                                );
                                (self.o.err_report_fn)(&EraLexErrorInfo {
                                    src_info,
                                    is_error: true,
                                    msg,
                                });
                                make_token_fn(self, EraTokenKind::Invalid)
                            }
                        } else {
                            (self.o.err_report_fn)(&EraLexErrorInfo {
                                src_info,
                                is_error: true,
                                msg: "`[` does not have corresponding closing `]`".to_owned(),
                            });
                            make_token_fn(self, EraTokenKind::Invalid)
                        }
                    }
                    _ => {
                        let msg = if ch.is_ascii_graphic() {
                            format!("`{}`: unrecognized token kind", ch as char)
                        } else {
                            format!("`{ch}`: unrecognized token kind")
                        };
                        (self.o.err_report_fn)(&EraLexErrorInfo {
                            src_info,
                            is_error: true,
                            msg,
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
            EraLexerMode::CommaRawStr => {
                let ch = match self.advance_char() {
                    Some(ch) => ch,
                    None => return self.make_eof_token(),
                };

                match ch {
                    b'\n' => make_token_fn(self, EraTokenKind::LineBreak),
                    b',' => make_token_fn(self, EraTokenKind::Comma),
                    _ => {
                        self.skip_char_while(|x| !matches!(x, b'\n' | b','));
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
                    b'\\' if self.peek_char() == Some(b'@') => {
                        self.advance_char();
                        make_token_fn(self, EraTokenKind::TernaryStrFormMarker)
                    }
                    _ => {
                        if ch == b'\\' {
                            self.advance_char();
                        }
                        self.skip_char_while(|x| !matches!(x, b'{' | b'\n' | b'%' | b'\\'));
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
                    b'\\' if self.peek_char() == Some(b'@') => {
                        self.advance_char();
                        make_token_fn(self, EraTokenKind::TernaryStrFormMarker)
                    }
                    _ => {
                        if ch == b'\\' {
                            self.advance_char();
                        }
                        self.skip_char_while(|x| !matches!(x, b'{' | b'"' | b'%' | b'\\'));
                        make_token_fn(self, EraTokenKind::PlainStringLiteral)
                    }
                }
            }
            EraLexerMode::TernaryStrForm => {
                let ch = match self.advance_char() {
                    Some(ch) => ch,
                    None => return self.make_eof_token(),
                };

                match ch {
                    b'{' => make_token_fn(self, EraTokenKind::LCurlyBracket),
                    b'%' => make_token_fn(self, EraTokenKind::Percentage),
                    b'#' => make_token_fn(self, EraTokenKind::NumberSign),
                    b'\\' if self.peek_char() == Some(b'@') => {
                        self.advance_char();
                        make_token_fn(self, EraTokenKind::TernaryStrFormMarker)
                    }
                    _ => {
                        if ch == b'\\' {
                            self.advance_char();
                        }
                        self.skip_char_while(|x| !matches!(x, b'{' | b'%' | b'#' | b'\\'));
                        make_token_fn(self, EraTokenKind::PlainStringLiteral)
                    }
                }
            }
            EraLexerMode::CallForm => {
                let ch = match self.advance_char() {
                    Some(ch) => ch,
                    None => return self.make_eof_token(),
                };

                match ch {
                    b'{' => make_token_fn(self, EraTokenKind::LCurlyBracket),
                    b'\n' => make_token_fn(self, EraTokenKind::LineBreak),
                    b'%' => make_token_fn(self, EraTokenKind::Percentage),
                    b'(' => make_token_fn(self, EraTokenKind::LBracket),
                    b',' => make_token_fn(self, EraTokenKind::Comma),
                    b'\\' if self.peek_char() == Some(b'@') => {
                        self.advance_char();
                        make_token_fn(self, EraTokenKind::TernaryStrFormMarker)
                    }
                    _ => {
                        self.skip_char_while(|x| {
                            !matches!(x, b'{' | b'\n' | b'%' | b'(' | b',' | b'\\')
                        });
                        make_token_fn(self, EraTokenKind::PlainStringLiteral)
                    }
                }
            }
        };

        // dbg!(token)
        token
    }

    fn get_lexeme(&self) -> &[u8] {
        if let Some(ptr) = self.lexeme_start {
            unsafe { std::slice::from_raw_parts(ptr.as_ptr(), self.lexeme_len) }
        } else {
            self.lexeme_cart.as_slice()
        }
    }
    fn reset_lexeme(&mut self) {
        self.lexeme_start = None;
        self.lexeme_len = 0;
        self.lexeme_cart.clear();
    }
    fn is_whitespace(ch: u8) -> bool {
        matches!(ch, b' ' | b'\t')
    }
    // NOTE: Assuming input is UTF-8
    fn is_ident_char(ch: u8) -> bool {
        ch.is_ascii_alphanumeric() || ch == b'_' || !ch.is_ascii()
    }
    fn is_at_end(&self) -> bool {
        self.i.src.is_empty()
    }
    fn skip_whitespace(&mut self) {
        // NOTE: self.i.last_is_newline remains untouched
        // NOTE: lexeme is automatically reset
        self.reset_lexeme();
        while let [b' ' | b'\t', rest @ ..] = self.i.alternative_src {
            self.i.alternative_src = rest;
            if rest.is_empty() {
                self.i.cur_column += self.i.replace_size;
            }
        }
        if !self.i.alternative_src.is_empty() {
            return;
        }
        while let [b' ' | b'\t', rest @ ..] = self.i.src {
            self.i.src = rest;
            self.i.cur_column += 1;
        }
    }
    // NOTE: Newline is handled in a specific way
    fn advance_char(&mut self) -> Option<u8> {
        self.i.last_is_newline = false;
        // Try find replace
        // NOTE: If replace failed (such as not found), we still carry on as if
        //       it were not a replace.
        self.handle_src_replace();
        // NOTE: Replace does not contain any newlines; also we need to treat source
        //       pos info carefully.
        let advance_lexeme_fn = |this: &mut Self, old_src: &[u8], ch: &u8| unsafe {
            if this.lexeme_len == 0 {
                this.lexeme_start = Some(NonNull::new_unchecked(old_src.as_ptr() as _));
            } else if this.lexeme_start.map(|x| x.as_ptr().add(this.lexeme_len))
                != Some(ch as *const _ as _)
            {
                // Impossible to stay continuous without extra allocation
                if let Some(ptr) = this.lexeme_start {
                    let lexeme = std::slice::from_raw_parts(ptr.as_ptr(), this.lexeme_len);
                    this.lexeme_start = None;
                    this.lexeme_cart.extend_from_slice(lexeme);
                }
                this.lexeme_cart.push(*ch);
            }
            this.lexeme_len += 1;
        };
        if let [ch, rest @ ..] = self.i.alternative_src {
            // unsafe {
            //     if self.lexeme_len == 0 {
            //         self.lexeme_start =
            //             Some(NonNull::new_unchecked(self.i.alternative_src.as_ptr() as _));
            //     } else if self.lexeme_start.map(|x| x.as_ptr().add(self.lexeme_len))
            //         != Some(ch as *const _ as _)
            //     {
            //         if let Some(ptr) = self.lexeme_start {
            //             self.lexeme_start = None;
            //             self.lexeme_cart
            //                 .extend_from_slice(std::slice::from_raw_parts(
            //                     ptr.as_ptr(),
            //                     self.lexeme_len,
            //                 ));
            //         }
            //         self.lexeme_cart.push(*ch);
            //     }
            // }
            advance_lexeme_fn(self, self.i.alternative_src, ch);
            self.i.alternative_src = rest;
            if rest.is_empty() {
                self.i.cur_column += self.i.replace_size;
            }
            return Some(*ch);
        }
        // Handle newline
        if let [b'\r', b'\n', rest @ ..] | [b'\r' | b'\n', rest @ ..] = self.i.src {
            self.i.last_is_newline = true;
            self.i.src = rest;
            (self.i.cur_line, self.i.cur_column) = (self.i.cur_line + 1, 1);
            return Some(b'\n');
        }
        // Handle normal
        if let [ch, rest @ ..] = self.i.src {
            advance_lexeme_fn(self, self.i.src, ch);
            let ch = *ch;
            self.i.src = rest;
            // Count in chars; assuming UTF-8 input is good
            if (ch & 0xc0) != 0x80 {
                self.i.cur_column += 1;
            }
            Some(ch)
        } else {
            None
        }
    }
    fn peek_char(&mut self) -> Option<u8> {
        self.handle_src_replace();
        if let &[ch, ..] = self.i.alternative_src {
            return Some(ch);
        }
        self.i
            .src
            .get(0)
            .copied()
            .map(|x| if x == b'\r' { b'\n' } else { x })
    }
    fn handle_src_replace(&mut self) {
        if !self.i.alternative_src.is_empty() {
            return;
        }
        if let [b'[', b'[', rest @ ..] = self.i.src {
            let src_info = self.make_src_info();

            let search_len = memchr::memchr2(b'\r', b'\n', rest).unwrap_or(rest.len());
            let Some(end_pos) = memchr::memmem::find(&rest[..search_len], b"]]") else {
                // Closing not found; treat as if it were not a replace, without emitting errors
                // (self.o.err_report_fn)(&EraLexErrorInfo {
                //     src_info,
                //     is_error: true,
                //     msg: "replacement `[[` does not have corresponding closing `]]`".to_owned(),
                // });
                return;
            };
            let in_replace = &rest[..end_pos];
            self.i.src = &rest[(end_pos + 2)..];
            let Some(out_replace) = self.o.replace_list.get(in_replace) else {
                // Slience the `error`
                // (self.o.err_report_fn)(&EraLexErrorInfo {
                //     src_info,
                //     is_error: true,
                //     msg: format!(
                //         "replacement `[[{}]]` does not exist",
                //         String::from_utf8_lossy(in_replace)
                //     ),
                // });
                return;
            };
            // Count in chars; assuming UTF-8 input is good
            self.i.replace_size = in_replace
                .iter()
                .fold(2 + 2, |acc, x| acc + ((*x & 0xc0) != 0x80) as u32);
            self.i.alternative_src = out_replace;
        }
    }
    fn make_src_info(&self) -> SourcePosInfo {
        SourcePosInfo {
            line: self.i.cur_line,
            column: self.i.cur_column,
        }
    }
    // Precondition: self.is_at_end() == true
    fn make_eof_token(&self) -> EraToken<'static> {
        EraToken {
            kind: EraTokenKind::Eof,
            lexeme: b"",
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
