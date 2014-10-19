use {Span, Spanned};

pub enum Token {
    /// Non-preprocessor line, with line ending included
    RegularLine(Vec<u8>),

    ///
    Identifier(Vec<u8>),

    /// An integer.
    Integer(u64),
    /// A boolean.
    Boolean(bool),
    /// A floating point,
    Float(f64),

    /// Whitespaces
    Whitespace(Vec<u8>),

    /// `#`
    Bash,
    /// `##`
    DoubleBash,

    /// `if` keyword
    If,
    /// `ifdef` keyword
    Ifdef,
    /// `ifndef` keyword
    Ifndef,
    /// `elif` keyword
    Elif,
    /// `else` keyword
    Else,
    /// `endif` keyword
    Endif,
    /// `define` keyword
    Define,
    /// `defined` keyword
    Defined,
    /// `undef` keyword
    Undef,
    /// `error` keyword
    Error,
    /// `pragma` keyword
    Pragma,
    /// `extension` keyword
    Extension,
    /// `version` keyword
    Version,
    /// `line` keyword
    Line,

    ///
    SingleLineComment(Vec<u8>),
    /// 
    MultilineComment(Vec<u8>),

    /// `++`
    Increment,
    /// `--`
    Decrement,
    /// `<=`
    LessOrEqual,
    /// `>=`
    MoreOrEqual,
    /// `==`
    Equal,
    /// `!=`
    NotEqual,

    /// `&&`
    AndOp,
    /// `||`
    OrOp,
    /// `^^`
    XorOp,
    /// `*=`
    MulAssign,
    /// `/=`
    DivAssign,
    /// `+=`
    AddAssign,
    /// `%=`
    ModAssign,
    /// `-=`
    SubAssign,

    /// `(`
    LeftParenthesis,
    /// `)`
    RightParenthesis,
    /// `[`
    LeftBracket,
    /// `]`
    RightBracket,
    /// `{`
    LeftBrace,
    /// `}`
    RightBrace,
    /// `.`
    Dot,

    /// `,`
    Comma,
    /// `:`
    Colon,
    /// `=`
    Assign,
    /// `;`
    SemiColon,
    /// `!`
    Bang,
    /// `-`
    Dash,
    /// `~`
    Tilde,
    /// `+`
    Plus,
    /// `*`
    Star,
    /// `/`
    Slash,
    /// `%`
    Percent,

    /// `<`
    LeftAngle,
    /// `>`
    RightAngle,
    /// `|`
    VerticalBar,
    /// `^`
    Caret,
    /// `&`
    Ampersand,
    /// `?`
    Question,
}

pub struct Lexer<R> {
    reader: R,
    at_line_start: bool,
    next_byte: Option<u8>,
    span: Span,
}

impl<R: Reader> Lexer<R> {
    pub fn new(reader: R) -> Lexer<R> {
        Lexer {
            reader: reader,
            at_line_start: true,
            next_byte: None,
            span: Span { line: 1, offset: 0, },
        }
    }

    fn read_next(&mut self) {
        let prev_is_cr = self.next_byte == Some(b'\r');

        self.next_byte = self.reader.read_byte().ok();

        match self.next_byte {
            Some(b'\n') if !prev_is_cr => {
                self.span.line += 1;
                self.span.offset = 0;
            },
            Some(b'\r') => {
                self.span.line += 1;
                self.span.offset = 0;
            },
            Some(_) => {
                self.span.offset += 1;
            },
            _ => ()
        }
    }

    fn get_space_tabs(&mut self) -> Vec<u8> {
        let mut buffer = Vec::new();

        loop {
            if let Some(byte) = self.next_byte {
                if is_space_tab(byte) {
                    buffer.push(byte);
                    continue;
                }
            }

            break;
        }

        buffer
    }

    /// Call this after reading `/` and `*`.
    ///
    /// The function will read the content of the comment, and skip the closing `*` and `/`.
    // TODO: return an error if we encounter EOF
    fn parse_multiline_comment_body(&mut self) -> Vec<u8> {
        let mut buffer = Vec::new();

        let mut perhaps_comment_end = false;
        loop {
            if perhaps_comment_end && self.next_byte == Some(b'/') {
                buffer.pop();
                self.read_next();
                break;
            }
            if let Some(b) = self.next_byte {
                perhaps_comment_end = b == b'*';
                buffer.push(b);
            } else {
                break;
            }
            self.read_next();
        }

        buffer
    }

    /// Reads until we are at the start of a new line.
    ///
    /// If `include_comments` is true, do not interrupt if we are in the middle of a multiline
    /// comment.
    ///
    /// For example 'int a = 5; /* hello\n world*/ int b = 15;' will be read entirely if `true`,
    /// but will stop at the `\n` if false.
    fn jump_to_next_line(&mut self, include_comments: bool) -> Vec<u8> {
        let mut buffer = Vec::new();

        loop {
            if let Some(byte) = self.next_byte {
                if byte == b'\r' || byte == b'\n' {
                    break;
                }

                if include_comments && byte == b'/' {
                    self.read_next();

                    if self.next_byte == Some(b'*') {
                        self.parse_multiline_comment_body();
                    } else {
                        buffer.push(b'/');
                    }

                } else {
                    buffer.push(byte);
                    self.read_next();
                }

            } else {
                return buffer;
            }
        }

        let prev_is_cr = self.next_byte == Some(b'\r');
        self.read_next();
        if prev_is_cr && self.next_byte == Some(b'\n') {
            buffer.push(b'\n');
            self.read_next();
        }

        buffer
    }

    fn parse_identifier_or_keyword(&mut self) -> Spanned<Token> {
        let start = self.span.clone();

        let mut buffer = Vec::new();
        while self.next_byte.is_some() && is_ident(self.next_byte.unwrap()) {
            buffer.push(self.next_byte.unwrap());
            self.read_next();
        }

        match parse_keyword(buffer[]) {
            Some(kw) => Spanned {
                content: kw,
                start: start,
            },
            None => Spanned {
                content: Identifier(buffer),
                start: start,
            }
        }
    }
}

impl<R: Reader> Iterator<Spanned<Token>> for Lexer<R> {
    fn next(&mut self) -> Option<Spanned<Token>> {
        let span_start = self.span.clone();

        if self.next_byte.is_none() {
            self.read_next();
            if self.next_byte.is_none() { return None; }
        }

        if self.at_line_start {
            let mut line = self.get_space_tabs();
            if self.next_byte == Some(b'#') {
                self.at_line_start = false;
                return Some(Spanned {
                    start: span_start,
                    content: Whitespace(line),
                });
            } else {
                line.extend(self.jump_to_next_line(true).into_iter());
                return Some(Spanned {
                    start: span_start,
                    content: RegularLine(line),
                });
            }
        }

        match self.next_byte {
            Some(b'#') => {
                self.read_next();
                if self.next_byte == Some(b'#') {
                    self.read_next();
                    return Some(Spanned {
                        start: span_start,
                        content: DoubleBash,
                    });
                } else {
                    return Some(Spanned {
                        start: span_start,
                        content: Bash,
                    });
                }
            },

            Some(b'\r') => {
                self.read_next();
                if self.next_byte == Some(b'\n') {
                    self.read_next();
                    self.at_line_start = true;
                    return Some(Spanned {
                        start: span_start,
                        content: Whitespace(b"\r\n".to_vec()),
                    });
                } else {
                    return Some(Spanned {
                        start: span_start,
                        content: Whitespace(b"\r".to_vec()),
                    });
                }
            }

            Some(c) if is_space_tab(c) => {
                let mut buffer = vec![c];
                loop {
                    self.read_next();
                    match self.next_byte {
                        Some(a) if is_space_tab(a) => buffer.push(a),
                        _ => return Some(Spanned {
                            start: span_start,
                            content: Whitespace(buffer)
                        }),
                    }
                }
            }

            Some(b'/') => {
                self.read_next();
                if self.next_byte == Some(b'*') {
                    return Some(Spanned {
                        start: span_start,
                        content: MultilineComment(self.parse_multiline_comment_body()),
                    });
                } else if self.next_byte == Some(b'/') {
                    return Some(Spanned {
                        start: span_start,
                        content: SingleLineComment(self.jump_to_next_line(false)),
                    });
                } else {
                    return Some(Spanned {
                        start: span_start,
                        content: Slash,
                    });
                }
            }

            Some(c) if is_ident_start(c) => {
                return Some(self.parse_identifier_or_keyword());
            },

            Some(c) => {
                self.read_next();
                if let Some(d) = self.next_byte {
                    if let Some(symbol) = parse_symbol(&[c, d]) {
                        return Some(Spanned {
                            start: span_start,
                            content: symbol,
                        });
                    } else if let Some(symbol) = parse_symbol(&[c]) {
                        return Some(Spanned {
                            start: span_start,
                            content: symbol,
                        });
                    } else {
                        fail!("preprocessor lexing error {} at {}", c, span_start);  // TODO: handle properly
                    }
                }
            },

            None => return None
        };

        unreachable!()
    }
}

fn is_ident_start(c: u8) -> bool {
    match c {
        b'a' ... b'z' => true,
        b'A' ... b'Z' => true,
        b'_' => true,
        _ => false,
    }
}

fn is_ident(c: u8) -> bool {
    match c {
        c if is_ident_start(c) => true,
        b'0' ... b'9' => true,
        _ => false,
    }
}

fn is_space_tab(c: u8) -> bool {
    match c {
        b'\t' | b' ' | 12u8 => true,
        _ => false
    }
}

fn parse_keyword(buffer: &[u8]) -> Option<Token> {
    use std::ascii::AsciiExt;

    match buffer.to_ascii_lower().as_slice() {
        b"if" => Some(If),
        b"ifdef" => Some(Ifdef),
        b"ifndef" => Some(Ifndef),
        b"elif" => Some(Elif),
        b"else" => Some(Else),
        b"endif" => Some(Endif),
        b"define" => Some(Define),
        b"defined" => Some(Defined),
        b"undef" => Some(Undef),
        b"error" => Some(Error),
        b"pragma" => Some(Pragma),
        b"extension" => Some(Extension),
        b"version" => Some(Version),
        b"line" => Some(Line),
        _ => None,
    }
}

fn parse_symbol(buffer: &[u8]) -> Option<Token> {
    match buffer {
        b"++" => Some(Increment),
        b"--" => Some(Decrement),
        b"<=" => Some(LessOrEqual),
        b">=" => Some(MoreOrEqual),
        b"==" => Some(Equal),
        b"!=" => Some(NotEqual),

        b"&&" => Some(AndOp),
        b"||" => Some(OrOp),
        b"^^" => Some(XorOp),
        b"*=" => Some(MulAssign),
        b"/=" => Some(DivAssign),
        b"+=" => Some(AddAssign),
        b"%=" => Some(ModAssign),
        b"-=" => Some(SubAssign),

        b"(" => Some(LeftParenthesis),
        b")" => Some(RightParenthesis),
        b"[" => Some(LeftBracket),
        b"]" => Some(RightBracket),
        b"{" => Some(LeftBrace),
        b"}" => Some(RightBrace),
        b"." => Some(Dot),

        b"," => Some(Comma),
        b":" => Some(Colon),
        b"=" => Some(Assign),
        b";" => Some(SemiColon),
        b"!" => Some(Bang),
        b"-" => Some(Dash),
        b"~" => Some(Tilde),
        b"+" => Some(Plus),
        b"*" => Some(Star),
        b"/" => Some(Slash),
        b"%" => Some(Percent),

        b"<" => Some(LeftAngle),
        b">" => Some(RightAngle),
        b"|" => Some(VerticalBar),
        b"^" => Some(Caret),
        b"&" => Some(Ampersand),
        b"?" => Some(Question),

        _ => None
    }
}
