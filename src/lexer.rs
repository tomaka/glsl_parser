use {Span, Spanned};

/// Token that are found in the source code.
#[deriving(Clone, PartialEq)]
pub enum Token {
    /// An identifier, like a function or variable name.
    Identifier(String),
    /// An integer.
    Integer(u64),
    /// A boolean.
    Boolean(bool),
    /// A floating point,
    Float(f64),

    /// A white space.
    Whitespace(String),

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
    /// '#'
    Sharp,

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

    /// `attribute` keyword.
    Attribute,
    /// `bool` keyword.
    BOOL,
    /// `break` keyword.
    BREAK,
    /// `bvec2` keyword.
    BVEC2,
    /// `bvec3` keyword.
    BVEC3,
    /// `bvec4` keyword.
    BVEC4,
    /// `const` keyword.
    CONST,
    /// `continue` keyword.
    CONTINUE,
    DISCARD,
    DO,
    ELSE,
    FLOAT,
    FOR,
    HighPrecision,
    IF,
    IN,
    INOUT,
    INT,
    INVARIANT,
    IVEC2,
    IVEC3,
    IVEC4,
    LowPrecision,
    MAT2,
    MAT3,
    MAT4,
    MediumPrecision,
    OUT,
    PRECISION,
    RETURN,
    SAMPLER2D,
    SAMPLERCUBE,
    STRUCT, 
    UNIFORM,
    VARYING,
    VEC2,
    VEC3,
    /// `vec4` keyword.
    VEC4,
    /// `void` keyword.
    VOID,
    /// `while` keyword.
    While,
}

impl ::std::fmt::Show for Token {
    fn fmt(&self, formatter: &mut ::std::fmt::Formatter) -> Result<(), ::std::fmt::FormatError> {
        match *self {
            Identifier(ref s) => s.fmt(formatter),
            Integer(val) => val.fmt(formatter),
            Boolean(val) => if val { "true".fmt(formatter) } else { "false".fmt(formatter) },
            Float(val) => val.fmt(formatter),

            Whitespace(ref s) => s.fmt(formatter),

            Increment => "++".fmt(formatter),
            Decrement => "--".fmt(formatter),
            LessOrEqual => "<=".fmt(formatter),
            MoreOrEqual => ">=".fmt(formatter),
            Equal => "==".fmt(formatter),
            NotEqual => "!=".fmt(formatter),

            AndOp => "&&".fmt(formatter),
            OrOp => "||".fmt(formatter),
            XorOp => "^^".fmt(formatter),
            MulAssign => "*=".fmt(formatter),
            DivAssign => "/=".fmt(formatter),
            AddAssign => "+=".fmt(formatter),
            ModAssign => "%=".fmt(formatter),
            SubAssign => "-=".fmt(formatter),

            LeftParenthesis => "(".fmt(formatter),
            RightParenthesis => ")".fmt(formatter),
            LeftBracket => "[".fmt(formatter),
            RightBracket => "]".fmt(formatter),
            LeftBrace => "{".fmt(formatter),
            RightBrace => "}".fmt(formatter),
            Dot => ".".fmt(formatter),

            Comma => ",".fmt(formatter),
            Colon => ":".fmt(formatter),
            Assign => "=".fmt(formatter),
            SemiColon => ";".fmt(formatter),
            Bang => "!".fmt(formatter),
            Dash => "-".fmt(formatter),
            Tilde => "~".fmt(formatter),
            Plus => "+".fmt(formatter),
            Star => "*".fmt(formatter),
            Slash => "/".fmt(formatter),
            Percent => "%".fmt(formatter),
            Sharp => "#".fmt(formatter),

            LeftAngle => "<".fmt(formatter),
            RightAngle => ">".fmt(formatter),
            VerticalBar => "|".fmt(formatter),
            Caret => "^".fmt(formatter),
            Ampersand => "&".fmt(formatter),
            Question => "?".fmt(formatter),

            Attribute => "attribute".fmt(formatter),
            BOOL => "bool".fmt(formatter),
            BREAK => "break".fmt(formatter),
            BVEC2 => "bvec2".fmt(formatter),
            BVEC3 => "bvec3".fmt(formatter),
            BVEC4 => "bvec4".fmt(formatter),
            CONST => "const".fmt(formatter),
            CONTINUE => "continue".fmt(formatter),
            DISCARD => "discard".fmt(formatter),
            DO => "do".fmt(formatter),
            ELSE => "else".fmt(formatter),
            FLOAT => "float".fmt(formatter),
            FOR => "for".fmt(formatter),
            HighPrecision => "highp".fmt(formatter),
            IF => "if".fmt(formatter),
            IN => "in".fmt(formatter),
            INOUT => "inout".fmt(formatter),
            INT => "int".fmt(formatter),
            INVARIANT => "invariant".fmt(formatter),
            IVEC2 => "ivec2".fmt(formatter),
            IVEC3 => "ivec3".fmt(formatter),
            IVEC4 => "ivec4".fmt(formatter),
            LowPrecision => "lowp".fmt(formatter),
            MAT2 => "mat2".fmt(formatter),
            MAT3 => "mat3".fmt(formatter),
            MAT4 => "mat4".fmt(formatter),
            MediumPrecision => "mediump".fmt(formatter),
            OUT => "out".fmt(formatter),
            PRECISION => "precision".fmt(formatter),
            RETURN => "return".fmt(formatter),
            SAMPLER2D => "sampler2d".fmt(formatter),
            SAMPLERCUBE => "samplercube".fmt(formatter),
            STRUCT => "struct".fmt(formatter), 
            UNIFORM => "uniform".fmt(formatter),
            VARYING => "varying".fmt(formatter),
            VEC2 => "vec2".fmt(formatter),
            VEC3 => "vec3".fmt(formatter),
            VEC4 => "vec4".fmt(formatter),
            VOID => "void".fmt(formatter),
            While => "while".fmt(formatter),
        }
    }
}

pub struct Lexer<R> {
    reader: R,
    next_byte: Option<u8>,
    span: Span,
}

impl<R: Reader> Lexer<R> {
    pub fn new(reader: R) -> Lexer<R> {
        Lexer {
            reader: reader,
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

    // TODO: just wrong
    fn parse_number(&mut self) -> Spanned<Token> {
        let span = self.span.clone();

        let mut value = 0u64;

        loop {
            match self.next_byte.unwrap() {
                b'0' ... b'9' => {
                    value *= 10;
                    value += (self.next_byte.unwrap() - b'0') as u64;
                },
                _ => break
            }

            self.read_next();
        }

        Spanned {
            start: span,
            content: Integer(value),
        }
    }

    fn parse_whitespace(&mut self) -> Spanned<Token> {
        let start = self.span.clone();

        let mut buffer = Vec::new();
        while self.next_byte.is_some() && is_whitespace(self.next_byte.unwrap()) {
            buffer.push(self.next_byte.unwrap());
            self.read_next();
        }

        let buffer = buffer[].to_ascii().as_str_ascii().to_string();

        Spanned {
            content: Whitespace(buffer),
            start: start,
        }
    }

    fn parse_symbol(&mut self) -> Spanned<Token> {
        let start = self.span.clone();

        let mut buffer = Vec::new();
        while self.next_byte.is_some() &&
              !is_ident_start(self.next_byte.unwrap()) &&
              !is_whitespace(self.next_byte.unwrap()) &&
              !(self.next_byte.unwrap() >= b'0' &&
              self.next_byte.unwrap() <= b'9')
        {
            buffer.push(self.next_byte.unwrap());
            self.read_next();

            if let Some(sym) = parse_symbol(buffer.as_slice()) {
                return Spanned {
                    content: sym,
                    start: start,
                };
            }
        }

        match parse_symbol(buffer.as_slice()) {
            Some(sym) => Spanned {
                content: sym,
                start: start,
            },
            None => fail!("lexing error: {} at {}", buffer, start)         // TODO: return error
        }
    }

    fn parse_identifier_or_keyword(&mut self) -> Spanned<Token> {
        let start = self.span.clone();

        let mut buffer = Vec::new();
        while self.next_byte.is_some() && is_ident(self.next_byte.unwrap()) {
            buffer.push(self.next_byte.unwrap());
            self.read_next();
        }

        let buffer = buffer[].to_ascii().as_str_ascii().to_string();

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
        if self.next_byte.is_none() {
            self.read_next();
            if self.next_byte.is_none() { return None; }
        }

        if is_whitespace(self.next_byte.unwrap()) {
            Some(self.parse_whitespace())

        } else if is_ident_start(self.next_byte.unwrap()) {
            Some(self.parse_identifier_or_keyword())

        } else if self.next_byte.unwrap() >= b'0' && self.next_byte.unwrap() <= b'9' {
            Some(self.parse_number())

        } else {
            Some(self.parse_symbol())
        }
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

fn is_whitespace(c: u8) -> bool {
    match c {
        b'\r' | b'\n' | b'\t' | b' ' | 12u8 => true,
        _ => false
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
        b"#" => Some(Sharp),

        b"<" => Some(LeftAngle),
        b">" => Some(RightAngle),
        b"|" => Some(VerticalBar),
        b"^" => Some(Caret),
        b"&" => Some(Ampersand),
        b"?" => Some(Question),

        _ => None
    }
}

fn parse_keyword(buffer: &str) -> Option<Token> {
    use std::ascii::AsciiExt;

    match buffer.to_ascii_lower().as_slice() {
        "attribute" => Some(Attribute),
        "bool" => Some(BOOL),
        "break" => Some(BREAK),
        "bvec2" => Some(BVEC2),
        "bvec3" => Some(BVEC3),
        "bvec4" => Some(BVEC4),
        "const" => Some(CONST),
        "continue" => Some(CONTINUE),
        "discard" => Some(DISCARD),
        "do" => Some(DO),
        "else" => Some(ELSE),
        "float" => Some(FLOAT),
        "for" => Some(FOR),
        "highp" => Some(HighPrecision),
        "if" => Some(IF),
        "in" => Some(IN),
        "inout" => Some(INOUT),
        "int" => Some(INT),
        "invariant" => Some(INVARIANT),
        "ivec2" => Some(IVEC2),
        "ivec3" => Some(IVEC3),
        "ivec4" => Some(IVEC4),
        "lowp" => Some(LowPrecision),
        "mat2" => Some(MAT2),
        "mat3" => Some(MAT3),
        "mat4" => Some(MAT4),
        "mediump" => Some(MediumPrecision),
        "out" => Some(OUT),
        "precision" => Some(PRECISION),
        "return" => Some(RETURN),
        "sampler2D" => Some(SAMPLER2D),
        "samplerCube" => Some(SAMPLERCUBE),
        "struct" => Some(STRUCT),
        "uniform" => Some(UNIFORM),
        "varying" => Some(VARYING),
        "vec2" => Some(VEC2),
        "vec3" => Some(VEC3),
        "vec4" => Some(VEC4),
        "void" => Some(VOID),
        "while" => Some(While),
        _ => None,
    }
}


#[cfg(test)]
mod test {
    use std::io::BufReader;
    use super::Lexer;

    #[test]
    fn basic() {
        let src = b"\
                #version 110

                uniform mat4 uMatrix;

                attribute vec2 iPosition;
                attribute vec2 iTexCoords;

                varying vec2 vTexCoords;

                void main() {
                    gl_Position = uMatrix * vec4(iPosition, 0.0, 1.0);
                    vTexCoords = iTexCoords;
                }";

        let reader = BufReader::new(src);

        let lexer = Lexer::new(reader);

        let dest = lexer.map(|e| e.content.to_string()).collect::<Vec<_>>().concat();
        assert_eq!(src, dest.as_bytes());
    }
}
