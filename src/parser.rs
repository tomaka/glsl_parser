use {Span, Spanned};
use lexer::{mod, Lexer, Token};

#[deriving(Clone)]
pub enum ParseError {
    UnexpectedEndOfFile,

    /// An unexpected token has been encountered.
    UnexpectedToken(Spanned<Token>, String),
}

impl ::std::fmt::Show for ParseError {
    fn fmt(&self, formatter: &mut ::std::fmt::Formatter) -> Result<(), ::std::fmt::FormatError> {
        match self {
            &UnexpectedEndOfFile => {
                "unexpected end of file".fmt(formatter)
            },
            &UnexpectedToken(ref loc, ref expect) => {
                (format!("unexpected token `{}` at line {}:{}, expected {} instead",
                    loc.content, loc.start.line, loc.start.offset, expect)).fmt(formatter)
            }
        }
    }
}

#[deriving(Show, Clone)]
pub struct TranslationUnit(Vec<ExternalDeclaration>);

#[deriving(Show, Clone)]
pub enum ExternalDeclaration {
    /// Function definition.
    ExternalDeclarationFunctionDefinition(FunctionDeclaration, Vec<Statement>),
    ExternalDeclarationDeclaration(Declaration),
}

#[deriving(Show, Clone)]
pub enum Declaration {
    DeclarationVariable(FullySpecifiedType, String, Option<Expression>),
    DeclarationFunction(FunctionDeclaration),
}

#[deriving(Show, Clone)]
/// `$retvalue $name($params)`
pub struct FunctionDeclaration(FullySpecifiedType, String, Vec<()>);

#[deriving(Show, Clone)]
pub struct StructDefinition(Vec<()>);

#[deriving(Show, Clone)]
pub struct FullySpecifiedType {
    pub ty: Type,
    pub storage_qualifiers: Vec<StorageQualifier>,
    pub parameter_qualifier: Option<ParameterQualifier>,
    pub precision_qualifier: Option<PrecisionQualifier>,
}

#[deriving(Show, Clone)]
pub enum StorageQualifier {
    StorageQualifierConst,
    StorageQualifierInOut,
    StorageQualifierIn,
    StorageQualifierOut,
    StorageQualifierCentroid,
    StorageQualifierPatch,
    StorageQualifierSample,
    StorageQualifierUniform,
    StorageQualifierBuffer,
    StorageQualifierShared,
    StorageQualifierCoherent,
    StorageQualifierVolatile,
    StorageQualifierRestrict,
    StorageQualifierReadonly,
    StorageQualifierWriteonly,
    StorageQualifierSubroutine(Vec<()>),        // TODO: 
}

#[deriving(Show, Clone)]
pub enum PrecisionQualifier {
    PrecisionQualifierHigh,
    PrecisionQualifierMedium,
    PrecisionQualifierLow,
}

#[deriving(Show, Clone)]
pub enum ParameterQualifier {
    ParameterQualifierIn,
    ParameterQualifierOut,
    ParameterQualifierInOut,
}

#[deriving(Show, Clone)]
pub enum Type {
    Void,
    Float,
    Int,
    Bool,
    Vec2,
    Vec3,
    Vec4,
    BVec2,
    BVec3,
    BVec4,
    IVec2,
    IVec3,
    IVec4,
    Mat2,
    Mat3,
    Mat4,
    Sampler2D,
    SamplerCube,
    TypeStruct(StructDefinition),
    TypeIdentifier(String),
}

#[deriving(Show, Clone)]
pub enum Statement {
    StatementExpression(Expression),
    /// `{ $statements }`
    StatementScope(Vec<Statement>),
    /// if `Expression` then `Statement` else `Option<Statement>`.
    StatementIf(Expression, Box<Statement>, Option<Box<Statement>>),
    /// `continue;`
    StatementContinue,
    /// `break;`
    StatementBreak,
    /// `return $expression;`
    StatementReturn(Expression),
    /// `discard;`
    StatementDiscard,
    StatementDeclaration(Declaration),
}

#[deriving(Show, Clone)]
pub enum Expression {
    /// `$op $2`
    ExpressionUnaryOperation(UnaryOperation, Box<Expression>),
    /// `$1 $op $2`
    ExpressionBinaryOperation(Box<Expression>, BinaryOperation, Box<Expression>),
    /// `$1 ? $2 : $3`
    ExpressionCondition(Box<Expression>, Box<Expression>, Box<Expression>),
    /// `$1($2)`
    ExpressionFunctionCall(Box<Expression>, Vec<Expression>),
    /// `$1`,
    ExpressionIdentifier(String),
    /// `__LINE__`
    ExpressionLine,
    /// `__FILE__`
    ExpressionFile,
    /// `__VERSION__`
    ExpressionVersion,
}

#[deriving(Show, Clone)]
pub enum UnaryOperation {
    /// `+`
    UnaryOperationPlus,
    /// `-`
    UnaryOperationMinus,
    /// `!`
    UnaryOperationNot,
    /// `++e`
    UnaryOperationPreInc,
    /// `--e`
    UnaryOperationPreDec,
    /// `e++`
    UnaryOperationPostInc,
    /// `e--`
    UnaryOperationPostDec,
}

#[deriving(Show, Clone)]
pub enum BinaryOperation {
    /// `+`
    BinaryOperationAddition,
    /// `-`
    BinaryOperationSubstraction,
    /// `*`
    BinaryOperationMultiplication,
    /// `/`
    BinaryOperationDivision,
    /// `%`
    BinaryOperationMod,
    /// `^`
    BinaryOperationXor,
    /// `&&`
    BinaryOperationAnd,
    /// `||`
    BinaryOperationOr,
    /// `&`
    BinaryOperationBinAnd,
    /// `|`
    BinaryOperationBinOr,
}

pub fn parse<R: Reader>(data: R) -> Result<TranslationUnit, ParseError> {
    let mut parser = Parser {
        lexer: Lexer::new(data),
        next_token: None,
    };

    try!(parser.read_next());
    parser.parse_translation_unit()
}

struct Parser<R> {
    lexer: Lexer<R>,
    next_token: Option<Spanned<Token>>,
}

impl<R: Reader> Parser<R> {
    fn read_next(&mut self) -> Result<(), ParseError> {
        self.next_token = self.lexer.next();
        Ok(())
    }

    fn parse_translation_unit(&mut self) -> Result<TranslationUnit, ParseError> {
        let mut result = Vec::new();

        loop {
            match try!(self.parse_external_declaration()) {
                Some(decl) => result.push(decl),
                None => break
            };
        }

        Ok(TranslationUnit(result))
    }

    fn parse_external_declaration(&mut self) -> Result<Option<ExternalDeclaration>, ParseError> {
        self.skip_whitespaces();

        let token = match self.next_token {
            Some(ref token) => token.clone(),
            None => return Ok(None),
        };

        if token.content == lexer::Sharp {
            unimplemented!()
        }

        let ty = try!(self.parse_fully_specified_type());
        try!(self.expect_whitespace());
        let name = try!(self.parse_identifier());

        match (try!(self.peek())).content {
            lexer::SemiColon => {
                try!(self.read_next());
                return Ok(Some(ExternalDeclarationDeclaration(
                    DeclarationVariable(ty, name, None)
                )));
            },
            lexer::LeftParenthesis => {
                try!(self.read_next());
                // TODO: function parameters
                try!(self.expect_token(lexer::RightParenthesis));
                self.skip_whitespaces();

                match (try!(self.peek())).content {
                    lexer::LeftBrace => {
                        try!(self.read_next());
                        self.skip_whitespaces();
                        let body = try!(self.parse_statements_list());
                        self.skip_whitespaces();
                        try!(self.expect_token(lexer::RightBrace));

                        return Ok(Some(ExternalDeclarationFunctionDefinition(
                            FunctionDeclaration(ty, name, Vec::new()),
                            body
                        )));
                    },
                    lexer::SemiColon => {
                        try!(self.read_next());
                        return Ok(Some(ExternalDeclarationDeclaration(
                            DeclarationFunction(FunctionDeclaration(ty, name, Vec::new()))
                        )));
                    },
                    _ => ()
                }
            }
            _ => ()
        };

        Err(UnexpectedToken(try!(self.peek()), format!("external declaration")))
    }

    fn peek(&mut self) -> Result<Spanned<Token>, ParseError> {
        match self.next_token {
            Some(ref token) => Ok(token.clone()),
            None => Err(UnexpectedEndOfFile),
        }
    }

    fn expect_whitespace(&mut self) -> Result<(), ParseError> {
        let token = try!(self.peek());

        if let lexer::Whitespace(_) = token.content {
            try!(self.read_next());
            return Ok(());
        } else {
            return Err(UnexpectedToken(token.clone(), format!("whitespace")));
        }
    }

    fn skip_whitespaces(&mut self) {
        loop {
            let token = match self.next_token {
                Some(ref token) => token.clone(),
                None => return,
            };

            if let lexer::Whitespace(_) = token.content {
                self.read_next();
            } else {
                break;
            }
        }
    }

    fn parse_identifier(&mut self) -> Result<String, ParseError> {
        let token = try!(self.peek());

        if let lexer::Identifier(ref id) = token.content {
            let val = Ok(id.clone());
            try!(self.read_next());
            val
        } else {
            Err(UnexpectedToken(token.clone(), format!("identifier")))
        }
    }

    // TODO: incomplete
    fn parse_fully_specified_type(&mut self) -> Result<FullySpecifiedType, ParseError> {
        let mut token;

        let mut storage_qualifiers = Vec::new();

        loop {
            token = try!(self.peek());

            match token.content {
                lexer::Uniform => {
                    storage_qualifiers.push(StorageQualifierUniform);
                },
                lexer::Attribute => {
                    storage_qualifiers.push(StorageQualifierIn);
                },
                lexer::Varying => {
                    //storage_qualifiers.push(StorageQualifierOut);
                    // TODO: same as `in` in vertex shaders and `out` in fragment shaders
                },
                _ => break
            }

            try!(self.read_next());
            self.expect_whitespace();
        }

        token = try!(self.peek());

        let ty = match token.content {
            lexer::Void => Void,
            lexer::Int => Int,
            lexer::Mat4 => Mat4,
            lexer::Vec2 => Vec2,
            _ => return Err(UnexpectedToken(token, format!("type")))
        };

        try!(self.read_next());

        Ok(FullySpecifiedType {
            ty: ty,
            storage_qualifiers: storage_qualifiers,
            parameter_qualifier: None,
            precision_qualifier: None,
        })
    }

    fn parse_expression(&mut self) -> Result<Expression, ParseError> {
        // TODO: operators precedence

        let token = try!(self.peek());

        let expression = match token.content {
            lexer::Plus | lexer::Dash | lexer::Bang | lexer::Increment | lexer::Decrement => {
                try!(self.read_next());

                let op = match token.content {
                    lexer::Plus => UnaryOperationPlus,
                    lexer::Dash => UnaryOperationMinus,
                    lexer::Bang => UnaryOperationNot,
                    lexer::Increment => UnaryOperationPreInc,
                    lexer::Decrement => UnaryOperationPreDec,
                    _ => fail!()
                };

                let expr = try!(self.parse_expression());
                ExpressionUnaryOperation(op, box expr)
            },
            lexer::Identifier(ref id) => {
                try!(self.read_next());
                ExpressionIdentifier(id.clone())
            },
            _ => return Err(UnexpectedToken(token.clone(), format!("expression")))
        };

        match self.next_token.as_ref().map(|e| e.clone()) {
            Some(ref a) if a.content == lexer::Increment => {
                try!(self.read_next());
                Ok(ExpressionUnaryOperation(UnaryOperationPostInc, box expression))
            },
            Some(ref a) if a.content == lexer::Decrement => {
                try!(self.read_next());
                Ok(ExpressionUnaryOperation(UnaryOperationPostDec, box expression))
            },
            Some(ref a) if a.content == lexer::LeftParenthesis => {
                try!(self.read_next());
                unimplemented!()
            },
            _ => Ok(expression),
        }
    }

    fn parse_statements_list(&mut self) -> Result<Vec<Statement>, ParseError> {
        let mut result = Vec::new();
        while self.next_token.as_ref().map(|e| e.content.clone()) != Some(lexer::RightBrace) {
            result.push(try!(self.parse_statement()));
            self.skip_whitespaces();
        }
        Ok(result)
    }

    fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        let token = try!(self.peek());

        match token.content {
            lexer::LeftBrace => {
                try!(self.read_next());
                let list = try!(self.parse_statements_list());
                try!(self.expect_token(lexer::RightBrace));
                Ok(StatementScope(list))
            },
            lexer::Continue => {
                try!(self.read_next());
                try!(self.expect_semicolon());
                Ok(StatementContinue)
            },
            lexer::Break => {
                try!(self.read_next());
                try!(self.expect_semicolon());
                Ok(StatementBreak)
            },
            lexer::Return => {
                try!(self.read_next());
                let expr = try!(self.parse_expression());
                try!(self.expect_semicolon());
                Ok(StatementReturn(expr))
            },
            lexer::Discard => {
                try!(self.read_next());
                try!(self.expect_semicolon());
                Ok(StatementDiscard)
            },
            _ => {
                self.parse_expression().map(|e| StatementExpression(e))
            }
        }
    }

    fn expect_semicolon(&mut self) -> Result<(), ParseError> {
        self.expect_token(lexer::SemiColon)
    }

    fn expect_token(&mut self, token: Token) -> Result<(), ParseError> {
        let val = match self.next_token {
            None => Err(UnexpectedEndOfFile),
            Some(ref a) if a.content == token => Ok(()),
            Some(ref a) => Err(UnexpectedToken(a.clone(), token.to_string())),
        };

        if val.is_ok() {
            try!(self.read_next());
        }

        val
    }
}


#[cfg(test)]
mod test {
    use std::io::BufReader;
    use super::parse;

    #[test]
    fn test() {
        let src = b"\
                uniform mat4 uMatrix;

                attribute vec2 iPosition;
                attribute vec2 iTexCoords;

                varying vec2 vTexCoords;

                void main() {
                    gl_Position = uMatrix * vec4(iPosition, 0.0, 1.0);
                    vTexCoords = iTexCoords;
                }";

        let reader = BufReader::new(src);

        fail!("{}", parse(reader));
    }
}
