mod lexer;

pub struct Preprocessor<R> {
    lexer: lexer::Lexer<R>,
}

pub enum ParserOutput {
    /// Regular GLSL data.
    Data(Vec<u8>),

    /// Encountered a `#line` element.
    LineChange(uint),

    /// Encountered a `#version` directive.
    VersionDirective(uint),

    /// Encountered an `#extension` directive.
    ///
    /// 
    ExtensionDirective(Option<Vec<u8>>, ExtensionBehavior),

    /// Encountered an `#error` directive.
    ErrorDirective(Vec<u8>),

    /// Encountered a `#pragma` directive.
    PragmaDirective(Vec<u8>),
}

pub enum ExtensionBehavior {
    Require,
    Enable,
    Warn,
    Disable
}

impl<R: Reader> Preprocessor<R> {
    pub fn new(reader: R) -> Preprocessor<R> {
        Preprocessor {
            lexer: lexer::Lexer::new(reader),
        }
    }
}

impl<R: Reader> Iterator<ParserOutput> for Preprocessor<R> {
    fn next(&mut self) -> Option<ParserOutput> {
        unimplemented!()
    }
}
