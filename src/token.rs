use crate::{error::Error, Materializable};

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenType {
    // Single-character tokens
    LeftParen, 
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    //Literals
    Identifier,
    String,
    Number,

    //Keywords
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Eof
}

#[derive(Debug, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: Lexeme,
    line: usize
}

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone, Default)]
pub struct Lexeme {
    pub start: usize,
    pub end: usize
}

impl TokenType {
    pub(crate) fn from_str(s: &str, line: usize, position: usize) -> Result<Self, Error> {
        match s {
            "(" => Ok(TokenType::LeftParen),
            ")" => Ok(TokenType::RightParen),
            "{" => Ok(TokenType::LeftBrace),
            "}" => Ok(TokenType::RightBrace),
            "," => Ok(TokenType::Comma),
            "." => Ok(TokenType::Dot),
            "-" => Ok(TokenType::Minus),
            "+" => Ok(TokenType::Plus),
            ";" => Ok(TokenType::Semicolon),
            "*" => Ok(TokenType::Star),
            "/" => Ok(TokenType::Slash),
            "!" => Ok(TokenType::Bang),
            "=" => Ok(TokenType::Equal),
            "<" => Ok(TokenType::Less),
            ">" => Ok(TokenType::Greater),
            // keywords
            "and" => Ok(TokenType::And),
            "class" => Ok(TokenType::Class),
            "else" => Ok(TokenType::Else),
            "false" => Ok(TokenType::False),
            "for" => Ok(TokenType::For),
            "fun" => Ok(TokenType::Fun),
            "if" => Ok(TokenType::If),
            "nil" => Ok(TokenType::Nil),
            "or" => Ok(TokenType::Or),
            "print" => Ok(TokenType::Print),
            "return" => Ok(TokenType::Return),
            "super" => Ok(TokenType::Super),
            "this" => Ok(TokenType::This),
            "true" => Ok(TokenType::True),
            "var" => Ok(TokenType::Var),
            "while" => Ok(TokenType::While),
            _ => Err(Error::ScannerError(format!("Unexpected character: `{s}`").to_string(), line, position))
        }
    }
}

impl Token {
    pub fn new(token_type: TokenType, (start, end): (usize, usize), line: usize) -> Self {
        Token {
            token_type,
            lexeme: Lexeme::new(start, end),
            line
        }
    }
}

impl Lexeme {
    pub fn new(start: usize, end: usize) -> Self {
        Lexeme {
            start,
            end
        }
    }

    pub fn union(&self, other: &Self) -> Self {
        Lexeme { 
            start: usize::min(self.start, other.start), 
            end: usize::max(self.end, other.end) 
        }
    }
}

impl<'a> Materializable<'a, &'a str> for Lexeme {
    fn materialize(&self, context: &'a crate::context::Context) -> &'a str {
        &context.source[self.start..self.end]
    }
}