use std::str::FromStr;

#[derive(Debug, PartialEq)]
pub enum TokenType<'a> {
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
    String(&'a str),
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

#[derive(Debug)]
pub enum Literal<'a> {
    String(&'a str)
}

#[derive(Debug)]
pub struct Token<'a> {
    pub token_type: TokenType<'a>,
    lexeme: &'a str,
    line: usize
}

impl<'a> FromStr for TokenType<'a> {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
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
            _ => Err(format!("Unexpected character: `{s}`").to_string())
        }
    }
}

impl<'a> Token<'a> {
    pub fn new(token_type: TokenType<'a>, lexeme: &'a str, line: usize) -> Self {
        Token {
            token_type,
            lexeme,
            line
        }
    }
}