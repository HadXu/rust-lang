#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    ILLEGAL,
    EOF,

    IDENT(String),
    INT(i64),
    BOOL(bool),
    STRING(String),

    ASSIGN,
    PLUS,
    MINUS,
    BANG,
    ASTERISK,
    SLASH,

    LT,
    GT,
    LTE,
    GTE,

    COMMA,
    SEMICOLON,
    COLON,

    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,

    LBRACKET,
    RBRACKET,

    FUNCTION,
    LET,
    IF,
    ELSE,
    RETURN,

    EQ,
    #[allow(non_camel_case_types)]
    NOT_EQ,

    FOR,
}
