#[derive(PartialEq, Clone, Debug)]
pub struct Ident(pub String);

#[derive(PartialEq, Clone, Debug)]
pub enum Expr {
    Ident(Ident),
    Literal(Literal),
    Prefix(Prefix, Box<Expr>),
    Infix(Infix, Box<Expr>, Box<Expr>),
    Index(Box<Expr>, Box<Expr>),
    If {
        cond: Box<Expr>,
        consequence: BlockStmt,
        alternative: Option<BlockStmt>,
    },
    Func {
        params: Vec<Ident>,
        body: BlockStmt,
    },
    Call {
        func: Box<Expr>,
        args: Vec<Expr>,
    },
}

#[derive(PartialEq, Clone, Debug)]
pub enum Literal {
    Int(i64),
    Bool(bool),
    String(String),
    Array(Vec<Expr>),
    Hash(Vec<(Expr, Expr)>),
}

#[derive(PartialEq, Clone, Debug)]
pub enum Stmt {
    Let(Ident, Expr),
    Expr(Expr),
    Return(Expr),
}

pub type BlockStmt = Vec<Stmt>;

pub type Program = BlockStmt;

#[derive(PartialEq, PartialOrd, Debug, Clone)]
pub enum Precedence {
    LOWEST,
    EQUALS,      // ==
    LESSGREATER, // >, <, >=, <=
    SUM,         // +
    PRODUCT,     // *
    PREFIX,      // -X or !X
    CALL,        // myFunction(x)
    Index,       // array[index]
}

#[derive(PartialEq, Clone, Debug)]
pub enum Prefix {
    PLUS,
    MINUS,
    NOT,
}

#[derive(PartialEq, Clone, Debug)]
pub enum Infix {
    PLUS,
    MINUS,
    DIVIDE,
    MULTIPLY,
    EQUAL,
    NOTEQUAL,
    GREATERTHAN,
    LESSTHAN,
}
