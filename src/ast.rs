
#[derive(PartialEq, Clone, Debug)]
pub struct Ident(pub String);

#[derive(PartialEq, Clone, Debug)]
pub enum Expr {
    Literal(Literal),
}

#[derive(PartialEq, Clone, Debug)]
pub enum Literal {
    Int(i64)
}

#[derive(PartialEq, Clone, Debug)]
pub enum Stmt {
    Let(Ident, Expr),
    Expr(Expr),
}

pub type BlockStmt = Vec<Stmt>;

pub type Program = BlockStmt;