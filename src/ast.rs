use std::rc::Rc;

#[derive(Debug)]
pub struct Program {
    pub f: Function
}

#[derive(Debug)]
pub struct Function {
    pub name: Rc<String>,
    pub stmt: Statement
}

#[derive(Debug)]
pub enum Statement {
    Return(Expr)
}

#[derive(Debug)]
pub enum Expr {
    Constant(i32),
    Unary(UnaryOp, Box<Expr>)
}

#[derive(Debug)]
pub enum UnaryOp {
    Complement,
    Negate
}