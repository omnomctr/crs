use std::rc::Rc;

#[derive(Debug)]
pub struct Program {
    pub f: Function
}

#[derive(Debug)]
pub struct Function {
    pub name: Identifier,
    pub body: Vec<BlockItem>
}

#[derive(Debug)]
pub enum BlockItem {
    S(Statement),
    D(Declaration),
}

#[derive(Debug)]
pub enum Statement {
    Return(Expr),
    Expression(Expr),
    Empty, /* eg while(1) ; <- in between the paren and semicolon */
}

#[derive(Debug)]
pub struct Declaration {
    pub name: Identifier,
    pub initializer: Option<Expr>,
}

#[derive(Debug)]
pub enum Expr {
    Constant(i32),
    Unary(UnaryOp, Box<Expr>),
    Binary(BinaryOp, Box<Expr>, Box<Expr>),
    Var(Identifier),
    Assignment(Box<Expr>, Box<Expr>), /* lvalue, '=', rvalue */
}

pub type Identifier = Rc<String>;

#[derive(Debug)]
pub enum UnaryOp {
    Complement,
    Negate,
    Not,
}

#[derive(Debug)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    LeftShift,
    RightShift,
    And,
    Or,
    Eq,
    NEq,
    LT,
    LTE,
    GT,
    GTE,
}

