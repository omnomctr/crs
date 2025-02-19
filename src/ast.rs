use std::rc::Rc;

#[derive(Debug)]
pub struct Program {
    pub f: Function
}

#[derive(Debug)]
pub struct Function {
    pub name: Identifier,
    pub body: Block
}

pub(crate) type Block = Vec<BlockItem>;

#[derive(Debug)]
pub enum BlockItem {
    S(Statement),
    D(Declaration),
}

#[derive(Debug)]
pub enum Statement {
    Return(Expr),
    Expression(Expr),
    If(IfStatement),
    Empty, /* eg while(1) ; <- in between the paren and semicolon */
    LabeledStatement(Identifier, Box<Statement>), /* label: stmts for goto */
    JmpStatement(Identifier),
    Block(Block),
}

#[derive(Debug)]
pub struct IfStatement {
    pub condition: Expr,
    pub then: Block,
    pub otherwise: Option<Block>,
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
    CompoundAssignment(BinaryOp, Box<Expr>, Box<Expr>), /* op, lvalue, rvalue */
    PrefixInc(Incrementation, Box<Expr>), /* ++x / --x */
    PostfixInc(Incrementation, Box<Expr>), /* x++ / x-- */
    Ternary(Box<Expr>, Box<Expr>, Box<Expr>), /* condition ? then : else */
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

#[derive(Debug)]
pub enum Incrementation {
    Increment,
    Decrement
}