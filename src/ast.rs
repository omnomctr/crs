use std::rc::Rc;

#[derive(Debug)]
pub struct Program {
    pub functions: Vec<FunctionDeclaration>,
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
    While(Expr, Box<Statement>, Option<usize>), /* condition, body, label (used in semantic analysis */
    DoWhile(Expr, Box<Statement>, Option<usize>),
    Break(Option<usize>),
    Continue(Option<usize>),
    ForLoop(Option<ForInitializer>, Option<Expr>, Option<Expr>, Box<Statement>, Option<usize>), /* for (1;2;3) 4 */
}

#[derive(Debug)]
pub enum ForInitializer {
    Decl(VariableDeclaration),
    Expr(Expr)
}

#[derive(Debug)]
pub struct IfStatement {
    pub condition: Expr,
    pub then: Block,
    pub otherwise: Option<Block>,
}

#[derive(Debug)]
pub enum Declaration {
    Fun(FunctionDeclaration),
    Var(VariableDeclaration),
}

#[derive(Debug)]
pub struct VariableDeclaration {
    pub name: Identifier,
    pub initializer: Option<Expr>,
}

#[derive(Debug)]
pub struct FunctionDeclaration {
    pub name: Identifier,
    pub params: Vec<Identifier>,
    pub body: Option<Block>,
}


#[derive(Debug)]
pub enum Expr {
    Constant(i32),
    Unary(UnaryOp, Box<Expr>),
    Binary(BinaryOp, Box<Expr>, Box<Expr>),
    Var(Identifier),
    Assignment(Box<Expr>, Box<Expr>), /* lvalue, '=', rvalue */
    CompoundAssignment(BinaryOp, Box<Expr>, Box<Expr>), /* op, lvalue, rvalue */
    PrefixInc(Incrementation, Box<Expr>), /* x is an lvalue: ++x / --x */
    PostfixInc(Incrementation, Box<Expr>), /* x is an lvalue: x++ / x-- */
    Ternary(Box<Expr>, Box<Expr>, Box<Expr>), /* condition ? then : else */
    FunCall(Identifier, Vec<Expr>, Option<bool>), /* name, params, extern? (done in semantic analysis */
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