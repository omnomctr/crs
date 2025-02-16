use std::fmt::{Display, Formatter};
use std::rc::Rc;
use crate::ast;
use crate::ast::{Expr, Statement};

/* intermediate representation
 * page 36 of the book
 */

#[derive(Debug)]
pub struct Program {
    pub function_definition: Function,
}

#[derive(Debug)]
pub struct Function {
    pub name: Identifier,
    pub body: Vec<Instruction>,
}

#[derive(Debug)]
pub enum Instruction {
    Return(Val),
    Unary(UnaryOp, Val, Val), /* op, src, dst */
    Binary(BinaryOp, Val, Val, Val), /* op, lhs, rhs, dst */
}

#[derive(Debug, Clone)]
pub enum Val {
    Constant(i32),
    Var(Identifier)
}

#[derive(Debug)]
pub enum UnaryOp {
    Complement,
    Negate
}

#[derive(Debug)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
    BitwiseAnd,
    BitWiseOr,
    BitWiseXor,
    LeftShift,
    RightShift,
}

struct EmitterState {
    tmp_namen: usize,
}

type Identifier = Rc<String>;

pub fn emit_ir(prog: ast::Program) -> Program {
    let name = Rc::clone(&prog.f.name);
    let mut state = EmitterState {
        tmp_namen: 0,
    };

    let mut insts = Vec::new();
    emit_statement(prog.f.stmt, &mut state, &mut insts);

    Program {
        function_definition: Function {
            name: Rc::clone(&name),
            body: insts
        }
    }
}

fn emit_statement(stmt: ast::Statement, es: &mut EmitterState, insts: &mut Vec<Instruction>) {
    match stmt {
        Statement::Return(expr) => {
            let val = emit_expr(&expr, es, insts);
            insts.push(Instruction::Return(val));
        }
    }
}

fn emit_expr(expr: &ast::Expr, es: &mut EmitterState, insts: &mut Vec<Instruction>) -> Val {
    match expr {
        Expr::Constant(c) => Val::Constant(*c),
        Expr::Unary(op, rhs) => {
            let src = emit_expr(rhs, es, insts);
            let dst = make_temporary(es);
            insts.push(Instruction::Unary(
                convert_unary(op),
                src,
                dst.clone()
            ));

            dst
        },
        Expr::Binary(op, lhs, rhs) => {
            let lhs = emit_expr(lhs, es, insts);
            let rhs = emit_expr(rhs, es, insts);
            let dst = make_temporary(es);
            insts.push(Instruction::Binary(
                convert_binary(op),
                lhs,
                rhs,
                dst.clone()
            ));

            dst
        },
    }
}

fn make_temporary(es: &mut EmitterState) -> Val {
    let str = format!("tmp.{}", es.tmp_namen);
    es.tmp_namen += 1;

    Val::Var(Rc::new(str))
}

fn convert_unary(op: &ast::UnaryOp) -> UnaryOp {
    match op {
        ast::UnaryOp::Complement => UnaryOp::Complement,
        ast::UnaryOp::Negate => UnaryOp::Negate,
    }
}

fn convert_binary(op: &ast::BinaryOp) -> BinaryOp {
    use ast::BinaryOp as B;
    match op {
        B::Add => BinaryOp::Add,
        B::Subtract => BinaryOp::Subtract,
        B::Multiply => BinaryOp::Multiply,
        B::Divide => BinaryOp::Divide,
        B::Remainder => BinaryOp::Remainder,
        B::BitwiseAnd => BinaryOp::BitwiseAnd,
        B::BitwiseOr => BinaryOp::BitWiseOr,
        B::BitwiseXor => BinaryOp::BitWiseXor,
        B::LeftShift => BinaryOp::LeftShift,
        B::RightShift => BinaryOp::RightShift,
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} (\n", self.function_definition.name)?;
        for inst in &self.function_definition.body {
            write!(f, "\t{}\n", inst)?;
        }
        write!(f, ")")?;
        Ok(())
    }
}

impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::Return(val) => write!(f, "return {}", val),
            Instruction::Unary(op, src, dst) => write!(f, "{} = {}{}", dst, op, src),
            Instruction::Binary(op, lhs, rhs, dst) => write!(f, "{} = {} {} {}", dst, lhs, op, rhs),
        }
    }
}

impl Display for Val {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Val::Constant(i) => write!(f, "{}", i),
            Val::Var(name) => write!(f, "{}", name),
        }
    }
}

impl Display for UnaryOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOp::Complement => write!(f, "~"),
            UnaryOp::Negate => write!(f, "-"),
        }
    }
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryOp::Add => write!(f, "+"),
            BinaryOp::Subtract => write!(f, "-"),
            BinaryOp::Multiply => write!(f, "*"),
            BinaryOp::Divide => write!(f, "/"),
            BinaryOp::Remainder => write!(f, "%"),
            BinaryOp::BitwiseAnd => write!(f, "&"),
            BinaryOp::BitWiseOr => write!(f, "|"),
            BinaryOp::BitWiseXor => write!(f, "^"),
            BinaryOp::LeftShift => write!(f, "<<"),
            BinaryOp::RightShift => write!(f, ">>"),
        }
    }
}