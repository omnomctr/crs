use std::rc::Rc;
use crate::ast;
use crate::ast::{Expr, Statement};

/* intermediate representation
 * page 36 of the book
 */

#[derive(Debug)]
pub struct Program {
    function_definition: Function,
}

#[derive(Debug)]
pub struct Function {
    name: Identifier,
    body: Vec<Instruction>,
}

#[derive(Debug)]
pub enum Instruction {
    Return(Val),
    Unary(UnaryOp, Val, Val), /* op, src, dst */
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