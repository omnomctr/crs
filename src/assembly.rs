use std::rc::Rc;
use crate::ast;
use crate::ast::Expr;

#[derive(Debug)]
pub struct Program {
    pub function_definition: FunctionDefinition,
}

#[derive(Debug)]
pub struct FunctionDefinition {
    pub name: Rc<String>,
    pub instructions: Vec<Instruction>
}

#[derive(Debug)]
pub enum Instruction {
    Mov(Operand, Operand),
    Ret,
}

#[derive (Debug)]
pub enum Operand {
    Imm(i32),
    Reg(Register)
}

#[derive(Debug)]
pub enum Register {
    EAX,
}

pub fn to_assembly_program(prog: ast::Program) -> Program {
    let name = Rc::clone(&prog.f.name);
    let mut instructions = Vec::new();

    generate_instructions(prog.f.stmt, &mut instructions);

    Program {
        function_definition: FunctionDefinition {
            name,
            instructions
        }
    }
}

fn generate_instructions(stmt: ast::Statement, out: &mut Vec<Instruction>) {
    use ast::Statement as s;
    match stmt {
        s::Return(expr) => {
            let constant = if let Expr::Constant(i) = expr {
                i
            } else {
                panic!();
            };

            out.push(Instruction::Mov(
                Operand::Imm(constant),
                Operand::Reg(Register::EAX)
            ));
            out.push(Instruction::Ret);
        }
    }
}