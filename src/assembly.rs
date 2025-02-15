use std::collections::HashMap;
use std::rc::Rc;
use crate::ir;
use crate::ir::Val;

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
    Mov(Operand, Operand), /* src, dst */
    Unary(UnaryOp, Operand),
    Binary(BinaryOp, Operand, Operand), /* op, rhs, dst */
    Idiv(Operand),
    Cdq,
    AllocateStack(usize),
    Ret,
}

#[derive(Debug)]
pub enum Operand {
    Imm(i32),
    Reg(Register),
    Pseudo(Identifier),
    Stack(usize),
}

type Identifier = Rc<String>;

#[derive(Debug)]
pub enum UnaryOp {
    Neg,
    Not,
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOp {
    Add, Sub, Mult
}

#[derive(Debug)]
pub enum Register {
    AX,
    R10,
    R11,
    DX,
}

pub fn to_assembly_program(prog: ir::Program) -> Program {
    let name = Rc::clone(&prog.function_definition.name);
    let mut instructions = Vec::new();

    // first pass
    {
        generate_instructions(&prog.function_definition.body, &mut instructions);
    }
    // second pass
    // replace pseudoregisters with stack operands
    let mut stack_offset = 0;
    {
        let mut pr_map = HashMap::new();

        for inst in &mut instructions {
            match inst {
                Instruction::Mov(src, dst) => {
                    convert_pseudoregister(src, &mut pr_map, &mut stack_offset);
                    convert_pseudoregister(dst, &mut pr_map, &mut stack_offset);
                },
                Instruction::Unary(_, op) => {
                    convert_pseudoregister(op, &mut pr_map, &mut stack_offset);
                }
                Instruction::AllocateStack(_) => {}
                Instruction::Ret => {}
                Instruction::Binary(_, rhs, dst) => {
                    convert_pseudoregister(rhs, &mut pr_map, &mut stack_offset);
                    convert_pseudoregister(dst, &mut pr_map, &mut stack_offset);
                }

                Instruction::Idiv(v) => {
                    convert_pseudoregister(v, &mut pr_map, &mut stack_offset);
                }
                Instruction::Cdq => {}
            }
        }
    }
    // third pass - add AllocateStack instruction, fix various x86 instruction restraints
    let instructions = {
        let mut instructions_ = Vec::with_capacity(instructions.len() + 1);
        instructions_.push(Instruction::AllocateStack(stack_offset));

        for inst in instructions {
            match inst {
                Instruction::Mov(Operand::Stack(src), Operand::Stack(dst)) => {
                    // only one mov argument can be a memory address
                    instructions_.push(Instruction::Mov(
                        Operand::Stack(src),
                        Operand::Reg(Register::R10),
                    ));
                    instructions_.push(Instruction::Mov(
                        Operand::Reg(Register::R10),
                        Operand::Stack(dst)
                    ));
                },
                Instruction::Idiv(Operand::Imm(i)) => {
                    // if the idiv arg is a constant (it doesnt like that)
                    instructions_.push(Instruction::Mov(
                        Operand::Imm(i),
                        Operand::Reg(Register::R10),
                    ));
                    instructions_.push(Instruction::Idiv(
                        Operand::Reg(Register::R10)
                    ));
                },
                Instruction::Binary(binop @ BinaryOp::Add | binop @ BinaryOp::Sub, Operand::Stack(lhs), Operand::Stack(rhs)) => {
                    // Add and Sub instructions cant use memory addresses as source and destination
                    instructions_.push(Instruction::Mov(
                        Operand::Stack(lhs),
                        Operand::Reg(Register::R10)
                    ));
                    instructions_.push(Instruction::Binary(
                        binop,
                        Operand::Reg(Register::R10),
                        Operand::Stack(rhs)
                    ));
                },
                Instruction::Binary(BinaryOp::Mult, lhs, Operand::Stack(rhs)) => {
                    instructions_.push(Instruction::Mov(
                        Operand::Stack(rhs),
                        Operand::Reg(Register::R11),
                    ));
                    instructions_.push(Instruction::Binary(
                        BinaryOp::Mult,
                        lhs,
                        Operand::Reg(Register::R11),
                    ));
                    instructions_.push(Instruction::Mov(
                        Operand::Reg(Register::R11),
                        Operand::Stack(rhs),
                    ));
                },
                _ => instructions_.push(inst),
            }
        }

        instructions_
    };

    Program {
        function_definition: FunctionDefinition {
            name,
            instructions
        }
    }
}

fn generate_instructions(body: &Vec<ir::Instruction>, out: &mut Vec<Instruction>) {
    for inst in body {
        use ir::Instruction as I;
        match inst {
            I::Return(val) => {
                out.push(Instruction::Mov(
                    convert_val(val),
                    Operand::Reg(Register::AX)
                ));
                out.push(Instruction::Ret);
            },
            I::Unary(op, src, dst) => {
                out.push(Instruction::Mov(
                    convert_val(src),
                    convert_val(dst)
                ));
                out.push(Instruction::Unary(
                    convert_unary_op(op),
                    convert_val(dst),
                ))
            }
            I::Binary(op, lhs, rhs, dst) => {
                if let ir::BinaryOp::Divide = op {
                    out.push(Instruction::Mov(
                        convert_val(lhs),
                        Operand::Reg(Register::AX)
                    ));
                    out.push(Instruction::Cdq);
                    out.push(Instruction::Idiv(
                        convert_val(rhs)
                    ));
                    out.push(Instruction::Mov(
                       Operand::Reg(Register::AX),
                       convert_val(dst),
                    ));
                } else if let ir::BinaryOp::Remainder = op {
                    out.push(Instruction::Mov(
                        convert_val(lhs),
                        Operand::Reg(Register::AX)
                    ));
                    out.push(Instruction::Cdq);
                    out.push(Instruction::Idiv(
                        convert_val(rhs)
                    ));
                    out.push(Instruction::Mov(
                        Operand::Reg(Register::DX),
                        convert_val(dst)
                    ));
                } else {
                    out.push(Instruction::Mov(
                        convert_val(lhs),
                        convert_val(dst),
                    ));
                    use ir::BinaryOp as B;
                    let op_ = match op {
                        B::Add => BinaryOp::Add,
                        B::Subtract => BinaryOp::Sub,
                        B::Multiply => BinaryOp::Mult,
                        B::Divide | B::Remainder => panic!()
                    };
                    out.push(Instruction::Binary(
                        op_,
                        convert_val(rhs),
                        convert_val(dst),
                    ));
                }
            }
        }
    }
}

fn convert_val(val: &ir::Val) -> Operand {
    match val {
        Val::Constant(c) => Operand::Imm(*c),
        Val::Var(s) => Operand::Pseudo(Rc::clone(&s)),
    }
}

fn convert_unary_op(op: &ir::UnaryOp) -> UnaryOp {
    match op {
        ir::UnaryOp::Complement => UnaryOp::Not,
        ir::UnaryOp::Negate => UnaryOp::Neg,
    }
}

fn convert_pseudoregister(op: &mut Operand, pr_map: &mut HashMap<Rc<String>, usize>, stack_offset: &mut usize) {
    if let Operand::Pseudo(s) = op {
        *op = Operand::Stack(*pr_map
                                .entry(Rc::clone(s))
                                .or_insert_with(|| {
                                    *stack_offset += 4;
                                    return *stack_offset;
                                }));
    }
}