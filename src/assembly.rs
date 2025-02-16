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
    Add, Sub, Mult, And, Or, Xor, LShift, RShift,
}

#[derive(Debug)]
pub enum Register {
    AX,
    R10,
    R11,
    DX,
    CX,
    CL,
}

pub fn to_assembly_program(prog: ir::Program) -> Program {
    let name = Rc::clone(&prog.function_definition.name);
    let mut instructions = Vec::new();

    // first pass
    {
        for inst in &prog.function_definition.body {
            use ir::Instruction as I;
            match inst {
                I::Return(val) => {
                    instructions.push(Instruction::Mov(
                        convert_val(val),
                        Operand::Reg(Register::AX)
                    ));
                    instructions.push(Instruction::Ret);
                },
                I::Unary(op, src, dst) => {
                    instructions.push(Instruction::Mov(
                        convert_val(src),
                        convert_val(dst)
                    ));
                    instructions.push(Instruction::Unary(
                        convert_unary_op(op),
                        convert_val(dst),
                    ))
                }
                I::Binary(op, lhs, rhs, dst) => {
                    if let ir::BinaryOp::Divide = op {
                        instructions.push(Instruction::Mov(
                            convert_val(lhs),
                            Operand::Reg(Register::AX)
                        ));
                        instructions.push(Instruction::Cdq);
                        instructions.push(Instruction::Idiv(
                            convert_val(rhs)
                        ));
                        instructions.push(Instruction::Mov(
                            Operand::Reg(Register::AX),
                            convert_val(dst),
                        ));
                    } else if let ir::BinaryOp::Remainder = op {
                        instructions.push(Instruction::Mov(
                            convert_val(lhs),
                            Operand::Reg(Register::AX)
                        ));
                        instructions.push(Instruction::Cdq);
                        instructions.push(Instruction::Idiv(
                            convert_val(rhs)
                        ));
                        instructions.push(Instruction::Mov(
                            Operand::Reg(Register::DX),
                            convert_val(dst)
                        ));
                    } else if let ir::BinaryOp::BitwiseAnd | ir::BinaryOp::BitWiseOr | ir::BinaryOp::BitWiseXor = op {
                        // dest <- dest & src
                        instructions.push(Instruction::Mov(
                            convert_val(rhs),
                            convert_val(dst),
                        ));
                        instructions.push(Instruction::Binary(
                            match op {
                                ir::BinaryOp::BitWiseOr => BinaryOp::Or,
                                ir::BinaryOp::BitwiseAnd => BinaryOp::And,
                                ir::BinaryOp::BitWiseXor => BinaryOp::Xor,
                                _ => panic!(),
                            },
                            convert_val(lhs),
                            convert_val(dst),
                        ));
                    } else if let ir::BinaryOp::LeftShift | ir::BinaryOp::RightShift = op {
                        // dest <- dest << / >> rhs
                        instructions.push(Instruction::Mov(
                            convert_val(lhs),
                            convert_val(dst),
                        ));
                        instructions.push(Instruction::Binary(
                            match op {
                                ir::BinaryOp::LeftShift => BinaryOp::LShift,
                                ir::BinaryOp::RightShift => BinaryOp::RShift,
                                _ => panic!()
                            },
                            convert_val(rhs),
                            convert_val(dst),
                        ));
                    } else {
                        // dest <- dest + src;
                        instructions.push(Instruction::Mov(
                            convert_val(lhs),
                            convert_val(dst),
                        ));
                        use ir::BinaryOp as B;
                        let op_ = match op {
                            B::Add => BinaryOp::Add,
                            B::Subtract => BinaryOp::Sub,
                            B::Multiply => BinaryOp::Mult,
                            B::Divide | B::Remainder | B::BitwiseAnd | B::BitWiseOr
                            | B::BitWiseXor | B::RightShift | B::LeftShift => panic!()
                        };
                        instructions.push(Instruction::Binary(
                            op_,
                            convert_val(rhs),
                            convert_val(dst),
                        ));
                    }
                }
            }
        }
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
                    // idiv argument cant be an immediate value
                    instructions_.push(Instruction::Mov(
                        Operand::Imm(i),
                        Operand::Reg(Register::R10),
                    ));
                    instructions_.push(Instruction::Idiv(
                        Operand::Reg(Register::R10)
                    ));
                },
                Instruction::Binary(binop @ BinaryOp::Add | binop @ BinaryOp::Sub, Operand::Stack(lhs), Operand::Stack(rhs)) => {
                    // Add and Sub instructions cant use memory addresses as both source and destination
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
                Instruction::Binary(op @ BinaryOp::And | op @ BinaryOp::Or | op @ BinaryOp::Xor, lhs, Operand::Stack(rhs)) => {
                    /* dest must be a register */
                    instructions_.push(Instruction::Mov(
                        Operand::Stack(rhs),
                        Operand::Reg(Register::R10),
                    ));
                    instructions_.push(Instruction::Binary(
                        op,
                        lhs,
                        Operand::Reg(Register::R10),
                    ));
                    instructions_.push(Instruction::Mov(
                        Operand::Reg(Register::R10),
                        Operand::Stack(rhs),
                    ));
                },
                Instruction::Binary(op @ BinaryOp::LShift | op @ BinaryOp::RShift, Operand::Stack(rhs), dst) => {
                    /* pg 1828 of intel manual "The destination operand can be a
                     * register or a memory location. The count operand can be an immediate value or
                     * the CL register"
                     */
                    // CL is the lowest 8 bits of CX
                    instructions_.push(Instruction::Mov(
                        Operand::Stack(rhs),
                        Operand::Reg(Register::CX),
                    ));
                    instructions_.push(Instruction::Binary(
                        op,
                        Operand::Reg(Register::CL),
                        dst,
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