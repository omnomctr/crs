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
    Cmp(Operand, Operand), /* lhs, rhs */
    Jmp(Label),
    JmpCond(Condition, Label),
    SetCond(Condition, Operand),
    Label(Label),
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
type Label = Rc<String>;

#[derive(Debug)]
pub enum UnaryOp {
    Neg,
    Not,
}

#[derive(Debug)]
pub enum Condition {
    Eq, NEq, GT, GTE, LT, LTE
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
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
                I::Unary(ir::UnaryOp::Not, src, dst) => {
                    instructions.push(Instruction::Cmp(
                        Operand::Imm(0),
                        convert_val(src),
                    ));
                    instructions.push(Instruction::Mov(
                        Operand::Imm(0),
                        convert_val(dst),
                    ));
                    instructions.push(Instruction::SetCond(
                        Condition::Eq,
                        convert_val(dst),
                    ));
                },
                I::Unary(ir::UnaryOp::Negate, src, dst) => {
                    instructions.push(Instruction::Mov(
                        convert_val(src),
                        convert_val(dst)
                    ));
                    instructions.push(Instruction::Unary(
                        UnaryOp::Neg,
                        convert_val(dst),
                    ))
                },
                I::Unary(ir::UnaryOp::Complement, src, dst) => {
                    instructions.push(Instruction::Mov(
                        convert_val(src),
                        convert_val(dst)
                    ));
                    instructions.push(Instruction::Unary(
                        UnaryOp::Not,
                        convert_val(dst),
                    ))
                },
                I::Binary(ir::BinaryOp::Divide, lhs, rhs, dst) => {
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
                },
                I::Binary(ir::BinaryOp::Remainder, lhs, rhs, dst) => {
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
                },
                I::Binary(op @ ir::BinaryOp::BitWiseOr
                        | op @ ir::BinaryOp::BitwiseAnd
                        | op @ ir::BinaryOp::BitWiseXor, lhs, rhs, dst) => {
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
                },
                I::Binary(op @ ir::BinaryOp::LeftShift
                        | op @ ir::BinaryOp::RightShift, lhs, rhs, dst) => {
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
                },
                I::Binary(ir::BinaryOp::Equal, lhs, rhs, dst) => {
                    instructions.push(Instruction::Mov(
                        Operand::Imm(0),
                        convert_val(dst),
                    ));
                    instructions.push(Instruction::Cmp(
                        convert_val(lhs),
                        convert_val(rhs),
                    ));
                    instructions.push(Instruction::SetCond(
                        Condition::Eq,
                        convert_val(dst),
                    ));
                },
                I::Binary(ir::BinaryOp::NotEqual, lhs, rhs, dst) => {
                    instructions.push(Instruction::Mov(
                        Operand::Imm(0),
                        convert_val(dst),
                    ));
                    instructions.push(Instruction::Cmp(
                        convert_val(lhs),
                        convert_val(rhs),
                    ));
                    instructions.push(Instruction::SetCond(
                        Condition::NEq,
                        convert_val(dst),
                    ));
                },
                I::Binary(ir::BinaryOp::LessThan, lhs, rhs, dst) => {
                    instructions.push(Instruction::Mov(
                        Operand::Imm(0),
                        convert_val(dst)
                    ));
                    instructions.push(Instruction::Cmp(
                        convert_val(rhs),
                        convert_val(lhs),
                    ));
                    instructions.push(Instruction::SetCond(
                        Condition::LT,
                        convert_val(dst),
                    ));
                },
                I::Binary(ir::BinaryOp::GreaterThan, lhs, rhs, dst) => {
                    instructions.push(Instruction::Mov(
                        Operand::Imm(0),
                        convert_val(dst)
                    ));
                    instructions.push(Instruction::Cmp(
                        convert_val(rhs),
                        convert_val(lhs),
                    ));
                    instructions.push(Instruction::SetCond(
                        Condition::GT,
                        convert_val(dst),
                    ));
                },
                I::Binary(ir::BinaryOp::LessOrEqual, lhs, rhs, dst) => {
                    instructions.push(Instruction::Mov(
                        Operand::Imm(0),
                        convert_val(dst)
                    ));
                    instructions.push(Instruction::Cmp(
                        convert_val(rhs),
                        convert_val(lhs),
                    ));
                    instructions.push(Instruction::SetCond(
                        Condition::LTE,
                        convert_val(dst),
                    ));
                },
                I::Binary(ir::BinaryOp::GreaterOrEqual, lhs, rhs, dst) => {
                    instructions.push(Instruction::Mov(
                        Operand::Imm(0),
                        convert_val(dst)
                    ));
                    instructions.push(Instruction::Cmp(
                        convert_val(rhs),
                        convert_val(lhs),
                    ));
                    instructions.push(Instruction::SetCond(
                        Condition::GTE,
                        convert_val(dst),
                    ));
                },
                I::Binary(ir::BinaryOp::Add, lhs, rhs, dst) => {
                    // dest <- dest + src;
                    instructions.push(Instruction::Mov(
                        convert_val(lhs),
                        convert_val(dst),
                    ));
                    instructions.push(Instruction::Binary(
                        BinaryOp::Add,
                        convert_val(rhs),
                        convert_val(dst),
                    ));
                },
                I::Binary(ir::BinaryOp::Subtract, lhs, rhs, dst) => {
                    // dest <- dest - src;
                    instructions.push(Instruction::Mov(
                        convert_val(lhs),
                        convert_val(dst),
                    ));
                    instructions.push(Instruction::Binary(
                        BinaryOp::Sub,
                        convert_val(rhs),
                        convert_val(dst),
                    ));
                },
                I::Binary(ir::BinaryOp::Multiply, lhs, rhs, dst) => {
                    // dest <- dest + src;
                    instructions.push(Instruction::Mov(
                        convert_val(lhs),
                        convert_val(dst),
                    ));
                    instructions.push(Instruction::Binary(
                        BinaryOp::Mult,
                        convert_val(rhs),
                        convert_val(dst),
                    ));
                },
                I::Jump(target) => {
                    instructions.push(Instruction::Jmp(Rc::clone(&target)));
                },
                I::Label(lbl) => {
                    instructions.push(Instruction::Label(Rc::clone(&lbl)));
                },
                I::JumpZero(cond, target) => {
                    instructions.push(Instruction::Cmp(
                        Operand::Imm(0),
                        convert_val(cond),
                    ));
                    instructions.push(Instruction::JmpCond(
                       Condition::Eq,
                       Rc::clone(&target)
                    ));
                },
                I::JumpNotZero(cond, target) => {
                    instructions.push(Instruction::Cmp(
                        Operand::Imm(0),
                        convert_val(cond),
                    ));
                    instructions.push(Instruction::JmpCond(
                        Condition::NEq,
                        Rc::clone(&target)
                    ));
                },
                I::Copy(src, dst) => {
                    instructions.push(Instruction::Mov(
                        convert_val(src),
                        convert_val(dst),
                    ));
                },
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
                Instruction::Cmp(lhs, rhs) => {
                    convert_pseudoregister(lhs, &mut pr_map, &mut stack_offset);
                    convert_pseudoregister(rhs, &mut pr_map, &mut stack_offset);
                }
                Instruction::Jmp(_) => {}
                Instruction::JmpCond(_, _) => {}
                Instruction::SetCond(_, val) => {
                    convert_pseudoregister(val, &mut pr_map, &mut stack_offset);
                }
                Instruction::Label(_) => {}
            }
        }
    }
    // third pass - add AllocateStack instruction, fix various x86 instruction restraints
    let instructions = {
        let mut instructions_ = Vec::with_capacity(instructions.capacity() + 1);
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
                        Operand::Reg(Register::CX),
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
                Instruction::Cmp(lhs @ Operand::Stack(_), rhs @ Operand::Stack(_)) => {
                    /* only one operand can be on the stack */
                    instructions_.push(Instruction::Mov(
                        lhs,
                        Operand::Reg(Register::R10),
                    ));
                    instructions_.push(Instruction::Cmp(
                        Operand::Reg(Register::R10),
                        rhs
                    ));
                },
                Instruction::Cmp(lhs, rhs @ Operand::Imm(_)) => {
                    instructions_.push(Instruction::Mov(
                        rhs,
                        Operand::Reg(Register::R11),
                    ));
                    instructions_.push(Instruction::Cmp(
                        lhs,
                        Operand::Reg(Register::R11),
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

