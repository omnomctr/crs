use std::fmt::{Display, Formatter};
use std::fs::File;
use std::io::BufWriter;
use crate::assembly;
use crate::assembly::{BinaryOp, Condition, Instruction, Operand, Register, UnaryOp};
use std::io::Write;
use crate::emit::WordSize::{DWORD, WORD};

pub fn emit(f: File, prog: assembly::Program) -> std::io::Result<()> {
    let mut writer = BufWriter::new(f);

    write!(&mut writer, "\t.globl {}\n", prog.function_definition.name)?;
    write!(&mut writer, "{}:\n", prog.function_definition.name)?;
    write!(&mut writer, "\tpushq  %rbp\n\tmovq   %rsp, %rbp\n")?;

    for inst in prog.function_definition.instructions {
        emit_instruction(&mut writer, inst)?;
        write!(&mut writer, "\n")?;
    }

    write!(&mut writer, ".section .note.GNU-stack,\"\",@progbits\n")?;

    writer.flush()?;
    Ok(())
}

fn emit_instruction(writer: &mut BufWriter<File>, inst: assembly::Instruction) -> std::io::Result<()> {
    match inst {
        Instruction::Mov(src, dest) => {
            write!(writer, "\tmovl   ")?;
            emit_operand(writer, src, DWORD)?;
            write!(writer, ", ")?;
            emit_operand(writer, dest, DWORD)?;
        },
        Instruction::Ret => {
            write!(writer, "\tmovq   %rbp, %rsp\n\t")?;
            write!(writer, "popq   %rbp\n\t")?;
            write!(writer, "ret")?;
        },
        Instruction::Unary(op, operand) => {
            write!(writer, "\t{}   ", match op {
                UnaryOp::Not => "notl",
                UnaryOp::Neg => "negl",
            })?;
            emit_operand(writer, operand, DWORD)?;
        },
        Instruction::AllocateStack(i) => {
            write!(writer, "\tsubq   ${}, %rsp", i)?;
        },
        Instruction::SetCond(cond, op) => {
            write!(writer, "\tset{}   ",
                cond)?;
            emit_operand(writer, op, WORD)?;
        },
        Instruction::Binary(op, rhs, dst) => {
            if op == BinaryOp::LShift  {
                write!(writer, "\tshll   ")?;
                emit_operand(writer, rhs, WORD)?;
                write!(writer, ", ")?;
                emit_operand(writer, dst, DWORD)?;
            } else if op == BinaryOp::RShift {
                write!(writer, "\tshrl   ")?;
                emit_operand(writer, rhs, WORD)?;
                write!(writer, ", ")?;
                emit_operand(writer, dst, DWORD)?;
            } else {
                write!(writer, "\t{}   ", match op {
                    BinaryOp::Add => "addl",
                    BinaryOp::Sub => "subl",
                    BinaryOp::Mult => "imull",
                    BinaryOp::And => "andl",
                    BinaryOp::Or => "orl ",
                    BinaryOp::Xor => "xorl",
                    BinaryOp::LShift | BinaryOp::RShift => panic!(),
                })?;
                emit_operand(writer, rhs, DWORD)?;
                write!(writer, ", ")?;
                emit_operand(writer, dst, DWORD)?;
            }

        },
        Instruction::Idiv(op) => {
            write!(writer, "\tidivl   ")?;
            emit_operand(writer, op, DWORD)?;
        },
        Instruction::Cdq => {
            write!(writer, "\tcdq")?;
        }
        Instruction::Cmp(op1, op2) => {
            write!(writer, "\tcmpl   ")?;
            emit_operand(writer, op1, DWORD)?;
            write!(writer, ", ")?;
            emit_operand(writer, op2, DWORD)?;
        }
        Instruction::Jmp(lbl) => {
            write!(writer, "\tjmp    .L{}", lbl)?;
        }
        Instruction::JmpCond(cond, lbl) => {
            write!(writer, "\tj{}    .L{}", cond, lbl)?;
        }
        Instruction::Label(lbl) => {
            write!(writer, ".L{}:", lbl)?;
        }
    }

    Ok(())
}

fn emit_operand(writer: &mut BufWriter<File>, op: assembly::Operand, word_size: WordSize) -> std::io::Result<()> {
    match op {
        Operand::Imm(i) => write!(writer, "${}", i)?,
        Operand::Reg(r) => emit_register(writer, r, word_size)?,
        Operand::Stack(i) => write!(writer, "-{}(%rbp)", i)?,
        Operand::Pseudo(_) => panic!(),
    }

    Ok(())
}

enum WordSize {
    DWORD, WORD,
}
fn emit_register(writer: &mut BufWriter<File>, reg: assembly::Register, word_size: WordSize) -> std::io::Result<()> {
    match word_size {
        WordSize::DWORD => {
            match reg {
                Register::AX => write!(writer, "%eax"),
                Register::R10 => write!(writer, "%r10d"),
                Register::R11 => write!(writer, "%r11d"),
                Register::DX => write!(writer, "%edx"),
                Register::CX => write!(writer, "%ecx"),
            }
        },
        WordSize::WORD => {
            match reg {
                Register::AX => write!(writer, "%al"),
                Register::R10 => write!(writer, "%r10b"),
                Register::R11 => write!(writer, "%r11b"),
                Register::DX => write!(writer, "%dl"),
                Register::CX => write!(writer, "%cl"),
            }
        }
    }
}

impl Display for Condition {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Condition::Eq => write!(f, "e"),
            Condition::NEq => write!(f, "ne"),
            Condition::GT => write!(f, "g"),
            Condition::GTE => write!(f, "ge"),
            Condition::LT => write!(f, "l"),
            Condition::LTE => write!(f, "le"),
        }
    }
}