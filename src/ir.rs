use std::fmt::{Display, Formatter};
use std::rc::Rc;
use crate::ast;
use crate::ast::{BlockItem, Expr, Incrementation, Statement};


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
    Copy(Val, Val), /* src, dst */
    Jump(Label), /* target */
    JumpZero(Val, Label), /* condition, target */
    JumpNotZero(Val, Label), /* condition, target */
    Label(Label)
}

#[derive(Debug, Clone)]
pub enum Val {
    Constant(i32),
    Var(Identifier)
}

#[derive(Debug)]
pub enum UnaryOp {
    Complement,
    Negate,
    Not
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
    Equal,
    NotEqual,
    LessThan,
    LessOrEqual,
    GreaterThan,
    GreaterOrEqual,
}

struct EmitterState {
    tmp_namen: usize,
    tmp_labeln: usize,
}

type Identifier = Rc<String>;
type Label = Rc<String>;

pub fn emit_ir(prog: ast::Program) -> Program {
    let name = Rc::clone(&prog.f.name);
    let mut state = EmitterState {
        tmp_namen: 0,
        tmp_labeln: 0,
    };

    let mut insts = Vec::new();
    for item in prog.f.body {
        match item {
            BlockItem::S(stmt) => emit_statement(stmt, &mut state, &mut insts),
            BlockItem::D(ast::Declaration { name, initializer: Some(e) }) => {
                let result = emit_expr(&e, &mut state, &mut insts);
                insts.push(Instruction::Copy(
                    result,
                    Val::Var(Rc::clone(&name))
                ));
            },
            _ => (),
        }
    }

    // if it's the main function and it doesn't have a return
    // the standard tells us we need to add it automatically
    if name.as_str() == "main" {
        if let Some(Instruction::Return(_)) = insts.last() {}
        else {
            insts.push(Instruction::Return(Val::Constant(0)));
        }
    }


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
        Statement::Expression(expr) => {
            let _ = emit_expr(&expr, es, insts); /* ignore the result */
        }
        Statement::Empty => (),
    }
}

fn emit_expr(expr: &ast::Expr, es: &mut EmitterState, insts: &mut Vec<Instruction>) -> Val {
    match expr {
        Expr::Constant(c) => Val::Constant(*c),
        Expr::Unary(op, rhs) => {
            let src = emit_expr(rhs, es, insts);
            let dst = make_temporary_val(es);
            insts.push(Instruction::Unary(
                convert_unary(op),
                src,
                dst.clone()
            ));

            dst
        },
        Expr::Binary(ast::BinaryOp::And, lhs, rhs) => {
            /*
             implementing short circuiting

                lhs = <eval lhs>
                jmp if lhs == 0 fbranch
                rhs = <eval rhs>
                jmp if rhs == 0 fbranch

                result = 1 ; true

                jmp end

            fbranch:
                result = 0

            end:
                <end>, returning result

             */
            let result = make_temporary_val(es);
            let lhs = emit_expr(lhs, es, insts);
            let false_branch = make_temporary_label(es, "fbranch");
            let end_branch = make_temporary_label(es, "end");
            insts.push(Instruction::JumpZero(
                lhs, Rc::clone(&false_branch)
            ));
            let rhs = emit_expr(rhs, es, insts);
            insts.push(Instruction::JumpZero(
                rhs, Rc::clone(&false_branch)
            ));

            insts.push(Instruction::Copy(
                Val::Constant(1), result.clone(),
            ));

            insts.push(Instruction::Jump(
                Rc::clone(&end_branch),
            ));

            insts.push(Instruction::Label(
                Rc::clone(&false_branch),
            ));

            insts.push(Instruction::Copy(
                Val::Constant(0), result.clone()
            ));

            insts.push(Instruction::Label(
                Rc::clone(&end_branch),
            ));

            result
        },
        Expr::Binary(ast::BinaryOp::Or, lhs, rhs) => {
            /*
                lhs = <eval lhs>
                if lhs != 0 jmp true_branch
                rhs = <eval rhs>
                if rhs != 0 jmp true_branch
                result = 0
                jmp end

            true_branch:
                result = 1

            end:
                <end>, returning result

             */
            let result = make_temporary_val(es);
            let end = make_temporary_label(es, "end");
            let true_branch = make_temporary_label(es, "tbranch");

            let lhs = emit_expr(lhs, es, insts);
            insts.push(Instruction::JumpNotZero(
                lhs, Rc::clone(&true_branch),
            ));

            let rhs = emit_expr(rhs, es, insts);
            insts.push(Instruction::JumpNotZero(
                rhs, Rc::clone(&true_branch),
            ));

            insts.push(Instruction::Copy(
                Val::Constant(0), result.clone(),
            ));

            insts.push(Instruction::Jump(
                Rc::clone(&end)
            ));

            insts.push(Instruction::Label(
                Rc::clone(&true_branch)
            ));

            insts.push(Instruction::Copy(
                Val::Constant(1), result.clone(),
            ));

            insts.push(Instruction::Label(
                Rc::clone(&end),
            ));

            result
        },
        Expr::Binary(op, lhs, rhs) => {
            let lhs = emit_expr(lhs, es, insts);
            let rhs = emit_expr(rhs, es, insts);
            let dst = make_temporary_val(es);
            insts.push(Instruction::Binary(
                convert_binary(op),
                lhs,
                rhs,
                dst.clone()
            ));

            dst
        },
        Expr::CompoundAssignment(op, var, rhs) => {
            let v = if let Expr::Var(x) = var.as_ref() {
                x
            } else {
                panic!()
            };

            let rhs = emit_expr(rhs.as_ref(), es, insts);

            insts.push(Instruction::Binary(
                convert_binary(op),
                Val::Var(Rc::clone(&v)),
                rhs,
                Val::Var(Rc::clone(&v)),
            ));

            Val::Var(Rc::clone(&v))
        }
        Expr::Var(v) => Val::Var(Rc::clone(v)),
        Expr::Assignment(v, rhs) => {
            let result = emit_expr(&**rhs, es, insts);
            let v = if let ast::Expr::Var(x) = v.as_ref() {
                x
            } else {
                panic!()
            };
            insts.push(Instruction::Copy(
                result,
                Val::Var(Rc::clone(&v))
            ));

            Val::Var(Rc::clone(&v))
        },

        Expr::PrefixInc(incrementation, var) => {
            /*
                tmp.0 = var + 1
                var = tmp.0
                <return tmp.0>
             */
            let result = make_temporary_val(es);
            let var = if let Expr::Var(ident) = var.as_ref() {
                ident
            } else {
                panic!()
            };

            insts.push(Instruction::Binary(
                match incrementation {
                    Incrementation::Increment => BinaryOp::Add,
                    Incrementation::Decrement => BinaryOp::Subtract,
                },
                Val::Var(Rc::clone(&var)),
                Val::Constant(1),
                result.clone(),
            ));

            insts.push(Instruction::Copy(
                result.clone(),
                Val::Var(Rc::clone(&var)),
            ));

            result
        },
        Expr::PostfixInc(incrementation, var) => {
            /*
                tmp.0 = var
                var = var + 1
                <return tmp.0>
             */
            let result = make_temporary_val(es);
            let var = if let Expr::Var(ident) = var.as_ref() {
                ident
            } else {
                panic!()
            };
            insts.push(Instruction::Copy(
                Val::Var(Rc::clone(&var)),
                result.clone(),
            ));
            insts.push(Instruction::Binary(
               match incrementation {
                   Incrementation::Increment => BinaryOp::Add,
                   Incrementation::Decrement => BinaryOp::Subtract,
               },
               Val::Var(Rc::clone(&var)),
               Val::Constant(1),
               Val::Var(Rc::clone(&var)),
            ));

            result
        }
    }
}

fn make_temporary_val(es: &mut EmitterState) -> Val {
    let str = format!("tmp.{}", es.tmp_namen);
    es.tmp_namen += 1;

    Val::Var(Rc::new(str))
}

fn make_temporary_label(es: &mut EmitterState, hint: &str) -> Label {
    let str = format!("_{}_{}", hint, es.tmp_labeln);
    es.tmp_labeln += 1;

    Rc::new(str)
}

fn convert_unary(op: &ast::UnaryOp) -> UnaryOp {
    match op {
        ast::UnaryOp::Complement => UnaryOp::Complement,
        ast::UnaryOp::Negate => UnaryOp::Negate,
        ast::UnaryOp::Not => UnaryOp::Not,
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
        B::LTE => BinaryOp::LessOrEqual,
        B::LT => BinaryOp::LessThan,
        B::GTE => BinaryOp::GreaterOrEqual,
        B::GT => BinaryOp::GreaterThan,
        B::NEq => BinaryOp::NotEqual,
        B::Eq => BinaryOp::Equal,
        B::And | B::Or => panic!("and and or can't be used here"),
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} (\n", self.function_definition.name)?;
        for inst in &self.function_definition.body {
            write!(f, "{}\n", inst)?;
        }
        write!(f, ")")?;
        Ok(())
    }
}

impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::Return(val) => write!(f, "\treturn {}", val),
            Instruction::Unary(op, src, dst) => write!(f, "\t{} = {}{}", dst, op, src),
            Instruction::Binary(op, lhs, rhs, dst) => write!(f, "\t{} = {} {} {}", dst, lhs, op, rhs),
            Instruction::Copy(src, dst) => write!(f, "\t{} = {}", dst, src),
            Instruction::Jump(target) => write!(f, "\tjmp {}", target),
            Instruction::JumpZero(condition, target) => write!(f, "\tif {} = 0 jmp {}", condition, target),
            Instruction::JumpNotZero(condition, target) => write!(f, "\tif {} != 0 jmp {}", condition, target),
            Instruction::Label(ident) => write!(f, "{}:", ident),
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
            UnaryOp::Not => write!(f, "!"),
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
            BinaryOp::Equal => write!(f, "=="),
            BinaryOp::NotEqual => write!(f, "!="),
            BinaryOp::LessThan => write!(f, "<"),
            BinaryOp::LessOrEqual => write!(f, "<="),
            BinaryOp::GreaterThan => write!(f, ">"),
            BinaryOp::GreaterOrEqual => write!(f, ">="),
        }
    }
}