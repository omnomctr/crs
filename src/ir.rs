use std::fmt::{Display, Formatter};
use std::rc::Rc;
use crate::ast;
use crate::ast::{BlockItem, Declaration, Expr, ForInitializer, FunctionDeclaration, IfStatement, Incrementation, Statement, VariableDeclaration};


#[derive(Debug)]
pub struct Program {
    pub function_definitions: Vec<Function>,
}

#[derive(Debug)]
pub struct Function {
    pub name: Identifier,
    pub params: Vec<Identifier>,
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
    Label(Label),
    FunCall(Identifier, Vec<Val>, Val), /* name, args, dst */
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
    let mut state = EmitterState {
        tmp_namen: 0,
        tmp_labeln: 0,
    };

    let mut functions = Vec::with_capacity(prog.functions.len());
    for f in prog.functions {
        functions.push(emit_function(f, &mut state));
    }

    Program {
        function_definitions: functions,
    }
}

fn emit_function(f: ast::FunctionDeclaration, es: &mut EmitterState) -> Function {
    let name = Rc::clone(&f.name);


    let mut insts = Vec::new();
    if let Some(body) = f.body {
        emit_block(body, es, &mut insts);
    }


    // if it's the main function and it doesn't have a return
    // the standard tells us we need to add it automatically
    if name.as_str() == "main" {
        if let Some(Instruction::Return(_)) = insts.last() {}
        else {
            insts.push(Instruction::Return(Val::Constant(0)));
        }
    }

    Function {
        name,
        params: f.params,
        body: insts,
    }
}

fn emit_block(block: ast::Block, state: &mut EmitterState, insts: &mut Vec<Instruction>) {
    for item in block {
        match item {
            BlockItem::S(stmt) => emit_statement(stmt, state, insts),
            BlockItem::D(ast::Declaration::Var (VariableDeclaration{ name, initializer: Some(e) })) => {
                let result = emit_expr(&e, state, insts);
                insts.push(Instruction::Copy(
                    result,
                    Val::Var(Rc::clone(&name))
                ));
            },
            BlockItem::D(ast::Declaration::Var(VariableDeclaration { name: _, initializer: None })) => (),
            BlockItem::D(Declaration::Fun(FunctionDeclaration { body: Some(_), .. })) => panic!(),
            BlockItem::D(Declaration::Fun(_)) => (),
        }
    }

}

fn emit_statement(stmt: ast::Statement, es: &mut EmitterState, insts: &mut Vec<Instruction>) {
    match stmt {
        Statement::Return(expr) => {
            let val = emit_expr(&expr, es, insts);
            insts.push(Instruction::Return(val));
        },
        Statement::Expression(expr) => {
            let _ = emit_expr(&expr, es, insts); /* ignore the result */
        },
        Statement::Empty => (),
        Statement::If(IfStatement{ condition, then, otherwise }) => {
            /*
                tmp.0 = <eval condition>
                jmp if tmp.0 == 0 jmp .Lelse

                <then block>
                jmp .Lifend
             .Lelse:
                <othewise block>

             .Lifend:
                <return>
             */
            let condition = emit_expr(&condition, es, insts);
            let else_lbl = make_temporary_label(es, "elsebrnch");
            let if_end_lbl = make_temporary_label(es, "ifend");

            insts.push(Instruction::JumpZero(
               condition,
               else_lbl.clone(),
            ));

            insts.push(Instruction::Label(make_temporary_label(es, "thenbrnch")));
            emit_block(then, es, insts);

            insts.push(Instruction::Jump(if_end_lbl.clone()));
            insts.push(Instruction::Label(else_lbl.clone()));

            if let Some(block) = otherwise {
                emit_block(block, es, insts);
            }

            insts.push(Instruction::Label(if_end_lbl));
        },
        Statement::LabeledStatement(lbl, rhs) => {
            insts.push(Instruction::Label(Rc::clone(&lbl)));
            emit_statement(*rhs, es, insts);
        }
        Statement::JmpStatement(lbl) => {
            insts.push(Instruction::Jump(Rc::clone(&lbl)));
        },
        Statement::Block(block) => emit_block(block, es, insts),
        Statement::While(_, _, None) => panic!(),
        Statement::While(cond, body, Some(id)) => {
            /*
             .Lloop<id>
                cond = <eval cond>
                if cond == 0 jmp .Lloop<id>end

                <eval body>

                jmp .Lloop<id>
             .Lbreak.<id>

             */
            let loop_label = Rc::new(format!("continue.{}", id));
            let end_label = Rc::new(format!("break.{}", id));

            insts.push(Instruction::Label(loop_label.clone()));

            let cond = emit_expr(&cond, es, insts);

            insts.push(Instruction::JumpZero(
                cond,
                end_label.clone()
            ));

            emit_statement(*body, es, insts);

            insts.push(Instruction::Jump(loop_label));

            insts.push(Instruction::Label(end_label))
        }
        Statement::Break(None) => panic!(),
        Statement::Break(Some(id)) => {
            insts.push(Instruction::Jump(Rc::new(format!("break.{}", id))))
        }
        Statement::Continue(None) => panic!(),
        Statement::Continue(Some(id)) => {
            insts.push(Instruction::Jump(Rc::new(format!("continue.{}", id))))
        }
        Statement::DoWhile(_, _, None) => panic!(),
        Statement::DoWhile(cond, body, Some(id)) => {
            /*
            .Lcontinue.<id>
                <eval body>

                cond = <eval cond>
                if cond == 0 jmp .Lbreak.<id>
                jmp .Lcontinue.<id>
             .Lbreak.<id>

             */

            let continue_label = Rc::new(format!("continue.{}", id));
            let break_label = Rc::new(format!("break.{}", id));

            insts.push(Instruction::Label(continue_label.clone()));

            emit_statement(*body, es, insts);

            let cond = emit_expr(&cond, es, insts);
            insts.push(Instruction::JumpZero(
                cond,
                break_label.clone()
            ));

            insts.push(Instruction::Jump(continue_label));

            insts.push(Instruction::Label(break_label));
        },
        Statement::ForLoop(_, _, _, _, None) => panic!(),
        Statement::ForLoop(expr1, expr2, expr3, body, Some(id)) => {
            /*
                <expr1, if exists>
            .Lforstart.<id>
                cond = <expr2, if exists, otherwise 1>
                if cond == 0 jmp .Lbreak.<id>
                <body>
            .Lcontinue.<id>
                <expr3, if exists>
                jmp .Lforstart.<id>
            .Lbreak.<id>

             */

            let start_label = Rc::new(format!("for_start.{}", id));
            let continue_label = Rc::new(format!("continue.{}", id));
            let break_label = Rc::new(format!("break.{}", id));

            if let Some(expr1) = expr1 {
                match expr1 {
                    ForInitializer::Decl(x) => {
                        // I cant be bothered to write this better

                        emit_block(vec![BlockItem::D(Declaration::Var(x))], es, insts);
                    }
                    ForInitializer::Expr(e) => {
                        emit_expr(&e, es, insts);
                    }
                }
            }

            insts.push(Instruction::Label(start_label.clone()));

            if let Some(expr2) = expr2 {
                let cond = emit_expr(&expr2, es, insts);
                insts.push(Instruction::JumpZero(cond, break_label.clone()))
            }

            emit_statement(*body, es, insts);

            insts.push(Instruction::Label(continue_label));

            if let Some(expr3) = expr3 {
                emit_expr(&expr3, es, insts);
            }

            insts.push(Instruction::Jump(start_label));

            insts.push(Instruction::Label(break_label));
        }
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
        Expr::Ternary(cond, then, otherwise) => {
            /*
                cond = <eval cond>
                jmp if cond == 0 .Lelsebrnch

            .Lthenbrnch
                 tmp.0 = <eval then>
                 jmp .Lend
            .Lelsebrnch
                tmp.0  = <eval otherwise>
            .Lend
                <return tmp.0>
             */
            let result = make_temporary_val(es);
            let then_branch = make_temporary_label(es, "ternarythenbrnch");
            let else_branch = make_temporary_label(es, "ternaryelsebrnch");
            let end = make_temporary_label(es, "ternaryend");

            let cond = emit_expr(cond.as_ref(), es, insts);
            insts.push(Instruction::JumpZero(
                cond,
                else_branch.clone()
            ));

            insts.push(Instruction::Label(
                then_branch
            ));
            let then = emit_expr(then.as_ref(), es, insts);
            insts.push(Instruction::Copy(
                then,
                result.clone()
            ));
            insts.push(Instruction::Jump(end.clone()));

            insts.push(Instruction::Label(
               else_branch.clone()
            ));
            let otherwise = emit_expr(otherwise.as_ref(), es, insts);
            insts.push(Instruction::Copy(
                otherwise,
                result.clone()
            ));

            insts.push(Instruction::Label(
                end
            ));

            result
        }
        Expr::FunCall(ident, args) => {
            /*
                arg1 = <eval arg1>
                arg2 = <eval arg2>
                ...
                argN = <eval argN>

                result = FunCall(ident, [arg1, arg2, ..., argN])
             */

            let result = make_temporary_val(es);
            let mut arg_handles = Vec::with_capacity(args.len());
            for arg in args {
                arg_handles.push(emit_expr(arg, es, insts));
            }

            insts.push(Instruction::FunCall(
                Rc::clone(&ident),
                arg_handles,
                result.clone()
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
        let mut fs = self.function_definitions.iter().peekable();
        while fs.peek().is_some() {
            write!(f, "{}{}", fs.next().unwrap(), if fs.peek().is_some() { ",\n" } else { "" })?;
        }
        Ok(())
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)?;
        write!(f," (")?;
        let mut params_iter = self.params.iter().peekable();
        while params_iter.peek().is_some() {
            write!(f, "{}{}", params_iter.next().unwrap(), if params_iter.peek().is_some() { ", " } else { "" })?;
        }
        write!(f, ") ")?;
        write!(f, "(\n")?;
        for inst in &self.body {
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
            Instruction::FunCall(ident, params, dest) => {
                write!(f, "\t{} = call {}(", dest, ident)?;
                let mut fs = params.iter().peekable();
                while fs.peek().is_some() {
                    write!(f, "{}{}", fs.next().unwrap(), if fs.peek().is_some() { ", " } else { "" })?;
                }
                write!(f, ")")
            }
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