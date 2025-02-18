use std::collections::HashMap;
use std::rc::Rc;
use crate::ast;
use crate::ast::{BlockItem, IfStatement, Statement};

struct AnalysisState {
    variable_map: HashMap<ast::Identifier, ast::Identifier>,
    label_map: HashMap<ast::Identifier, ast::Identifier>,
    temp_var_increment: usize,
    temp_label_increment: usize,
}

#[derive(Debug)]
pub struct SemanticAnalysisError {
    #[allow(dead_code)]
    reason: SemanticAnalysisErrorKind
}

#[allow(dead_code)]
#[derive(Debug)]
pub enum SemanticAnalysisErrorKind {
    DuplicateVariableDecl(ast::Identifier),
    UndeclaredVariable(ast::Identifier),
    InvalidLvalue(ast::Expr),
    DuplicateLabelDecl(ast::Identifier),
    UndeclaredLabel(ast::Identifier),
}

type AnalysisResult<T> = Result<T, SemanticAnalysisError>;

pub fn analyse(ast: ast::Program) -> AnalysisResult<ast::Program> {
    let mut state = AnalysisState {
        variable_map: HashMap::new(),
        label_map: HashMap::new(),
        temp_var_increment: 0,
        temp_label_increment: 0,
    };

    let body = state.resolve_block(ast.f.body)?;
    let body = state.resolve_goto(body)?;

    Ok(ast::Program {
        f: ast::Function {
            name: ast.f.name,
            body,
        }
    })
}

impl AnalysisState {
    fn resolve_block(&mut self, block: ast::Block) -> AnalysisResult<ast::Block> {
        let mut body = Vec::with_capacity(block.len());
        for block_item in block {
            use ast::BlockItem as BlockItem;
            body.push(
                match block_item {
                    BlockItem::S(s) => BlockItem::S(self.resolve_statement(s)?),
                    BlockItem::D(d) => BlockItem::D(self.resolve_declaration(d)?),
                }
            )
        }

        Ok(body)
    }
    fn resolve_declaration(&mut self, decl: ast::Declaration) -> AnalysisResult<ast::Declaration> {
        if self.variable_map.contains_key(&decl.name) {
            return Err(SemanticAnalysisError {
                reason: SemanticAnalysisErrorKind::DuplicateVariableDecl(Rc::clone(&decl.name)),
            })
        }

        let name = self.make_temporary_var(&decl.name);
        self.variable_map.insert(Rc::clone(&decl.name), Rc::clone(&name));
        let new_initializer = decl.initializer.map(|x| self.resolve_expr(x)).transpose()?;

        Ok(ast::Declaration {
            name,
            initializer: new_initializer,
        })
    }

    fn resolve_statement(&mut self, stmt: ast::Statement) -> AnalysisResult<ast::Statement> {
        use ast::Statement as Statement;
        Ok(match stmt {
            Statement::Return(e) => Statement::Return(self.resolve_expr(e)?),
            Statement::Expression(e) => Statement::Expression(self.resolve_expr(e)?),
            Statement::Empty => Statement::Empty,
            Statement::LabeledStatement(lbl, rhs) => {
                if self.label_map.contains_key(&lbl) {
                    return Err(SemanticAnalysisError {
                        reason: SemanticAnalysisErrorKind::DuplicateLabelDecl(lbl)
                    });
                }

                let name = self.make_temporary_label(&lbl);
                self.label_map.insert(Rc::clone(&lbl), Rc::clone(&name));

                Statement::LabeledStatement(
                    name,
                    Box::new(self.resolve_statement(*rhs)?)
                )
            },
            Statement::If(IfStatement { condition, then, otherwise }) => {
                Statement::If(
                    IfStatement{
                        condition: self.resolve_expr(condition)?,
                        then: self.resolve_block(then)?,
                        otherwise: otherwise.map(|x| self.resolve_block(x)).transpose()?
                    }
                )
            }
            stmt @ Statement::JmpStatement(_) => stmt
        })
    }

    fn make_temporary_var(&mut self, original: &ast::Identifier) -> ast::Identifier {
        let ret = format!("var.{}.{}", original, self.temp_var_increment);
        self.temp_var_increment += 1;
        Rc::new(ret)
    }

    fn make_temporary_label(&mut self, original: &ast::Identifier) -> ast::Identifier {
        let ret = format!("lbl.{}.{}", original, self.temp_label_increment);
        self.temp_label_increment += 1;
        Rc::new(ret)
    }

    fn resolve_expr(&mut self, expr: ast::Expr) -> AnalysisResult<ast::Expr> {
        use ast::Expr as Expr;
        Ok(match expr {
            Expr::Constant(_) => expr,
            Expr::Assignment(lval, rval) => {
                if let Expr::Var(_) = *lval {
                    Expr::Assignment(
                        Box::new(self.resolve_expr(*lval)?),
                        Box::new(self.resolve_expr(*rval)?),
                    )
                } else {
                    return Err(SemanticAnalysisError {
                        reason: SemanticAnalysisErrorKind::InvalidLvalue(*lval),
                    })
                }
            },
            Expr::Var(ident) => {
                if !self.variable_map.contains_key(&ident) {
                    return Err(SemanticAnalysisError{
                        reason: SemanticAnalysisErrorKind::UndeclaredVariable(Rc::clone(&ident)),
                    });
                }

                Expr::Var(Rc::clone(self.variable_map.get(&ident).unwrap()))
            },
            Expr::Unary(op, rhs) => Expr::Unary(op, Box::new(self.resolve_expr(*rhs)?)),
            Expr::Binary(op, lhs, rhs) => {
                Expr::Binary(
                    op,
                    Box::new(self.resolve_expr(*lhs)?),
                    Box::new(self.resolve_expr(*rhs)?)
                )
            },
            Expr::PrefixInc(incrementation, expr) => {
                if let Expr::Var(_) = *expr {
                    Expr::PrefixInc(
                        incrementation,
                        Box::new(self.resolve_expr(*expr)?),
                    )
                } else {
                    return Err(SemanticAnalysisError {
                        reason: SemanticAnalysisErrorKind::InvalidLvalue(*expr),
                    })
                }
            },
            Expr::PostfixInc(incrementation, expr) => {
                if let Expr::Var(_) = *expr {
                    Expr::PostfixInc(
                        incrementation,
                        Box::new(self.resolve_expr(*expr)?)
                    )
                } else {
                    return Err(SemanticAnalysisError {
                        reason: SemanticAnalysisErrorKind::InvalidLvalue(*expr),
                    })
                }
            },
            Expr::CompoundAssignment(op, var, rhs) => {
                if let Expr::Var(_) = *var {
                    Expr::CompoundAssignment(
                        op,
                        Box::new(self.resolve_expr(*var)?),
                        Box::new(self.resolve_expr(*rhs)?),
                    )
                } else {
                    return Err(SemanticAnalysisError {
                        reason: SemanticAnalysisErrorKind::InvalidLvalue(*var),
                    })
                }
            },
            Expr::Ternary(cond, then, otherwise) => {
                Expr::Ternary(
                    Box::new(self.resolve_expr(*cond)?),
                    Box::new(self.resolve_expr(*then)?),
                    Box::new(self.resolve_expr(*otherwise)?)
                )
            },
        })
    }

    fn resolve_goto(&mut self, block: ast::Block) -> AnalysisResult<ast::Block> {
        let mut block_ = Vec::with_capacity(block.len());
        for item in block {
            block_.push(match item {
                BlockItem::S(s) => {
                    BlockItem::S(self.resolve_goto_statement(s)?)
                },
                decl @ BlockItem::D(_) => decl // we don't need to worry about statements
            })
        }

        Ok(block_)
    }

    fn resolve_goto_statement(&mut self, stmt: ast::Statement) -> AnalysisResult<ast::Statement> {
        Ok(match stmt {
            Statement::Return(_) => stmt,
            Statement::Expression(_) => stmt,
            Statement::If(IfStatement { condition, then, otherwise}) => {
                Statement::If(IfStatement {
                    condition,
                    then: self.resolve_goto(then)?,
                    otherwise: otherwise.map(|x| self.resolve_goto(x)).transpose()?
                })
            },
            Statement::Empty => stmt,
            Statement::LabeledStatement(lbl, rhs) => {
                Statement::LabeledStatement(lbl, Box::new(self.resolve_goto_statement(*rhs)?))
            }
            Statement::JmpStatement(lbl) => {
                if !self.label_map.contains_key(&lbl) {
                    return Err(SemanticAnalysisError {
                        reason: SemanticAnalysisErrorKind::UndeclaredLabel(lbl)
                    })
                }
                Statement::JmpStatement(Rc::clone(self.label_map.get(&lbl).unwrap()))
            }
        })
    }
}
