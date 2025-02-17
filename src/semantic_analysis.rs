use std::collections::HashMap;
use std::rc::Rc;
use crate::ast;

struct AnalysisState {
    variable_map: HashMap<ast::Identifier, ast::Identifier>,
    temp_var_increment: usize,
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
}

type AnalysisResult<T> = Result<T, SemanticAnalysisError>;

pub fn analyse(ast: ast::Program) -> AnalysisResult<ast::Program> {
    let mut state = AnalysisState {
        variable_map: HashMap::new(),
        temp_var_increment: 0,
    };
    let mut body = Vec::with_capacity(ast.f.body.len());
    for block_item in ast.f.body {
        use ast::BlockItem as BlockItem;
        body.push(
            match block_item {
                BlockItem::S(s) => BlockItem::S(state.resolve_statement(s)?),
                BlockItem::D(d) => BlockItem::D(state.resolve_declaration(d)?),
            }
        )
    }

    Ok(ast::Program {
        f: ast::Function {
            name: ast.f.name,
            body,
        }
    })
}

impl AnalysisState {
    fn resolve_declaration(&mut self, decl: ast::Declaration) -> AnalysisResult<ast::Declaration> {

        if self.variable_map.contains_key(&decl.name) {
            return Err(SemanticAnalysisError {
                reason: SemanticAnalysisErrorKind::DuplicateVariableDecl(Rc::clone(&decl.name)),
            })
        }

        let name = self.make_temporary(&decl.name);
        self.variable_map.insert(Rc::clone(&decl.name), Rc::clone(&name));

        let new_initializer = if let Some(expr) = decl.initializer {
            Some(self.resolve_expr(expr)?)
        } else {
            None
        };

        Ok(ast::Declaration {
            name: name,
            initializer: new_initializer,
        })
    }

    fn resolve_statement(&mut self, stmt: ast::Statement) -> AnalysisResult<ast::Statement> {
        use ast::Statement as Statement;
        Ok(match stmt {
            Statement::Return(e) => Statement::Return(self.resolve_expr(e)?),
            Statement::Expression(e) => Statement::Expression(self.resolve_expr(e)?),
            Statement::Empty => Statement::Empty,
        })
    }

    fn make_temporary(&mut self, original: &ast::Identifier) -> ast::Identifier {
        let ret = format!("var.{}.{}", original, self.temp_var_increment);
        self.temp_var_increment += 1;
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
            }
        })
    }
}
