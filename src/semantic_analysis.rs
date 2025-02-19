use std::collections::HashMap;
use std::rc::Rc;
use crate::ast;
use crate::ast::{BlockItem, IfStatement, Statement};

struct AnalysisState {
    label_map: HashMap<ast::Identifier, ast::Identifier>,
    temp_var_increment: usize,
    temp_label_increment: usize,
    loop_id: usize // for breaks / continues
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
    IllegalBreak,
    IllegalContinue,
}

type AnalysisResult<T> = Result<T, SemanticAnalysisError>;

pub struct ScopeMap<'a> {
    variable_map: HashMap<ast::Identifier, ast::Identifier>,
    parent: Option<&'a ScopeMap<'a>>
}

pub struct VariableEntry {
    unique_name: ast::Identifier,
    from_current_block: bool,
}

impl<'a> ScopeMap<'a> {
    pub fn new(parent: Option<&'a ScopeMap>) -> ScopeMap<'a> {
        ScopeMap {
            variable_map: HashMap::new(),
            parent
        }
    }

    pub fn get(&self, ident: &ast::Identifier) -> Option<VariableEntry> {
        match self.variable_map.get(ident) {
            Some(x) => {
                Some(VariableEntry {
                    unique_name: Rc::clone(&x),
                    from_current_block: true
                })
            }
            None => {
                self.parent.as_ref().and_then(|map| {
                    map.get(ident).map(|mut entry| {
                        entry.from_current_block = false;
                        entry
                    })
                })
            }
        }
    }

    pub fn contains_at_current_scope(&self, ident: &ast::Identifier) -> bool {
        self.variable_map.contains_key(ident)
    }

    pub fn insert(&mut self, ident: ast::Identifier, name: ast::Identifier) -> Option<ast::Identifier> {
        self.variable_map.insert(ident, name)
    }

    pub fn contains_key(&self, ident: &ast::Identifier) -> bool {
        self.variable_map.contains_key(ident) || self.parent.is_some_and(|x| x.contains_key(ident))
    }
}

pub fn analyse(ast: ast::Program) -> AnalysisResult<ast::Program> {
    let mut state = AnalysisState {
        label_map: HashMap::new(),
        temp_var_increment: 0,
        temp_label_increment: 0,
        loop_id: 0,
    };

    let mut scope = ScopeMap::new(None);

    let body = state.resolve_block(ast.f.body, &mut scope, None)?;
    let body = state.resolve_goto(body)?;

    Ok(ast::Program {
        f: ast::Function {
            name: ast.f.name,
            body,
        }
    })
}

impl AnalysisState {
    fn resolve_block(&mut self, block: ast::Block, scope: &mut ScopeMap, loop_id: Option<usize>) -> AnalysisResult<ast::Block> {
        let mut body = Vec::with_capacity(block.len());
        let mut scope_ = ScopeMap::new(Some(scope));
        for block_item in block {
            use ast::BlockItem as BlockItem;
            body.push(
                match block_item {
                    BlockItem::S(s) => BlockItem::S(self.resolve_statement(s, &mut scope_, loop_id)?),
                    BlockItem::D(d) => BlockItem::D(self.resolve_declaration(d, &mut scope_)?),
                }
            )
        }

        Ok(body)
    }
    fn resolve_declaration(&mut self, decl: ast::Declaration, scope: &mut ScopeMap) -> AnalysisResult<ast::Declaration> {
        if scope.contains_at_current_scope(&decl.name) {
            return Err(SemanticAnalysisError {
                reason: SemanticAnalysisErrorKind::DuplicateVariableDecl(Rc::clone(&decl.name)),
            })
        }

        let name = self.make_temporary_var(&decl.name);
        scope.insert(Rc::clone(&decl.name), Rc::clone(&name));
        let new_initializer = decl.initializer.map(|x| self.resolve_expr(x, scope)).transpose()?;

        Ok(ast::Declaration {
            name,
            initializer: new_initializer,
        })
    }

    fn resolve_statement(&mut self, stmt: ast::Statement, scope: &mut ScopeMap, loop_id: Option<usize>) -> AnalysisResult<ast::Statement> {
        use ast::Statement as Statement;
        Ok(match stmt {
            Statement::Return(e) => Statement::Return(self.resolve_expr(e, scope)?),
            Statement::Expression(e) => Statement::Expression(self.resolve_expr(e, scope)?),
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
                    Box::new(self.resolve_statement(*rhs, scope, loop_id)?)
                )
            },
            Statement::If(IfStatement { condition, then, otherwise }) => {
                Statement::If(
                    IfStatement{
                        condition: self.resolve_expr(condition, scope)?,
                        then: self.resolve_block(then, scope, loop_id)?,
                        otherwise: otherwise.map(|x| self.resolve_block(x, scope, loop_id)).transpose()?
                    }
                )
            }
            stmt @ Statement::JmpStatement(_) => stmt,
            Statement::Block(block) => {
                Statement::Block(self.resolve_block(block, scope, loop_id)?)
            }
            Statement::While(condition, body, id) => {
                assert_eq!(id, None);

                let id = self.loop_id;
                self.loop_id += 1;

                Statement::While(
                    self.resolve_expr(condition, scope)?,
                    self.resolve_block(body, scope, Some(id))?,
                    Some(id),
                )
            },
            Statement::Continue(id) => {
                assert_eq!(id, None);
                if loop_id == None {
                    return Err(SemanticAnalysisError {
                        reason: SemanticAnalysisErrorKind::IllegalContinue
                    });
                }

                Statement::Continue(loop_id)
            }
            Statement::Break(id) => {
                assert_eq!(id, None);
                if loop_id == None {
                    return Err(SemanticAnalysisError {
                        reason: SemanticAnalysisErrorKind::IllegalBreak
                    });
                }
                Statement::Break(loop_id)
            }
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

    fn resolve_expr(&mut self, expr: ast::Expr, scope: &mut ScopeMap) -> AnalysisResult<ast::Expr> {
        use ast::Expr as Expr;
        Ok(match expr {
            Expr::Constant(_) => expr,
            Expr::Assignment(lval, rval) => {
                if let Expr::Var(_) = *lval {
                    Expr::Assignment(
                        Box::new(self.resolve_expr(*lval, scope)?),
                        Box::new(self.resolve_expr(*rval, scope)?),
                    )
                } else {
                    return Err(SemanticAnalysisError {
                        reason: SemanticAnalysisErrorKind::InvalidLvalue(*lval),
                    })
                }
            },
            Expr::Var(ident) => {
                if !scope.contains_key(&ident) {
                    return Err(SemanticAnalysisError{
                        reason: SemanticAnalysisErrorKind::UndeclaredVariable(Rc::clone(&ident)),
                    });
                }

                Expr::Var(Rc::clone(&scope.get(&ident).unwrap().unique_name))
            },
            Expr::Unary(op, rhs) => Expr::Unary(op, Box::new(self.resolve_expr(*rhs, scope)?)),
            Expr::Binary(op, lhs, rhs) => {
                Expr::Binary(
                    op,
                    Box::new(self.resolve_expr(*lhs, scope)?),
                    Box::new(self.resolve_expr(*rhs, scope)?)
                )
            },
            Expr::PrefixInc(incrementation, expr) => {
                if let Expr::Var(_) = *expr {
                    Expr::PrefixInc(
                        incrementation,
                        Box::new(self.resolve_expr(*expr, scope)?),
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
                        Box::new(self.resolve_expr(*expr, scope)?)
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
                        Box::new(self.resolve_expr(*var, scope)?),
                        Box::new(self.resolve_expr(*rhs, scope)?),
                    )
                } else {
                    return Err(SemanticAnalysisError {
                        reason: SemanticAnalysisErrorKind::InvalidLvalue(*var),
                    })
                }
            },
            Expr::Ternary(cond, then, otherwise) => {
                Expr::Ternary(
                    Box::new(self.resolve_expr(*cond, scope)?),
                    Box::new(self.resolve_expr(*then, scope)?),
                    Box::new(self.resolve_expr(*otherwise, scope)?)
                )
            },
        })
    }

    fn resolve_goto(&mut self, block: ast::Block) -> AnalysisResult<ast::Block> {
        let mut block_ = Vec::with_capacity(block.len());
        for item in block {
            block_.push(self.resolve_goto_block_item(item)?)
        }

        Ok(block_)
    }

    fn resolve_goto_block_item(&mut self, item: ast::BlockItem) -> AnalysisResult<ast::BlockItem> {
        Ok(match item {
            BlockItem::S(s) => {
                BlockItem::S(self.resolve_goto_statement(s)?)
            },
            decl @ BlockItem::D(_) => decl // we don't need to worry about statements
        })
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
            Statement::Block(block) => {
                let mut block_ = Vec::with_capacity(block.len());
                for item in block {
                    block_.push(self.resolve_goto_block_item(item)?)
                }

                ast::Statement::Block(block_)
            },

            Statement::While(cond, body, id) => {
                Statement::While(cond, {
                    let mut block = Vec::with_capacity(body.len());
                    for item in body {
                        block.push(self.resolve_goto_block_item(item)?)
                    }
                    block
                }, id)
            },
            Statement::Break(_) => stmt,
            Statement::Continue(_) => stmt,
        })
    }
}
