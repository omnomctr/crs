use std::collections::HashMap;
use std::rc::Rc;
use crate::ast;
use crate::ast::{BlockItem, Declaration, ForInitializer, IfStatement, Statement};

struct AnalysisState {
    label_map: HashMap<ast::Identifier, ast::Identifier>,
    temp_var_increment: usize,
    temp_label_increment: usize,
    loop_id_increment: usize, // for breaks / continues
    function_map: HashMap<ast::Identifier, FunctionEntry>,
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
    DuplicateFunctionDefinition(ast::Identifier),
    WrongFunctionArity(ast::Identifier, usize, usize), /* name, expected, got */
    UndeclaredFunction(ast::Identifier),
}

#[derive(Debug)]
pub struct FunctionEntry {
    has_body: bool,
    arity: usize,
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
        loop_id_increment: 0,
        function_map: HashMap::new(),
    };

    let mut scope = ScopeMap::new(None);

    let mut functions = Vec::with_capacity(ast.functions.len());
    for function in ast.functions {
        functions.push(state.resolve_function_declaration(function, &mut scope)?);
    }

    /* semantic analysis second pass */
    /* here we resolve whether or not function calls are external */
    let mut functions_ = Vec::with_capacity(functions.len());
    for function in functions {
        functions_.push(state.second_pass_resolve_function_declaration(function)?)
    }

    Ok(ast::Program {
        functions: functions_,
    })
}

impl AnalysisState {
    fn resolve_block(&mut self, block: ast::Block, scope: &mut ScopeMap, loop_id: Option<usize>) -> AnalysisResult<ast::Block> {
        let mut body = Vec::with_capacity(block.len());
        let mut scope_ = ScopeMap::new(Some(scope));
        for block_item in block {
            body.push(self.resolve_block_item(block_item, &mut scope_, loop_id)?);
        }

        Ok(body)
    }

    fn resolve_block_item(&mut self, block_item: BlockItem, scope: &mut ScopeMap, loop_id: Option<usize>) -> AnalysisResult<BlockItem> {
        use ast::BlockItem as BlockItem;
        Ok(match block_item {
            BlockItem::S(s) => BlockItem::S(self.resolve_statement(s, scope, loop_id)?),
            BlockItem::D(d) => BlockItem::D(self.resolve_declaration(d, scope)?),
        })
    }

    fn resolve_declaration(&mut self, decl: ast::Declaration, scope: &mut ScopeMap) -> AnalysisResult<ast::Declaration> {
        Ok(match decl {
            Declaration::Fun(f) => Declaration::Fun(self.resolve_function_declaration(f, scope)?),
            Declaration::Var(v) => Declaration::Var(self.resolve_variable_declaration(v, scope)?),
        })
    }
    fn resolve_variable_declaration(&mut self, decl: ast::VariableDeclaration, scope: &mut ScopeMap) -> AnalysisResult<ast::VariableDeclaration> {
        if scope.contains_at_current_scope(&decl.name) {
            return Err(SemanticAnalysisError {
                reason: SemanticAnalysisErrorKind::DuplicateVariableDecl(Rc::clone(&decl.name)),
            })
        }

        let name = self.make_temporary_var(&decl.name);
        scope.insert(Rc::clone(&decl.name), Rc::clone(&name));
        let new_initializer = decl.initializer.map(|x| self.resolve_expr(x, scope)).transpose()?;

        Ok(ast::VariableDeclaration {
            name,
            initializer: new_initializer,
        })
    }

    fn resolve_function_declaration(&mut self, decl: ast::FunctionDeclaration, scope: &mut ScopeMap) -> AnalysisResult<ast::FunctionDeclaration> {
        // function is already defined
        match self.function_map.get_mut(&decl.name) {
            Some(FunctionEntry { has_body: true, .. }) => {
                return Err(SemanticAnalysisError {
                    reason: SemanticAnalysisErrorKind::DuplicateFunctionDefinition(decl.name),
                });
            },
            Some(FunctionEntry { arity, .. }) if *arity != decl.params.len() => {
                return Err(SemanticAnalysisError {
                    reason: SemanticAnalysisErrorKind::WrongFunctionArity(decl.name, *arity, decl.params.len())
                });
            },
            None => {
                self.function_map.insert(Rc::clone(&decl.name), FunctionEntry { has_body: decl.body.is_some(), arity: decl.params.len() });
            },
            Some(entry) if !entry.has_body => entry.has_body = true,
            _ => {}
        }

        // TODO: merge regular variable identifier and function identifier "namespace"
        // right now this code compiles:
        //  int foo(void);
        //  int main(void) {
        //      int foo = 0;
        //      foo++; // refers to foo variable
        //      foo(); // refers to foo function
        //      return 0;
        //  }

        let mut scope = ScopeMap::new(Some(scope));
        let mut params = Vec::with_capacity(decl.params.len());
        for arg in &decl.params {
            let arg_ = self.make_temporary_var(&arg);
            scope.insert(Rc::clone(&arg), Rc::clone(&arg_));
            params.push(arg_);
        }

        Ok(ast::FunctionDeclaration {
            name: decl.name,
            params,
            body: decl.body.map(|b| {
                let block = self.resolve_block(b, &mut scope, None)?;
                let block = self.resolve_goto(block)?;
                Ok(block)
            }).transpose()?
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

                let id = self.loop_id_increment;
                self.loop_id_increment += 1;

                Statement::While(
                    self.resolve_expr(condition, scope)?,
                    Box::new(self.resolve_statement(*body, scope, Some(id))?),
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
            },
            Statement::Break(id) => {
                assert_eq!(id, None);
                if loop_id == None {
                    return Err(SemanticAnalysisError {
                        reason: SemanticAnalysisErrorKind::IllegalBreak
                    });
                }
                Statement::Break(loop_id)
            },
            Statement::DoWhile(cond, body,id) => {
                assert_eq!(id, None);

                let id = self.loop_id_increment;
                self.loop_id_increment += 1;

                Statement::DoWhile(
                    self.resolve_expr(cond, scope)?,
                    Box::new(self.resolve_statement(*body, scope, Some(id))?),
                    Some(id),
                )
            },
            Statement::ForLoop(expr1, expr2, expr3, body, id) => {
                assert_eq!(id, None);

                let scope = &mut ScopeMap::new(Some(scope));

                let id = self.loop_id_increment;
                self.loop_id_increment += 1;

                Statement::ForLoop(
                    expr1.map(|b| {
                        Ok(match b {
                            ast::ForInitializer::Decl(d) => ast::ForInitializer::Decl(self.resolve_variable_declaration(d, scope)?),
                            ast::ForInitializer::Expr(e) => ast::ForInitializer::Expr(self.resolve_expr(e, scope)?),
                        })
                    }).transpose()?,
                    expr2.map(|x| self.resolve_expr(x, scope)).transpose()?,
                    expr3.map(|x| self.resolve_expr(x, scope)).transpose()?,
                    Box::new(self.resolve_statement(*body, scope, Some(id))?),
                    Some(id)
                )
            },
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
            Expr::FunCall(_, _, Some(_)) => panic!(),
            Expr::FunCall(ident, args, None) => {
                if !self.function_map.contains_key(&ident) {
                    return Err(SemanticAnalysisError {
                        reason: SemanticAnalysisErrorKind::UndeclaredFunction(ident)
                    });
                }

                let arity = self.function_map.get(&ident).unwrap().arity;

                if arity != args.len() {
                    return Err(SemanticAnalysisError {
                        reason: SemanticAnalysisErrorKind::WrongFunctionArity(ident, arity, args.len())
                    });
                }

                let mut args_ = Vec::with_capacity(args.len());
                for arg in args {
                    args_.push(self.resolve_expr(arg, scope)?);
                }

                Expr::FunCall(
                    ident,
                    args_,
                    None,
                )
            }
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
                Statement::While(cond, Box::new(self.resolve_goto_statement(*body)?), id)
            },
            Statement::Break(_) => stmt,
            Statement::Continue(_) => stmt,
            Statement::DoWhile(cond, body, id) => {
                Statement::DoWhile(cond, Box::new(self.resolve_goto_statement(*body)?), id)
            }
            Statement::ForLoop(expr1, expr2, expr3, body, id) => {
                Statement::ForLoop(expr1, expr2, expr3, Box::new(self.resolve_goto_statement(*body)?), id)
            }
        })
    }

    fn second_pass_resolve_function_declaration(&mut self, f: ast::FunctionDeclaration) -> AnalysisResult<ast::FunctionDeclaration> {
        Ok(
            ast::FunctionDeclaration {
                name: f.name,
                params: f.params,
                body: f.body.map(|b| self.second_pass_resolve_block(b)).transpose()?
            }
        )
    }

    fn second_pass_resolve_block(&mut self, block: ast::Block) -> AnalysisResult<ast::Block> {
        let mut block_items = Vec::with_capacity(block.len());
        for item in block {
            match item {
                // only looking for expressions (funcalls) here
                BlockItem::S(s) => block_items.push(BlockItem::S(self.second_pass_resolve_statement(s)?)),
                x @ BlockItem::D(_) => block_items.push(x)
            }
        }

        Ok(block_items)
    }
    fn second_pass_resolve_statement(&mut self, s: ast::Statement) -> AnalysisResult<ast::Statement> {
        use Statement as S;
        Ok(match s {
            S::Expression(e) => S::Expression(self.second_pass_resolve_expr(e)?),
            S::Block(b) => S::Block(self.second_pass_resolve_block(b)?),
            S::Return(e) => S::Return(self.second_pass_resolve_expr(e)?),
            S::If(IfStatement { condition, then, otherwise }) => {
                  S::If(IfStatement {
                      condition: self.second_pass_resolve_expr(condition)?,
                      then: self.second_pass_resolve_block(then)?,
                      otherwise: otherwise.map(|b| self.second_pass_resolve_block(b)).transpose()?
                  })
            },
            S::LabeledStatement(lbl, stmt) => S::LabeledStatement(lbl, Box::new(self.second_pass_resolve_statement(*stmt)?)),
            S::While(e, s, lbl) => {
                S::While(
                    self.second_pass_resolve_expr(e)?,
                    Box::new(self.second_pass_resolve_statement(*s)?),
                    lbl
                )
            },
            S::DoWhile(e, s, lbl) => {
                S::DoWhile(
                    self.second_pass_resolve_expr(e)?,
                    Box::new(self.second_pass_resolve_statement(*s)?),
                    lbl
                )
            },
            S::ForLoop(e1, e2, e3, stmt, lbl) => {
                S::ForLoop(
                    e1.map(|fi| match fi {
                        x @ ForInitializer::Decl(_) => Ok(x),
                        ForInitializer::Expr(e) => Ok(ForInitializer::Expr(self.second_pass_resolve_expr(e)?))
                    }).transpose()?,
                    e2.map(|e| self.second_pass_resolve_expr(e)).transpose()?,
                    e3.map(|e| self.second_pass_resolve_expr(e)).transpose()?,
                    Box::new(self.second_pass_resolve_statement(*stmt)?),
                    lbl,
                )
            }

            x => x,
        })
    }
    fn second_pass_resolve_expr(&mut self, expr: ast::Expr) -> AnalysisResult<ast::Expr> {
        use ast::Expr as E;
        Ok(match expr {
            E::Unary(op, e) => E::Unary(op, Box::new(self.second_pass_resolve_expr(*e)?)),
            E::Binary(op, lhs, rhs) => {
                E::Binary(
                    op,
                    Box::new(self.second_pass_resolve_expr(*lhs)?),
                    Box::new(self.second_pass_resolve_expr(*rhs)?),
                )
            },
            E::Assignment(lval, rval) => E::Assignment(lval, Box::new(self.second_pass_resolve_expr(*rval)?)),
            E::CompoundAssignment(op, lval, rval) => {
                E::CompoundAssignment(op, lval, Box::new(self.second_pass_resolve_expr(*rval)?))
            },
            E::Ternary(cond, then, elsee) => {
                E::Ternary(
                    Box::new(self.second_pass_resolve_expr(*cond)?),
                    Box::new(self.second_pass_resolve_expr(*then)?),
                    Box::new(self.second_pass_resolve_expr(*elsee)?),
                )
            }
            E::FunCall(_, _, Some(_)) => panic!(),
            E::FunCall(ident, params, None) => {
                E::FunCall(
                    Rc::clone(&ident),
                    {
                        let mut params_ = Vec::with_capacity(params.len());
                        for param in params {
                            params_.push(self.second_pass_resolve_expr(param)?)
                        }
                        params_
                    },
                    // this is a little confusing but its external if it doesn't have a body
                    Some(self.function_map.get(&ident).map_or(true, |entry| !entry.has_body)),
                )
            }
            e => e,
        })
    }
}
