use std::collections::HashMap;

use flywheel_ast as ast;
use flywheel_exchange as ex;
use flywheel_error::{CompileMessage, CompileResult, CompileResultExt};
use flywheel_sources::{Span, Symbol};

use crate::context::{AllFunctionSignatures, LoweringContext};
use crate::namespace::{Item, Namespace, SearchPath, Value};
use crate::types::Type;

struct Scopes<'ast> {
    scopes: Vec<Namespace<'ast>>,
}

impl<'ast> Scopes<'ast> {
    fn current(&self) -> &Namespace<'ast> {
        self.scopes.last().unwrap()
    }

    fn current_mut(&mut self) -> &mut Namespace<'ast> {
        self.scopes.last_mut().unwrap()
    }
}

impl<'ctx, 'ast> IntoIterator for &'ctx Scopes<'ast> {
    type Item = &'ctx Namespace<'ast>;
    type IntoIter = std::slice::Iter<'ctx, Namespace<'ast>>;

    fn into_iter(self) -> Self::IntoIter {
        self.scopes.iter()
    }
}

struct FunctionTypeResolver<'ctx, 'ast> {
    ctx: &'ctx LoweringContext<'ast, AllFunctionSignatures>,
    return_type: Type<'ast>,

    scopes: Scopes<'ast>,

    builder: ex::FunctionBuilder,
}

impl<'ctx, 'ast> FunctionTypeResolver<'ctx, 'ast> {
    fn add_local(&mut self, name: Symbol, ty: Type<'ast>, span: Span) -> CompileResult<()> {
        use std::collections::hash_map::Entry;

        let current_scope: &mut Namespace<'ast> = self.scopes.current_mut();
        match current_scope.items.entry(name) {
            Entry::Occupied(_) => todo!(),
            Entry::Vacant(vacant) => {
                vacant.insert(Item::Local {
                    index: self.builder.mark_local(),
                    ty,
                    span
                });
                Ok(())
            }
        }
    }

    fn unify_types(&self, actual: Type<'ast>, expected: Option<Type<'ast>>) -> CompileResult<Type<'ast>> {
        let Some(expected) = expected else {
            return Ok(actual);
        };

        if actual != expected {
            todo!()
        }

        Ok(actual)
    }

    fn lower_block<'a>(&mut self, block: &'ast ast::Block<'ast>, expected: Option<Type<'ast>>) -> CompileResult<Type<'ast>> {
        for stmt in block.stmts {
            self.lower_stmt(stmt)?;
        }

        match &block.trailing_expr {
            Some(expr) => Ok(self.lower_expr(expr, expected)?),
            None => self.unify_types(Type::Unit, expected),
        }
    }

    fn lower_stmt<'a>(&mut self, stmt: &'ast ast::Stmt<'ast>) -> CompileResult<()> {
        use ast::Stmt;

        match stmt {
            Stmt::Expr(expr) => {
                self.lower_expr(expr, None)?;
                self.builder.pop();
            }
            Stmt::Return(stmt) => {
                let _ = self.lower_expr(&stmt.expr, Some(self.return_type.clone()))?;
                self.builder.return_();
            }
            Stmt::Let(stmt) => {
                let expected_ty = if let Some(ty) = &stmt.ty {
                    Some(self.ctx.resolve_type(&self.scopes, ty)?)
                } else {
                    None
                };
                let actual_ty = self.lower_expr(&stmt.value, expected_ty)?;
                self.add_local(stmt.name, actual_ty, stmt.span)?;
            }
            Stmt::While(stmt) => {
                let cond_block = self.builder.new_block();
                let body_block = self.builder.new_block();
                let after_block = self.builder.new_block();

                self.builder.jump(cond_block);

                self.builder.switch_block(cond_block);
                self.lower_expr(&stmt.condition, Some(Type::Bool))?;
                self.builder.if_else(body_block, after_block);

                self.builder.switch_block(body_block);
                self.lower_block(&stmt.body, Some(Type::Unit))?;
                self.builder.jump(cond_block);

                self.builder.switch_block(after_block);
            }
        }

        Ok(())
    }

    fn lower_expr<'a>(&mut self, expr: &'ast ast::Expr<'ast>, expected: Option<Type<'ast>>) -> CompileResult<Type<'ast>> {
        use ast::Expr;

        let actual_ty = match *expr {
            Expr::Bool((symbol, _)) => {
                let builtins = self.ctx.builtins();
                assert!(symbol == builtins.kw_false || symbol == builtins.kw_true);
                self.builder.push_bool(symbol == builtins.kw_true);
                Type::Bool
            }
            Expr::Integer((symbol, _)) => {
                let text = self.ctx.ast().sources.get_symbol(symbol);
                let number = text.parse::<i64>().unwrap();
                self.builder.push_integer(number);
                Type::U32
            }
            Expr::Name(name) => {
                let value = self.ctx.resolve_name_as_value(&self.scopes, name)?;
                match value {
                    Value::Local { index, ty, span: _ } => {
                        self.builder.load_local(index);
                        ty.clone()
                    }
                }
            }
            _ => todo!()
        };

        Ok(actual_ty)
    }
}

pub(crate) fn check_types(ctx: LoweringContext<AllFunctionSignatures>) -> CompileResult<ex::Module> {
    let mut functions = Vec::new();
    for (path_in_module, file) in &ctx.ast().contents {
        for top_level in file.top_levels() {
            match *top_level {
                ast::TopLevel::Function(func_) => {
                    let signature = &ctx.all_function_signatures().signatures[func_];
                    let mut this = FunctionTypeResolver {
                        ctx: &ctx,
                        return_type: signature.return_type.clone(),
                        scopes: Scopes { scopes: vec![] },
                        builder: ex::FunctionBuilder::new(ctx.ast().sources.get_symbol(func_.name.symbol), vec![], ctx.lower_type(&signature.return_type)),
                    };
                    this.lower_block(&func_.body, Some(signature.return_type.clone()))?;
                    this.builder.return_();

                    functions.push(this.builder.finish());
                }
                _ => (),
            };
        }
    }
    Ok(ex::Module {
        functions
    })
}
