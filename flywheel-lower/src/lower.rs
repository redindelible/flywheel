use std::collections::HashMap;

use flywheel_ast as ast;
use flywheel_exchange as ex;
use flywheel_error::{CompileMessage, CompileResult};
use flywheel_sources::{Span, Symbol};

use crate::context::{AllFunctionSignatures, LoweringContext};
use crate::types::Type;


enum Value<'ast> {
    Local { index: ex::LocalId, ty: Type<'ast>, span: Span }
}

struct ScopeItem<'ast> {
    value: Option<Value<'ast>>,
    ty: Option<Type<'ast>>,
    span: Span,
}

struct Scope<'ast> {
    items: HashMap<Symbol, ScopeItem<'ast>>,
}

struct Scopes<'ast>(Vec<Scope<'ast>>);

impl<'ast> Scopes<'ast> {
    fn current(&self) -> &Scope<'ast> {
        self.0.last().unwrap()
    }

    fn current_mut(&mut self) -> &mut Scope<'ast> {
        self.0.last_mut().unwrap()
    }

    fn resolve(&self, name: Symbol) -> CompileResult<&ScopeItem<'ast>> {
        for scope in self.0.iter().rev() {
            if let Some(item) = scope.items.get(&name) {
                return Ok(item);
            }
        }
        Err(CompileMessage::error_dyn(move |s| format!("Could not resolve {}", s.get_symbol(name))))
    }

    fn resolve_type(&mut self, ty: &'ast ast::Type<'ast>) -> CompileResult<Type<'ast>> {
        let resolved: &ScopeItem = self.resolve(ty.name)?;
        if let Some(ty) = &resolved.ty {
            Ok(ty.clone())
        } else {
            todo!()
        }
    }

    fn resolve_as_value(&self, name: Symbol) -> CompileResult<&Value<'ast>> {
        let resolved: &ScopeItem = self.resolve(name)?;
        if let Some(value) = &resolved.value {
            Ok(value)
        } else {
            todo!()
        }
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

        let current_scope: &mut Scope<'ast> = self.scopes.current_mut();
        match current_scope.items.entry(name) {
            Entry::Occupied(_) => todo!(),
            Entry::Vacant(vacant) => {
                vacant.insert(ScopeItem {
                    value: Some(Value::Local { index: self.builder.mark_local(), ty, span }),
                    ty: None,
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
                    Some(self.scopes.resolve_type(ty)?)
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
            Expr::Name((symbol, _)) => {
                let value = self.scopes.resolve_as_value(symbol)?;
                match value {
                    Value::Local { index, ty, span: _ } => {
                        self.builder.load_local(*index);
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
    let mut functions = HashMap::new();
    for (path_in_module, file) in &ctx.ast().contents {
        for top_level in file.top_levels() {
            match *top_level {
                ast::TopLevel::Function(func_) => {
                    let signature = &ctx.all_function_signatures().signatures[func_];
                    let mut this = FunctionTypeResolver {
                        ctx: &ctx,
                        return_type: signature.return_type.clone(),
                        scopes: Scopes(vec![]),
                        builder: ex::FunctionBuilder::new(ctx.ast().sources.get_symbol(func_.name), vec![], ctx.lower_type(&signature.return_type)),
                    };
                    this.lower_block(&func_.body, Some(signature.return_type.clone()))?;
                    this.builder.return_();

                    functions.insert(signature.id, this.builder.finish());
                }
                _ => (),
            };
        }
    }
    Ok(ex::Module {
        functions
    })
}
