mod context;
mod namespace;

use std::collections::hash_map::Entry;
use std::collections::HashMap;

use flywheel_ast as ast;
use flywheel_exchange as ex;
use flywheel_error::{CompileMessage, CompileResult};
use flywheel_exchange::{FunctionBuilder, LocalId};
use flywheel_sources::{Span, Symbol};

use crate::context::{AllFunctionSignatures, AllFunctionSignaturesData, AllStructFields, AllStructFieldsData, AstMap, Builtins, CollectedNames, CollectedNamesData, FunctionSignature, LoweringContext};
use crate::namespace::{Builtin, Item, Namespace, SearchPath};

#[derive(Clone)]
enum Type<'ast> {
    Unit,
    Bool,
    U32,
    Struct(&'ast ast::Struct<'ast>),
}

impl<'ast> PartialEq for Type<'ast> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (&Type::Unit, &Type::Unit) => true,
            (&Type::Bool, &Type::Bool) => true,
            (&Type::U32, &Type::U32) => true,
            (&Type::Struct(a_ptr), &Type::Struct(b_ptr)) => std::ptr::eq(a_ptr, b_ptr),
            (_, _) => false,
        }
    }
}

impl<'ast> LoweringContext<'ast, CollectedNames> {
    fn resolve_name_as_type(&self, search: &SearchPath<'_, 'ast>, name: Symbol) -> CompileResult<Type<'ast>> {
        match search.resolve(name) {
            None => Err(CompileMessage::error_dyn(move |s| format!("No type found called {}", s.get_symbol(name)))),
            Some(Item::Function(_)) => {
                Err(CompileMessage::error_dyn(move |s| format!("No type found called {}", s.get_symbol(name))))
            }
            Some(Item::Imported(import)) => {
                assert!(import.anchor.is_none());
                self.resolve_name_as_type(&SearchPath::from(&self.collected_names().file_namespaces[import.path]), name) // todo cache this and prevent cycles
            }
            Some(Item::Struct(struct_)) => Ok(Type::Struct(struct_)),
            Some(Item::Builtin(Builtin::U32)) => Ok(Type::U32),
        }
    }

    fn resolve_type(&self, search: &SearchPath<'_, 'ast>, ast_ty: &ast::Type<'ast>) -> CompileResult<Type<'ast>> {
        self.resolve_name_as_type(search, ast_ty.name)
    }

    fn lower_type(&self, ty: &Type<'ast>) -> ex::Type {
        match ty {
            Type::Unit => ex::Type::Unit,
            Type::Bool => ex::Type::Bool,
            Type::U32 => ex::Type::Integer,
            Type::Struct(struct_) => todo!(),
        }
    }
}

pub const BUILTINS: &[&str] = &["u32", "true", "false"];

fn collect_names(ctx: LoweringContext<()>) -> CompileResult<LoweringContext<CollectedNames>> {
    let mut prelude_ns = Namespace::new();
    prelude_ns.add(ctx.builtins().kw_u32, Item::Builtin(Builtin::U32)).unwrap();
    let prelude_ns = prelude_ns;

    let mut file_namespaces: HashMap<&[Symbol], Namespace> = HashMap::new();
    for (path_in_module, file) in &ctx.ast().contents {
        let mut file_ns = Namespace::new();
        // file_ns.add_all(&prelude_ns)?;

        for top_level in file.top_levels() {
            match *top_level {
                ast::TopLevel::Import(import) => {
                    file_ns.add(import.item, Item::Imported(import))?;
                }
                ast::TopLevel::Function(func) => {
                    file_ns.add(func.name, Item::Function(func))?;
                }
                ast::TopLevel::Struct(struct_) => {
                    file_ns.add(struct_.name, Item::Struct(struct_))?;
                }
            }
        }
        file_namespaces.insert(path_in_module, file_ns);
    }

    Ok(ctx.add_collected_names(CollectedNamesData { file_namespaces, prelude_ns }))
}

fn collect_struct_fields(ctx: LoweringContext<CollectedNames>) -> CompileResult<LoweringContext<AllStructFields>> {
    let mut all_struct_fields = AstMap::new();
    for (path_in_module, file) in &ctx.ast().contents {
        let file_namespace = &ctx.collected_names().file_namespaces[path_in_module.as_slice()];
        let search_path = SearchPath::from(file_namespace).then(&ctx.collected_names().prelude_ns);

        for top_level in file.top_levels() {
            match *top_level {
                ast::TopLevel::Struct(struct_) => {
                    let mut fields = HashMap::new();

                    for field in struct_.fields {
                        let ty = ctx.resolve_type(&search_path, &field.ty)?;
                        fields.insert(field.name, ty);
                    }
                    all_struct_fields.insert(struct_, fields);
                }
                ast::TopLevel::Function(_) => (),
                ast::TopLevel::Import(_) => (),
            };
        }
    }

    Ok(ctx.add_all_struct_fields(AllStructFieldsData { all_struct_fields }))
}

fn collect_function_signatures(
    ctx: LoweringContext<AllStructFields>,
) -> CompileResult<LoweringContext<AllFunctionSignatures>> {
    let mut signatures = AstMap::new();
    for (path_in_module, file) in &ctx.ast().contents {
        let mut next_function_id = (0..).map(ex::FunctionId);
        let file_namespace = &ctx.collected_names().file_namespaces[path_in_module.as_slice()];
        let search_path = SearchPath::from(file_namespace).then(&ctx.collected_names().prelude_ns);

        for top_level in file.top_levels() {
            match *top_level {
                ast::TopLevel::Function(func_) => {
                    let return_type = ctx.resolve_type(&search_path, &func_.return_type)?;
                    signatures.insert(func_, FunctionSignature {
                        return_type,
                        id: next_function_id.next().unwrap()
                    });
                }
                _ => (),
            };
        }
    }

    Ok(ctx.add_all_function_signatures(AllFunctionSignaturesData { signatures }))
}

enum Value<'ast> {
    Local { index: LocalId, ty: Type<'ast>, span: Span }
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

    builder: FunctionBuilder,
}

impl<'ctx, 'ast> FunctionTypeResolver<'ctx, 'ast> {
    fn add_local(&mut self, name: Symbol, ty: Type<'ast>, span: Span) -> CompileResult<()> {
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

        let actual_ty = match expr {
            Expr::Bool(span_and_symbol) => {
                let sym = span_and_symbol.symbol();
                let builtins = self.ctx.builtins();
                assert!(sym == builtins.kw_false || sym == builtins.kw_true);
                self.builder.push_bool(sym == builtins.kw_true);
                Type::Bool
            }
            Expr::Integer(span_and_symbol) => {
                let symbol = span_and_symbol.symbol();
                let text = self.ctx.ast().sources.get_symbol(symbol);
                let number = text.parse::<i64>().unwrap();
                self.builder.push_integer(number);
                Type::U32
            }
            Expr::Name(symbol_and_span) => {
                let value = self.scopes.resolve_as_value(symbol_and_span.symbol())?;
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

fn check_types(ctx: LoweringContext<AllFunctionSignatures>) -> CompileResult<ex::Module> {
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
                        builder: FunctionBuilder::new(ctx.ast().sources.get_symbol(func_.name), vec![], ctx.lower_type(&signature.return_type)),
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

pub fn lower_module(ast: &ast::Module, builtins: &HashMap<&'static str, Symbol>) -> CompileResult<ex::Module> {
    let ctx = LoweringContext::new(ast, Builtins {
        kw_true: builtins["true"],
        kw_false: builtins["false"],
        kw_u32: builtins["u32"],
    });
    let ctx = collect_names(ctx)?;
    let ctx = collect_struct_fields(ctx)?;
    let ctx = collect_function_signatures(ctx)?;
    let module = check_types(ctx)?;

    Ok(module)
}
