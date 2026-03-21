mod context;
mod namespace;
mod collect_top_levels;
mod types;
mod collect_struct_fields;
mod collect_function_signatures;
mod lower;

use std::collections::HashMap;

use flywheel_ast as ast;
use flywheel_exchange as ex;
use flywheel_error::CompileResult;
use flywheel_sources::Symbol;
use crate::collect_function_signatures::collect_function_signatures;
use crate::collect_struct_fields::collect_struct_fields;
use crate::collect_top_levels::collect_names;
use crate::context::{Builtins, LoweringContext};
use crate::lower::check_types;

// todo replace this by making `Builtins` have a constructor that takes in an interner
pub const BUILTINS: &[&str] = &["u32", "true", "false"];

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
