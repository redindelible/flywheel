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
use flywheel_sources::{InternerState, Symbol};
use crate::collect_function_signatures::collect_function_signatures;
use crate::collect_struct_fields::collect_struct_fields;
use crate::collect_top_levels::collect_names;
use crate::context::LoweringContext;
use crate::lower::check_types;

pub struct Builtins {
    pub(crate) kw_true: Symbol,
    pub(crate) kw_false: Symbol,
    pub(crate) kw_u32: Symbol,
}

impl Builtins {
    pub fn new(interner_state: &mut InternerState) -> Builtins {
        let symbols = interner_state.add_builtins(&["u32", "true", "false"]);
        Builtins {
            kw_true: symbols["true"],
            kw_false: symbols["false"],
            kw_u32: symbols["u32"],
        }
    }
}


pub fn lower_module(ast: &ast::Module, builtins: &Builtins) -> CompileResult<ex::Module> {
    let ctx = LoweringContext::new(ast, builtins);
    let ctx = collect_names(ctx)?;
    let ctx = collect_struct_fields(ctx)?;
    let ctx = collect_function_signatures(ctx)?;
    let module = check_types(ctx)?;

    Ok(module)
}
