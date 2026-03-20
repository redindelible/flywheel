use flywheel_ast as ast;
use flywheel_error::CompileResult;
use flywheel_exchange as ex;

use crate::context::{AllFunctionSignatures, AllFunctionSignaturesData, AllStructFields, AstMap, FunctionSignature, LoweringContext};
use crate::namespace::SearchPath;


pub(crate) fn collect_function_signatures(
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
