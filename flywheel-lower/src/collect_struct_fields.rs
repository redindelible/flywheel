use std::collections::HashMap;

use flywheel_ast as ast;
use flywheel_error::CompileResult;

use crate::context::{AllStructFields, AllStructFieldsData, AstMap, CollectedNames, LoweringContext};
use crate::namespace::SearchPath;

pub(crate) fn collect_struct_fields(ctx: LoweringContext<CollectedNames>) -> CompileResult<LoweringContext<AllStructFields>> {
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