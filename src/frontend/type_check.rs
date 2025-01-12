use std::collections::{HashMap, HashSet};

use futures_util::StreamExt;
use futures_util::stream::{FuturesOrdered, FuturesUnordered};
use triomphe::{Arc, ArcBorrow};

use crate::frontend::ast::{self, AstRef};
use crate::frontend::driver::Handle;
use crate::frontend::error::{CompileError, CompileResult};
use crate::frontend::parser::Parse;
use crate::frontend::query::Processor;
use crate::frontend::source::{Location, SourceID, SourceInput, Sources};
use crate::utils::{InternedString, Interner};

#[derive(Copy, Clone)]
enum NamespaceRef {
    Root,
    Module(SourceID),
}

#[derive(Copy, Clone)]
pub enum Type {
    Integer,
    Struct(SourceID, AstRef<ast::Struct>),
}

#[derive(Copy, Clone)]
enum Value {
    Import(SourceID),
    Struct(SourceID, AstRef<ast::Struct>),
    TypeAlias(Type),
}

struct Namespace {
    parent: Option<NamespaceRef>,
    names: im::HashMap<InternedString, (Value, Option<Location>)>,
}

impl Namespace {
    fn new(parent: Option<NamespaceRef>) -> Self {
        Namespace { parent, names: im::HashMap::new() }
    }

    fn get(&self, name: InternedString) -> Option<&(Value, Option<Location>)> {
        self.names.get(&name)
    }

    fn insert(
        &mut self,
        name: InternedString,
        value: Value,
        location: Option<Location>,
    ) -> Result<(), Option<Location>> {
        use im::hashmap::Entry;

        match self.names.entry(name) {
            Entry::Occupied(previous) => Err(previous.into_mut().1),
            Entry::Vacant(vacant) => {
                vacant.insert((value, location));
                Ok(())
            }
        }
    }
}

pub struct DeclaredNames {
    ast: Arc<ast::FileAST>,

    direct_imports: HashSet<SourceID>,
    file_namespace: Namespace,
}

pub struct ComputeDeclaredNames;

impl Processor for ComputeDeclaredNames {
    type Input = SourceID;
    type Output = DeclaredNames;

    async fn process(handle: Handle, source_id: SourceID) -> CompileResult<DeclaredNames> {
        let ast = handle.query::<Parse>(source_id).await?.clone_arc();
        let strings = ast.strings();

        let mut imports_stream = ast
            .top_levels()
            .iter()
            .filter_map(|top_level| match top_level {
                &ast::TopLevel::Import(ast_import) => {
                    let relative_path = camino::Utf8PathBuf::from(ast.resolve(ast.get_node(ast_import).relative_path));

                    let handle_ref = &handle;
                    Some(async move {
                        let maybe_source = match SourceInput::from_relative_path(
                            handle_ref.get_source(source_id).get(),
                            &relative_path,
                        ) {
                            Ok(input) => handle_ref.query::<Sources>(input).await.map(|s| *s),
                            Err(e) => Err(e),
                        };
                        (relative_path, maybe_source)
                    })
                }
                _ => None,
            })
            .collect::<FuturesOrdered<_>>();

        let mut direct_imports = HashSet::new();
        let mut file_namespace = Namespace::new(Some(NamespaceRef::Root));
        for top_level in ast.top_levels() {
            let (name, value, location) = match *top_level {
                ast::TopLevel::Import(ast_import) => {
                    let (relative_path, maybe_source) = imports_stream.next().await.unwrap();
                    let source = maybe_source?;
                    direct_imports.insert(source); // todo check no duplicates?
                    let value = Value::Import(source);
                    // todo do this during parsing
                    let name = strings.get_or_intern(
                        relative_path.file_name().and_then(|path| path.strip_suffix(".fly")).unwrap_or_else(|| todo!()),
                    );
                    let location = ast.get_location(ast_import);
                    (name, value, location)
                }
                ast::TopLevel::Struct(ast_struct) => {
                    let name = ast.get_node(ast_struct).name;
                    let value = Value::Struct(source_id, ast_struct);
                    let location = ast.get_location(ast_struct);
                    (name, value, location)
                }
                ast::TopLevel::Function(_) => continue,
            };

            if let Err(_previous_location) = file_namespace.insert(name, value, Some(location)) {
                // todo note
                return Err(CompileError::with_description_and_location(
                    "type-check/redefinition",
                    format!("Name '{}' was already defined in this scope.", ast.resolve(name)),
                    location,
                ));
            };
        }

        Ok(DeclaredNames { ast, direct_imports, file_namespace })
    }
}

pub struct DefinedTypes {
    dependencies: HashSet<SourceID>,
    structs: HashMap<AstRef<ast::Struct>, StructDefinition>,
}

pub struct StructDefinition {
    fields: HashMap<InternedString, Type>,
}

struct NamespaceResolver<'a> {
    handle: &'a Handle,
    names: HashMap<SourceID, &'a Namespace>,
}

impl<'a> NamespaceResolver<'a> {
    fn new(handle: &'a Handle, names: HashMap<SourceID, &'a Namespace>) -> Self {
        NamespaceResolver { handle, names }
    }

    fn get_namespace(&self, namespace: NamespaceRef) -> &'a Namespace {
        match namespace {
            NamespaceRef::Root => &self.handle.processor::<ComputeDefinedTypes>().root_namespace,
            NamespaceRef::Module(source) => self.names.get(&source).unwrap(),
        }
    }

    fn resolve(&self, in_namespace: NamespaceRef, name: InternedString) -> Option<(Value, Option<Location>)> {
        let mut curr = Some(in_namespace);

        while let Some(namespace) = curr {
            let namespace = self.get_namespace(namespace);
            if let Some(resolved) = namespace.get(name) {
                return Some(*resolved);
            } else {
                curr = namespace.parent;
            }
        }

        None
    }

    fn resolve_type(
        &self,
        in_namespace: NamespaceRef,
        ast: &ast::FileAST,
        ast_type: AstRef<ast::Type>,
    ) -> CompileResult<Type> {
        let type_ = ast.get_node(ast_type);
        let resolved = match type_ {
            &ast::Type::Name(name) => {
                let Some((value, _location)) = self.resolve(in_namespace, name) else {
                    return Err(CompileError::with_description_and_location(
                        "type-check/unknown-name",
                        format!("Name '{}' was not found in this scope.", ast.resolve(name)),
                        ast.get_location(ast_type),
                    ));
                };
                match value {
                    Value::Import(_) => {
                        // todo note
                        return Err(CompileError::with_description_and_location(
                            "type-check/not-a-type",
                            format!("Name '{}' is not a type.", ast.resolve(name)),
                            ast.get_location(ast_type),
                        ));
                    }
                    Value::Struct(source, ast_struct) => Type::Struct(source, ast_struct),
                    Value::TypeAlias(type_) => type_,
                }
            }
        };
        Ok(resolved)
    }
}

async fn parallel_trace_imports(
    handle: &Handle,
    source_id: SourceID,
) -> CompileResult<HashMap<SourceID, ArcBorrow<'_, DeclaredNames>>> {
    let mut visited = HashMap::from([(source_id, None)]);
    let mut to_visit = vec![source_id];
    let mut in_progress = FuturesUnordered::new();

    loop {
        while let Some(next) = to_visit.pop() {
            debug_assert!(visited.contains_key(&next));
            in_progress.push(async move { (next, handle.query::<ComputeDeclaredNames>(next).await) });
        }
        if let Some((next_id, next_declared)) = in_progress.next().await {
            let next_declared = next_declared?;
            *visited.get_mut(&next_id).unwrap() = Some(next_declared);

            for &import in &next_declared.direct_imports {
                if visited.entry(import).or_default().is_none() {
                    to_visit.push(import);
                }
            }
        } else {
            break;
        }
    }

    Ok(HashMap::from_iter(visited.into_iter().map(|(s, opt)| (s, opt.unwrap()))))
}

pub struct ComputeDefinedTypes {
    root_namespace: Namespace,
}

impl ComputeDefinedTypes {
    pub fn new(strings: &Interner) -> Self {
        let mut root_namespace = Namespace::new(None);
        root_namespace.insert(strings.get_or_intern_static("u32"), Value::TypeAlias(Type::Integer), None).unwrap();

        ComputeDefinedTypes { root_namespace }
    }
}

impl Processor for ComputeDefinedTypes {
    type Input = SourceID;
    type Output = DefinedTypes;

    async fn process(handle: Handle, source_id: SourceID) -> CompileResult<DefinedTypes> {
        let all_dependencies = parallel_trace_imports(&handle, source_id).await?;
        let ast = &all_dependencies[&source_id].ast;

        let resolver = NamespaceResolver::new(
            &handle,
            all_dependencies.iter().map(|(&source, &declared)| (source, &declared.get().file_namespace)).collect(),
        );

        let mut structs = HashMap::new();
        for top_level in ast.top_levels() {
            match *top_level {
                ast::TopLevel::Struct(ast_struct) => {
                    let struct_ = ast.get_node(ast_struct);
                    let mut fields = HashMap::new();
                    for &ast_field in ast.get_list(struct_.fields) {
                        let field = ast.get_node(ast_field);
                        let field_type = resolver.resolve_type(NamespaceRef::Module(source_id), ast, field.ty)?;
                        fields.insert(field.name, field_type); // todo check no duplicates
                    }
                    structs.insert(ast_struct, StructDefinition { fields }); // todo check no duplicates
                }
                ast::TopLevel::Import(_) => (),
                ast::TopLevel::Function(_) => (),
            }
        }

        Ok(DefinedTypes { dependencies: all_dependencies.keys().copied().collect(), structs })
    }
}
