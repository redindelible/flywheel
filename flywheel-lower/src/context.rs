use std::collections::HashMap;
use std::ops::Index;

use by_address::ByAddress;
use flywheel_ast as ast;
use flywheel_exchange as ex;
use flywheel_sources::Symbol;
use crate::Builtins;
use crate::types::Type;
use crate::namespace::Namespace;

macro_rules! lowering_context {
    { $vis:vis struct $name:ident<$li:lifetime> { $($default_field:ident: $default_field_ty:ty,)* $([$stage:ident] $($field:ident: $field_ty:ty,)*)* } } => {
        ::paste::paste! {
            #[repr(transparent)] $vis struct $name<$li, S>([<$name Data>]<$li>, ::std::marker::PhantomData<S>);

            struct [<$name Data>]<$li> {
                $($default_field: $default_field_ty,)+
                $([<$stage:snake>]: Option<[<$stage Data>]<$li>>,)*
            }

            impl<$li> $name<$li, ()> {
                pub fn new($($default_field: $default_field_ty),+) -> $name<$li, ()> {
                    $name([<$name Data>] { $($default_field),+, $([<$stage:snake>]: ::std::option::Option::None),*  }, ::std::marker::PhantomData)
                }

                $(
                pub fn $default_field(&self) -> &$default_field_ty {
                    &self.0.$default_field
                }
                )+
            }
        }

        lowering_stage_rec! { $vis $name $li () $([$stage] $($field: $field_ty,)*)* }
    };
}

macro_rules! lowering_stage_rec {
    ($vis:vis $name:ident $li:lifetime $prev:ty [$stage:ty] $($field:ident: $field_ty:ty,)* $([$rest_stage:ident] $($rest_field:ident: $rest_field_ty:ty,)*)*) => {
        lowering_stage! { $vis $name $li $prev [$stage] $($field: $field_ty,)* }

        lowering_stage_rec! { $vis $name $li $stage $([$rest_stage] $($rest_field: $rest_field_ty,)*)* }
    };

    ($vis:vis $name:ident $li:lifetime $prev:ty) => {

    };
}

macro_rules! lowering_stage {
    ($vis:vis $name:ident $li:lifetime $prev:ty [$stage:ty] $($field:ident: $field_ty:ty,)*) => {
        ::paste::paste! {
        $vis struct $stage;

        $vis struct [<$stage Data>] <$li> {
            $(pub $field: $field_ty,)+
        }

        impl<$li> $name<$li, $stage> {
            pub fn [<$stage:snake>](&self) -> &[<$stage Data>]<$li> {
                unsafe { self.0.[<$stage:snake>].as_ref().unwrap_unchecked() }
            }
        }

        impl<$li> ::std::ops::Deref for $name<$li, $stage> {
            type Target = $name<$li, $prev>;

            fn deref(&self) -> &Self::Target {
                unsafe { ::std::mem::transmute(self) }
            }
        }

        impl<$li> $name<$li, $prev> {
            pub fn [<add_ $stage:snake>](self, stage_data: [<$stage Data>]<$li>) -> $name<$li, $stage> {
                $name([<$name Data>] { [<$stage:snake>]: Some(stage_data), ..self.0 }, ::std::marker::PhantomData)
            }
        }
        }
    };
}

pub struct AstMap<'ast, N, V>(HashMap<ByAddress<&'ast N>, V>);

impl<'ast, N, V> AstMap<'ast, N, V> {
    pub fn new() -> Self {
        AstMap(HashMap::new())
    }

    pub fn insert(&mut self, node: &'ast N, value: V) {
        assert!(self.0.insert(ByAddress(node), value).is_none());
    }
}

impl<'ast, N, V> Index<&'ast N> for AstMap<'ast, N, V> {
    type Output = V;

    fn index(&self, index: &'ast N) -> &V {
        &self.0[&ByAddress(index)]
    }
}

pub struct FunctionSignature<'ast> {
    pub return_type: Type<'ast>,
    pub id: ex::FunctionId,
}

lowering_context! {
    pub struct LoweringContext<'ast> {
        ast: &'ast ast::Module,
        builtins: &'ast Builtins,

        [CollectedNames]
        prelude_ns: Namespace<'ast>,
        file_namespaces: HashMap<&'ast [Symbol], Namespace<'ast>>,

        [AllStructFields]
        all_struct_fields: AstMap<'ast, ast::Struct<'ast>, HashMap<Symbol, Type<'ast>>>,

        [AllFunctionSignatures]
        signatures: AstMap<'ast, ast::Function<'ast>, FunctionSignature<'ast>>,
    }
}
