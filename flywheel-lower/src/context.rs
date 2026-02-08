use std::collections::HashMap;
use flywheel_ast as ast;
use flywheel_sources::{SourceMap, Symbol};
use std::sync::Arc;
use paste::paste;
use by_address::ByAddress;
use crate::namespace::{Builtin, Item, Namespace};
use crate::Type;

macro_rules! lowering_context {
    { $vis:vis struct $name:ident<$li:lifetime> { $($default_field:ident: $default_field_ty:ty,)* $([$stage:ident] $($field:ident: $field_ty:ty,)*)* } } => {
        paste! {
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
        paste! {
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

lowering_context! {
    pub struct LoweringContext<'ast> {
        ast: &'ast ast::Module,
        builtins: &'ast HashMap<&'static str, Symbol>,

        [CollectedNames]
        file_namespaces: HashMap<&'ast [Symbol], Arc<Namespace<'ast>>>,
        prelude_ns: Arc<Namespace<'ast>>,

        [StructFields]
        struct_fields: HashMap<ByAddress<&'ast ast::Struct<'ast>>, HashMap<Symbol, Type<'ast>>>,
    }
}
