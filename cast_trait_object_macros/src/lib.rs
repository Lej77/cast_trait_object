use {
    either::*,
    proc_macro::TokenStream,
    proc_macro2::{Span, TokenStream as TokenStream2},
    proc_macro_error::*,
    quote::{quote, ToTokens},
    std::{env, iter, path::PathBuf},
    syn::{
        parse::{Parse, ParseStream, Parser},
        parse_macro_input,
        punctuated::Punctuated,
        GenericParam, Generics, Ident, Item, ItemEnum, ItemImpl, ItemStruct, ItemTrait, ItemUnion,
        LifetimeDef, Path, Token, TraitBound, TraitBoundModifier, Type, TypePath, Visibility,
    },
};

#[proc_macro_error]
#[proc_macro_attribute]
pub fn dyn_upcast(attr: TokenStream, item: TokenStream) -> TokenStream {
    let item = parse_macro_input!(item as Item);
    common(
        item,
        MacroInfo {
            macro_type: MacroType::Upcast,
            attr: attr.into(),
        },
    )
    .into()
}

#[proc_macro_error]
#[proc_macro_attribute]
pub fn dyn_cast(attr: TokenStream, item: TokenStream) -> TokenStream {
    let item = parse_macro_input!(item as Item);
    common(
        item,
        MacroInfo {
            macro_type: MacroType::Cast,
            attr: attr.into(),
        },
    )
    .into()
}

#[derive(Clone, Copy, Debug, Ord, PartialOrd, Eq, PartialEq)]
enum MacroType {
    Upcast,
    Cast,
}
impl MacroType {
    fn name(&self) -> &'static str {
        match self {
            Self::Upcast { .. } => "#[dyn_upcast]",
            Self::Cast { .. } => "#[dyn_cast]",
        }
    }
}

struct MacroInfo {
    macro_type: MacroType,
    attr: TokenStream2,
}
impl MacroInfo {
    fn name(&self) -> &'static str {
        self.macro_type.name()
    }
    /// Returns `None` if we should cast to `Self`.
    fn trait_target(self) -> Option<impl Iterator<Item = Path>> {
        let name = self.name();
        let attr = self.attr;
        match self.macro_type {
            MacroType::Upcast => {
                if !attr.is_empty() {
                    abort!(
                        attr,
                        "{} doesn't take any arguments when used on a trait.",
                        name
                    );
                }
                None
            }
            MacroType::Cast => {
                let parser = Punctuated::<Path, Token![,]>::parse_terminated;
                let list = parser.parse2(attr).expect_or_abort("expected a comma separated list of paths to traits which this trait should support casting into.");
                if list.is_empty() {
                    abort!(list, "expected a comma separated list of paths to traits which this trait should support casting into.");
                }
                Some(list.into_iter())
            }
        }
    }
    /// Use when we don't know the source trait. This will require that users specify both
    /// the source and target traits (`Source => Target`) for casting and only the source
    /// (`Source`) when upcasting.
    fn full_cast_config(self) -> impl Iterator<Item = (Path, Path)> {
        let attr = self.attr;
        match self.macro_type {
            MacroType::Upcast => {
                let parser = Punctuated::<Path, Token![,]>::parse_terminated;
                let list = parser.parse2(attr)
                    .expect_or_abort("expected a comma separated list of paths to traits that this type implements and wants to support upcasting into.");
                if list.is_empty() {
                    abort!(list, "expected a comma separated list of paths to traits that this type implements and wants to support upcasting into.");
                }
                Left(list.into_iter().map(|path| (path.clone(), path)))
            }
            MacroType::Cast => {
                let list = syn::parse2::<SourceWithTargets>(attr)
                    .expect_or_abort("expected a source trait that this type implements followed by an `=>` and then a comma separated list of paths to traits that this type should support casting into.");
                let source = list.source;
                Right(
                    list.targets
                        .into_iter()
                        .map(move |target| (source.clone(), target)),
                )
            }
        }
    }
}

struct SourceWithTargets {
    source: Path,
    #[allow(dead_code)]
    arrow: Token![=>],
    targets: Punctuated<Path, Token![,]>,
}
impl Parse for SourceWithTargets {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let this = Self {
            source: input.parse()?,
            arrow: input.parse()?,
            targets: input.parse_terminated(Path::parse)?,
        };
        if this.targets.first().is_none() {
            Err(input.error("expected at least one path"))
        } else {
            Ok(this)
        }
    }
}

fn common(item: Item, info: MacroInfo) -> TokenStream2 {
    // If something goes wrong just output the input:
    set_dummy(item.to_token_stream());

    match item {
        Item::Trait(item) => {
            let targets = match info.trait_target() {
                Some(v) => Left(v),
                None => Right(iter::once({
                    let ident = &item.ident;
                    // Parameters without any bounds:
                    let params = item.generics.split_for_impl().1;
                    if item.generics.params.is_empty() {
                        Path::from(item.ident.clone())
                    } else {
                        syn::parse2::<Path>(quote! { #ident #params })
                            .expect("internal error: failed to generate a path to the defined trait")
                    }
                })),
            };
            add_dyn_cast_super_traits(item, targets)
        }
        Item::Enum(ItemEnum {
            ref ident,
            ref generics,
            ..
        })
        | Item::Struct(ItemStruct {
            ref ident,
            ref generics,
            ..
        })
        | Item::Union(ItemUnion {
            ref ident,
            ref generics,
            ..
        }) => {
            let extra = generate_dyn_cast_impl(
                TypePath {
                    qself: None,
                    path: ident.clone().into(),
                }
                .into(),
                generics,
                info.full_cast_config(),
            );
            // Keep the original item unmodified and just append to it:
            let mut stream = item.into_token_stream();
            stream.extend(extra);
            stream
        }
        Item::Impl(ItemImpl {
            ref generics,
            ref self_ty,
            trait_: Some(ref trait_),
            ..
        }) => {
            let targets = match info.trait_target() {
                Some(v) => Left(v),
                None => Right(iter::once(Path::from(trait_.1.clone())))
            };
            let extra = generate_dyn_cast_impl(
                (**self_ty).clone(),
                generics,
                targets.map(|target| (trait_.1.clone(), target))
            );
            // Keep the original item unmodified and just append to it:
            let mut stream = item.into_token_stream();
            stream.extend(extra);
            stream
        }
        other => abort!(
            other,
            "{} can only be used on trait, struct, enum or union definitions and on trait implementations.",
            info.name()
        ),
    }
}

fn add_dyn_cast_super_traits(
    mut trait_def: ItemTrait,
    targets: impl Iterator<Item = Path>,
) -> TokenStream2 {
    let my_crate = my_crate();
    let mut output = TokenStream2::new();
    for target in targets {
        // Try to generate a unique name for the config type:
        let config_name = {
            let target_name = target
                .segments
                .iter()
                .map(|path_seg| path_seg.ident.to_string())
                .collect::<Vec<_>>();
            let target_name = target_name.join("_");

            Ident::new(
                &format!("__{}To{}DynCastConfig", trait_def.ident, target_name),
                Span::call_site(),
            )
        };
        let config_path = {
            let trait_vis = &trait_def.vis;
            let source_ident = &trait_def.ident;

            let generics = &mut trait_def.generics;
            move_bounds_to_where_clause(generics);
            let where_clause = generics
                .where_clause
                .as_ref()
                .map(|where_clause| &where_clause.predicates);
            let params = &generics.params;
            let phantom_marker: Punctuated<TokenStream2, Token![,]> = params
                .iter()
                .map(|param| {
                    if let syn::GenericParam::Lifetime(_) = param {
                        quote!(::core::marker::PhantomData<&#param ()>)
                    } else {
                        quote!(::core::marker::PhantomData<#param>)
                    }
                })
                .collect();

            match trait_vis {
                Visibility::Public(_) | Visibility::Crate(_) => {
                    // Hide generated config types in private modules so that they aren't
                    // exposed to users of crates that makes use of this macro:
                    output.extend(quote! {
                        #[doc(hidden)]
                        mod #config_name {
                            #trait_vis struct Config<#params>(#phantom_marker) where #where_clause;
                        }
                        #my_crate::impl_dyn_cast_config!(
                            for<#params> #config_name::Config<#params> where {#where_clause} = #source_ident<#params> => #target
                        );
                    });
                    quote! { #config_name::Config<#params> }
                }
                Visibility::Restricted(_) | Visibility::Inherited => {
                    // It would be hard to modify the `Restricted` path to be valid
                    // in another module and since the type's visibility is restricted
                    // it won't be visible to users of the current crate anyway, so
                    // lets just generate it in the current module. (For `Inherited`
                    // visibility there is nothing to gain from putting the type
                    // inside another module as the type can't be less visible than
                    // it already is.)
                    output.extend(quote! {
                        #my_crate::create_dyn_cast_config!(
                            #[doc(hidden)]
                            #trait_vis #config_name<#params> where {#where_clause} = #source_ident<#params> => #target
                        );
                    });
                    quote! { #config_name<#params> }
                }
            }
        };
        trait_def.supertraits.push(
            TraitBound {
                paren_token: None,
                modifier: TraitBoundModifier::None,
                lifetimes: None,
                path: syn::parse2(quote! { #my_crate::DynCast<#config_path> })
                    .expect("internal error: failed to generate a supertrait bound"),
            }
            .into(),
        )
    }
    output.extend(trait_def.into_token_stream());
    output
}

fn generate_dyn_cast_impl(
    self_type: Type,
    generics: &Generics,
    config: impl Iterator<Item = (Path, Path)>,
) -> TokenStream2 {
    let mut generics = generics.clone();
    move_bounds_to_where_clause(&mut generics);
    let where_clause = generics
        .where_clause
        .as_ref()
        .map(|where_clause| &where_clause.predicates)
        .filter(|predicates| !predicates.is_empty());
    let params = &generics.params;

    let my_crate = my_crate();
    let mut output = TokenStream2::new();
    for (source, target) in config {
        if params.is_empty() && where_clause.is_none() {
            // This macro implementation is currently simpler and might allow for
            // foreign types (more relaxed regarding orphan rules) so we use it
            // when we can.
            output.extend(quote! {
                #my_crate::impl_dyn_cast!(#self_type as #source => #target);
            });
        } else {
            output.extend(quote! {
                #my_crate::impl_dyn_cast!(for<#params> #self_type as #source where {#where_clause} => #target);
            });
        }
    }
    output
}

/// Move any bounds in the parameters into the where clause.
///
/// For example `<T: Clone>` would be changed into `<T> where T: Clone`.
fn move_bounds_to_where_clause(generics: &mut Generics) {
    generics.make_where_clause();
    let where_clause = generics.where_clause.as_mut().unwrap();
    for outer_param in generics.params.iter_mut() {
        match outer_param {
            GenericParam::Type(param) => {
                let ident = &param.ident;
                let bounds = &param.bounds;
                if !bounds.is_empty() {
                    where_clause.predicates.push(
                        syn::parse2(quote! {#ident: #bounds})
                            .expect("internal error: failed to generate a type bound"),
                    );
                }
                *outer_param = GenericParam::Type(ident.clone().into());
            }
            GenericParam::Lifetime(param) => {
                let lifetime = &param.lifetime;
                let bounds = &param.bounds;
                if !bounds.is_empty() {
                    where_clause.predicates.push(
                        syn::parse2(quote! {#lifetime: #bounds})
                            .expect("internal error: failed to generate a lifetime bound"),
                    );
                }
                *outer_param =
                    GenericParam::Lifetime(LifetimeDef::new(param.lifetime.clone()).into());
            }
            GenericParam::Const(param) => {
                param.attrs.clear();
                param.eq_token.take();
                param.default.take();
            }
        }
    }
}

/// Get an identifier that resolves to the current crate. Can be used where `$crate`
/// would be used in a declarative macro.
fn my_crate() -> TokenStream2 {
    const ORIGINAL_NAME: &str = "cast_trait_object";

    let is_test = {
        // This is only true if we are compiling the parent directory:
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .and_then(|macro_crate| {
                let macro_crate = macro_crate.join("cast_trait_object");
                let current = PathBuf::from(env::var_os("CARGO_MANIFEST_DIR")?);
                Some(macro_crate == PathBuf::from(current))
            })
    };
    if is_test.unwrap_or(false) {
        quote!(crate)
    } else {
        let name = proc_macro_crate::crate_name(ORIGINAL_NAME).unwrap_or_else(|e| {
            abort_call_site!(
                "expected `{}` to be present in `Cargo.toml`: {}",
                ORIGINAL_NAME,
                e
            );
            // ORIGINAL_NAME.to_string()
        });
        let ident = Ident::new(&name, Span::call_site());
        quote! { ::#ident }
    }
}
