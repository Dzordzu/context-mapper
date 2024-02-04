extern crate proc_macro;

use proc_macro2::TokenTree;
use quote::{quote, ToTokens};
use std::collections::BTreeMap;
use std::collections::HashMap;
use syn::parse_macro_input;
use syn::punctuated::Punctuated;
use syn::token::PathSep;
use syn::DeriveInput;
use syn::Field;
use syn::Fields;
use syn::Ident;
use syn::PathSegment;
use syn::Token;

#[derive(Clone, Debug)]
struct Member {
    ident: Ident,
    name: String,
    rename: HashMap<syn::Path, String>,
    converter: HashMap<syn::Path, syn::Path>,
    skip_contexts: Vec<syn::Path>,
    move_contexts: Vec<syn::Path>,
}

type Members = BTreeMap<String, Member>;

#[derive(Debug)]
enum TraitNaming {
    /// Existing traits have to be (&self) -> HashMap<String, ContextType>
    ExistingGeneric {
        path: syn::Path,
        method_name: Ident,
    },
    Existing {
        path: syn::Path,
        method_name: Ident,
    },
    // Use generate_mapper_trait!(name_of_the_trait, method_name)
    // Use generate_mapper_trait_generic!(name_of_the_trait, method_name)
    //
    // Generate {
    //     /// By default IntoType
    //     name: Option<Ident>,
    //     /// By default into_type_map
    //     method_name: Option<Ident>,
    // },
    // GenerateGeneric {
    //     /// By default IntoType
    //     name: Option<Ident>,
    //     /// By default into_type_map
    //     method_name: Option<Ident>,
    // },
}

impl Default for TraitNaming {
    fn default() -> Self {
        Self::ExistingGeneric {
            path: syn::parse2(quote! {::context_mapper::IntoType}).expect("1"),
            method_name: syn::parse2(quote! {into_type_map}).expect("2"),
        }
    }
}

#[derive(Debug)]
struct CommonFields {
    ctx: syn::Path,
    ctx_type: syn::Type,
    converter: syn::Path,
    move_type: bool,
}

#[derive(Debug)]
struct Trait {
    ctx: syn::Path,
    ctx_type: syn::Type,
    converter: syn::Path,
    move_type: bool,
    naming: TraitNaming,
}

#[derive(Debug)]
struct Function {
    ctx: syn::Path,
    ctx_type: syn::Type,
    converter: syn::Path,
    move_type: bool,
    naming: Ident,
    visibility: syn::Visibility,
}

#[derive(Debug, Default)]
struct ContextConfig {
    /// By default traits are using IntoType from the context-mapper crate
    pub traits: BTreeMap<String, Trait>,
    pub functions: BTreeMap<String, Function>,
    pub impls: BTreeMap<String, Function>
}

fn read_trait_naming(inpname: &str, input: &proc_macro2::TokenStream) -> TraitNaming {
    let params = group_attributes(
        input.clone(),
        &[],
        &["path", "method_name"]
            .into_iter()
            .map(|x| x.to_string())
            .collect::<Vec<String>>(),
    );

    let path: Option<syn::Path> = params
        .iter()
        .filter(|x| x.0 == "path")
        .last()
        .map(|x| syn::parse2(x.1.clone()).expect("name should be parseable to ident"));

    let method_name: Option<syn::Ident> = params
        .iter()
        .filter(|x| x.0 == "method_name")
        .last()
        .map(|x| syn::parse2(x.1.clone()).expect("method_name should be parseable to ident"));

    match inpname {
        "simple" => TraitNaming::Existing {
            path: path.expect("Expected path"),
            method_name: method_name.expect("Excepected mehtod_name"),
        },
        "generic" => TraitNaming::ExistingGeneric {
            path: path.expect("Expected path"),
            method_name: method_name.expect("Expected method_name"),
        },
        _ => unreachable!("Unknown naming"),
    }
}

fn read_common_fields(groups: &[(String, proc_macro2::TokenStream)]) -> CommonFields {
    let ctx: syn::Path = groups
        .iter()
        .filter(|x| x.0 == "context")
        .last()
        .map(|x| syn::parse2(x.1.clone()).expect("context mapping error"))
        .unwrap_or(syn::parse2(quote! { default }).expect("?"));

    let ctx_type: syn::Type = groups
        .iter()
        .filter(|x| x.0 == "type")
        .last()
        .map(|x| syn::parse2(x.1.clone()).expect("type mapping error"))
        .unwrap_or(syn::parse2(quote! { std::string::String }).expect("?"));

    let converter: syn::Path = groups
        .iter()
        .filter(|x| x.0 == "converter")
        .last()
        .map(|x| syn::parse2(x.1.clone()).expect("converter mapping errro"))
        .unwrap_or(syn::parse2(quote! { std::string::ToString::to_string }).expect("?"));

    let move_type = groups
        .iter()
        .filter_map(|x| {
            if x.0 == "move" {
                let b: syn::LitBool = syn::parse2(x.1.clone()).expect("Skip expected bool");
                Some(b.value)
            } else {
                None
            }
        })
        .last()
        .unwrap_or(false);

    CommonFields {
        ctx,
        ctx_type,
        converter,
        move_type,
    }
}

impl ContextConfig {
    pub fn add_trait(&mut self, input: proc_macro2::TokenStream) -> &mut Self {
        let grp_attrs: Vec<String> = ["simple", "generic"]
            .into_iter()
            .map(|x| x.into())
            .collect();

        let groups = group_attributes(
            input,
            &grp_attrs,
            &["context", "type", "converter", "move"]
                .into_iter()
                .map(|x| x.into())
                .collect::<Vec<String>>(),
        );

        let naming_base = groups
            .iter()
            .filter(|x| grp_attrs.contains(&x.0))
            .last()
            .clone();

        let naming = naming_base
            .map(|x| read_trait_naming(&x.0, &x.1))
            .unwrap_or(TraitNaming::default());

        let common_fields = read_common_fields(&groups);

        self.traits.insert(
            common_fields.ctx.to_token_stream().to_string(),
            Trait {
                ctx: common_fields.ctx,
                ctx_type: common_fields.ctx_type,
                converter: common_fields.converter,
                naming,
                move_type: common_fields.move_type,
            },
        );

        self
    }
    fn add_fun_base(input: proc_macro2::TokenStream) -> Function {
        let groups = group_attributes(
            input,
            &[],
            &[
                "context",
                "type",
                "converter",
                "move",
                "naming",
                "fn",
                "visibility",
                "vis",
            ]
            .into_iter()
            .map(|x| x.into())
            .collect::<Vec<String>>(),
        );

        let common_fields = read_common_fields(&groups);

        let naming: syn::Ident = groups
            .iter()
            .filter(|x| x.0 == "naming" || x.0 == "fn")
            .last()
            .map(|x| syn::parse2(x.1.clone()).expect("naming mapping error"))
            .expect("naming field undefined for funcion mapping");

        let visibility: syn::Visibility = groups
            .iter()
            .filter(|x| x.0 == "visibility" || x.0 == "vis")
            .last()
            .map(|x| syn::parse2(x.1.clone()).expect("visibility mapping error"))
            .unwrap_or(syn::Visibility::Inherited);

        Function {
            ctx: common_fields.ctx,
            ctx_type: common_fields.ctx_type,
            converter: common_fields.converter,
            move_type: common_fields.move_type,
            naming,
            visibility,
        }
    }
    pub fn add_function(&mut self, input: proc_macro2::TokenStream) -> &mut Self {
        let base = Self::add_fun_base(input);

        self.functions.insert(
            base.ctx.to_token_stream().to_string(),
            base
        );

        self
    }

    pub fn add_impl(&mut self, input: proc_macro2::TokenStream) -> &mut Self {
        let base = Self::add_fun_base(input);

        self.impls.insert(
            base.ctx.to_token_stream().to_string(),
            base
        );

        self
    }
}

fn path_is_allowed(ident: syn::Path, allowed: &[String]) -> bool {
    allowed.contains(&ident.to_token_stream().to_string())
}

fn group_attributes(
    input: proc_macro2::TokenStream,
    allowed_group_idents: &[String],
    allowed_eq_idents: &[String],
) -> Vec<(String, proc_macro2::TokenStream)> {
    let mut groups: Vec<(String, proc_macro2::TokenStream)> = Vec::new();

    let mut last_eq = false;
    let mut last_path: syn::Path = syn::Path {
        leading_colon: None,
        segments: Punctuated::new(),
    };
    let mut last_stream: Vec<TokenTree> = Vec::new();
    let iterator = input.into_iter();

    for tk in iterator {
        if last_eq {
            match tk.clone() {
                TokenTree::Ident(_) | TokenTree::Literal(_) | TokenTree::Group(_) => {
                    last_stream.push(tk);
                    continue;
                }
                TokenTree::Punct(x) if x.to_string() != "," => {
                    last_stream.push(x.into());
                    continue;
                }
                _ => last_eq = false,
            }
        }
        match tk {
            TokenTree::Group(group) => {
                if path_is_allowed(last_path.clone(), &allowed_group_idents) {
                    let last_path = last_path.clone().to_token_stream().to_string();
                    groups.push((last_path, group.stream()));
                } else {
                    let loc = &group.span().source_text().unwrap();
                    let last_ident = last_path.into_token_stream().to_string();
                    panic!("Unknown identifier detected ({last_ident}); allowed: {allowed_group_idents:?}\n{loc}");
                }
            }
            TokenTree::Ident(ident) => {
                last_path.segments.push(PathSegment::from(ident));
            }
            TokenTree::Punct(punct) => match punct.as_char() {
                ',' => {
                    if !last_stream.is_empty() {
                        groups.push((
                            last_path.clone().into_token_stream().to_string(),
                            proc_macro2::TokenStream::from_iter(last_stream.clone().into_iter()),
                        ));
                    } else {
                        if !last_path.segments.is_empty()
                            && path_is_allowed(last_path.clone(), &allowed_eq_idents)
                        {
                            groups.push((
                                last_path.clone().into_token_stream().to_string(),
                                quote! {true},
                            ));
                        }
                    }
                    last_stream.clear();
                    last_path.segments.clear()
                }
                '.' | ':' => {
                    if !last_path.segments.trailing_punct() {
                        last_path.segments.push_punct(PathSep::default())
                    }
                }
                '=' => {
                    if path_is_allowed(last_path.clone(), &allowed_eq_idents) {
                        last_eq = true
                    } else {
                        let lp = last_path.to_token_stream().to_string();
                        panic!("Identifier {lp:?} not allowed. Allowed:
                       {allowed_eq_idents:?}")
                    }
                }
                _ => panic!("Unknown punctuation detected"),
            },
            TokenTree::Literal(_) => panic!("Literal not allowed in the context definitions"),
        }
    }
    if !last_stream.is_empty() {
        groups.push((
            last_path.clone().into_token_stream().to_string(),
            proc_macro2::TokenStream::from_iter(last_stream.clone().into_iter()),
        ));
    } else {
        if !last_path.segments.is_empty() && path_is_allowed(last_path.clone(), &allowed_eq_idents)
        {
            groups.push((
                last_path.clone().into_token_stream().to_string(),
                quote! {true},
            ));
        }
    }

    groups
}

fn read_struct_config(input: &DeriveInput) -> ContextConfig {
    let mut result = ContextConfig::default();

    for attribute in input.attrs.clone().into_iter().filter_map(|x| {
        if x.path().is_ident("context_mapper") {
            Some(x.meta.require_list().and_then(|y| Ok(y.tokens.clone())))
        } else {
            None
        }
    }) {
        let groups = group_attributes(
            attribute.expect("Expected list of attributes"),
            &["trait", "function", "impl"]
                .into_iter()
                .map(|x| x.into())
                .collect::<Vec<String>>(),
            &[],
        );

        for group in groups {
            match group.0.as_ref() {
                "trait" => result.add_trait(group.1),
                "function" => result.add_function(group.1),
                "impl" => result.add_impl(group.1),
                _ => unreachable!("Attribtues not parsed properly"),
            };
        }
    }

    result
}

fn read_member(member: &Field) -> Member {
    let ident = member.ident.clone().unwrap();

    let mut result = Member {
        ident: ident.clone(),
        name: ident.to_string(),
        rename: Default::default(),
        skip_contexts: Default::default(),
        converter: Default::default(),
        move_contexts: Default::default(),
    };

    for config in member.attrs.clone().into_iter().filter_map(|x| {
        if x.path().is_ident("context_attribute") {
            Some(x.meta.require_list().and_then(|y| Ok(y.tokens.clone())))
        } else {
            None
        }
    }) {
        let groups = group_attributes(
            config.expect("Expected list of attribtues for {ident}"),
            &["context".to_string()],
            &[],
        );

        for group in groups {
            let attrs = group_attributes(
                group.1,
                &[],
                &["name", "skip", "converter", "rename", "move"]
                    .into_iter()
                    .map(|x| x.into())
                    .collect::<Vec<String>>(),
            );

            let name: syn::Path = syn::parse2(
                attrs
                    .iter()
                    .filter_map(|x| {
                        if x.0 == "name" {
                            Some(x.1.clone())
                        } else {
                            None
                        }
                    })
                    .last()
                    .unwrap_or(quote! { default }),
            )
            .unwrap();

            let skip = attrs
                .iter()
                .filter_map(|x| {
                    if x.0 == "skip" {
                        let b: syn::LitBool = syn::parse2(x.1.clone()).expect("Skip expected bool");
                        Some(b.value)
                    } else {
                        None
                    }
                })
                .last()
                .unwrap_or(false);

            let converter: Option<syn::Path> = attrs
                .iter()
                .filter(|x| x.0 == "converter")
                .last()
                .map(|x| syn::parse2(x.1.clone()).expect("converter mapping errro"));

            let rename = attrs
                .iter()
                .filter_map(|x| {
                    if x.0 == "rename" {
                        let b: syn::LitStr = syn::parse2(x.1.clone()).expect("Rename expected str");
                        Some(b.value())
                    } else {
                        None
                    }
                })
                .last();

            let move_type = attrs
                .iter()
                .filter_map(|x| {
                    if x.0 == "move" {
                        let b: syn::LitBool = syn::parse2(x.1.clone()).expect("Skip expected bool");
                        Some(b.value)
                    } else {
                        None
                    }
                })
                .last()
                .unwrap_or(false);

            if skip {
                result.skip_contexts.push(name.clone());
            }
            if let Some(converter) = converter {
                result.converter.insert(name.clone(), converter);
            }
            if let Some(rename) = rename {
                result.rename.insert(name.clone(), rename);
            }
            if move_type {
                result.move_contexts.push(name);
            }
        }
    }

    result
}

fn read_members(input: &DeriveInput) -> Members {
    let mut members = Members::new();

    match &input.data {
        syn::Data::Struct(x) => match &x.fields {
            Fields::Named(named) => {
                for field in named.named.iter() {
                    let member = read_member(field);
                    members.insert(member.name.clone(), member);
                }
            }
            Fields::Unnamed(_) => panic!("Unnamed fields are currently unsupported"),
            Fields::Unit => panic!("Unit structs are unsupported"),
        },
        _ => panic!("Currently only structs are supported"),
    }

    members
}

fn generate_impls(
    name: &Ident,
    config: &ContextConfig,
    members: &Members,
) -> proc_macro2::TokenStream {
    let mut impls: Vec<proc_macro2::TokenStream> = Vec::new();

    for trt in config.impls.iter() {
        let ctx = &trt.1.ctx;
        let ctx_type = &trt.1.ctx_type;
        let naming = &trt.1.naming;
        let visibility = &trt.1.visibility;

        let members_code = members.iter().fold(
            quote! {
                let mut result = std::collections::HashMap::new();
            },
            |acc, member| {
                if member.1.skip_contexts.contains(ctx) {
                    acc
                } else {
                    let field = &member.1.ident;
                    let name = member
                        .1
                        .rename
                        .get(ctx)
                        .cloned()
                        .unwrap_or(member.1.name.clone());
                    let converter = member
                        .1
                        .converter
                        .get(ctx)
                        .cloned()
                        .unwrap_or(trt.1.converter.clone());

                    let move_type = if member.1.move_contexts.contains(ctx) || trt.1.move_type {
                        quote! {}
                    } else {
                        quote! {&}
                    };

                    quote! {
                        #acc
                        result.insert(#name.to_string(), #converter(#move_type self.#field));
                    }
                }
            },
        );
        let members_code = quote! {
            #members_code
            result
        };

        impls.push(quote! {
            #visibility fn #naming(&self) -> std::collections::HashMap<String, #ctx_type> {
                #members_code
            }
        })
    }

    let partial = impls.into_iter().fold(quote! {}, |acc, x| {
        quote! {
            #acc
            #x
        }
    });

    quote! {
        impl #name {
            #partial
        }
    }
}

fn generate_functions(
    name: &Ident,
    config: &ContextConfig,
    members: &Members,
) -> proc_macro2::TokenStream {
    let mut functions: Vec<proc_macro2::TokenStream> = Vec::new();

    for trt in config.functions.iter() {
        let ctx = &trt.1.ctx;
        let ctx_type = &trt.1.ctx_type;
        let naming = &trt.1.naming;
        let visibility = &trt.1.visibility;

        let members_code = members.iter().fold(
            quote! {
                let mut result = std::collections::HashMap::new();
            },
            |acc, member| {
                if member.1.skip_contexts.contains(ctx) {
                    acc
                } else {
                    let field = &member.1.ident;
                    let name = member
                        .1
                        .rename
                        .get(ctx)
                        .cloned()
                        .unwrap_or(member.1.name.clone());
                    let converter = member
                        .1
                        .converter
                        .get(ctx)
                        .cloned()
                        .unwrap_or(trt.1.converter.clone());

                    let move_type = if member.1.move_contexts.contains(ctx) || trt.1.move_type {
                        quote! {}
                    } else {
                        quote! {&}
                    };

                    quote! {
                        #acc
                        result.insert(#name.to_string(), #converter(#move_type arg.#field));
                    }
                }
            },
        );
        let members_code = quote! {
            #members_code
            result
        };

        functions.push(quote! {
            #visibility fn #naming(arg: &#name) -> std::collections::HashMap<String, #ctx_type> {
                #members_code
            }
        })
    }

    functions.into_iter().fold(quote! {}, |acc, x| {
        quote! {
            #acc
            #x
        }
    })
}

fn generate_traits_impl(
    name: &Ident,
    config: &ContextConfig,
    members: &Members,
) -> proc_macro2::TokenStream {
    let mut impls: Vec<proc_macro2::TokenStream> = Vec::new();

    for trt in config.traits.iter() {
        let ctx = &trt.1.ctx;
        let ctx_type = &trt.1.ctx_type;

        let members_code = members.iter().fold(
            quote! {
                let mut result = std::collections::HashMap::new();
            },
            |acc, member| {
                if member.1.skip_contexts.contains(ctx) {
                    acc
                } else {
                    let field = &member.1.ident;
                    let name = member
                        .1
                        .rename
                        .get(ctx)
                        .cloned()
                        .unwrap_or(member.1.name.clone());
                    let converter = member
                        .1
                        .converter
                        .get(ctx)
                        .cloned()
                        .unwrap_or(trt.1.converter.clone());

                    let move_type = if member.1.move_contexts.contains(ctx) || trt.1.move_type {
                        quote! {}
                    } else {
                        quote! {&}
                    };

                    quote! {
                        #acc
                        result.insert(#name.to_string(), #converter(#move_type self.#field));
                    }
                }
            },
        );
        let members_code = quote! {
            #members_code
            result
        };

        match &trt.1.naming {
            TraitNaming::ExistingGeneric { path, method_name } => impls.push(quote! {
                impl #path<#ctx_type> for #name {
                    fn #method_name(&self) -> std::collections::HashMap<String, #ctx_type> {
                        #members_code
                    }
                }
            }),
            TraitNaming::Existing { path, method_name } => impls.push(quote! {
                impl #path for #name {
                    fn #method_name(&self) -> std::collections::HashMap<String, #ctx_type> {
                        #members_code
                    }
                }
            }),
        }
    }

    impls.into_iter().fold(quote! {}, |acc, x| {
        quote! {
            #acc
            #x
        }
    })
}

/// Generate mapping with the different contexts
///
/// # General usage
/// **NOTE** This is NOT a working example, just a pseudocode
///
/// ## Mappers configuration
/// ```
/// #[derive(ContextMapper)]
/// #[context_mapper(
///
///     /// Indicates that mapper will be using a trait
///     trait(
///         // Name of the context. Defaults to `default`
///         context = path::of::the::context::default,
///
///         // Result type of the mapping. Defaults to `std::string::String`
///         type = usize,
///
///         // Conversion function/method. Defaults to `std::string::ToString`
///         converter = my_function_with_proper_signature,
///
///         // Indicates how the type should be passed to the function. Flag
///         // If false (or not defined): passed by reference
///         // Else: value is moved
///         move
///
///         /// Traits require either simple or generic naming
///         simple(
///             path = path::to::the::trait,
///             method_name = name_of_the_method
///         )
///         generic(
///             path = path::to::the::trait,
///             method_name = name_of_the_method
///         )
///
///     )
///
///     function(
///         context = path::of::the::context::default,
///         type = usize,
///         converter = my_function_with_proper_signature,
///         move
///
///         /// Name of the generated function. Both are identical
///         naming = my_new_function
///         fn = my_new_function
///
///         /// Optional. Visibility of the generated function. Both are identical
///         vis = pub(crate)
///         visibility = pub(crate)
///
///     )
///
///     impls(
///         context = path::of::the::context::default,
///         type = usize,
///         converter = my_function_with_proper_signature,
///         move
///
///         // As in `function`
///         naming = my_new_function
///         fn = my_new_function
///
///         /// As in `function`
///         vis = pub(crate)
///         visibility = pub(crate)
///     )
/// )]
/// ```
///
/// ## Attribtues configuration
///
/// ```
/// #[context_attribute(
///     context(
///         name = path::to::the::context,
///         converter = you::can::override::Mapper::function,
///
///         /// You can change mapper key for the certain context
///         rename = "new mapper key",
///
///         /// Indicates that field should be skipped in the given context. Flag
///         skip,
///
///         /// You can override context `move` flag
///         move
///     )
/// )]
/// ```
///
/// # Basic e xample
/// ```
/// use context_mapper::ContextMapper;
/// use context_mapper::IntoType;
/// #[derive(ContextMapper, Serialize)]
/// #[context_mapper(trait())]
/// #[context_mapper(
///     trait(
///         context=other_context,
///         type=usize,
///         converter=my_function,
///     )
/// )]
/// pub struct X {
///     #[context_attribute(
///         context(
///             name=other_context,
///             converter=my_other_function,
///             rename="my_other_name",
///         )
///     )]
///     pub x: String,
///
///     #[context_attribute(
///         context(
///             name=other_context,
///             move
///         )
///     )]
///     pub y: usize,
///     pub y0: i32,
///
///     #[context_attribute(context(name=other_context, skip))]
///     pub z: i32
/// }
/// ```
/// That generates the following
/// ```
/// impl ::context_mapper::IntoType<std::string::String> for X {
///     fn into_type_map(&self) -> std::collections::HashMap<String, std::string::String> {
///         let mut result = std::collections::HashMap::new();
///         result.insert("x".to_string(), std::string::ToString::to_string(&self.x));
///         result.insert("y".to_string(), std::string::ToString::to_string(&self.y));
///         result.insert("y0".to_string(), std::string::ToString::to_string(&self.y0));
///         result.insert("z".to_string(), std::string::ToString::to_string(&self.z));
///         result
///     }
/// }
/// impl ::context_mapper::IntoType<usize> for X {
///     fn into_type_map(&self) -> std::collections::HashMap<String, usize> {
///         let mut result = std::collections::HashMap::new();
///         result.insert("my_other_name".to_string(), my_other_function(&self.x));
///         result.insert("y".to_string(), my_function(self.y));
///         result.insert("y0".to_string(), my_function(&self.y0));
///         result
///     }
/// }
///
/// ```
///
#[proc_macro_derive(ContextMapper, attributes(context_mapper, context_attribute))]
pub fn context_mapper(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident.clone();

    let config = read_struct_config(&input);
    let members = read_members(&input);

    let traits_impl = generate_traits_impl(&name, &config, &members);
    let functs = generate_functions(&name, &config, &members);
    let impls = generate_impls(&name, &config, &members);

    let expanded = quote! {
        #traits_impl
        #functs
        #impls
    };

    proc_macro::TokenStream::from(expanded)
}
