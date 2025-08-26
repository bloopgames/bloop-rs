//! This is largely copied from the `variadics_please` crate, with aded support
//! for `all_tuples_enumerated_with_size`.

use proc_macro::TokenStream;
use proc_macro2::{Literal, Span, TokenStream as TokenStream2};
use quote::{format_ident, quote};
use syn::{
    DeriveInput, ExprLit, FnArg, Ident, ItemFn, Lit, LitInt, LitStr, Path, Result, Token, Type,
    parse::{Parse, ParseStream},
    parse_macro_input,
    token::Comma,
};

#[proc_macro_derive(EcsType)]
pub fn derive_ecs_type(input: TokenStream) -> TokenStream {
    let DeriveInput { ident, .. } = parse_macro_input!(input);

    let cid = Ident::new(
        &format!("_{}_CID", ident.to_string().to_uppercase()),
        Span::call_site(),
    );

    let sid = LitStr::new(&ident.to_string(), Span::call_site());

    quote!(
        static mut #cid: Option<ComponentId> = None;

        impl EcsType for #ident {
            fn id() -> ComponentId {
                unsafe { #cid.expect("ComponentId unassigned") }
            }

            unsafe fn set_id(id: ComponentId) {
                unsafe {
                    #cid = Some(id);
                }
            }

            fn string_id() -> &'static std::ffi::CStr {
                unsafe { ::std::ffi::CStr::from_bytes_with_nul_unchecked(concat!(module_path!(), "::", #sid, "\0").as_bytes()) }
            }
        }
    )
    .into()
}

#[proc_macro_attribute]
pub fn component(args: TokenStream, input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);

    let engine_path: Path = if let Ok(arg) = syn::parse::<Ident>(args)
        && arg == "engine"
    {
        syn::parse_str("crate").unwrap()
    } else {
        syn::parse_str("::engine").unwrap()
    };

    let ident = &ast.ident;

    let cid = Ident::new(
        &format!("_{}_CID", ident.to_string().to_uppercase()),
        Span::call_site(),
    );

    let sid = LitStr::new(&ident.to_string(), Span::call_site());

    quote!(
        #[derive(Copy, Clone, #engine_path ::serde::Deserialize)]
        #ast

        static mut #cid: Option<ComponentId> = None;

        impl Component for #ident {}

        impl EcsType for #ident {
            fn id() -> ComponentId {
                unsafe { #cid.expect("ComponentId unassigned") }
            }

            unsafe fn set_id(id: ComponentId) {
                unsafe {
                    #cid = Some(id);
                }
            }

            fn string_id() -> &'static std::ffi::CStr {
                unsafe { ::std::ffi::CStr::from_bytes_with_nul_unchecked(concat!(module_path!(), "::", #sid, "\0").as_bytes()) }
            }
        }
    )
    .into()
}

#[proc_macro_attribute]
pub fn resource(args: TokenStream, input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);

    let (deserialize_path, serialize_path) = if let Ok(arg) = syn::parse::<Ident>(args)
        && arg == "engine"
    {
        (
            syn::parse_str::<Path>("snapshot_derive::DeserializeEngine").unwrap(),
            syn::parse_str::<Path>("snapshot_derive::SerializeEngine").unwrap(),
        )
    } else {
        (
            syn::parse_str::<Path>("::engine::snapshot::Deserialize").unwrap(),
            syn::parse_str::<Path>("::engine::snapshot::Serialize").unwrap(),
        )
    };

    let ident = &ast.ident;

    let cid = Ident::new(
        &format!("_{}_CID", ident.to_string().to_uppercase()),
        Span::call_site(),
    );

    let sid = LitStr::new(&ident.to_string(), Span::call_site());

    quote! {
        #[derive(#deserialize_path, #serialize_path)]
        #ast

        static mut #cid: Option<ComponentId> = None;

        impl Resource for #ident {
            fn new() -> Self {
                Self::default()
            }
        }

        impl EcsType for #ident {
            fn id() -> ComponentId {
                unsafe { #cid.expect("ComponentId unassigned") }
            }

            unsafe fn set_id(id: ComponentId) {
                unsafe {
                    #cid = Some(id);
                }
            }

            fn string_id() -> &'static std::ffi::CStr {
                unsafe { ::std::ffi::CStr::from_bytes_with_nul_unchecked(concat!(module_path!(), "::", #sid, "\0").as_bytes()) }
            }
        }
    }
    .into()
}

/// This version of `Resource` is used internally to allow resources to opt out
/// of state snapshot serialization.
///
/// This should eventually be made private to the engine.
#[proc_macro_attribute]
pub fn resource_skip_serialize(_args: TokenStream, input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);

    let ident = &ast.ident;
    let generics = &ast.generics;

    let cid = Ident::new(
        &format!("_{}_CID", ident.to_string().to_uppercase()),
        Span::call_site(),
    );

    let sid = LitStr::new(&ident.to_string(), Span::call_site());

    let generic_type_idents = generics.type_params().map(|param| &param.ident);
    let generic_type_idents2 = generics.type_params().map(|param| &param.ident);

    quote! {
        #ast

        static mut #cid: Option<ComponentId> = None;

        impl Resource for #ident {
            fn new() -> Self {
                Self::default()
            }
        }

        impl EcsType for #ident {
            fn id() -> ComponentId {
                unsafe { #cid.expect("ComponentId unassigned") }
            }

            unsafe fn set_id(id: ComponentId) {
                unsafe {
                    #cid = Some(id);
                }
            }

            fn string_id() -> &'static std::ffi::CStr {
                unsafe { ::std::ffi::CStr::from_bytes_with_nul_unchecked(concat!(module_path!(), "::", #sid, "\0").as_bytes()) }
            }
        }

        impl #generics ::snapshot::Serialize for #ident <#(#generic_type_idents,)*> {
            #[inline]
            fn serialize<W>(&self, _: &mut ::snapshot::Serializer<W>) -> ::snapshot::Result<()>
            where
                W: ::snapshot::WriteUninit,
            {
                Ok(())
            }
        }

        impl #generics ::snapshot::Deserialize for #ident <#(#generic_type_idents2,)*> {
            #[inline]
            unsafe fn deserialize<R>(_: &mut ::snapshot::Deserializer<R>) -> ::snapshot::Result<Self>
            where
                R: ::snapshot::ReadUninit,
            {
                panic!("use deserialize_in_place()!")
            }

            #[inline]
            unsafe fn deserialize_in_place<R>(&mut self, _: &mut ::snapshot::Deserializer<R>) -> ::snapshot::Result<()>
            where
            R: ::snapshot::ReadUninit,
            {
                Ok(())
            }
        }
    }
    .into()
}

/// This version of `Resource` is used internally to allow resources to provide
/// custom serialization.
///
/// This should eventually be made private to the engine.
#[proc_macro_attribute]
pub fn resource_manual_serialize(_args: TokenStream, input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);

    let ident = &ast.ident;

    let cid = Ident::new(
        &format!("_{}_CID", ident.to_string().to_uppercase()),
        Span::call_site(),
    );

    let sid = LitStr::new(&ident.to_string(), Span::call_site());

    quote! {
        #ast

        static mut #cid: Option<ComponentId> = None;

        impl Resource for #ident {
            fn new() -> Self {
                Self::default()
            }
        }

        impl EcsType for #ident {
            fn id() -> ComponentId {
                unsafe { #cid.expect("ComponentId unassigned") }
            }

            unsafe fn set_id(id: ComponentId) {
                unsafe {
                    #cid = Some(id);
                }
            }

            fn string_id() -> &'static std::ffi::CStr {
                unsafe { ::std::ffi::CStr::from_bytes_with_nul_unchecked(concat!(module_path!(), "::", #sid, "\0").as_bytes()) }
            }
        }
    }
    .into()
}

/// `system_once` is a marker attribute for FFI codegen.
#[proc_macro_attribute]
pub fn system_once(_attr: TokenStream, item: TokenStream) -> TokenStream {
    item
}

/// `system` is a marker attribute for FFI codegen.
#[proc_macro_attribute]
pub fn system(_attr: TokenStream, item: TokenStream) -> TokenStream {
    item
}

/// `init` is a marker attribute to run the function when the module starts.
/// `init` functions should take no parameters and not have a return type.
#[proc_macro_attribute]
pub fn init(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let input_function = parse_macro_input!(item as ItemFn);

    let has_params = input_function
        .sig
        .inputs
        .iter()
        .any(|argument| match argument {
            FnArg::Receiver(_) | FnArg::Typed(_) => true,
        });

    if has_params {
        return syn::Error::new_spanned(
            input_function.sig.fn_token,
            "init function must not have any parameters",
        )
        .to_compile_error()
        .into();
    }

    let returns_only_unit = match &input_function.sig.output {
        syn::ReturnType::Default => true,
        syn::ReturnType::Type(_, return_type) => {
            matches!(**return_type, Type::Tuple(ref tuple) if tuple.elems.is_empty())
        }
    };

    if !returns_only_unit {
        return syn::Error::new_spanned(
            &input_function.sig.output,
            "Function must return nothing or the unit type",
        )
        .to_compile_error()
        .into();
    }

    quote! {
        #input_function
    }
    .into()
}

/// `deinit` is a marker attribute to run a function when a module is unloaded.
/// The intention is to allow a caller to clean up anything that was in a
/// corresponding `init` function. `deinit` functions should take no parameters
/// and not have a return type.
#[proc_macro_attribute]
pub fn deinit(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let input_function = parse_macro_input!(item as ItemFn);

    let has_params = input_function
        .sig
        .inputs
        .iter()
        .any(|argument| match argument {
            FnArg::Receiver(_) | FnArg::Typed(_) => true,
        });

    if has_params {
        return syn::Error::new_spanned(
            input_function.sig.fn_token,
            "deinit function must not have any parameters",
        )
        .to_compile_error()
        .into();
    }

    let returns_only_unit = match &input_function.sig.output {
        syn::ReturnType::Default => true,
        syn::ReturnType::Type(_, return_type) => {
            matches!(**return_type, Type::Tuple(ref tuple) if tuple.elems.is_empty())
        }
    };

    if !returns_only_unit {
        return syn::Error::new_spanned(
            &input_function.sig.output,
            "Function must return nothing or the unit type",
        )
        .to_compile_error()
        .into();
    }

    quote! {
        #input_function
    }
    .into()
}

struct SetSystemEnabledInput {
    system_enabled_bool_flag: bool,
    func_idents: Vec<Ident>,
}

impl Parse for SetSystemEnabledInput {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let bool_expr_lit: ExprLit = input.parse()?;

        let system_enabled_bool_flag = match &bool_expr_lit.lit {
            Lit::Bool(boolean) => boolean.value(),
            _ => {
                panic!("First parameter must be a bool for whether the system is enabled or not");
            }
        };
        input.parse::<Token![,]>()?;
        let mut func_idents = vec![input.parse::<Ident>()?];
        while input.peek(Token![,]) {
            input.parse::<Token![,]>()?;
            if input.is_empty() {
                break;
            }
            func_idents.push(input.parse::<Ident>()?);
        }
        Ok(Self {
            system_enabled_bool_flag,
            func_idents,
        })
    }
}

/// Helper around [`Engine::set_system_enabled`]
#[proc_macro]
pub fn set_system_enabled(input: TokenStream) -> TokenStream {
    let SetSystemEnabledInput {
        func_idents,
        system_enabled_bool_flag,
    } = parse_macro_input!(input as SetSystemEnabledInput);
    // let func_name_str = func_ident.to_string();

    let system_enabled_functions: proc_macro2::TokenStream = func_idents.iter().fold(proc_macro2::TokenStream::new(), |token_stream, system_name_ident| {
        let system_name = system_name_ident.to_string();
        quote! {
            #token_stream

            let system_name = ::std::ffi::CStr::from_bytes_with_nul(concat!(#system_name, "\0").as_bytes()).unwrap();
            let full_system_name = ::engine::system::system_name_generator_c(unsafe { ::std::ffi::CStr::from_ptr(module_name()) }, system_name);
            unsafe {
                (::engine::_SET_SYSTEM_ENABLED_FN).unwrap_unchecked()(full_system_name.as_ptr(), enabled);
            }
        }
    });

    quote! {
        let enabled = #system_enabled_bool_flag;
        #system_enabled_functions
    }
    .into()
}

struct AllTuples {
    macro_ident: Ident,
    start: usize,
    end: usize,
    idents: Vec<Ident>,
}

impl Parse for AllTuples {
    fn parse(input: ParseStream) -> Result<Self> {
        let macro_ident = input.parse::<Ident>()?;
        input.parse::<Comma>()?;
        let start = input.parse::<LitInt>()?.base10_parse()?;
        input.parse::<Comma>()?;
        let end = input.parse::<LitInt>()?.base10_parse()?;
        input.parse::<Comma>()?;
        let mut idents = vec![input.parse::<Ident>()?];
        while input.parse::<Comma>().is_ok() {
            idents.push(input.parse::<Ident>()?);
        }

        Ok(AllTuples {
            macro_ident,
            start,
            end,
            idents,
        })
    }
}

/// Like `to_ident_tuple`, but it enumerates the identifiers
fn to_ident_tuple_enumerated(idents: impl Iterator<Item = Ident>, idx: usize) -> TokenStream2 {
    let idx = Literal::usize_unsuffixed(idx);
    quote! { (#idx, #(#idents),*) }
}

fn choose_ident_tuples_enumerated(ident_tuples: &[TokenStream2], i: usize) -> TokenStream2 {
    let ident_tuples = &ident_tuples[..i];
    quote! { #(#ident_tuples),* }
}

#[proc_macro]
pub fn all_tuples_enumerated_with_size(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as AllTuples);
    let len = 1 + input.end - input.start;
    let mut ident_tuples = Vec::with_capacity(len);

    for i in 0..=len {
        let idents = input
            .idents
            .iter()
            .map(|ident| format_ident!("{}{}", ident, i));

        ident_tuples.push(to_ident_tuple_enumerated(idents, i));
    }

    let macro_ident = &input.macro_ident;

    let invocations = (input.start..=input.end).map(|i| {
        let ident_tuples = choose_ident_tuples_enumerated(&ident_tuples, i);

        quote! {
            #macro_ident!(#len, #ident_tuples);
        }
    });

    TokenStream::from(quote! {
        #(
            #invocations
        )*
    })
}
