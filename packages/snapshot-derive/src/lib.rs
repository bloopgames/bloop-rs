#![allow(clippy::todo)]

use proc_macro::TokenStream;
use quote::quote;
use syn::{
    Data, DataEnum, DataStruct, DeriveInput, Generics, Ident, Path, parse, parse_str,
    spanned::Spanned,
};

#[proc_macro_derive(Serialize)]
pub fn derive_serialize(input: TokenStream) -> TokenStream {
    let DeriveInput {
        ident,
        data,
        generics,
        ..
    } = parse(input).unwrap();

    let engine_path = parse_str("::engine").unwrap();

    match &data {
        Data::Struct(data_struct) => {
            derive_serialize_struct(&engine_path, &ident, &generics, data_struct)
        }
        Data::Enum(data_enum) => derive_serialize_enum(&engine_path, &ident, &generics, data_enum),
        Data::Union(_) => todo!("unions not yet supported"),
    }
}

/// For use only by the `engine` package. Should be made private.
#[proc_macro_derive(SerializeEngine)]
pub fn derive_serialize_engine(input: TokenStream) -> TokenStream {
    let DeriveInput {
        ident,
        data,
        generics,
        ..
    } = parse(input).unwrap();

    let engine_path = parse_str("crate").unwrap();

    match &data {
        Data::Struct(data_struct) => {
            derive_serialize_struct(&engine_path, &ident, &generics, data_struct)
        }
        Data::Enum(data_enum) => derive_serialize_enum(&engine_path, &ident, &generics, data_enum),
        Data::Union(_) => todo!("unions not yet supported"),
    }
}

fn derive_serialize_struct(
    engine_path: &Path,
    ident: &Ident,
    generics: &Generics,
    data_struct: &DataStruct,
) -> TokenStream {
    let generic_type_idents = generics.type_params().map(|param| &param.ident);

    if data_struct.fields.iter().all(|field| field.ident.is_some()) {
        let field_idents = data_struct
            .fields
            .iter()
            .map(|field| field.ident.as_ref().unwrap());
        let field_types = data_struct.fields.iter().map(|field| &field.ty);

        quote! {
            impl #generics #engine_path ::snapshot::Serialize for #ident <#(#generic_type_idents,)*> {
                #[inline]
                fn serialize<W>(&self, serializer: &mut #engine_path ::snapshot::Serializer<W>) -> #engine_path ::snapshot::Result<()>
                where
                    W: #engine_path ::snapshot::WriteUninit,
                {
                    #(<#field_types as #engine_path ::snapshot::Serialize>::serialize(&self.#field_idents, serializer)?;)*
                    Ok(())
                }
            }
        }.into()
    } else if let Some(field) = data_struct.fields.iter().next() {
        assert!(field.ident.is_none(), "unexpected struct field");
        let field_type = &field.ty;

        quote! {
            impl #generics #engine_path ::snapshot::Serialize for #ident <#(#generic_type_idents,)*> {
                #[inline]
                fn serialize<W>(&self, serializer: &mut #engine_path ::snapshot::Serializer<W>) -> #engine_path ::snapshot::Result<()>
                where
                    W: #engine_path ::snapshot::WriteUninit,
                {
                    <#field_type as #engine_path ::snapshot::Serialize>::serialize(&self.0, serializer)
                }
            }
        }.into()
    } else {
        // Empty struct.

        quote! {
            impl #generics #engine_path ::snapshot::Serialize for #ident <#(#generic_type_idents,)*> {
                #[inline]
                fn serialize<W>(&self, serializer: &mut #engine_path ::snapshot::Serializer<W>) -> #engine_path ::snapshot::Result<()>
                where
                    W: #engine_path ::snapshot::WriteUninit,
                {
                    Ok(())
                }
            }
        }.into()
    }
}

fn derive_serialize_enum(
    engine_path: &Path,
    ident: &Ident,
    generics: &Generics,
    data_enum: &DataEnum,
) -> TokenStream {
    let generic_type_idents = generics.type_params().map(|param| &param.ident);

    let variants = data_enum.variants.iter().enumerate().map(|(i, variant)| {
        let i = u8::try_from(i).expect("too many variants");
        let variant_ident = &variant.ident;

        // Check if the variant is a tuple field.
        if variant
            .fields
            .iter()
            .next()
            .is_some_and(|field| field.ident.is_none())
        {
            let field_idents = variant
                .fields
                .iter()
                .enumerate()
                .map(|(i, field)| Ident::new(&((b'a' + i as u8) as char).to_string(), field.span()));
            let field_idents2 = field_idents.clone();
            let field_types = variant.fields.iter().map(|field| &field.ty);

            return quote! {
                #ident::#variant_ident ( #(#field_idents),* ) => {
                    <u8 as #engine_path ::snapshot::Serialize>::serialize(&#i, serializer)?;
                    #(<#field_types as #engine_path ::snapshot::Serialize>::serialize(#field_idents2, serializer)?;)*
                    Ok(())
                }
            };
        }

        let field_types = variant.fields.iter().map(|field| &field.ty);
        let field_idents = variant
            .fields
            .iter()
            .map(|field| field.ident.as_ref().unwrap());
        let field_idents2 = variant
            .fields
            .iter()
            .map(|field| field.ident.as_ref().unwrap());

        quote! {
            #ident::#variant_ident { #(#field_idents),* } => {
                <u8 as #engine_path ::snapshot::Serialize>::serialize(&#i, serializer)?;
                #(<#field_types as #engine_path ::snapshot::Serialize>::serialize(#field_idents2, serializer)?;)*
                Ok(())
            }
        }
    });

    quote! {
        impl #generics #engine_path ::snapshot::Serialize for #ident <#(#generic_type_idents,)*> {
            #[inline]
            fn serialize<W>(&self, serializer: &mut #engine_path ::snapshot::Serializer<W>) -> #engine_path ::snapshot::Result<()>
            where
                W: #engine_path ::snapshot::WriteUninit,
            {
                match self {
                    #(#variants,)*
                    _ => unreachable!(),
                }
            }
        }
    }.into()
}

#[proc_macro_derive(SerializeMut)]
pub fn derive_serialize_mut(input: TokenStream) -> TokenStream {
    let DeriveInput {
        ident,
        data,
        generics,
        ..
    } = parse(input).unwrap();

    let engine_path = parse_str("::engine").unwrap();

    match &data {
        Data::Struct(data_struct) => {
            derive_serialize_mut_struct(&engine_path, &ident, &generics, data_struct)
        }
        Data::Enum(data_enum) => {
            derive_serialize_mut_enum(&engine_path, &ident, &generics, data_enum)
        }
        Data::Union(_) => todo!("unions not yet supported"),
    }
}

/// For use only by the `engine` package. Should be made private.
#[proc_macro_derive(SerializeMutEngine)]
pub fn derive_serialize_mut_engine(input: TokenStream) -> TokenStream {
    let DeriveInput {
        ident,
        data,
        generics,
        ..
    } = parse(input).unwrap();

    let engine_path = parse_str("crate").unwrap();

    match &data {
        Data::Struct(data_struct) => {
            derive_serialize_mut_struct(&engine_path, &ident, &generics, data_struct)
        }
        Data::Enum(data_enum) => {
            derive_serialize_mut_enum(&engine_path, &ident, &generics, data_enum)
        }
        Data::Union(_) => todo!("unions not yet supported"),
    }
}

fn derive_serialize_mut_struct(
    engine_path: &Path,
    ident: &Ident,
    generics: &Generics,
    data_struct: &DataStruct,
) -> TokenStream {
    let generic_type_idents = generics.type_params().map(|param| &param.ident);

    if data_struct.fields.iter().all(|field| field.ident.is_some()) {
        let field_idents = data_struct
            .fields
            .iter()
            .map(|field| field.ident.as_ref().unwrap());
        let field_types = data_struct.fields.iter().map(|field| &field.ty);

        quote! {
            impl #generics #engine_path ::snapshot::SerializeMut for #ident <#(#generic_type_idents,)*> {
                #[inline]
                fn serialize_mut<W>(&mut self, serializer: &mut #engine_path ::snapshot::Serializer<W>) -> #engine_path ::snapshot::Result<()>
                where
                    W: #engine_path ::snapshot::WriteUninit,
                {
                    #(<#field_types as #engine_path ::snapshot::SerializeMut>::serialize_mut(&mut self.#field_idents, serializer)?;)*
                    Ok(())
                }
            }
        }.into()
    } else if let Some(field) = data_struct.fields.iter().next() {
        assert!(field.ident.is_none(), "unexpected struct field");
        let field_type = &field.ty;

        quote! {
            impl #generics #engine_path ::snapshot::SerializeMut for #ident <#(#generic_type_idents,)*> {
                #[inline]
                fn serialize_mut<W>(&mut self, serializer: &mut #engine_path ::snapshot::Serializer<W>) -> #engine_path ::snapshot::Result<()>
                where
                    W: #engine_path ::snapshot::WriteUninit,
                {
                    <#field_type as #engine_path ::snapshot::SerializeMut>::serialize_mut(&mut self.0, serializer)
                }
            }
        }.into()
    } else {
        // Empty struct.

        quote! {
            impl #generics #engine_path ::snapshot::SerializeMut for #ident <#(#generic_type_idents,)*> {
                #[inline]
                fn serialize_mut<W>(&mut self, _: &mut #engine_path ::snapshot::Serializer<W>) -> #engine_path ::snapshot::Result<()>
                where
                    W: #engine_path ::snapshot::WriteUninit,
                {
                    Ok(())
                }
            }
        }.into()
    }
}

fn derive_serialize_mut_enum(
    engine_path: &Path,
    ident: &Ident,
    generics: &Generics,
    data_enum: &DataEnum,
) -> TokenStream {
    let generic_type_idents = generics.type_params().map(|param| &param.ident);

    let variants = data_enum.variants.iter().enumerate().map(|(i, variant)| {
        let i = u8::try_from(i).expect("too many variants");
        let variant_ident = &variant.ident;

        // Check if the variant is a tuple field.
        if variant
            .fields
            .iter()
            .next()
            .is_some_and(|field| field.ident.is_none())
        {
            let field_idents = variant
                .fields
                .iter()
                .enumerate()
                .map(|(i, field)| Ident::new(&((b'a' + i as u8) as char).to_string(), field.span()));
            let field_idents2 = field_idents.clone();
            let field_types = variant.fields.iter().map(|field| &field.ty);

            return quote! {
                #ident::#variant_ident ( #(#field_idents),* ) => {
                    <u8 as #engine_path ::snapshot::Serialize>::serialize(&#i, serializer)?;
                    #(<#field_types as #engine_path ::snapshot::SerializeMut>::serialize_mut(#field_idents2, serializer)?;)*
                    Ok(())
                }
            };
        }

        let field_types = variant.fields.iter().map(|field| &field.ty);
        let field_idents = variant
            .fields
            .iter()
            .map(|field| field.ident.as_ref().unwrap());
        let field_idents2 = variant
            .fields
            .iter()
            .map(|field| field.ident.as_ref().unwrap());

        quote! {
            #ident::#variant_ident { #(#field_idents),* } => {
                <u8 as #engine_path ::snapshot::Serialize>::serialize(&#i, serializer)?;
                #(<#field_types as #engine_path ::snapshot::SerializeMut>::serialize_mut(#field_idents2, serializer)?;)*
                Ok(())
            }
        }
    });

    quote! {
        impl #generics #engine_path ::snapshot::SerializeMut for #ident <#(#generic_type_idents,)*> {
            #[inline]
            fn serialize_mut<W>(&mut self, serializer: &mut #engine_path ::snapshot::Serializer<W>) -> #engine_path ::snapshot::Result<()>
            where
                W: #engine_path ::snapshot::WriteUninit,
            {
                match self {
                    #(#variants,)*
                    _ => unreachable!(),
                }
            }
        }
    }.into()
}

#[proc_macro_derive(Deserialize)]
pub fn derive_deserialize(input: TokenStream) -> TokenStream {
    let DeriveInput {
        ident,
        data,
        generics,
        ..
    } = parse(input).unwrap();

    let engine_path = parse_str("::engine").unwrap();

    match &data {
        Data::Struct(data_struct) => {
            derive_deserialize_struct(&engine_path, &ident, &generics, data_struct)
        }
        Data::Enum(data_enum) => {
            derive_deserialize_enum(&engine_path, &ident, &generics, data_enum)
        }
        Data::Union(_) => todo!("unions not yet supported"),
    }
}

/// For use only by the `engine` package. Should be made private.
#[proc_macro_derive(DeserializeEngine)]
pub fn derive_deserialize_engine(input: TokenStream) -> TokenStream {
    let DeriveInput {
        ident,
        data,
        generics,
        ..
    } = parse(input).unwrap();

    let engine_path = parse_str("crate").unwrap();

    match &data {
        Data::Struct(data_struct) => {
            derive_deserialize_struct(&engine_path, &ident, &generics, data_struct)
        }
        Data::Enum(data_enum) => {
            derive_deserialize_enum(&engine_path, &ident, &generics, data_enum)
        }
        Data::Union(_) => todo!("unions not yet supported"),
    }
}

fn derive_deserialize_struct(
    engine_path: &Path,
    ident: &Ident,
    generics: &Generics,
    data_struct: &DataStruct,
) -> TokenStream {
    let generic_type_idents = generics.type_params().map(|param| &param.ident);

    if data_struct.fields.iter().all(|field| field.ident.is_some()) {
        let field_idents = data_struct
            .fields
            .iter()
            .map(|field| field.ident.as_ref().unwrap());
        let field_types = data_struct.fields.iter().map(|field| &field.ty);

        let field_idents2 = field_idents.clone();
        let field_types2 = field_types.clone();

        quote! {
            impl #generics #engine_path ::snapshot::Deserialize for #ident <#(#generic_type_idents,)*> {
                #[inline]
                unsafe fn deserialize<R>(deserializer: &mut #engine_path ::snapshot::Deserializer<R>) -> #engine_path ::snapshot::Result<Self>
                where
                    R: #engine_path ::snapshot::ReadUninit,
                {
                    Ok(Self {
                        #(#field_idents: unsafe {
                            <#field_types as #engine_path ::snapshot::Deserialize>::deserialize(deserializer)?
                        },)*
                    })
                }

                #[inline]
                unsafe fn deserialize_in_place<R>(&mut self, deserializer: &mut #engine_path ::snapshot::Deserializer<R>) -> #engine_path ::snapshot::Result<()>
                where
                    R: #engine_path ::snapshot::ReadUninit,
                {
                    #(unsafe {
                        <#field_types2 as #engine_path ::snapshot::Deserialize>::deserialize_in_place(&mut self.#field_idents2, deserializer)?;
                    })*
                    Ok(())
                }
            }
        }.into()
    } else if let Some(field) = data_struct.fields.iter().next() {
        assert!(field.ident.is_none(), "unexpected struct field");
        let field_type = &field.ty;

        quote! {
            impl #generics #engine_path ::snapshot::Deserialize for #ident <#(#generic_type_idents,)*> {
                #[inline]
                unsafe fn deserialize<R>(deserializer: &mut #engine_path ::snapshot::Deserializer<R>) -> #engine_path ::snapshot::Result<Self>
                where
                    R: #engine_path ::snapshot::ReadUninit,
                {
                    Ok(Self( unsafe { <#field_type as #engine_path ::snapshot::Deserialize>::deserialize(deserializer)? }))
                }

                #[inline]
                unsafe fn deserialize_in_place<R>(&mut self, deserializer: &mut #engine_path ::snapshot::Deserializer<R>) -> #engine_path ::snapshot::Result<()>
                where
                    R: #engine_path ::snapshot::ReadUninit,
                {
                    unsafe {
                        <#field_type as #engine_path ::snapshot::Deserialize>::deserialize_in_place(&mut self.0, deserializer)
                    }
                }
            }
        }.into()
    } else {
        // Empty struct.

        quote! {
            impl #generics #engine_path ::snapshot::Deserialize for #ident <#(#generic_type_idents,)*> {
                #[inline]
                unsafe fn deserialize<R>(_: &mut #engine_path ::snapshot::Deserializer<R>) -> #engine_path ::snapshot::Result<Self>
                where
                    R: #engine_path ::snapshot::ReadUninit,
                {
                    Ok(Self)
                }
            }
        }.into()
    }
}

fn derive_deserialize_enum(
    engine_path: &Path,
    ident: &Ident,
    generics: &Generics,
    data_enum: &DataEnum,
) -> TokenStream {
    let generic_type_idents = generics.type_params().map(|param| &param.ident);

    let variants = data_enum.variants.iter().enumerate().map(|(i, variant)| {
        let i = u8::try_from(i).expect("too many variants");
        let variant_ident = &variant.ident;

        // Check if the variant is a tuple field.
        if variant
            .fields
            .iter()
            .next()
            .is_some_and(|field| field.ident.is_none())
        {
            let field_types = variant.fields.iter().map(|field| &field.ty);

            return quote! {
                #i => {
                    Ok(Self::#variant_ident(#(unsafe {
                        <#field_types as #engine_path ::snapshot::Deserialize>::deserialize(deserializer)?
                    },)*))
                }
            };
        }

        // Not a tuple field.
        let field_types = variant.fields.iter().map(|field| &field.ty);
        let field_idents = variant
            .fields
            .iter()
            .map(|field| field.ident.as_ref().unwrap());

        quote! {
            #i => {
                Ok(Self::#variant_ident { #(#field_idents: unsafe {
                    <#field_types as #engine_path ::snapshot::Deserialize>::deserialize(deserializer)?
                }),* })
            }
        }
    });

    quote! {
        impl #generics #engine_path ::snapshot::Deserialize for #ident <#(#generic_type_idents,)*> {
            #[inline]
            unsafe fn deserialize<R>(deserializer: &mut #engine_path ::snapshot::Deserializer<R>) -> #engine_path ::snapshot::Result<Self>
            where
                R: #engine_path ::snapshot::ReadUninit,
            {
                match u8::deserialize(deserializer)? {
                    #(#variants,)*
                    _ => unreachable!(),
                }
            }
        }
    }.into()
}
