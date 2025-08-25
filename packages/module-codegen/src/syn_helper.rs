use syn::{GenericArgument, Ident, Path, PathArguments, Type};

/// Given `Ref<Component>`, gives `Component`.
pub fn parse_ref(function_ident: &Ident, arg_path: &Path) -> Path {
    let PathArguments::AngleBracketed(inputs) = &arg_path.segments.last().unwrap().arguments else {
        panic!("fn {function_ident}(): invalid `Ref`/`Mut` generics")
    };

    assert_eq!(
        inputs.args.len(),
        1,
        "`Ref`/`Mut` may only take a single generic"
    );

    let GenericArgument::Type(input) = inputs.args.first().unwrap() else {
        panic!("`Ref`/`Mut` generic must be a value type");
    };

    let Type::Path(path) = input else {
        panic!("`Ref`/`Mut` generic must be a path");
    };

    path.path.clone()
}
