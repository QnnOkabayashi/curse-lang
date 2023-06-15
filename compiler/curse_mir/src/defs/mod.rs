use crate::TypeIdent;

#[derive(Copy, Clone, Debug)]
pub struct StructDefId(pub usize);
#[derive(Copy, Clone, Debug)]
pub struct ChoiceDefId(pub usize);

#[derive(Copy, Clone, Debug)]
pub enum FieldKind<'cx> {
    Newtype(FieldType<'cx>),
    Record(&'cx [NamedField<'cx>]),
}

/// Named field of a record, e.g. `age: I32`.
#[derive(Copy, Clone, Debug)]
pub struct NamedField<'cx> {
    name: TypeIdent,
    ty: FieldType<'cx>,
}

#[derive(Copy, Clone, Debug)]
pub enum FieldType<'cx> {
    /// A struct with possible generics, e.g. `Vec(I32)`.
    Struct {
        generics: &'cx [FieldType<'cx>],
        def: StructDefId,
    },
    /// A choice with possible generics, e.g. `Option(I32)`.
    Choice {
        generics: &'cx [FieldType<'cx>],
        def: ChoiceDefId,
    },
    /// A generic type introduced in the definition, e.g. `T`.
    Generic { position: u32 },
    Tuple {
        elements: &'cx [FieldType<'cx>],
    },
    Function {
        lhs: &'cx FieldType<'cx>,
        rhs: &'cx FieldType<'cx>,
        output: &'cx FieldType<'cx>,
    },
    /// A primitive type, e.g. `I32`.
    Primitive(PrimitiveType),
}

#[derive(Copy, Clone, Debug)]
pub enum PrimitiveType {
    I32,
    Bool,
    // TODO(quinn): add more later
}

#[derive(Copy, Clone, Debug)]
pub struct StructDef<'cx> {
    name: TypeIdent,
    generics: &'cx [TypeIdent],
    kind: FieldKind<'cx>,
}

#[derive(Copy, Clone, Debug)]
pub struct ChoiceDef<'cx> {
    name: TypeIdent,
    generics: &'cx [TypeIdent],
    variants: &'cx [VariantDef<'cx>],
}

#[derive(Copy, Clone, Debug)]
pub struct VariantDef<'cx> {
    name: TypeIdent,
    kind: FieldKind<'cx>,
}
