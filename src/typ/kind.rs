use super::*;

/// A variant of [`TypeEnum`]. Note that [`TypeEnum`] dereferences to a 
/// `&dyn [TypeEnumVariant]`, meaning it implements this trait as well.
pub trait TypeEnumVariant {
    /// The "Full name" of a type. For example, an anonymous struct type with two integer
    /// fields might have the full name { i32, i32 }. A reference to that type might have 
    /// the full name &{i32, i32}, etc. If this type has a name, its full name will contain
    /// its name. This applies to generics as well.
    fn full_name(&self) -> String;
}

impl TypeEnumVariant for Type {
    fn full_name(&self) -> String { self.as_type_enum().full_name() }
}

impl TypeEnumVariant for RefType {
    fn full_name(&self) -> String { "&".to_string() + &*self.base.full_name() }
}

impl TypeEnumVariant for FuncType {
    fn full_name(&self) -> String {
        let args_names: String = self
            .args
            .iter()
            .map(Type::full_name)
            .collect::<Vec<String>>()
            .join(", ");
        let ret_name = self.ret.full_name();

        format!("({args_names}); {ret_name}")
    }
}

impl TypeEnumVariant for StructType {
    fn full_name(&self) -> String {
        let mut output = String::new();
        let fields_iter = self.fields.iter().enumerate();

        output += "{ ";

        if self.is_tuple() {
            for (f, Field { inherited, typ, .. }) in fields_iter {
                if f != 0 { output += ", "; }

                if *inherited { output += "+"; }
                output += &*format!("{}", &*typ.full_name());
            }
        } else {
            for (f, Field { name, inherited, typ, }) in fields_iter {
                if f != 0 { output += ", "; }

                if *inherited { output += "+"; }
                let type_name = typ.full_name();

                output += &*format!("{name}: {type_name}");
            }
        }

        output += " }";

        output
    }
}

impl TypeEnumVariant for UnionType {
    fn full_name(&self) -> String {
        let mut output = String::new();
        let fields_iter = self.fields.iter().enumerate();

        output += "{ ";

        for (f, Field { name, inherited, typ, }) in fields_iter {
            if f != 0 { output += " | "; }

            if *inherited { output += "+"; }
            let type_name = typ.full_name();

            output += &*format!("{name}: {type_name}");
        }

        output += " }";

        output
    }
}

impl TypeEnumVariant for IntType {
    fn full_name(&self) -> String {
        let prefix = if self.signed { 'i' } else { 'u' };
        format!("{prefix}{}", self.size.to_num())
    }
}

impl TypeEnumVariant for FloatType {
    fn full_name(&self) -> String {
        match self {
            FloatType::F32 => "f32",
            FloatType::F64 => "f64",
        }
        .to_string()
    }
}

impl TypeEnumVariant for BoolType {
    fn full_name(&self) -> String { "bool".into() }
}

impl TypeEnumVariant for ArrayType {
    fn full_name(&self) -> String { format!("[{}]{}", self.count, self.base.full_name()) }
}

impl TypeEnumVariant for UnknownType {
    fn full_name(&self) -> String { String::from("unknown") }
}

impl TypeEnumVariant for VoidType {
    fn full_name(&self) -> String { String::from("void") }
}

impl TypeEnumVariant for DestructorType {
    fn full_name(&self) -> String {
        let root_node = self.destructor.tree.get(self.destructor.tree.root);
        let code_string = root_node.to_string(&self.destructor.tree, &self.destructor.variables);
        format!("{:?} ~ ...", self.base)
    }
}
