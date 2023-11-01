use super::*;

/// A variant of [`TypeEnum`].
pub trait TypeEnumVariant {
    fn to_string(&self) -> String;
}

impl TypeEnumVariant for Type {
    fn to_string(&self) -> String { self.as_type_enum().to_string() }
}

impl TypeEnumVariant for RefType {
    fn to_string(&self) -> String { "&".to_string() + &*format!("{:?}", self.base) }
}

impl TypeEnumVariant for FuncType {
    fn to_string(&self) -> String {
        let args_names: String = self
            .args
            .iter()
            .map(|t| format!("{:?}", t))
            .collect::<Vec<String>>()
            .join(", ");
        let ret_name = &self.ret;

        format!("({args_names}); {ret_name:?}")
    }
}

impl TypeEnumVariant for StructType {
    fn to_string(&self) -> String {
        let mut output = String::new();
        let fields_iter = self.fields.iter().enumerate();

        output += "{ ";

        if self.is_tuple() {
            for (f, Field { typ, .. }) in fields_iter {
                if f != 0 { output += ", "; }

                output += &*format!("{}", &*typ.to_string());
            }
        } else {
            for (f, Field { name, typ, .. }) in fields_iter {
                if f != 0 { output += ", "; }

                output += &*format!("{name}: {typ:?}");
            }
        }

        output += " }";

        output
    }
}

impl TypeEnumVariant for IntType {
    fn to_string(&self) -> String {
        let prefix = if self.signed { 'i' } else { 'u' };
        format!("{prefix}{}", self.size.to_num())
    }
}

impl TypeEnumVariant for FloatType {
    fn to_string(&self) -> String {
        match self {
            FloatType::F32 => "f32",
            FloatType::F64 => "f64",
        }
        .to_string()
    }
}

impl TypeEnumVariant for BoolType {
    fn to_string(&self) -> String { "bool".into() }
}

impl TypeEnumVariant for ArrayType {
    fn to_string(&self) -> String { format!("[{}]{:?}", self.count, self.base) }
}

impl TypeEnumVariant for UnknownType {
    fn to_string(&self) -> String { String::from("unknown") }
}

impl TypeEnumVariant for VoidType {
    fn to_string(&self) -> String { String::from("void") }
}

impl TypeEnumVariant for DestructorType {
    fn to_string(&self) -> String {
        let root_node = self.destructor.tree.get(self.destructor.tree.root);
        let code_string = root_node.to_string(&self.destructor.tree, &self.destructor.variables);
        format!("{:?} ~ ...", self.base)
    }
}
