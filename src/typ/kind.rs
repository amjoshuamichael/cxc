use super::invalid_state::InvalidState;
use super::*;

pub trait Kind: InvalidState {
    fn to_string(&self) -> String;
}

impl Kind for Type {
    fn to_string(&self) -> String { self.as_type_enum().to_string() }
}

impl Kind for RefType {
    fn to_string(&self) -> String { "&".to_string() + &*format!("{:?}", self.base) }
}

impl Kind for FuncType {
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

impl Kind for StructType {
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

impl Kind for IntType {
    fn to_string(&self) -> String {
        let prefix = if self.signed { 'i' } else { 'u' };
        format!("{prefix}{}", self.size.to_num())
    }
}

impl Kind for FloatType {
    fn to_string(&self) -> String {
        match self {
            FloatType::F16 => "f16",
            FloatType::F32 => "f32",
            FloatType::F64 => "f64",
        }
        .to_string()
    }
}

impl Kind for BoolType {
    fn to_string(&self) -> String { "bool".into() }
}

impl Kind for ArrayType {
    fn to_string(&self) -> String { format!("[{}]{:?}", self.count, self.base) }
}

impl Kind for UnknownType {
    fn to_string(&self) -> String { String::from("Unknown") }
}

impl Kind for VoidType {
    fn to_string(&self) -> String { String::from("Void") }
}
