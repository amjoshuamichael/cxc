use super::invalid_state::InvalidState;
use super::*;
use std::cell::RefCell;
use std::collections::BTreeMap;
use std::iter::once;

use inkwell::context::Context;
use inkwell::types::AnyType;
use inkwell::types::AnyTypeEnum;
use inkwell::types::BasicMetadataTypeEnum;
use inkwell::types::BasicType;
use inkwell::types::BasicTypeEnum;
use inkwell::AddressSpace;

pub trait Kind: InvalidState {
    fn to_string(&self) -> String;
    fn to_any_type(&self, context: &'static Context) -> AnyTypeEnum<'static>;

    fn to_basic_type(&self, context: &'static Context) -> BasicTypeEnum<'static> {
        self.to_any_type(context).try_into().unwrap()
    }
}

std::thread_local! {
  static MEMOIZED_TO_ANY: RefCell<BTreeMap<Type, AnyTypeEnum<'static>>> =
      RefCell::new(BTreeMap::new());
}

impl Kind for Type {
    fn to_string(&self) -> String { self.as_type_enum().to_string() }

    fn to_any_type(&self, context: &'static Context) -> AnyTypeEnum<'static> {
        let possible_output = MEMOIZED_TO_ANY.with(|memoized| {
            let memoized = memoized.borrow();
            memoized.get(&self).cloned()
        });

        if let Some(possible_output) = possible_output {
            return possible_output;
        }

        let output = self.as_type_enum().to_any_type(context);

        MEMOIZED_TO_ANY.with(|memoized| {
            let mut memoized = memoized.borrow_mut();
            memoized.insert(self.clone(), output.clone());
        });

        output
    }
}

impl Kind for RefType {
    fn to_string(&self) -> String { "&".to_string() + &*format!("{:?}", self.base) }

    fn to_any_type(&self, context: &'static Context) -> AnyTypeEnum<'static> {
        if self.base.is_void() {
            context.i8_type().ptr_type(AddressSpace::default()).into()
        } else {
            self.base
                .to_basic_type(context)
                .ptr_type(AddressSpace::default())
                .into()
        }
    }
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

    fn to_any_type(&self, context: &'static Context) -> AnyTypeEnum<'static> {
        self.llvm_func_type(context)
            .ptr_type(AddressSpace::default())
            .as_any_type_enum()
    }
}

impl FuncType {
    pub fn llvm_func_type<'f>(&self, context: &'static Context) -> FunctionType<'f> {
        if self.ret.return_style() != ReturnStyle::Sret {
            let args: Vec<BasicMetadataTypeEnum> = self
                .args
                .iter()
                .map(|t| t.to_basic_type(context).into())
                .collect();

            if self.ret.is_void() {
                context.void_type().fn_type(&args[..], false)
            } else {
                let return_type = self.ret.raw_return_type().to_basic_type(context);
                return_type.fn_type(&args, false)
            }
        } else {
            let args: Vec<BasicMetadataTypeEnum> = once(&self.ret.clone().get_ref())
                .chain(self.args.iter())
                .map(|t| t.to_basic_type(context).into())
                .collect();

            context.void_type().fn_type(&args[..], false)
        }
    }
}

impl Kind for StructType {
    fn to_string(&self) -> String {
        let mut name = String::new();
        let mut fields_iter = self.fields.iter();

        name += "{ ";

        if self.is_tuple() {
            if let Some((_, first_typ)) = fields_iter.next() {
                name += &*first_typ.to_string();
                for (_, typ) in fields_iter {
                    name += &*format!(", {}", &*typ.to_string());
                }
            }
        } else if let Some((first_name, first_typ)) = fields_iter.next() {
            name += &*format!("{first_name}: {first_typ:?}");
            for (field_name, typ) in fields_iter {
                name += &*format!(", {field_name}: {typ:?}");
            }
        }

        name += " }";

        name
    }

    fn to_any_type(&self, context: &'static Context) -> AnyTypeEnum<'static> {
        let field_types: Vec<BasicTypeEnum> = self
            .fields
            .iter()
            .map(|(_, typ)| typ.to_basic_type(context))
            .collect();

        context
            .struct_type(&field_types[..], false)
            .as_any_type_enum()
    }
}

impl Kind for SumType {
    fn to_string(&self) -> String { format!("/{:?}/", self.variants) }

    fn to_any_type(&self, context: &'static Context) -> AnyTypeEnum<'static> {
        let size = self.largest_variant_as_struct().size() as u32;
        match size {
            0..=8 => context.custom_width_int_type(size * 8).into(),
            9..=16 => context
                .struct_type(&vec![context.i32_type().into(); size as usize / 4], false)
                .into(),
            17.. => {
                // TODO: enum variants like i1, i32, i32, i32, i32 won't work here
                let padding_size = size as usize - 4;
                let mut struct_fields = vec![context.i32_type().as_basic_type_enum()];
                struct_fields.extend(
                    vec![context.i32_type().as_basic_type_enum(); padding_size / 4].into_iter(),
                );
                context.struct_type(&struct_fields, false).into()
            },
        }
    }
}

impl Kind for VariantType {
    fn to_string(&self) -> String { format!("{:?}.{}", self.parent, self.tag) }

    fn to_any_type(&self, context: &'static Context) -> AnyTypeEnum<'static> {
        self.as_struct().to_basic_type(context).as_any_type_enum()
    }
}

impl Kind for IntType {
    fn to_string(&self) -> String {
        let prefix = if self.signed { 'i' } else { 'u' };
        format!("{prefix}{}", self.size)
    }

    fn to_any_type(&self, context: &'static Context) -> AnyTypeEnum<'static> {
        context.custom_width_int_type(self.size).as_any_type_enum()
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

    fn to_any_type(&self, context: &'static Context) -> AnyTypeEnum<'static> {
        match self {
            FloatType::F16 => context.f16_type().as_any_type_enum(),
            FloatType::F32 => context.f32_type().as_any_type_enum(),
            FloatType::F64 => context.f64_type().as_any_type_enum(),
        }
    }
}

impl Kind for BoolType {
    fn to_string(&self) -> String { "bool".into() }

    fn to_any_type(&self, context: &'static Context) -> AnyTypeEnum<'static> {
        context.bool_type().as_any_type_enum()
    }
}

impl Kind for ArrayType {
    fn to_string(&self) -> String { format!("[{}]{:?}", self.count, self.base) }

    fn to_any_type(&self, context: &'static Context) -> AnyTypeEnum<'static> {
        self.base
            .to_basic_type(context)
            .array_type(self.count)
            .as_any_type_enum()
    }
}

impl Kind for UnknownType {
    fn to_string(&self) -> String { String::from("Unknown") }

    fn to_any_type(&self, _: &'static Context) -> AnyTypeEnum<'static> {
        panic!("Unknown type cannot be converted to LLVM type")
    }
}

impl Kind for VoidType {
    fn to_string(&self) -> String { String::from("Void") }

    fn to_any_type(&self, context: &'static Context) -> AnyTypeEnum<'static> {
        context.void_type().as_any_type_enum()
    }
}
