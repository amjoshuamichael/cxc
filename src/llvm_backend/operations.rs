use inkwell::{values::BasicValueEnum, IntPredicate};
use inkwell::{values::*, FloatPredicate};
use crate::llvm_backend::compile_operand;
use crate::llvm_backend::to_llvm_type::ToLLVMType;
use crate::mir::MOperand;
use crate::{TypeEnum, Type, IntType};
use crate::{parse::Opcode, llvm_backend::FunctionCompilationState};

pub fn compile_bin_op(
    fcs: &FunctionCompilationState,
    math_type: &Type,
    op: Opcode,
    l: BasicValueEnum<'static>,
    r: BasicValueEnum<'static>,
    reg: &str,
) -> BasicValueEnum<'static> {
    use Opcode::*;

    let reg = &*reg.to_string();

    match math_type.as_type_enum() {
        TypeEnum::Int(_) | TypeEnum::Bool => {
            use IntPredicate::*;

            let l = l.into_int_value();
            let r = r.into_int_value();

            let signed = matches!(
                math_type.as_type_enum(), 
                TypeEnum::Int(IntType { signed: true, .. }),
            );

            match op {
                Plus => fcs.builder.build_int_add(l, r, reg),
                Minus => fcs.builder.build_int_sub(l, r, reg),
                Multiplier => fcs.builder.build_int_mul(l, r, reg),
                Divider => fcs.builder.build_int_unsigned_div(l, r, reg),
                Modulus => fcs.builder.build_int_unsigned_rem(l, r, reg),
                Inequal => fcs.builder.build_int_compare(NE, l, r, reg),
                Equal => fcs.builder.build_int_compare(EQ, l, r, reg),
                LessThan if signed => fcs.builder.build_int_compare(SLT, l, r, reg),
                LessThan => fcs.builder.build_int_compare(ULT, l, r, reg),
                GrtrThan if signed => fcs.builder.build_int_compare(SGT, l, r, reg),
                GrtrThan => fcs.builder.build_int_compare(UGT, l, r, reg),
                LessOrEqual if signed => fcs.builder.build_int_compare(SLE, l, r, reg),
                LessOrEqual => fcs.builder.build_int_compare(ULE, l, r, reg),
                GreaterOrEqual if signed => fcs.builder.build_int_compare(SGE, l, r, reg),
                GreaterOrEqual => fcs.builder.build_int_compare(UGE, l, r, reg),
                BitShiftL => fcs.builder.build_left_shift(l, r, reg),
                BitShiftR => fcs.builder.build_right_shift(l, r, false, reg),
                BitAND => fcs.builder.build_and(l, r, "and"),
                BitOR => fcs.builder.build_or(l, r, "or"),
                BitXOR => fcs.builder.build_xor(l, r, "xor"),
                _ if math_type.as_type_enum() == &TypeEnum::Bool => {
                    match op {
                        And => fcs.builder.build_and(l, r, reg),
                        Or => fcs.builder.build_or(l, r, reg),
                        _ => todo!(),
                    }
                },
                _ => todo!(),
            }.as_basic_value_enum()
        },
        TypeEnum::Float(_) => {
            use FloatPredicate::*;

            let l = l.into_float_value();
            let r = r.into_float_value();

            if matches!(op, Plus | Minus | Multiplier | Divider | Modulus) {
                match op {
                    Plus => fcs.builder.build_float_add(l, r, reg),
                    Minus => fcs.builder.build_float_sub(l, r, reg),
                    Multiplier => fcs.builder.build_float_mul(l, r, reg),
                    Divider => fcs.builder.build_float_div(l, r, reg),
                    Modulus => fcs.builder.build_float_rem(l, r, reg),
                    _ => unreachable!(),
                }.as_basic_value_enum()
            } else {
                match op {
                    Inequal => fcs.builder.build_float_compare(ONE, l, r, reg),
                    Equal => fcs.builder.build_float_compare(OEQ, l, r, reg),
                    LessThan => fcs.builder.build_float_compare(ULT, l, r, reg),
                    GrtrThan => fcs.builder.build_float_compare(UGT, l, r, reg),
                    _ => todo!(),
                }.as_basic_value_enum()
            }
        },
        TypeEnum::Ref(_) => {
            let l = l.into_pointer_value();
            let r = r.into_pointer_value();

            let result = if matches!(op, Inequal | Equal) {
                let i64 = fcs.context.i64_type();
                let l_int = fcs.builder.build_ptr_to_int(l, i64, "ptr_as_int");
                let r_int = fcs.builder.build_ptr_to_int(r, i64, "ptr_as_int");
                compile_bin_op(
                    fcs,
                    &Type::i(64),
                    op,
                    l_int.as_basic_value_enum(),
                    r_int.as_basic_value_enum(),
                    reg,
                )
            } else {
                todo!()
            };

            result.into()
        }
        _ => todo!("{math_type:?}"),
    }
}

pub fn compile_unar_op(
    fcs: &FunctionCompilationState,
    ret_type: &Type,
    op: Opcode,
    hs: &MOperand,
    reg: &str,
) -> BasicValueEnum<'static> {
    use Opcode::*;

    match op {
        Deref => {
            let hs = compile_operand(fcs, hs).into_pointer_value();

            return fcs.builder
                .build_load(ret_type.to_basic_type(fcs.context), hs, reg)
                .as_basic_value_enum()
        }
        _ => {},
    };

    match ret_type.as_type_enum() {
        TypeEnum::Bool => {
            let hs = compile_operand(fcs, hs).into_int_value();

            match op {
                Not => fcs.builder.build_not(hs, "not"),
                _ => unreachable!(),
            }.as_basic_value_enum()
        }
        _ => todo!("{:?}{:?}", op, ret_type),
    }
}
