use crate::hlr::prelude::*;
use crate::parse::prelude::Opcode;
use crate::to_llvm::prelude::*;
use inkwell::context::Context;
use inkwell::execution_engine::JitFunction;
use std::sync::Arc;

type NumGeneratorFunc = unsafe extern "C" fn() -> u32;
type SumFunc = unsafe extern "C" fn(u64, u64, u64) -> u64;

fn context_and_info<'a>() -> (Context, ProgramInfo<'a>) {
    (Context::create(), ProgramInfo::default())
}

fn num_lit<T: Into<u64>>(num: T, typ: &Arc<Type>) -> Box<Expr> {
    Box::new(Expr {
        return_type: typ.clone(),
        kind: ExprKind::Literal(LiteralKind::Number(num.into())),
    })
}

fn ident(name: &str, typ: &Arc<Type>) -> Box<Expr> {
    Box::new(Expr {
        return_type: typ.clone(),
        kind: ExprKind::Ident(name.into()),
    })
}

fn var_decl(name: &str, typ: &Arc<Type>) -> Box<Expr> {
    Box::new(Expr {
        return_type: typ.clone(),
        kind: ExprKind::VarDecl(name.into()),
    })
}

#[test]
fn basic_llvm() {
    let mut ci = context_and_info();
    let compiler = Compiler::from_context_and_info(&ci.0, &mut ci.1);

    // Actual function generation
    let i64_type = compiler.context.i64_type();
    let fn_type = i64_type.fn_type(&[i64_type.into(), i64_type.into(), i64_type.into()], false);
    let function = compiler.module.add_function("sum", fn_type, None);
    let basic_block = compiler.context.append_basic_block(function, "entry");

    compiler.builder.position_at_end(basic_block);

    let x = function.get_nth_param(0).unwrap().into_int_value();
    let y = function.get_nth_param(1).unwrap().into_int_value();
    let z = function.get_nth_param(2).unwrap().into_int_value();

    let sum = compiler.builder.build_int_add(x, y, "sum");
    let sum = compiler.builder.build_int_add(sum, z, "sum");

    compiler.builder.build_return(Some(&sum));

    let sum = unsafe { compiler.execution_engine.get_function("sum").ok() };
    let sum: JitFunction<SumFunc> = sum.unwrap();

    let x = 1u64;
    let y = 2u64;
    let z = 3u64;

    unsafe {
        assert_eq!(sum.call(x, y, z), x + y + z);
    }
}

#[test]
fn compile_from_hlr() {
    let mut ci = context_and_info();
    let compiler = Compiler::from_context_and_info(&ci.0, &mut ci.1);

    let i32_type = compiler.context.i32_type();
    let fn_type = i32_type.fn_type(&[i32_type.into(), i32_type.into(), i32_type.into()], false);
    let function = compiler.module.add_function("sum", fn_type, None);
    let basic_block = compiler.context.append_basic_block(function, "entry");

    let u32_t = compiler
        .program_info
        .borrow()
        .types
        .force_get(&"#prim::u32".into());
    compiler.builder.position_at_end(basic_block);

    let sum = compiler
        .compile_expr(&Expr {
            return_type: u32_t.clone(),
            kind: ExprKind::BinOp(
                Box::new(Expr {
                    return_type: u32_t.clone(),
                    kind: ExprKind::Literal(LiteralKind::Number(32)),
                }),
                Opcode::Plus,
                Box::new(Expr {
                    return_type: u32_t.clone(),
                    kind: ExprKind::Literal(LiteralKind::Number(85)),
                }),
            ),
        })
        .unwrap()
        .into_int_value();

    compiler.builder.build_return(Some(&sum));

    let sum = unsafe { compiler.execution_engine.get_function("sum").ok() };
    let sum: JitFunction<NumGeneratorFunc> = sum.unwrap();

    unsafe {
        assert_eq!(sum.call(), 32 + 85);
    }
}

#[test]
fn compile_with_assignment() {
    let x = 2_u32;
    let y = 21_u32;
    let z = 523;

    let mut ci = context_and_info();
    let mut compiler = Compiler::from_context_and_info(&ci.0, &mut ci.1);

    let i32_type = compiler.context.i32_type();
    let fn_type = i32_type.fn_type(&[], false);
    let function = compiler.module.add_function("sum", fn_type, None);
    let basic_block = compiler.context.append_basic_block(function, "entry");

    compiler.current_function = Some(function);

    compiler.builder.position_at_end(basic_block);

    let u32_t = compiler
        .program_info
        .borrow()
        .types
        .force_get(&"#prim::u32".into());

    // some_var = x
    compiler.compile_expr(&Expr {
        return_type: u32_t.clone(),
        kind: ExprKind::BinOp(
            var_decl("some_var", &u32_t),
            Opcode::Assignment,
            num_lit(x, &u32_t),
        ),
    });

    // some_var = some_var * y
    compiler.compile_expr(&Expr {
        return_type: u32_t.clone(),
        kind: ExprKind::BinOp(
            ident("some_var", &u32_t),
            Opcode::Assignment,
            Box::new(Expr {
                return_type: u32_t.clone(),
                kind: ExprKind::BinOp(
                    ident("some_var", &u32_t),
                    Opcode::Multiplier,
                    num_lit(y, &u32_t),
                ),
            }),
        ),
    });

    // if some_var == x * y { some_var = 20 }
    compiler.compile_expr(&Expr {
        return_type: u32_t.clone(),
        kind: ExprKind::IfThen(
            Box::new(Expr {
                return_type: u32_t.clone(),
                kind: ExprKind::BinOp(
                    ident("some_var", &u32_t),
                    Opcode::Equal,
                    num_lit(x * y, &u32_t),
                ),
            }),
            Box::new(Expr {
                return_type: u32_t.clone(),
                kind: ExprKind::BinOp(
                    ident("some_var", &u32_t),
                    Opcode::Assignment,
                    num_lit(z, &u32_t),
                ),
            }),
        ),
    });

    // some_var
    let result = compiler
        .compile_expr(&*ident("some_var", &u32_t))
        .unwrap()
        .into_int_value();

    compiler.builder.build_return(Some(&result));

    let sum = unsafe { compiler.execution_engine.get_function("sum").ok() };
    let sum: JitFunction<NumGeneratorFunc> = sum.unwrap();

    unsafe {
        assert_eq!(sum.call(), z);
    }
}
