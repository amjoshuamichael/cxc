use std::collections::{BTreeMap, HashMap};

use crate::{VarName, parse::Opcode, hlr::{hlr_data_output::HLR, expr_tree::{HNodeData, ExprTree}, hlr_data::{VariableInfo, ArgIndex}}, FuncType, TypeEnum, ArrayType, unit::FuncId, FuncQuery, Type};

pub use self::mir_data::*;

mod mir_data;
mod remove_post_return_statements;

use remove_post_return_statements::remove_post_return_statements;

pub fn mir(hlr: HLR, dependencies: HashMap<FuncQuery, FuncId>) -> MIR {
    #[cfg(feature = "xc-debug")]
    println!("--mir type: {:?}--", &hlr.func_type);

    let mut mir = MIR { 
        lines: Vec::new(), 
        variables: hlr.data_flow,
        func_type: hlr.func_type,
        dependencies,
        block_count: 0,
        reg_count: 0,
        addr_reg_count: 0,
        reg_types: BTreeMap::new(),
    };

    build_block(hlr.tree.get(hlr.tree.root), &hlr.tree, &mut mir);

    remove_post_return_statements(&mut mir);

    #[cfg(feature = "xc-debug")]
    {
        for (l, line) in mir.lines.iter().enumerate() {
            // print the index with three digits of space
            println!("{:03}: {}", l, format!("{:?}", line).replace("\n", " "));
        }

        println!("{}", format!("{:?}", &mir.reg_types).replace("\n", " "));
        println!("{}", format!("{:?}", &mir.variables).replace("\n", " "));
    }

    mir
}

fn build_block(node: HNodeData, tree: &ExprTree, mir: &mut MIR) {
    let HNodeData::Block { stmts, .. }  = node else { unreachable!() };

    fn new_block_id(mir: &mut MIR) -> u32 {
        let current_block = mir.block_count;
        mir.block_count += 1;
        current_block
    }

    for stmt in stmts {
        match tree.get(stmt) {
            HNodeData::Number { .. } | HNodeData::Float { .. } | HNodeData::Bool { .. } | HNodeData::Ident { .. } => {},
            HNodeData::Set { lhs, rhs, .. } => {
                let lhs = build_as_addr(tree.get(lhs), &tree, mir);
                let rhs = build_as_operand(tree.get(rhs), &tree, mir);

                mir.lines.push(MLine::Store { l: lhs, val: rhs });
            },
            HNodeData::Return { to_return: Some(expr), .. } => {
                let operand = build_as_operand(tree.get(expr), &tree, mir);

                mir.lines.push(MLine::Return(Some(operand)));
            },
            HNodeData::Return { to_return: None, .. } => {
                mir.lines.push(MLine::Return(None));
            },
            HNodeData::While { w, d } => {
                let beginwhile = new_block_id(mir);
                let whilecode = new_block_id(mir);
                let pastwhile = new_block_id(mir);

                mir.lines.push(MLine::Goto(beginwhile));
                mir.lines.push(MLine::Marker(beginwhile));

                let w = build_as_operand(tree.get(w), tree, mir);

                mir.lines.push(MLine::Branch { 
                    if_: w, 
                    yes: whilecode,
                    no: pastwhile,
                });

                mir.lines.push(MLine::Marker(whilecode));

                build_block(tree.get(d), tree, mir);

                mir.lines.push(MLine::Goto(beginwhile));

                mir.lines.push(MLine::Marker(pastwhile));
            },
            HNodeData::IfThen { i, t, .. } => {
                let i = build_as_operand(tree.get(i), tree, mir);

                let then = new_block_id(mir);
                let after = new_block_id(mir);

                mir.lines.push(MLine::Branch { 
                    if_: i, 
                    yes: then,
                    no: after,
                });

                mir.lines.push(MLine::Marker(then));

                build_block(tree.get(t), tree, mir);
                mir.lines.push(MLine::Goto(after));

                mir.lines.push(MLine::Marker(after));
            }
            _ => {
                let expr = build_as_expr(tree.get(stmt), tree, mir);
                mir.lines.push(MLine::Expr(expr));
            },
        }
    }
}

fn build_as_memloc(node: HNodeData, tree: &ExprTree, add_to: &mut MIR) -> MMemLoc {
    if let HNodeData::Ident { name, .. } = node {
        return MMemLoc::Var(name);
    }

    MMemLoc::Reg(build_as_reg(node, tree, add_to))
}

fn build_as_addr(node: HNodeData, tree: &ExprTree, mir: &mut MIR) -> MAddr {
    match node {
        HNodeData::Ident { name, .. } => {
            if let ArgIndex::Some(_) = mir.variables[&name].arg_index {
                let new_var_name = VarName::from(&*(name.to_string() + "addr"));

                if !mir.variables.contains_key(&new_var_name) {
                    mir.lines.insert(0, MLine::Store {
                        l: MAddr::Var(new_var_name.clone()),
                        val: MOperand::MemLoc(MMemLoc::Var(name.clone())),
                    });

                    mir.variables.insert(new_var_name.clone(), VariableInfo {
                        typ: mir.variables[&name].typ.clone(),
                        arg_index: ArgIndex::None,
                        ..Default::default()
                    });
                }

                MAddr::Var(new_var_name)
            } else {
                MAddr::Var(name)
            }
        },
        HNodeData::UnarOp { op, hs, .. } if op == Opcode::Deref => {
            let load = build_as_memloc(tree.get(hs), tree, mir);
            let new_areg = mir.new_addr_reg();

            mir.lines.push(MLine::SetAddr { 
                l: new_areg, 
                r: MAddrExpr::Expr(MExpr::MemLoc(load)) 
            });

            MAddr::Reg(new_areg)
        },
        _ => MAddr::Reg(build_as_addr_reg(node, tree, mir)),
    }
}

fn build_as_operand(node: HNodeData, tree: &ExprTree, add_to: &mut MIR) -> MOperand {
    match node {
        HNodeData::Number { lit_type, value } => {
            MOperand::Lit(MLit::Int { size: lit_type.size() as u32 * 8, val: value })
        },
        HNodeData::Float { lit_type, value } => {
            MOperand::Lit(MLit::Float { size: lit_type.size() as u32 * 8, val: value })
        },
        HNodeData::Bool { value } => MOperand::Lit(MLit::Bool(value)),
        HNodeData::Call { ref f, ref generics, .. } if &*f.to_string() == "size_of" => {
            MOperand::Lit(MLit::Int { size: 64, val: generics[0].size() as u64 })
        },
        HNodeData::Call { ref f, ref generics, .. } if &*f.to_string() == "typeobj" => {
            MOperand::Lit(MLit::Int { 
                size: 64, 
                val: unsafe { std::mem::transmute(generics[0].clone()) },
            })
        },
        _ => {
            MOperand::MemLoc(build_as_memloc(node, tree, add_to))
        },
    }
}

fn build_as_reg(node: HNodeData, tree: &ExprTree, mir: &mut MIR) -> MReg {
    let l = mir.new_reg(node.ret_type());
    let r = build_as_expr(node, tree, mir);

    mir.lines.push(MLine::Set { l, r, });

    l
}

fn build_as_addr_reg(node: HNodeData, tree: &ExprTree, mir: &mut MIR) -> MAddrReg {
    let l = mir.new_addr_reg();
    let r = build_as_addr_expr(node, tree, mir);

    mir.lines.push(MLine::SetAddr { l, r, });

    l
}

fn build_as_addr_reg_with_normal_expr(node: HNodeData, tree: &ExprTree, mir: &mut MIR) -> MAddrReg {
    let l = mir.new_addr_reg();
    let r = build_as_expr(node, tree, mir);

    mir.lines.push(MLine::SetAddr { l, r: MAddrExpr::Expr(r), });

    l
}

pub fn build_as_expr(node: HNodeData, tree: &ExprTree, mir: &mut MIR) -> MExpr {
    let ret_type = node.ret_type();

    match node {
        HNodeData::BinOp { lhs, op, rhs, .. } => {
            let left_type = tree.get(lhs).ret_type();

            let lhs = build_as_operand(tree.get(lhs), tree, mir);
            let rhs = build_as_operand(tree.get(rhs), tree, mir);

            MExpr::BinOp { left_type, op, l: lhs, r: rhs }
        },
        HNodeData::UnarOp { op, hs, .. } => {
            match op {
                Opcode::Ref => MExpr::Ref { 
                    on: build_as_addr(tree.get(hs), tree, mir) 
                },
                Opcode::Deref => MExpr::Deref { 
                    to: node.ret_type(),
                    on: build_as_operand(tree.get(hs), tree, mir) 
                },
                _ => MExpr::UnarOp { 
                    ret_type, 
                    op, 
                    hs: build_as_operand(tree.get(hs), tree, mir), 
                },
            }
        },
        HNodeData::Call { ref f, ref a, ref generics, .. } if &*f.to_string() == "cast" => {
            let to_type = generics[1].clone();

            let val = build_as_addr(tree.get(a[0]), tree, mir);
            let new_cast_out = mir.new_variable("cast_out", to_type.clone());

            mir.lines.push(MLine::MemCpy {
                from: val, 
                to: MAddr::Var(new_cast_out.clone()), 
                len: MOperand::Lit(MLit::Int { size: 64, val: to_type.size() as u64 }),
            });

            MExpr::MemLoc(MMemLoc::Var(new_cast_out))
        },
        HNodeData::Call { ref f, ref a, .. } if &*f.to_string() == "memcpy" => {
            let from = build_as_addr_reg_with_normal_expr(tree.get(a[0]), tree, mir);
            let to = build_as_addr_reg_with_normal_expr(tree.get(a[1]), tree, mir);
            let len = build_as_operand(tree.get(a[2]), tree, mir);

            mir.lines.push(MLine::MemCpy {
                from: MAddr::Reg(from), 
                to: MAddr::Reg(to), 
                len,
            });

            MExpr::Void
        },
        HNodeData::Call { ref f, ref a, .. } if &*f.to_string() == "memmove" => {
            let from = build_as_operand(tree.get(a[0]), tree, mir);
            let to = build_as_operand(tree.get(a[1]), tree, mir);
            let len = build_as_operand(tree.get(a[2]), tree, mir);

            mir.lines.push(MLine::MemMove {
                from, 
                to, 
                len,
            });

            MExpr::Void
        },
        HNodeData::Call { ref f, ref a, .. } if &*f.to_string() == "free" => {
            let ptr = build_as_operand(tree.get(a[0]), tree, mir);
            MExpr::Free { ptr }
        },
        HNodeData::Call { ref f, ref a, .. } if &**f == "intrinsic_alloc" => {
            let len = build_as_operand(tree.get(a[0]), tree, mir);
            MExpr::Alloc { len }
        },
        HNodeData::Call { ref a, ref ret_type, ref sret, .. } => {
            let func_query = tree.func_query_of_call(&node);
            let func_id = mir.dependencies[&func_query];

            let typ = FuncType {
                ret: ret_type.clone(),
                args: a.iter().map(|a| tree.get(*a).ret_type()).collect(),
            };

            let a = a.iter().map(|a| build_as_operand(tree.get(*a), tree, mir)).collect();
            let sret = sret.as_ref().map(|sret| build_as_memloc(tree.get(*sret), tree, mir));

            MExpr::Call { typ, f: MCallable::Func(func_id), a, sret }
        },
        HNodeData::IndirectCall { ref f, ref a, ref ret_type, ref sret, .. } => {
            let f = build_as_memloc(tree.get(*f), tree, mir);

            let typ = FuncType {
                ret: ret_type.clone(),
                args: a.iter().map(|a| tree.get(*a).ret_type()).collect(),
            };

            let a = a.iter().map(|a| build_as_operand(tree.get(*a), tree, mir)).collect();
            let sret = sret.as_ref().map(|sret| build_as_memloc(tree.get(*sret), tree, mir));

            MExpr::Call { typ, f: MCallable::FirstClass(f), a, sret }
        },
        HNodeData::Member { .. } | HNodeData::Index { .. } => {
            let addr = build_as_addr(node, tree, mir);

            MExpr::Addr(addr)
        },
        HNodeData::Ident { name, .. } => {
            MExpr::MemLoc(MMemLoc::Var(name))
        },
        _ => todo!("{:?}", node),
    }
}

pub fn build_as_addr_expr(node: HNodeData, tree: &ExprTree, mir: &mut MIR) -> MAddrExpr {
    match node {
        HNodeData::Member { object, field, .. } => {
            let object_node = tree.get(object);
            let object_type = object_node.ret_type().complete_deref();
            let object = build_as_addr(object_node, tree, mir);

            let TypeEnum::Struct(struct_type) = 
                object_type.as_type_enum() else { unreachable!() };
            let field_index = struct_type.get_field_index(&field).unwrap() as u32;
            
            MAddrExpr::Member { object_type, object, field_index }
        }
        HNodeData::Index { object, index, .. } => {
            let array_type = tree.get(object).ret_type();
            let TypeEnum::Array(ArrayType { base, .. }) = array_type.as_type_enum() 
                else { unreachable!() };
            let element_type = base.clone();

            let object = build_as_addr(tree.get(object), tree, mir);

            let index = build_as_operand(tree.get(index), tree, mir);

            MAddrExpr::Index { array_type, element_type, object, index }
        }
        
        HNodeData::ArrayLit { .. } 
        | HNodeData::Number { .. } 
        | HNodeData::Float { .. } 
        | HNodeData::Bool { .. } 
        | HNodeData::Call { .. } 
        | HNodeData::UnarOp { .. } 
        | HNodeData::BinOp { .. } => {
            let node_ret_type = node.ret_type();
            let expr = build_as_operand(node, tree, mir);

            if let MOperand::MemLoc(MMemLoc::Var(name)) = expr {
                MAddrExpr::Addr(MAddr::Var(name))
            } else {
                let new_var = mir.new_variable("temp_storage", node_ret_type);
                let addr = MAddr::Var(new_var);

                mir.lines.push(MLine::Store { l: addr.clone(), val: expr });

                MAddrExpr::Addr(addr)
            }
        },
        _ => panic!("{node:?}"),
    }
}
