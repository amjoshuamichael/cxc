use std::{collections::{BTreeMap, HashMap}, rc::Rc};

use crate::{parse::Opcode, hlr::{hlr_data_output::HLR, expr_tree::{HNodeData, ExprTree, ExprID, HCallable, HLit}}, FuncType, TypeEnum, ArrayType, unit::{FuncId, Global}, FuncQuery, typ::{ABI, EnumType}};

pub use self::mir_data::*;

mod mir_data;
mod remove_post_return_statements;
mod remove_redundant_gotos;

use remove_post_return_statements::remove_post_return_statements;
use remove_redundant_gotos::remove_redundant_gotos;
use slotmap::SecondaryMap;

impl MIR {
    fn new_block_id(&mut self) -> u32 {
        let current_block = self.block_count;
        self.block_count += 1;
        current_block
    }
}

pub fn mir(hlr: HLR, dependencies: HashMap<FuncQuery, FuncId>) -> MIR {
    #[cfg(feature = "mir-debug")]
    println!("--mir type: {:?}--", &hlr.func_type);

    let mut mir = MIR { 
        lines: Vec::new(), 
        variables: hlr.variables,
        func_type: hlr.func_type,
        dependencies,
        block_count: 0,
        reg_count: 0,
        addr_reg_count: 0,
        reg_types: BTreeMap::new(),
        block_labels: HashMap::new(),
        from_tree: ExprTree::default(),
        dbg_map: HashMap::new(),
    };

    for goto_label in hlr.tree.nodes.values() {
        let HNodeData::GotoLabel(name) = &goto_label.data else { continue };
        let new_block_id = mir.new_block_id();
        mir.block_labels.insert(name.clone(), new_block_id);
    }

    build_block(hlr.tree.get_ref(hlr.tree.root), &hlr.tree, &mut mir);
    mir.from_tree = hlr.tree;

    remove_post_return_statements(&mut mir);
    remove_redundant_gotos(&mut mir);

    #[cfg(feature = "mir-debug")]
    {
        for (l, line) in mir.lines.iter().enumerate() {
            // print the index with three digits of space
            println!("{:03}: {}", l, format!("{:?}", line).replace("\n", " "));
        }

        println!("{}", format!("{:?}", &mir.reg_types).replace("\n", " "));
        for (var_id, var_info) in &mir.variables {
            println!("{var_id:?}, {var_info:?}");
        }
    }

    mir
}

fn build_block(node: &HNodeData, tree: &ExprTree, mir: &mut MIR) {
    let HNodeData::Block { stmts, .. }  = node else { unreachable!() };

    for stmt in stmts {
        mir.dbg_map.insert(mir.lines.len() + 1, *stmt);

        build_stmt(*stmt, tree, mir);
    }
}

fn build_stmt(stmt: ExprID, tree: &ExprTree, mir: &mut MIR) {
    match tree.get_ref(stmt) {
        HNodeData::Lit { .. } | HNodeData::Ident { .. } => {},
        HNodeData::Set { lhs, rhs, .. } => {
            let lhs = build_as_addr(tree.get_ref(*lhs), &tree, mir);
            let rhs = build_as_operand(tree.get_ref(*rhs), &tree, mir);

            mir.lines.push(MLine::Store { l: lhs, val: rhs });
        },
        HNodeData::Return { to_return: Some(expr), .. } => {
            let operand = build_as_operand(tree.get_ref(*expr), &tree, mir);
            mir.lines.push(MLine::Return(Some(operand)));
        },
        HNodeData::Return { to_return: None, .. } => {
            mir.lines.push(MLine::Return(None));
        },
        HNodeData::GotoLabel(name) => {
            let block_id = mir.block_labels[&name];

            mir.lines.push(MLine::Goto(block_id));
            mir.lines.push(MLine::Marker(block_id));
        },
        HNodeData::Goto(name) => {
            let mut block = tree.block_of(stmt);
            loop {
                let HNodeData::Block { goto_labels, .. } = tree.get_ref(block)
                    else { unreachable!() };

                if let Some(goto_label_id) = goto_labels.get(&name) {
                    let block_id = mir.block_labels[goto_label_id];
                    mir.lines.push(MLine::Goto(block_id));
                    break;
                }

                let parent_block = tree.statement_and_block(block).1;

                if parent_block == block { panic!() }
                block = parent_block;
            }
        }
        HNodeData::While { w, d } => {
            let beginwhile = mir.new_block_id();
            let whilecode = mir.new_block_id();
            let pastwhile = mir.new_block_id();

            mir.lines.push(MLine::Goto(beginwhile));
            mir.lines.push(MLine::Marker(beginwhile));

            let w = build_as_operand(tree.get_ref(*w), tree, mir);

            mir.lines.push(MLine::Branch { 
                if_: w, 
                yes: whilecode,
                no: pastwhile,
            });

            mir.lines.push(MLine::Marker(whilecode));

            build_block(tree.get_ref(*d), tree, mir);

            mir.lines.push(MLine::Goto(beginwhile));

            mir.lines.push(MLine::Marker(pastwhile));
        },
        HNodeData::IfThenElse { i, t, e, .. } => {
            let i = build_as_operand(tree.get_ref(*i), tree, mir);

            let then = mir.new_block_id();
            let else_ = mir.new_block_id();
            let after = mir.new_block_id();

            mir.lines.push(MLine::Branch { 
                if_: i, 
                yes: then,
                no: else_,
            });

            mir.lines.push(MLine::Marker(then));
            build_stmt(*t, tree, mir);
            mir.lines.push(MLine::Goto(after));

            mir.lines.push(MLine::Marker(else_));
            build_stmt(*e, tree, mir);
            mir.lines.push(MLine::Goto(after));

            mir.lines.push(MLine::Marker(after));
        }
        HNodeData::Block { .. } => build_block(tree.get_ref(stmt), tree, mir),
        _ => {
            if let Some(expr) = build_as_expr(tree.get_ref(stmt), tree, mir) {
                mir.lines.push(MLine::Expr(expr));
            }
        },
    }
}

fn build_as_memloc(node: &HNodeData, tree: &ExprTree, add_to: &mut MIR) -> MMemLoc {
    if let HNodeData::Ident { var_id: name, .. } = node {
        return MMemLoc::Var(*name);
    } else if let HNodeData::GlobalLoad { global: Global::Value { name, .. }, .. } = node {
        return MMemLoc::Global(name.clone());
    }

    MMemLoc::Reg(build_as_reg(node, tree, add_to))
}

fn build_as_addr(node: &HNodeData, tree: &ExprTree, mir: &mut MIR) -> MAddr {
    match node {
        HNodeData::Ident { var_id: id, .. } => {
            let var = &mir.variables[*id];
            if var.is_arg_or_sret() {
                let addr_var = mir.new_variable(var.typ.raw_arg_type(ABI::C).clone());

                mir.lines.insert(0, MLine::Store {
                    l: MAddr::Var(addr_var),
                    val: MOperand::Memloc(MMemLoc::Var(*id)),
                });

                MAddr::Var(addr_var)
            } else {
                MAddr::Var(*id)
            }
        },
        HNodeData::UnarOp { op, hs, .. } if *op == Opcode::Deref => {
            let load = build_as_memloc(tree.get_ref(*hs), tree, mir);
            let new_areg = mir.new_addr_reg();

            mir.lines.push(MLine::SetAddr { 
                l: new_areg, 
                r: MAddrExpr::Expr(MExpr::MemLoc(load)) 
            });

            MAddr::Reg(new_areg)
        },
        HNodeData::UnarOp { op, hs, .. } if *op == Opcode::RemoveTypeWrapper =>
            return build_as_addr(tree.get_ref(*hs), tree, mir),
        _ => MAddr::Reg(build_as_addr_reg(node, tree, mir)),
    }
}

fn build_as_operand(node: &HNodeData, tree: &ExprTree, mir: &mut MIR) -> MOperand {
    match node {
        HNodeData::Lit { lit: HLit::Int(value), var_type } => {
            MOperand::Int { size: var_type.size() as u32 * 8, val: *value }
        },
        HNodeData::Lit { lit: HLit::Float(value), var_type } => {
            MOperand::Float { size: var_type.size() as u32 * 8, val: (*value).into() }
        },
        HNodeData::Lit { lit: HLit::Bool(value), .. } => MOperand::Bool(*value),
        HNodeData::Lit { lit: HLit::Variant(name), var_type, .. } => {
            let TypeEnum::Enum(EnumType { variants, .. }) = var_type.as_type_enum()
                else { unreachable!() };
            let value = variants.iter().position(|n| n == name).unwrap();
            MOperand::Int { size: var_type.size() as u32 * 8, val: value as u64 }
        },
        HNodeData::GlobalLoad { global: Global::Func(func_query), .. } => {
            let func_id = mir.dependencies[&func_query];
            MOperand::Function(func_id)
        }
        HNodeData::Call { call: HCallable::Direct(ref query), .. } 
            if &*query.name.to_string() == "size_of" => {
            MOperand::Int { size: 64, val: query.generics[0].size() as u64 }
        },
        HNodeData::Call { call: HCallable::Direct(ref query), .. } 
            if &*query.name.to_string() == "typeobj" => {
            MOperand::Int { 
                size: 64, 
                val: unsafe { std::mem::transmute(query.generics[0].clone()) },
            }
        },
        _ => {
            MOperand::Memloc(build_as_memloc(node, tree, mir))
        },
    }
}

fn build_as_reg(node: &HNodeData, tree: &ExprTree, mir: &mut MIR) -> MReg {
    let l = mir.new_reg(node.ret_type());
    let r = build_as_expr(node, tree, mir);

    mir.lines.push(MLine::Set { l, r: r.unwrap(), });

    l
}

fn build_as_addr_reg(node: &HNodeData, tree: &ExprTree, mir: &mut MIR) -> MAddrReg {
    let l = mir.new_addr_reg();
    let r = build_as_addr_expr(node, tree, mir);

    match r {
        MAddrExpr::Addr(MAddr::Reg(reg)) => {
            reg
        },
        _ => {
            mir.lines.push(MLine::SetAddr { l, r, });

            l
        }
    }
}

fn build_as_addr_reg_with_normal_expr(node: &HNodeData, tree: &ExprTree, mir: &mut MIR) -> MAddrReg {
    let l = mir.new_addr_reg();
    let node_type = node.ret_type();
    let r = build_as_expr(node, tree, mir).unwrap();

    if let MExpr::Addr(addr) = r {
        let lr = mir.new_reg(node_type);
        mir.lines.push(MLine::Set { l: lr, r: MExpr::Addr(addr) });
        mir.lines.push(MLine::SetAddr { l, r: MAddrExpr::Expr(MExpr::MemLoc(MMemLoc::Reg(lr))), });
    } else {
        mir.lines.push(MLine::SetAddr { l, r: MAddrExpr::Expr(r), });
    }

    l
}

pub fn build_as_expr(node: &HNodeData, tree: &ExprTree, mir: &mut MIR) -> Option<MExpr> {
    let ret_type = node.ret_type();
    let expr: MExpr = match node {
        HNodeData::BinOp { lhs, op, rhs, .. } => {
            let left_type = tree.get(*lhs).ret_type();

            let lhs = build_as_operand(tree.get_ref(*lhs), tree, mir);
            let rhs = build_as_operand(tree.get_ref(*rhs), tree, mir);

            MExpr::BinOp { left_type, op: *op, l: lhs, r: rhs }
        },
        HNodeData::UnarOp { op, hs, .. } => {
            match op {
                Opcode::Ref => MExpr::Ref { 
                    on: build_as_addr(tree.get_ref(*hs), tree, mir) 
                },
                Opcode::Deref => MExpr::Deref { 
                    to: node.ret_type(),
                    on: build_as_memloc(tree.get_ref(*hs), tree, mir) 
                },
                Opcode::RemoveTypeWrapper => return build_as_expr(tree.get_ref(*hs), tree, mir),
                _ => MExpr::UnarOp { 
                    ret_type, 
                    op: *op, 
                    hs: build_as_operand(tree.get_ref(*hs), tree, mir), 
                },
            }
        },
        HNodeData::Call { call: HCallable::Direct(ref query), ref a, .. } 
            if &*query.name == "cast" => {
            let from_type = query.generics[0].clone();
            let to_type = query.generics[1].clone();

            if from_type.is_ref() && to_type.is_ref() {
                build_as_expr(tree.get_ref(a[0]), tree, mir).unwrap()
            } else {
                let val = build_as_addr(tree.get_ref(a[0]), tree, mir);
                let new_cast_out = mir.new_variable(to_type.clone());

                mir.lines.push(MLine::MemCpy {
                    from: val, 
                    to: MAddr::Var(new_cast_out.clone()), 
                    len: MOperand::Int { size: 64, val: to_type.size() as u64 },
                });

                MExpr::MemLoc(MMemLoc::Var(new_cast_out))
            }
        },
        HNodeData::Call { call: HCallable::Direct(ref query), ref a, .. } 
            if &*query.name == "memcpy" => {
            let from = build_as_addr_reg_with_normal_expr(tree.get_ref(a[0]), tree, mir);
            let to = build_as_addr_reg_with_normal_expr(tree.get_ref(a[1]), tree, mir);
            let len = build_as_operand(tree.get_ref(a[2]), tree, mir);

            mir.lines.push(MLine::MemCpy {
                from: MAddr::Reg(from), 
                to: MAddr::Reg(to), 
                len,
            });

            return None;
        },
        HNodeData::Call { call: HCallable::Direct(ref query), ref a, .. } 
            if &*query.name == "memmove" => {
            let from = build_as_operand(tree.get_ref(a[0]), tree, mir);
            let to = build_as_operand(tree.get_ref(a[1]), tree, mir);
            let len = build_as_operand(tree.get_ref(a[2]), tree, mir);

            mir.lines.push(MLine::MemMove {
                from, 
                to, 
                len,
            });

            return None;
        },
        HNodeData::Call { call: HCallable::Direct(ref query), ref a, .. } 
            if &*query.name == "free" => {
            let ptr = build_as_operand(tree.get_ref(a[0]), tree, mir);
            MExpr::Free { ptr }
        },
        HNodeData::Call { call: HCallable::Direct(ref query), ref a, .. } 
            if &*query.name == "intrinsic_alloc" => {
            let len = build_as_operand(tree.get_ref(a[0]), tree, mir);
            MExpr::Alloc { len }
        },
        HNodeData::Call { call: HCallable::Direct(ref query), ref a, ref ret_type, ref sret, .. } => {
            let func_id = mir.dependencies[&query];

            let typ = FuncType {
                ret: ret_type.clone(),
                args: a.iter().map(|a| tree.get_ref(*a).ret_type()).collect(),
                abi: ABI::None,
            };

            let a = a.iter().map(|a| build_as_operand(tree.get_ref(*a), tree, mir)).collect();
            let sret = sret.as_ref().map(|sret| build_as_memloc(tree.get_ref(*sret), tree, mir));

            MExpr::Call { typ, f: MCallable::Func(func_id), a, sret }
        },
        HNodeData::Call { call: HCallable::Indirect(ref f), ref a, ref ret_type, ref sret, .. } => {
            let f = build_as_memloc(tree.get_ref(*f), tree, mir);

            let typ = FuncType {
                ret: ret_type.clone(),
                args: a.iter().map(|a| tree.get(*a).ret_type()).collect(),
                abi: ABI::None,
            };

            let a = a.iter().map(|a| build_as_operand(tree.get_ref(*a), tree, mir)).collect();
            let sret = sret.as_ref().map(|sret| build_as_memloc(tree.get_ref(*sret), tree, mir));

            MExpr::Call { typ, f: MCallable::FirstClass(f), a, sret }
        },
        HNodeData::Member { .. } | HNodeData::Index { .. } => {
            let addr = build_as_addr(node, tree, mir);

            MExpr::Addr(addr)
        },
        HNodeData::Ident { var_id, .. } => {
            MExpr::MemLoc(MMemLoc::Var(*var_id))
        },
        HNodeData::GlobalLoad { global: Global::Value { name, .. }, .. } => {
            MExpr::MemLoc(MMemLoc::Global(name.clone()))
        },
        _ => todo!("{:?}", node),
    };

    return Some(expr);
}

pub fn build_as_addr_expr(node: &HNodeData, tree: &ExprTree, mir: &mut MIR) -> MAddrExpr {
    match node {
        HNodeData::Member { object, field, .. } => {
            let object_node = tree.get_ref(*object);
            let object_type = object_node.ret_type();
            let object = build_as_addr(object_node, tree, mir);

            match object_type.as_type_enum() {
                TypeEnum::Struct(struct_type) => {
                    let field_index = struct_type.get_field_index(&field).unwrap() as u32;

                    if field_index == 0 {
                        // Since this is the first field, we don't have to add an offset 
                        // to the pointer in order to access it.
                        MAddrExpr::Addr(object)
                    } else {
                        MAddrExpr::Member { object_type, object, field_index }
                    }
                },
                TypeEnum::Union(union_type) => {
                    MAddrExpr::Addr(object)
                },
                _ => unreachable!(),
            }
        },
        HNodeData::Index { object, index, .. } => {
            let array_type = tree.get(*object).ret_type();
            let TypeEnum::Array(ArrayType { base, .. }) = array_type.as_type_enum() 
                else { unreachable!() };
            let element_type = base.clone();

            let object = build_as_addr(tree.get_ref(*object), tree, mir);

            let index = build_as_operand(tree.get_ref(*index), tree, mir);

            MAddrExpr::Index { array_type, element_type, object, index }
        },
        HNodeData::UnarOp { op, hs, .. } if *op == Opcode::RemoveTypeWrapper =>
            return build_as_addr_expr(tree.get_ref(*hs), tree, mir),
        HNodeData::ArrayLit { .. } 
        | HNodeData::Lit { .. } 
        | HNodeData::Call { .. } 
        | HNodeData::GlobalLoad { .. } 
        | HNodeData::UnarOp { .. } 
        | HNodeData::BinOp { .. } => {
            let node_ret_type = node.ret_type();
            let expr = build_as_operand(&node, tree, mir);

            let new_var = mir.new_variable(node_ret_type);
            let addr = MAddr::Var(new_var);

            mir.lines.push(MLine::Store { l: addr.clone(), val: expr });

            MAddrExpr::Addr(addr)
        },
        _ => panic!("{node:?}"),
    }
}
