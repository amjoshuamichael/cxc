use super::prelude::*;
use crate::parse::*;
use num_bigint::BigInt;
use std::collections::HashMap;
use std::sync::Arc;

pub fn do_type_inference(hlr: &mut HLR) {
    let var_locs = get_variable_locations(hlr);

    println!("{var_locs:?}");

    let mut var_types = HashMap::new();

    for (name, locs) in &var_locs {
        let constraints = find_constraints_for_var(hlr, locs);

        let possible_types: Vec<Arc<Type>> = hlr
            .types
            .0
            .iter()
            .filter(|t| t.impls.fits_constraints(&constraints))
            .map(|arc| arc.clone())
            .collect();

        let precedence_levels = possible_types
            .iter()
            .map(|t| get_precedence(t))
            .collect::<Vec<usize>>();

        let mut types_for_var = possible_types
            .iter()
            .zip(precedence_levels)
            .collect::<Vec<(&Arc<Type>, usize)>>();

        types_for_var.sort_by(|(_, p1), (_, p2)| p1.cmp(&p2));

        let type_for_var = types_for_var[0].0.clone();

        println!("&&&VARTYPE: {name}, {types_for_var:?}");

        var_types.insert(name, type_for_var);
    }

    for (name, locs) in &var_locs {
        let decl_data = NodeData::VarDecl {
            var_type: var_types.get(&name).unwrap().clone(),
            name: name.clone(),
        };

        hlr.tree.replace(locs[0], decl_data);

        let ident_data = NodeData::Ident {
            var_type: var_types.get(&name).unwrap().clone(),
            name: name.clone(),
        };

        for loc in locs.iter().skip(1) {
            hlr.tree.replace(*loc, ident_data.clone());
        }
    }
}

fn find_constraints_for_var(hlr: &HLR, locs: &Vec<ExprID>) -> Constraints {
    use NodeData::*;

    let mut constraints = Constraints::default();

    for loc in locs {
        match hlr.tree.get(hlr.tree.parent(*loc)) {
            BinOp { lhs, rhs, op, .. } => {
                let is_var_lhs = lhs == *loc;
                let other_side = if is_var_lhs { rhs } else { lhs };
                let other_side = hlr.tree.get(other_side);

                match op {
                    Opcode::Assignment => {
                        if is_var_lhs {
                            let rhs = hlr.tree.get(rhs);

                            match rhs.gen_ret_type() {
                                PrimInt => {
                                    if let Number(n) = rhs {
                                        constraints.0.push(Constraint::FromIntLiteral(n))
                                    }
                                }
                                _ => todo!(),
                            }
                        }
                    }
                    Opcode::Plus => match other_side {
                        Number(_) => constraints.0.push(Constraint::AddToIntLiteral),
                        Ident { name, .. } => constraints.0.push(Constraint::AddToType(name)),
                        _ => todo!(),
                    },
                    // TODO: other operators
                    _ => {}
                }
            }
            // TODO: other scenarios
            _ => {}
        };
    }

    constraints
}

#[derive(Default, Debug)]
pub struct Constraints(pub Vec<Constraint>);

#[derive(Debug)]
pub enum Constraint {
    FromIntLiteral(BigInt),
    AddToIntLiteral,
    AddToType(Arc<str>),
}

fn get_variable_locations(hlr: &HLR) -> HashMap<Arc<str>, Vec<ExprID>> {
    let mut variable_locations = HashMap::<Arc<str>, Vec<ExprID>>::new();

    for (id, data) in hlr.tree.iter() {
        match data {
            NodeData::Ident { name, .. } => {
                variable_locations
                    .entry(name.clone())
                    .or_insert(vec![])
                    .push(id);
            }
            _ => {}
        }
    }

    variable_locations
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lex::*;

    #[test]
    fn type_inference() {
        let mut hlr = hlr(parse(lex("
        {
            x = 4893
            x = x + 1
            y = 55
            x = y + x
            z = 21
        }
        ")));

        do_type_inference(&mut hlr);
    }
}
