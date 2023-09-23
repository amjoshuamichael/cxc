use std::collections::HashSet;

use super::{MIR, MLine};

pub fn remove_post_return_statements(mir: &mut MIR) {
    let mut previous_len = 0;

    while previous_len != mir.lines.len() {
        previous_len = mir.lines.len();

        let blocks_that_have_gotos: HashSet<u32> = {
            let mut blocks_that_have_gotos = HashSet::new();

            for line in &mir.lines {
                match line {
                    MLine::Goto(g) => { blocks_that_have_gotos.insert(*g); },
                    MLine::Branch { yes, no, .. } => {
                        blocks_that_have_gotos.insert(*yes);
                        blocks_that_have_gotos.insert(*no);
                    }
                    _ => {}
                }
            }

            blocks_that_have_gotos 
        };

        let mut remove = false;
        let mut n = 0;
        while let Some(line) = mir.lines.get(n) {
            match line {
                MLine::Return(_) => remove = true,
                MLine::Marker(m) => {
                    if blocks_that_have_gotos.contains(m) {
                        remove = false
                    }
                }
                _ => {
                    if remove {
                        mir.lines.remove(n);
                        n -= 1;
                    }
                }
            }

            n += 1;
        }

        let remaining_markers: HashSet<u32> = mir.lines
            .iter()
            .filter_map(|line| {
                let MLine::Marker(g) = line else { return None };
                Some(*g)
            })
            .collect();

        mir.lines.retain(|line| {
            let MLine::Goto(g) = line else { return true };
            remaining_markers.contains(g)
        });
    }
}
