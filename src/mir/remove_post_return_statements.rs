use super::{MIR, MLine};

pub fn remove_post_return_statements(mir: &mut MIR) {
    let mut remove = false;

    let mut n = 0;
    while let Some(line) = mir.lines.get(n) {
        match line {
            MLine::Return(_) => remove = true,
            MLine::Marker(_) => remove = false,
            _ => {
                if remove {
                    mir.lines.remove(n);
                    n -= 1;
                }
            }
        }

        n += 1;
    }
}
