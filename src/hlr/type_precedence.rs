use crate::hlr::prelude::*;

lazy_static! {
    static ref PRECENDENCE_LEVELS: Vec<String> = {
        vec![
            "#prim::u32".into(),
            "#prim::i32".into(),
            "#prim::bool".into(),
            "#prim::u64".into(),
            "#prim::i64".into(),
            "#prim::u16".into(),
            "#prim::i16".into(),
            "#prim::u8".into(),
            "#prim::i8".into(),
        ]
    };
}

pub fn get_precedence(t: &Type) -> usize {
    match PRECENDENCE_LEVELS.iter().position(|name| *name == t.name) {
        Some(index) => index,
        None => PRECENDENCE_LEVELS.len(),
    }
}
