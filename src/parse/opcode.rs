use super::*;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Opcode {
    Exponential,
    Plus,
    Minus,
    Multiplier,
    Divider,
    Modulus,
    BitAND,
    BitOR,
    BitXOR,
    BitShiftL,
    BitShiftR,
    Or,
    And,
    LessThan,
    GrtrThan,
    LessOrEqual,
    GreaterOrEqual,
    Equal,
    Inequal,
    TernaryQuestion,
    TernaryColon,
    Assignment,
    Ref(u8),
    Deref(u8),
}

impl Opcode {
    pub const MAX_UN_PREC: u8 = 0;
    pub fn un_prec_level(&self) -> Option<u8> {
        use Opcode::*;

        match self {
            Ref(_) | Deref(_) => Some(0),
            _ => None,
        }
    }

    pub const MAX_BIN_PREC: u8 = 10;
    pub fn bin_prec_level(&self) -> Option<u8> {
        use Opcode::*;

        // lower is more significant
        match self {
            Or => Some(10),
            And => Some(9),
            Equal | Inequal => Some(8),
            LessThan | GrtrThan | LessOrEqual | GreaterOrEqual => Some(7),
            BitOR => Some(6),
            BitXOR => Some(5),
            BitAND => Some(4),
            BitShiftL | BitShiftR => Some(3),
            Plus | Minus => Some(2),
            Multiplier | Divider => Some(1),
            Exponential => Some(0),
            _ => None,
        }
    }
}
