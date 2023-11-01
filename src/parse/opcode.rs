#[derive(Hash, Default, Debug, Copy, Clone, PartialEq, Eq)]
pub enum Opcode {
    #[default]
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
    Not,
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
    Ref,
    Deref,
    Dot,
    Transform,
    RemoveTypeWrapper,
    Destroy,
}

use Opcode::*;

impl Opcode {
    pub const MAX_UNARY_PRECEDENT_LEVEL: u8 = 4;
    pub fn un_prec_level(&self) -> Option<u8> {
        use Opcode::*;

        match self {
            Ref | Deref => Some(4),
            Not => Some(3),
            Transform => Some(2),
            Destroy => Some(1),
            _ => None,
        }
    }

    pub const MAX_BINARY_PRECEDENT_LEVEL: u8 = 11;
    pub fn bin_prec_level(&self) -> Option<u8> {
        // lower is more significant
        match self {
            Or => Some(11),
            And => Some(10),
            Equal | Inequal => Some(9),
            LessThan | GrtrThan | LessOrEqual | GreaterOrEqual => Some(8),
            BitOR => Some(7),
            BitXOR => Some(6),
            BitAND => Some(5),
            BitShiftL | BitShiftR => Some(4),
            Plus | Minus => Some(3),
            Multiplier | Divider => Some(2),
            Modulus => Some(1),
            _ => None,
        }
    }

    pub fn is_cmp(&self) -> bool {
        use Opcode::*;
        matches!(self, Equal | Inequal | LessThan | GrtrThan | LessOrEqual | GreaterOrEqual)
    }
}

impl ToString for Opcode {
    fn to_string(&self) -> String {
        match self {
            Plus => "+",
            Minus => "-",
            Multiplier => "*",
            Divider => "/",
            Modulus => "%",
            BitAND => "&",
            BitOR => "|",
            BitXOR => "^",
            BitShiftL => "<<",
            BitShiftR => ">>",
            Not => "!",
            Or => "||",
            Transform => "+",
            Destroy => "~",
            And => "&&",
            LessThan => "<",
            GrtrThan => ">",
            LessOrEqual => "<=",
            GreaterOrEqual => ">=",
            Equal => "==",
            Inequal => "!=",
            TernaryQuestion => "?",
            TernaryColon => ":",
            Assignment => "=",
            Ref => "&",
            Deref => "*",
            Dot => ".",
            RemoveTypeWrapper => "<>-",
        }
        .into()
    }
}
