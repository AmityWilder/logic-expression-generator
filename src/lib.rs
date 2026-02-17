use std::num::NonZeroUsize;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Gate {
    Buffer,
    And,
    Or,
    Xor,
    Not,
    Nand,
    Nor,
    Xnor,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct OpList {
    pub buffer: bool,
    pub and: bool,
    pub or: bool,
    pub xor: bool,
    pub not: bool,
    pub nand: bool,
    pub nor: bool,
    pub xnor: bool,
}

/// Negative = no subscript
/// Positive = subscript is that number
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VarName(i8);

impl VarName {
    #[inline]
    pub fn name(&self) -> char {
        if self.0.is_negative() {
            -self.0 as u8 as char
        } else {
            'v'
        }
    }

    #[inline]
    pub fn subscript(&self) -> Option<u8> {
        u8::try_from(self.0).ok()
    }
}

impl std::fmt::Display for VarName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = self.name();
        match self.subscript() {
            Some(subscript) => write!(f, "{name}_{subscript}"),
            None => name.fmt(f),
        }
    }
}

#[derive(Debug, Clone)]
pub enum BooleanExpr {
    Variable(VarName),
    Buffer(Box<BooleanExpr>),
    And(Box<(BooleanExpr, BooleanExpr)>),
    Or(Box<(BooleanExpr, BooleanExpr)>),
    Xor(Box<(BooleanExpr, BooleanExpr)>),
    Not(Box<BooleanExpr>),
    Nand(Box<(BooleanExpr, BooleanExpr)>),
    Nor(Box<(BooleanExpr, BooleanExpr)>),
    Xnor(Box<(BooleanExpr, BooleanExpr)>),
}

impl BooleanExpr {
    /// Stringify the expression as LaTeX (`xy`, `+`, `\overline`, `\oplus`)
    pub fn to_tex(&self) -> String {
        match self {
            BooleanExpr::Variable(var) => var.to_string(),
            BooleanExpr::Buffer(x) => format!("({})", x.to_tex()),
            BooleanExpr::And(x) => format!("({})({})", x.0.to_tex(), x.1.to_tex()),
            BooleanExpr::Or(x) => format!("({}) + ({})", x.0.to_tex(), x.1.to_tex()),
            BooleanExpr::Xor(x) => {
                format!("({}) \\oplus ({})", x.0.to_tex(), x.1.to_tex())
            }
            BooleanExpr::Not(x) => format!("\\overline{{({})}}", x.to_tex()),
            BooleanExpr::Nand(x) => format!("\\overline{{({})({})}}", x.0.to_tex(), x.1.to_tex()),
            BooleanExpr::Nor(x) => format!("\\overline{{({}) + ({})}}", x.0.to_tex(), x.1.to_tex()),
            BooleanExpr::Xnor(x) => {
                format!(
                    "\\overline{{({}) \\oplus ({})}}",
                    x.0.to_tex(),
                    x.1.to_tex()
                )
            }
        }
    }

    /// Stringify the expression as typical programming logic operators (`&&`, `||`, `!`, `^`, etc.)
    pub fn to_logic(&self) -> String {
        match self {
            BooleanExpr::Variable(var) => var.to_string(),
            BooleanExpr::Buffer(x) => format!("({})", x.to_logic()),
            BooleanExpr::And(x) => format!("({}) && ({})", x.0.to_logic(), x.1.to_logic()),
            BooleanExpr::Or(x) => format!("({}) || ({})", x.0.to_logic(), x.1.to_logic()),
            BooleanExpr::Xor(x) => format!("({}) ^ ({})", x.0.to_logic(), x.1.to_logic()),
            BooleanExpr::Not(x) => format!("!({})", x.to_logic()),
            BooleanExpr::Nand(x) => format!("!(({}) && ({}))", x.0.to_logic(), x.1.to_logic()),
            BooleanExpr::Nor(x) => format!("!(({}) || ({}))", x.0.to_logic(), x.1.to_logic()),
            BooleanExpr::Xnor(x) => format!("!(({}) ^ ({}))", x.0.to_logic(), x.1.to_logic()),
        }
    }

    /// Stringify the expression as python-style logic operators (`and`, `or`, `not`, `xor`, etc.)
    pub fn to_words(&self) -> String {
        match self {
            BooleanExpr::Variable(var) => var.to_string(),
            BooleanExpr::Buffer(x) => format!("({})", x.to_words()),
            BooleanExpr::And(x) => format!("({}) and ({})", x.0.to_words(), x.1.to_words()),
            BooleanExpr::Or(x) => format!("({}) or ({})", x.0.to_words(), x.1.to_words()),
            BooleanExpr::Xor(x) => format!("({}) xor ({})", x.0.to_words(), x.1.to_words()),
            BooleanExpr::Not(x) => format!("not ({})", x.to_words()),
            BooleanExpr::Nand(x) => format!("not (({}) and ({}))", x.0.to_words(), x.1.to_words()),
            BooleanExpr::Nor(x) => format!("not (({}) or ({}))", x.0.to_words(), x.1.to_words()),
            BooleanExpr::Xnor(x) => format!("not (({}) xor ({}))", x.0.to_words(), x.1.to_words()),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Circuit {
    gates: Vec<Gate>,
    wires: Vec<(usize, usize)>,
}

impl Circuit {
    /// Generate a random boolean expression with exactly `input_count` x and depth in `depth_range`,
    /// using only `operators` operators.
    ///
    /// Returns [`None`] if `operators` is empty.
    pub fn generate_random(
        input_count: NonZeroUsize,
        depth_range: std::ops::RangeInclusive<NonZeroUsize>,
        operators: OpList,
    ) -> Option<Self> {
        todo!()
    }

    /// Convert a circuit to a [`BooleanExpr`]
    pub fn to_boolean_expression(&self) -> BooleanExpr {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
}
