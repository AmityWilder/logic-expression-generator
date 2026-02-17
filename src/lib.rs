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

#[derive(Debug, Clone)]
pub enum BooleanExpr {
    Variable(char),
    Buffer(Box<BooleanExpr>),
    And(Vec<BooleanExpr>),
    Or(Vec<BooleanExpr>),
    Xor(Box<[BooleanExpr; 2]>),
    Not(Box<BooleanExpr>),
    Nand(Vec<BooleanExpr>),
    Nor(Vec<BooleanExpr>),
    Xnor(Box<[BooleanExpr; 2]>),
}

impl BooleanExpr {
    /// Stringify the expression as LaTeX (`xy`, `+`, `\overline`, `\oplus`)
    pub fn to_tex(&self) -> String {
        todo!()
    }

    /// Stringify the expression as typical programming logic operators (`&&`, `||`, `!`, `^`, etc.)
    pub fn to_logic(&self) -> String {
        todo!()
    }

    /// Stringify the expression as python-style logic operators (`and`, `or`, `not`, `xor`, etc.)
    pub fn to_words(&self) -> String {
        todo!()
    }
}

pub struct Circuit {
    gates: Vec<Gate>,
    wires: Vec<(usize, usize)>,
}

impl Circuit {
    /// Generate a random boolean expression with exactly `input_count` inputs and depth in `depth_range`,
    /// using only `operators` operators.
    pub fn generate_random(
        input_count: NonZeroUsize,
        depth_range: std::ops::RangeInclusive<NonZeroUsize>,
        operators: OpList,
    ) -> Self {
        todo!()
    }

    /// Convert a circuit to a [BooleanExpr]
    pub fn to_boolean_expression(&self) -> BooleanExpr {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
}
