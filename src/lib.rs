#![forbid(clippy::undocumented_unsafe_blocks, clippy::missing_safety_doc)]

use rand::prelude::*;
use std::num::{NonZeroU8, NonZeroU16};
use arrayvec::ArrayVec;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum Operator {
    #[default]
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

impl std::ops::Index<Operator> for OpList {
    type Output = bool;

    fn index(&self, index: Operator) -> &Self::Output {
        match index {
            Operator::Buffer => &self.buffer,
            Operator::And => &self.and,
            Operator::Or => &self.or,
            Operator::Xor => &self.xor,
            Operator::Not => &self.not,
            Operator::Nand => &self.nand,
            Operator::Nor => &self.nor,
            Operator::Xnor => &self.xnor,
        }
    }
}

impl std::ops::IndexMut<Operator> for OpList {
    fn index_mut(&mut self, index: Operator) -> &mut Self::Output {
        match index {
            Operator::Buffer => &mut self.buffer,
            Operator::And => &mut self.and,
            Operator::Or => &mut self.or,
            Operator::Xor => &mut self.xor,
            Operator::Not => &mut self.not,
            Operator::Nand => &mut self.nand,
            Operator::Nor => &mut self.nor,
            Operator::Xnor => &mut self.xnor,
        }
    }
}

impl OpList {
    #[inline]
    pub const fn is_empty(&self) -> bool {
        !(self.buffer
            || self.and
            || self.or
            || self.xor
            || self.not
            || self.nand
            || self.nor
            || self.xnor)
    }

    #[inline]
    pub const fn len(&self) -> usize {
        (self.buffer as u8
            + self.and as u8
            + self.or as u8
            + self.xor as u8
            + self.not as u8
            + self.nand as u8
            + self.nor as u8
            + self.xnor as u8) as usize
    }

    /// `len` of single-input operators
    #[inline]
    pub const fn mono_len(&self) -> usize {
        (self.buffer as u8 + self.not as u8) as usize
    }

    #[inline]
    pub fn iter(&self) -> impl Iterator<Item = Operator> {
        [
            self.buffer.then_some(Operator::Buffer),
            self.not.then_some(Operator::Not),
            // ^ single-input first ^
            self.and.then_some(Operator::And),
            self.or.then_some(Operator::Or),
            self.xor.then_some(Operator::Xor),
            self.nand.then_some(Operator::Nand),
            self.nor.then_some(Operator::Nor),
            self.xnor.then_some(Operator::Xnor),
        ]
        .into_iter()
        .flatten()
    }

    /// Iterator over single-input operators
    #[inline]
    pub fn iter_mono(&self) -> impl Iterator<Item = Operator> {
        [
            self.buffer.then_some(Operator::Buffer),
            self.not.then_some(Operator::Not),
        ]
        .into_iter()
        .flatten()
    }

    #[inline]
    pub fn array(&self) -> ArrayVec<Operator, 8> {
        self.iter().collect()
    }

    #[inline]
    pub fn mono_array(&self) -> ArrayVec<Operator, 2> {
        self.iter_mono().collect()
    }
}

/// Negative = no subscript
/// Positive = subscript is that number
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VarName(i8);

impl VarName {
    #[inline]
    pub const fn name(&self) -> char {
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

/// An expression whose `outer_precedence` is greater than the `inner_precedence`
/// of the expression it is an argument to must be wrapped in parentheses.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub enum OpPrecedence {
    Atom,
    Not,
    And,
    Xor,
    Or,
    #[default]
    Top,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BooleanExpr {
    Buffer(Box<BooleanExpr>),
    And(Box<(BooleanExpr, BooleanExpr)>),
    Or(Box<(BooleanExpr, BooleanExpr)>),
    Xor(Box<(BooleanExpr, BooleanExpr)>),
    Not(Box<BooleanExpr>),
    Nand(Box<(BooleanExpr, BooleanExpr)>),
    Nor(Box<(BooleanExpr, BooleanExpr)>),
    Xnor(Box<(BooleanExpr, BooleanExpr)>),
    Variable(VarName),
}

impl From<VarName> for BooleanExpr {
    #[inline]
    fn from(value: VarName) -> Self {
        Self::Variable(value)
    }
}

impl BooleanExpr {
    #[inline]
    pub fn buffer(self) -> Self {
        Self::Buffer(Box::new(self))
    }
    #[inline]
    pub fn and(self, rhs: Self) -> Self {
        Self::And(Box::new((self, rhs)))
    }
    #[inline]
    pub fn or(self, rhs: Self) -> Self {
        Self::Or(Box::new((self, rhs)))
    }
    #[inline]
    pub fn xor(self, rhs: Self) -> Self {
        Self::Xor(Box::new((self, rhs)))
    }
    #[inline]
    #[allow(clippy::should_implement_trait)]
    pub fn not(self) -> Self {
        Self::Not(Box::new(self))
    }
    #[inline]
    pub fn nand(self, rhs: Self) -> Self {
        Self::Nand(Box::new((self, rhs)))
    }
    #[inline]
    pub fn nor(self, rhs: Self) -> Self {
        Self::Nor(Box::new((self, rhs)))
    }
    #[inline]
    pub fn xnor(self, rhs: Self) -> Self {
        Self::Xnor(Box::new((self, rhs)))
    }

    pub const fn inner_precedence(&self) -> OpPrecedence {
        match self {
            Self::Buffer(_) => OpPrecedence::Top,
            Self::Nand(_) | Self::And(_) => OpPrecedence::And,
            Self::Nor(_) | Self::Or(_) => OpPrecedence::Or,
            Self::Xnor(_) | Self::Xor(_) => OpPrecedence::Xor,
            Self::Not(_) => OpPrecedence::Not,
            Self::Variable(_) => OpPrecedence::Atom,
        }
    }

    pub const fn outer_precedence(&self) -> OpPrecedence {
        match self {
            Self::Buffer(_) => OpPrecedence::Top,
            Self::And(_) => OpPrecedence::And,
            Self::Or(_) => OpPrecedence::Or,
            Self::Xor(_) => OpPrecedence::Xor,
            Self::Not(_) | Self::Nand(_) | Self::Nor(_) | Self::Xnor(_) => OpPrecedence::Not,
            Self::Variable(_) => OpPrecedence::Atom,
        }
    }

    /// Stringify the expression as LaTeX (`xy`, `+`, `\overline`, `\oplus`)
    pub fn to_tex(&self, parent_inner_precedence: OpPrecedence) -> String {
        let expr = match self {
            Self::Buffer(x) => format!("({})", x.to_tex(self.inner_precedence())),
            Self::And(x) => format!(
                "{}{}",
                x.0.to_tex(self.inner_precedence()),
                x.1.to_tex(self.inner_precedence())
            ),
            Self::Or(x) => format!(
                "{} + {}",
                x.0.to_tex(self.inner_precedence()),
                x.1.to_tex(self.inner_precedence())
            ),
            Self::Xor(x) => {
                format!(
                    "{} \\oplus {}",
                    x.0.to_tex(self.inner_precedence()),
                    x.1.to_tex(self.inner_precedence())
                )
            }
            Self::Not(x) => format!("\\overline{{{}}}", x.to_tex(self.inner_precedence())),
            Self::Nand(x) => format!(
                "\\overline{{{}{}}}",
                x.0.to_tex(self.inner_precedence()),
                x.1.to_tex(self.inner_precedence())
            ),
            Self::Nor(x) => format!(
                "\\overline{{{} + {}}}",
                x.0.to_tex(self.inner_precedence()),
                x.1.to_tex(self.inner_precedence())
            ),
            Self::Xnor(x) => {
                format!(
                    "\\overline{{{} \\oplus {}}}",
                    x.0.to_tex(self.inner_precedence()),
                    x.1.to_tex(self.inner_precedence())
                )
            }
            Self::Variable(var) => var.to_string(),
        };
        if self.outer_precedence() > parent_inner_precedence {
            format!("({expr})")
        } else {
            expr
        }
    }

    /// Stringify the expression as typical programming logic operators (`&&`, `||`, `!`, `^`, etc.)
    pub fn to_logic(&self, parent_inner_precedence: OpPrecedence) -> String {
        let expr = match self {
            Self::Buffer(x) => format!("({})", x.to_logic(self.inner_precedence())),
            Self::And(x) => format!(
                "{} && {}",
                x.0.to_logic(self.inner_precedence()),
                x.1.to_logic(self.inner_precedence())
            ),
            Self::Or(x) => format!(
                "{} || {}",
                x.0.to_logic(self.inner_precedence()),
                x.1.to_logic(self.inner_precedence())
            ),
            Self::Xor(x) => format!(
                "{} ^ {}",
                x.0.to_logic(self.inner_precedence()),
                x.1.to_logic(self.inner_precedence())
            ),
            Self::Not(x) => format!("!{}", x.to_logic(self.inner_precedence())),
            Self::Nand(x) => format!(
                "!({} && {})",
                x.0.to_logic(self.inner_precedence()),
                x.1.to_logic(self.inner_precedence())
            ),
            Self::Nor(x) => format!(
                "!({} || {})",
                x.0.to_logic(self.inner_precedence()),
                x.1.to_logic(self.inner_precedence())
            ),
            Self::Xnor(x) => format!(
                "!({} ^ {})",
                x.0.to_logic(self.inner_precedence()),
                x.1.to_logic(self.inner_precedence())
            ),
            Self::Variable(var) => var.to_string(),
        };
        if self.outer_precedence() > parent_inner_precedence {
            format!("({expr})")
        } else {
            expr
        }
    }

    /// Stringify the expression as python-style logic operators (`and`, `or`, `not`, `xor`, etc.)
    pub fn to_words(&self, parent_inner_precedence: OpPrecedence) -> String {
        let expr = match self {
            Self::Buffer(x) => format!("({})", x.to_words(self.inner_precedence())),
            Self::And(x) => format!(
                "{} and {}",
                x.0.to_words(self.inner_precedence()),
                x.1.to_words(self.inner_precedence())
            ),
            Self::Or(x) => format!(
                "{} or {}",
                x.0.to_words(self.inner_precedence()),
                x.1.to_words(self.inner_precedence())
            ),
            Self::Xor(x) => format!(
                "{} xor {}",
                x.0.to_words(self.inner_precedence()),
                x.1.to_words(self.inner_precedence())
            ),
            Self::Not(x) => format!("not {}", x.to_words(self.inner_precedence())),
            Self::Nand(x) => format!(
                "not ({} and {})",
                x.0.to_words(self.inner_precedence()),
                x.1.to_words(self.inner_precedence())
            ),
            Self::Nor(x) => format!(
                "not ({} or {})",
                x.0.to_words(self.inner_precedence()),
                x.1.to_words(self.inner_precedence())
            ),
            Self::Xnor(x) => format!(
                "not ({} xor {})",
                x.0.to_words(self.inner_precedence()),
                x.1.to_words(self.inner_precedence())
            ),
            Self::Variable(var) => var.to_string(),
        };
        if self.outer_precedence() > parent_inner_precedence {
            format!("({expr})")
        } else {
            expr
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AddGateError {
    OutOfIds,
    IndexOutOfBounds,
}

impl std::fmt::Display for AddGateError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AddGateError::OutOfIds => write!(f, "circuit cannot exceed {} elements", u16::MAX),
            AddGateError::IndexOutOfBounds => write!(f, "gate connects to an out of bounds index"),
        }
    }
}

impl std::error::Error for AddGateError {}

/// Number fields are input indices
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Gate {
    Buffer(u16),
    And(u16, u16),
    Or(u16, u16),
    Xor(u16, u16),
    Not(u16),
    Nand(u16, u16),
    Nor(u16, u16),
    Xnor(u16, u16),
    Variable(VarName),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct Rectangle {
    x: u32,
    y: u32,
    w: u32,
    h: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Element {
    pub rect: Rectangle,
    pub gate: Gate,
}

impl Gate {
    pub const fn is_valid(self, gate_count: usize) -> bool {
        match self {
            Gate::Buffer(x) | Gate::Not(x) => (x as usize) < gate_count,
            Gate::And(a, b)
            | Gate::Or(a, b)
            | Gate::Xor(a, b)
            | Gate::Nand(a, b)
            | Gate::Nor(a, b)
            | Gate::Xnor(a, b) => (a as usize) < gate_count && (b as usize) < gate_count,
            Gate::Variable(_) => true,
        }
    }
}

/// Append only, no remove.
///
/// Gates should never connect to a gate at a higher index than their own.
#[derive(Debug, Clone)]
pub struct Circuit {
    /// Inputs are all first.
    gates: Vec<Gate>,
}

impl Circuit {
    /// Returns the index of the newly added gate.
    ///
    /// Element is not added on error.
    pub fn add_gate(&mut self, gate: Gate) -> Result<u16, AddGateError> {
        if gate.is_valid(self.gates.len()) {
            u16::try_from(self.gates.len())
                .map_err(|_| AddGateError::OutOfIds)
                .inspect(|_| self.gates.push(gate))
        } else {
            Err(AddGateError::IndexOutOfBounds)
        }
    }

    /// Returns an iterator over the indices of the newly added gates.
    ///
    /// No elements are added on error.
    pub fn add_gates(&mut self, gates: &[Gate]) -> Result<std::ops::Range<u16>, AddGateError> {
        let start =
            u16::try_from(self.gates.len()).expect("circuit should never exceed u16::MAX elements");
        let end = self
            .gates
            .len()
            .checked_add(gates.len())
            .and_then(|n| u16::try_from(n).ok())
            .ok_or(AddGateError::OutOfIds)?;
        if gates.iter().enumerate().all(|(i, gate)| gate.is_valid(i)) {
            self.gates.reserve(gates.len());
            self.gates.extend(gates);
            Ok(start..end)
        } else {
            Err(AddGateError::IndexOutOfBounds)
        }
    }

    /// Generate a random boolean expression with exactly `input_count` x and depth in `depth_range`,
    /// using only `operators` operators.
    ///
    /// Returns [`None`] if `operators` is empty, or includes no single-input gates in a single-input circuit.
    pub fn generate_random(
        input_count: NonZeroU8,
        depth: NonZeroU16,
        operators: OpList,
    ) -> Option<Self> {
        if operators.is_empty() || (input_count.get() == 1 && operators.mono_len() == 0) {
            return None;
        }

        let ops = operators.array();
        let mono_ops = &ops[..operators.mono_len()];
        
        let mut rng = rand::rng();

        let mut circuit = Self {
            gates: Vec::with_capacity(input_count.get() as usize + depth.get() as usize),
        };

        const NON_SUBSCRIPT_NAMES: &[u8] = b"xyzw";
        if (input_count.get() as usize) <= NON_SUBSCRIPT_NAMES.len() {
            circuit.gates.extend(
                NON_SUBSCRIPT_NAMES
                    .iter()
                    .take(input_count.get() as usize)
                    .map(|&v| Gate::Variable(VarName(-(v as i8)))),
            );
        } else {
            circuit
                .gates
                .extend((0..input_count.get()).map(|v| Gate::Variable(VarName(v as i8))));
        };

        for i in input_count.get() as u16..input_count.get() as u16 + depth.get() {
            let gate = if i >= 2 {
                let [a, b] = rand::seq::index::sample_array::<_, 2>(&mut rng, i as usize)
                    .expect("should be guaranteed by i>=2");
                let [a, b] = [a as u16, b as u16];
                match ops
                    .choose(&mut rng)
                    .expect("should have exited if no operators")
                {
                    Operator::Buffer => Gate::Buffer(a),
                    Operator::And => Gate::And(a, b),
                    Operator::Or => Gate::Or(a, b),
                    Operator::Xor => Gate::Xor(a, b),
                    Operator::Not => Gate::Not(a),
                    Operator::Nand => Gate::Nand(a, b),
                    Operator::Nor => Gate::Nor(a, b),
                    Operator::Xnor => Gate::Xnor(a, b),
                }
            } else {
                let a = rand::random_range(..i);
                match mono_ops.choose(&mut rng).copied()
                    .expect("should have exited if only one input and no single-input operators")
                {
                    Operator::Buffer => Gate::Buffer(a),
                    Operator::Not => Gate::Not(a),
                    _ => unreachable!("mono_ops should never contain a multi-input operator"),
                }
            };
            circuit
                .add_gate(gate)
                .expect("for loop should ensure increment, and ID count should be guarded against");
        }
        Some(circuit)
    }

    /// Convert a circuit to a [`BooleanExpr`]
    ///
    /// Returns [`None`] if circuit is empty
    pub fn to_boolean_expr(&self) -> Option<BooleanExpr> {
        // Assumes no gate connects to a gate at a greater index than its own.

        fn convert(src: &[Gate], idx: u16) -> BooleanExpr {
            match src[idx as usize] {
                Gate::Buffer(x) => convert(src, x).buffer(),
                Gate::And(a, b) => convert(src, a).and(convert(src, b)),
                Gate::Or(a, b) => convert(src, a).or(convert(src, b)),
                Gate::Xor(a, b) => convert(src, a).xor(convert(src, b)),
                Gate::Not(x) => convert(src, x).not(),
                Gate::Nand(a, b) => convert(src, a).nand(convert(src, b)),
                Gate::Nor(a, b) => convert(src, a).nor(convert(src, b)),
                Gate::Xnor(a, b) => convert(src, a).xnor(convert(src, b)),
                Gate::Variable(v) => BooleanExpr::Variable(v),
            }
        }

        u16::try_from(self.gates.len())
            .expect("circuit should not exceed u16::MAX elements")
            .checked_sub(1)
            .map(|root| convert(self.gates.as_slice(), root))
    }
}

#[repr(C)]
pub struct CCircuit {
    data: *mut std::ffi::c_void,
    size: usize,
}

impl CCircuit {
    pub fn from_circuit(value: Option<Circuit>) -> Self {
        match value {
            Some(circ) => {
                let data = circ.gates.into_boxed_slice();
                let size = data.len();
                Self {
                    data: Box::into_raw(data).cast(),
                    size,
                }
            }
            None => Self {
                data: std::ptr::null_mut(),
                size: 0,
            },
        }
    }

    /// # Safety
    ///
    /// `self` must have been created with [`CCircuit::from_circuit`] and be untampered with.
    pub unsafe fn into_circuit(self) -> Option<Circuit> {
        (!self.data.is_null()).then(|| {
            let raw = std::ptr::slice_from_raw_parts_mut(self.data.cast::<Gate>(), self.size);
            Circuit {
                // SAFETY: Caller must uphold safety contract
                gates: unsafe { Box::from_raw(raw) }.to_vec(),
            }
        })
    }
}

/// Returns `null` for [`None`]
#[unsafe(no_mangle)]
pub extern "C" fn generate_random_circuit(
    input_count: u8,
    depth: u16,
    buffer: bool,
    and: bool,
    or: bool,
    xor: bool,
    not: bool,
    nand: bool,
    nor: bool,
    xnor: bool,
) -> CCircuit {
    CCircuit::from_circuit(
        NonZeroU8::new(input_count)
            .zip(NonZeroU16::new(depth))
            .and_then(|(input_count, depth)| {
                Circuit::generate_random(
                    input_count,
                    depth,
                    OpList {
                        buffer,
                        and,
                        or,
                        xor,
                        not,
                        nand,
                        nor,
                        xnor,
                    },
                )
            }),
    )
}

#[unsafe(no_mangle)]
pub extern "C" fn circuit_is_none(circuit: &CCircuit) -> bool {
    circuit.data.is_null()
}

#[cfg(test)]
mod tests {
    use super::*;

    impl VarName {
        pub const fn from_char(ch: char) -> Self {
            assert!(ch.is_ascii(), "VarName must be ascii");
            assert!((ch as u8) < i8::MAX as u8, "VarName out of bounds");
            Self(-(ch as u8 as i8))
        }
    }

    #[test]
    fn test_empty_to_boolean_expr() {
        assert_eq!(
            Circuit {
                gates: Vec::from([])
            }
            .to_boolean_expr(),
            None
        );
    }

    #[test]
    fn test_var_to_boolean_expr() {
        let a = VarName::from_char('a');
        assert_eq!(
            Circuit {
                gates: Vec::from([Gate::Variable(a)])
            }
            .to_boolean_expr(),
            Some(BooleanExpr::Variable(a))
        );
    }

    #[test]
    fn test_buffer_to_boolean_expr() {
        let a = VarName::from_char('a');
        assert_eq!(
            Circuit {
                gates: Vec::from([Gate::Variable(a), Gate::Buffer(0)])
            }
            .to_boolean_expr(),
            Some(BooleanExpr::Variable(a).buffer())
        );
    }

    #[test]
    fn test_combo_to_boolean_expr() {
        let a = VarName::from_char('a');
        let b = VarName::from_char('b');
        assert_eq!(
            Circuit {
                gates: Vec::from([Gate::Variable(a), Gate::Variable(b), Gate::And(0, 1),])
            }
            .to_boolean_expr(),
            Some(BooleanExpr::Variable(a).and(BooleanExpr::Variable(b)))
        );
    }

    #[test]
    fn test_combo_of_combo_to_boolean_expr() {
        let a = VarName::from_char('a');
        let b = VarName::from_char('b');
        assert_eq!(
            Circuit {
                gates: Vec::from([
                    Gate::Variable(a),
                    Gate::Variable(b),
                    Gate::And(0, 1),
                    Gate::Nor(0, 2)
                ])
            }
            .to_boolean_expr(),
            Some(
                BooleanExpr::Variable(a)
                    .nor(BooleanExpr::Variable(a).and(BooleanExpr::Variable(b)))
            )
        );
    }

    #[test]
    fn test_boolean_expr_to_tex() {
        let a = VarName::from_char('a');
        let b = VarName::from_char('b');
        assert_eq!(
            BooleanExpr::Variable(a)
                .nor(BooleanExpr::Variable(a).and(BooleanExpr::Variable(b)))
                .to_tex(Default::default())
                .as_str(),
            "\\overline{a + ab}"
        );
    }

    #[test]
    fn test_boolean_expr_to_tex_paren1() {
        let a = VarName::from_char('a');
        let b = VarName::from_char('b');
        assert_eq!(
            BooleanExpr::Variable(a)
                .and(BooleanExpr::Variable(a).or(BooleanExpr::Variable(b)))
                .to_tex(Default::default())
                .as_str(),
            "a(a + b)"
        );
    }

    #[test]
    fn test_boolean_expr_to_tex_paren2() {
        let a = VarName::from_char('a');
        let b = VarName::from_char('b');
        assert_eq!(
            BooleanExpr::Variable(a)
                .or(BooleanExpr::Variable(b))
                .and(BooleanExpr::Variable(a).or(BooleanExpr::Variable(b)))
                .to_tex(Default::default())
                .as_str(),
            "(a + b)(a + b)"
        );
    }

    #[test]
    fn test_gen_circuit() {
        let circ = Circuit::generate_random(
            NonZeroU8::new(2).unwrap(),
            NonZeroU16::new(10).unwrap(),
            OpList {
                buffer: false,
                and: true,
                or: true,
                xor: true,
                not: true,
                nand: false,
                nor: false,
                xnor: false,
            },
        )
        .unwrap();
        println!("{circ:?}");
        println!();
        let expr = circ.to_boolean_expr().unwrap();
        println!("{expr:?}");
        println!();
        println!("LaTeX: {}", expr.to_tex(Default::default()));
        println!("logic: {}", expr.to_logic(Default::default()));
        println!("words: {}", expr.to_words(Default::default()));
    }
}
