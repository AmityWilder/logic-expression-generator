use std::{
    collections::HashSet,
    io::{BufWriter, Cursor},
};

use crate::{Circuit, Gate, VarName};
use serde::{Serialize, ser::SerializeStructVariant};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct Rectangle {
    x: u32,
    y: u32,
    w: u32,
    h: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Put {
    Input0,
    Input1,
    Output,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Connector {
    put: Put,
    attach: u16,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ElementData {
    InputPin {
        name: VarName,
    },
    DelayGate,
    AndGate,
    OrGate,
    XorGate,
    NotGate,
    NandGate,
    NorGate,
    XnorGate,
    WireEnd {
        connect: Option<Connector>,
        wires: HashSet<u16>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Element {
    pub x: i32,
    pub y: i32,
    pub w: i32,
    pub h: i32,
    pub data: ElementData,
}

pub struct OrderedElement<'a>(u16, &'a Element);

impl<'a> Serialize for OrderedElement<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let (index, variant, len) = match &self.1.data {
            ElementData::DelayGate => (0, "DelayGate", 9),
            ElementData::AndGate => (1, "AndGate", 9),
            ElementData::OrGate => (2, "OrGate", 9),
            ElementData::XorGate => (3, "XorGate", 9),
            ElementData::NotGate => (4, "NotGate", 9),
            ElementData::NandGate => (5, "NandGate", 9),
            ElementData::NorGate => (6, "NorGate", 9),
            ElementData::XnorGate => (7, "XnorGate", 9),
            ElementData::InputPin { .. } => (8, "InputPin", 9),
            ElementData::WireEnd { connect, wires } => (
                9,
                "WireEnd",
                5 + if connect.is_some() { 2 } else { 0 } + wires.len(),
            ),
        };
        let mut elem = serializer.serialize_struct_variant("ELEMENT", index, variant, len)?;
        // element
        elem.serialize_field("int id", &self.0)?;
        elem.serialize_field("int x", &self.1.x)?;
        elem.serialize_field("int y", &self.1.y)?;
        elem.serialize_field("int width", &self.1.w)?;
        elem.serialize_field("int height", &self.1.h)?;
        match &self.1.data {
            ElementData::InputPin { name } => {
                elem.serialize_field("String name", name)?;
                elem.serialize_field("int bits", &1)?;
                elem.serialize_field("int watch", &0)?;
                elem.serialize_field("String orient", "RIGHT")?;
            }

            ElementData::DelayGate
            | ElementData::NotGate
            | ElementData::AndGate
            | ElementData::OrGate
            | ElementData::XorGate
            | ElementData::NandGate
            | ElementData::NorGate
            | ElementData::XnorGate => {
                // gate
                let (num_inputs, prop_delay) = match &self.1.data {
                    ElementData::DelayGate | ElementData::NotGate => (1, 5),

                    ElementData::AndGate
                    | ElementData::OrGate
                    | ElementData::XorGate
                    | ElementData::NandGate
                    | ElementData::NorGate
                    | ElementData::XnorGate => (2, 10),

                    _ => unreachable!(),
                };
                elem.serialize_field("int bits", &1)?;
                elem.serialize_field("int numInputs", &num_inputs)?;
                elem.serialize_field("String orientation", "right")?;
                elem.serialize_field("int delay", &prop_delay)?;
            }

            ElementData::WireEnd { connect, wires } => {
                if let Some(Connector { put, attach }) = connect {
                    elem.serialize_field(
                        "String put",
                        match put {
                            Put::Input0 => "input0",
                            Put::Input1 => "input1",
                            Put::Output => "output",
                        },
                    )?;
                    elem.serialize_field("ref attach", attach)?;
                }
                for wire in wires {
                    elem.serialize_field("ref wire", wire)?;
                }
            }
        }
        elem.end()
    }
}

#[derive(Debug, Clone)]
pub struct Diagram {
    elements: Vec<Element>,
}

impl Diagram {
    pub fn write_to<W>(&self, out: &mut W)
    where
        W: std::io::Write,
    {
        // let mut buffer = zip::ZipWriter::new(Cursor::new(Vec::new()));
        todo!()
    }
}

impl Serialize for Diagram {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.collect_seq(self.elements.iter().enumerate().map(|(id, element)| {
            OrderedElement(
                id.try_into()
                    .expect("circuit should not exceed u16::MAX elements"),
                element,
            )
        }))
    }
}

impl Diagram {
    pub fn from_circuit(circ: &Circuit) -> Self {
        let mut elements = Vec::with_capacity(circ.gates.len() * 3); // `* 3` to account for typical ratio of WireEnds per non-WireEnd
        for (row, col, gate) in circ
            .gates
            .chunk_by(|a, b| a.max_input() == b.max_input())
            .enumerate()
            .flat_map(|(i, chunk)| {
                std::iter::repeat(
                    i32::try_from(i)
                        .expect("circuit should never exceed u16::MAX and i32::MAX > u16::MAX"),
                )
                .zip(chunk.iter().copied())
                .enumerate()
                .map(|(row, (col, gate))| {
                    (
                        i32::try_from(row)
                            .expect("circuit should never exceed u16::MAX and i32::MAX > u16::MAX"),
                        col,
                        gate,
                    )
                })
            })
        {
            const GRID_SIZE: i32 = 6;
            const INPUT_WIDTH: i32 = 5 * GRID_SIZE;
            const INPUT_HEIGHT: i32 = 1 * GRID_SIZE;
            const GATE_WIDTH: i32 = 3 * GRID_SIZE;
            const GATE_HEIGHT: i32 = 3 * GRID_SIZE;
            const ELEMENT_GAP: i32 = 2 * GRID_SIZE;

            let x = INPUT_WIDTH + (ELEMENT_GAP + GATE_WIDTH) * col;
            let y = (ELEMENT_GAP + GATE_HEIGHT) * row;
            let (w, h) = match gate {
                Gate::Input(_) => (INPUT_WIDTH, INPUT_HEIGHT),
                _ => (GATE_WIDTH, GATE_HEIGHT),
            };
            elements.push(Element {
                x,
                y,
                w,
                h,
                data: match gate {
                    Gate::Buffer(_) => ElementData::DelayGate,
                    Gate::And(_, _) => ElementData::AndGate,
                    Gate::Or(_, _) => ElementData::OrGate,
                    Gate::Xor(_, _) => ElementData::XorGate,
                    Gate::Not(_) => ElementData::NotGate,
                    Gate::Nand(_, _) => ElementData::NandGate,
                    Gate::Nor(_, _) => ElementData::NorGate,
                    Gate::Xnor(_, _) => ElementData::XnorGate,
                    Gate::Input(name) => ElementData::InputPin { name },
                },
            });
        }
        Diagram { elements }
    }
}
