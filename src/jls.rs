use crate::{Circuit, Gate, VarName};
use serde::{Serialize, ser::SerializeStructVariant};
use std::collections::HashSet;

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
            ElementData::WireEnd { .. } => (9, "WireEnd", 8),
        };
        let mut elem = serializer.serialize_struct_variant("ELEMENT", index, variant, len)?;
        // element
        elem.serialize_field("id", &self.0)?;
        elem.serialize_field("x", &self.1.x)?;
        elem.serialize_field("y", &self.1.y)?;
        elem.serialize_field("width", &self.1.w)?;
        elem.serialize_field("height", &self.1.h)?;
        match &self.1.data {
            ElementData::InputPin { name } => {
                elem.serialize_field("name", name)?;
                elem.serialize_field("bits", &1)?;
                elem.serialize_field("watch", &0)?;
                elem.serialize_field("orient", "RIGHT")?;
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
                elem.serialize_field("bits", &1)?;
                elem.serialize_field("numInputs", &num_inputs)?;
                elem.serialize_field("orientation", "right")?;
                elem.serialize_field("delay", &prop_delay)?;
            }

            ElementData::WireEnd { connect, wires } => {
                if let Some(Connector { put, attach }) = connect {
                    elem.serialize_field(
                        "put",
                        match put {
                            Put::Input0 => "input0",
                            Put::Input1 => "input1",
                            Put::Output => "output",
                        },
                    )?;
                    elem.serialize_field("attach", attach)?;
                } else {
                    elem.skip_field("put")?;
                    elem.skip_field("attach")?;
                }
                elem.serialize_field("wires", wires)?;
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
    pub fn save<Writer>(&self, mut output: Writer) -> std::io::Result<()>
    where
        Writer: std::io::Write,
    {
        writeln!(output, "CIRCUIT example")?;
        for (id, Element { x, y, w, h, data }) in self.elements.iter().enumerate() {
            use ElementData::*;
            let typename = match data {
                InputPin { .. } => "InputPin",
                DelayGate => "DelayGate",
                AndGate => "AndGate",
                OrGate => "OrGate",
                XorGate => "XorGate",
                NotGate => "NotGate",
                NandGate => "NandGate",
                NorGate => "NorGate",
                XnorGate => "XnorGate",
                WireEnd { .. } => "WireEnd",
            };
            writeln!(output, "ELEMENT {typename}")?;
            writeln!(output, " int id {id}")?;
            writeln!(output, " int x {x}")?;
            writeln!(output, " int y {y}")?;
            writeln!(output, " int width {w}")?;
            writeln!(output, " int height {h}")?;
            match data {
                InputPin { name } => {
                    writeln!(output, "String name \"{name}\"")?; // VarName should be guaranteed sanitized
                    writeln!(output, "int bits 1")?;
                    writeln!(output, "int watch 0")?;
                    writeln!(output, "String orient \"RIGHT\"")?;
                }

                DelayGate | AndGate | OrGate | XorGate | NotGate | NandGate | NorGate
                | XnorGate => {
                    let (num_inputs, prop_delay) = match data {
                        DelayGate | NotGate => (1, 5),
                        AndGate | OrGate | XorGate | NandGate | NorGate | XnorGate => (2, 10),
                        _ => unreachable!(),
                    };
                    writeln!(output, "int bits 1")?;
                    writeln!(output, "int numInputs {num_inputs}")?;
                    writeln!(output, "String orientation \"right\"")?;
                    writeln!(output, "int delay {prop_delay}")?;
                }

                WireEnd { connect, wires } => {
                    if let Some(Connector { put, attach }) = connect {
                        let put = match put {
                            Put::Input0 => "input0",
                            Put::Input1 => "input1",
                            Put::Output => "output",
                        };
                        writeln!(output, "String put \"{put}\"")?;
                        writeln!(output, "ref attach {attach}")?;
                    }
                    for wire in wires {
                        writeln!(output, "ref wire {wire}")?;
                    }
                }
            }
            writeln!(output, "END")?;
        }
        writeln!(output, "ENDCIRCUIT")?;
        Ok(())
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
