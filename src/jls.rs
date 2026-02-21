use crate::{Circuit, Gate, VarName};
use serde::{Serialize, ser::SerializeStructVariant};
use std::collections::HashSet;

// Note that the purpose is not to have JLS's exact in-memory layout, but simply
// to replicate its file structure enough to open these files within stock JLS.

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Put {
    Input0,
    Input1,
    Output,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Connector {
    put: Put,
    attach: u32,
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
        wires: HashSet<u32>,
    },
}

impl From<Gate> for ElementData {
    fn from(value: Gate) -> Self {
        match value {
            Gate::Buffer(_) => ElementData::DelayGate,
            Gate::And(_, _) => ElementData::AndGate,
            Gate::Or(_, _) => ElementData::OrGate,
            Gate::Xor(_, _) => ElementData::XorGate,
            Gate::Not(_) => ElementData::NotGate,
            Gate::Nand(_, _) => ElementData::NandGate,
            Gate::Nor(_, _) => ElementData::NorGate,
            Gate::Xnor(_, _) => ElementData::XnorGate,
            Gate::Input(name) => ElementData::InputPin { name },
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Element {
    pub x: i32,
    pub y: i32,
    pub w: i32,
    pub h: i32,
    pub data: ElementData,
}

pub struct OrderedElement<'a>(u32, &'a Element);

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
    pub fn from_circuit(circ: &Circuit) -> Self {
        const WIRE_END_RADIUS: i32 = 6;
        const GRID_SIZE: i32 = 2 * WIRE_END_RADIUS;
        const INPUT_WIDTH: i32 = 3 * GRID_SIZE;
        const INPUT_HEIGHT: i32 = 2 * GRID_SIZE;
        const GATE_WIDTH: i32 = 4 * GRID_SIZE;
        const GATE_HEIGHT: i32 = 2 * GRID_SIZE;
        const ELEMENT_GAP: i32 = 2 * GRID_SIZE;

        let mut elements = Vec::with_capacity(circ.gates.len() * 3); // `* 3` to account for typical ratio of WireEnds per non-WireEnd

        // should guarantee order as follows:
        // {{gate|input}{output}}...{[input0][input1]}...
        // gate/input element id = 2*circuit id
        // output element id = 2*circuit id + 1
        // input0/1 element ids = (arbitrary for now)

        for (i, row, col, gate) in circ
            .gates
            .chunk_by(|a, b| a.max_input() == b.max_input())
            .enumerate()
            .flat_map(|(i, chunk)| {
                std::iter::repeat(
                    i32::try_from(i)
                        .expect("circuit should never exceed u16::MAX elements, and i32::MAX > u16::MAX"),
                )
                .zip(chunk.iter().copied())
                .enumerate()
                .map(|(row, (col, gate))| {
                    (
                        i32::try_from(row)
                            .expect("circuit should never exceed u16::MAX elements, and i32::MAX > u16::MAX"),
                        col,
                        gate,
                    )
                })
            })
            .enumerate()
            .map(|(i, (row, col, gate))| {
                (
                    u32::try_from(i)
                        .expect("circuit should never exceed u16::MAX elements, and u32::MAX > u16::MAX"),
                    row,
                    col,
                    gate,
                )
            })
        {
            let x = (ELEMENT_GAP + GATE_WIDTH) * col + ELEMENT_GAP;
            let y = (ELEMENT_GAP + GATE_HEIGHT) * row + ELEMENT_GAP;
            let (w, h) = if matches!(gate, Gate::Input(_)) {
                (INPUT_WIDTH, INPUT_HEIGHT)
            } else {
                (GATE_WIDTH, GATE_HEIGHT)
            };

            // element (2*id)
            elements.push(Element {
                x,
                y,
                w,
                h,
                data: ElementData::from(gate),
            });

            // output WireEnd (2*id + 1)
            elements.push(Element {
                x: x + w,
                y: y + h / 2,
                w: WIRE_END_RADIUS,
                h: WIRE_END_RADIUS,
                data: ElementData::WireEnd {
                    connect: Some(Connector {
                        put: Put::Output,
                        attach: 2*i,
                    }),
                    wires: HashSet::new(),
                },
            });
        }

        let mut wire_id = 2 * u32::try_from(circ.gates.len())
            .expect("circuit should never exceed u16::MAX elements, and u32::MAX > u16::MAX");

        for (i, gate) in circ.gates.iter().enumerate().map(|(i, gate)| {
            (
                u32::try_from(i).expect(
                    "circuit should never exceed u16::MAX elements, and u32::MAX > u16::MAX",
                ),
                gate,
            )
        }) {
            let attach = 2 * i;
            let el = &elements[attach as usize];
            let x = el.x;
            let y = el.y;

            // inputs

            match gate {
                &(Gate::Buffer(a) | Gate::Not(a)) => {
                    let a_wire_id = 2 * a as u32 + 1;
                    // create wire from source
                    elements.push(Element {
                        x,
                        y: y + GATE_HEIGHT / 2,
                        w: WIRE_END_RADIUS,
                        h: WIRE_END_RADIUS,
                        data: ElementData::WireEnd {
                            connect: Some(Connector {
                                put: Put::Input0,
                                attach,
                            }),
                            wires: HashSet::from([a_wire_id]),
                        },
                    });
                    // add that wire to source
                    if let ElementData::WireEnd { wires, .. } =
                        &mut elements[a_wire_id as usize].data
                    {
                        wires.insert(wire_id);
                    } else {
                        panic!("should be guaranteed by insertion order");
                    }
                    wire_id += 1;
                }

                &(Gate::And(a, b)
                | Gate::Or(a, b)
                | Gate::Xor(a, b)
                | Gate::Nand(a, b)
                | Gate::Nor(a, b)
                | Gate::Xnor(a, b)) => {
                    let a_wire_id = 2 * a as u32 + 1;
                    // create wire from source
                    elements.push(Element {
                        x,
                        y,
                        w: WIRE_END_RADIUS,
                        h: WIRE_END_RADIUS,
                        data: ElementData::WireEnd {
                            connect: Some(Connector {
                                put: Put::Input0,
                                attach,
                            }),
                            wires: HashSet::from([a_wire_id]),
                        },
                    });
                    // add that wire to source
                    if let ElementData::WireEnd { wires, .. } =
                        &mut elements[a_wire_id as usize].data
                    {
                        wires.insert(wire_id);
                    } else {
                        panic!("should be guaranteed by insertion order");
                    }
                    wire_id += 1;

                    let b_wire_id = 2 * b as u32 + 1;
                    // create wire from source
                    elements.push(Element {
                        x,
                        y: y + GATE_HEIGHT,
                        w: WIRE_END_RADIUS,
                        h: WIRE_END_RADIUS,
                        data: ElementData::WireEnd {
                            connect: Some(Connector {
                                put: Put::Input1,
                                attach,
                            }),
                            wires: HashSet::from([b_wire_id]),
                        },
                    });
                    // add that wire to source
                    if let ElementData::WireEnd { wires, .. } =
                        &mut elements[b_wire_id as usize].data
                    {
                        wires.insert(wire_id);
                    } else {
                        panic!("should be guaranteed by insertion order");
                    }
                    wire_id += 1;
                }

                Gate::Input(_) => {
                    // do nothing
                }
            }
        }
        Diagram { elements }
    }

    pub fn save<W, S>(&self, mut output: W, name: S) -> std::io::Result<()>
    where
        W: std::io::Write,
        S: std::fmt::Display,
    {
        writeln!(output, "CIRCUIT {name}")?;
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
                    writeln!(output, " String name \"{name}\"")?; // VarName should be guaranteed sanitized
                    writeln!(output, " int bits 1")?;
                    writeln!(output, " int watch 0")?;
                    writeln!(output, " String orient \"RIGHT\"")?;
                }

                DelayGate | AndGate | OrGate | XorGate | NotGate | NandGate | NorGate
                | XnorGate => {
                    let (num_inputs, prop_delay) = match data {
                        DelayGate | NotGate => (1, 5),
                        AndGate | OrGate | XorGate | NandGate | NorGate | XnorGate => (2, 10),
                        _ => unreachable!(),
                    };
                    writeln!(output, " int bits 1")?;
                    writeln!(output, " int numInputs {num_inputs}")?;
                    writeln!(output, " String orientation \"right\"")?;
                    writeln!(output, " int delay {prop_delay}")?;
                }

                WireEnd { connect, wires } => {
                    if let Some(Connector { put, attach }) = connect {
                        let put = match put {
                            Put::Input0 => "input0",
                            Put::Input1 => "input1",
                            Put::Output => "output",
                        };
                        writeln!(output, " String put \"{put}\"")?;
                        writeln!(output, " ref attach {attach}")?;
                    }
                    for wire in wires {
                        writeln!(output, " ref wire {wire}")?;
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
                    .expect("circuit should not exceed u16::MAX elements, and u32::MAX > u16::MAX"),
                element,
            )
        }))
    }
}
