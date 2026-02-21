#![deny(clippy::undocumented_unsafe_blocks, clippy::missing_safety_doc)]

use clap::Parser;
use logic_expression_generator::*;
use serde::Serialize;
use std::num::{NonZeroU8, NonZeroU16};

#[derive(Debug, Parser)]
#[command(version, about)]
struct Cli {
    inputs: NonZeroU8,
    depth: NonZeroU16,
    operators: Vec<Operator>,
    #[arg(short, long)]
    tex: bool,
    #[arg(short, long)]
    logic: bool,
    #[arg(short, long)]
    words: bool,
    #[arg(short, long)]
    diagram: bool,
}

#[derive(Debug, Serialize, Default)]
struct Output {
    tex: Option<String>,
    logic: Option<String>,
    words: Option<String>,
    diagram: Option<Diagram>,
}

fn main() {
    let args = Cli::parse();
    let ops = OpList::from_iter(args.operators);
    let Some(circuit) = Circuit::generate_random(args.inputs, args.depth, ops) else {
        eprintln!("cannot generate a circuit with the given combination of inputs and operators");
        std::process::exit(1);
    };
    let mut output = Output::default();
    if args.tex || args.logic || args.words {
        let expr = circuit.to_boolean_expr();
        if args.tex {
            output.tex = Some(expr.to_tex(OpPrecedence::Top));
        }
        if args.logic {
            output.logic = Some(expr.to_logic(OpPrecedence::Top));
        }
        if args.words {
            output.words = Some(expr.to_words(OpPrecedence::Top));
        }
    }
    if args.diagram {
        output.diagram = Some(circuit.to_diagram());
    }
    match serde_json::to_string(&output) {
        Ok(s) => println!("{s}"),
        Err(e) => {
            eprintln!("{e}");
            std::process::exit(1);
        }
    }
}
