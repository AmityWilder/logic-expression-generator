#![deny(clippy::undocumented_unsafe_blocks, clippy::missing_safety_doc)]

use clap::Parser;
use logic_expression_generator::{jls::*, *};
use serde::Serialize;
use std::{
    io::Write,
    num::{NonZeroU8, NonZeroU16},
    path::PathBuf,
};
use zip::write::SimpleFileOptions;

#[derive(Debug, Parser)]
#[command(version, about)]
struct Cli {
    /// How many inputs the diagram should have
    inputs: NonZeroU8,

    /// How many layers of gates the diagram should have
    depth: NonZeroU16,

    /// What operators the generator is allowed to use
    ///
    /// The generator may fail if there are no gates, or if there are no single-input gates when `inputs`=1
    operators: Vec<Operator>,

    /// Convert the circuit to boolean logic in LaTeX format
    #[arg(short, long)]
    tex: bool,

    /// Convert the circuit to boolean logic in common programming format
    #[arg(short, long)]
    logic: bool,

    /// Convert the circuit to boolean logic in Python format
    #[arg(short, long)]
    words: bool,

    /// Convert the circuit to a JLS diagram in JSON form alongside the output
    #[arg(short, long)]
    diagram: bool,

    /// Store the circuit diagram as a JLS file
    #[arg(short, long)]
    jls: Option<PathBuf>,

    /// Output the jls file without zipping (useful for debugging)
    #[arg(short, long)]
    unzipped: bool,
}

#[derive(Debug, Serialize, Default)]
struct Output {
    #[serde(skip_serializing_if = "Option::is_none")]
    tex: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    logic: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    words: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    diagram: Option<Diagram>,
}

fn inner_main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Cli::parse();
    let ops = OpList::from_iter(args.operators);
    let circuit = Circuit::generate_random(args.inputs, args.depth, ops)
        .ok_or("cannot generate a circuit with the given combination of inputs and operators")?;
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
    if args.diagram || args.jls.is_some() {
        let diagram = circuit.to_diagram();
        if let Some(zip_path) = args.jls {
            let name = zip_path
                .file_stem()
                .ok_or("zip must be a file, not a directory")?;
            let file = std::fs::File::create(&zip_path)?;
            let mut stream = std::io::BufWriter::new(file);
            if args.unzipped {
                diagram.save(&mut stream, name.display())?;
            } else {
                let mut zip = zip::ZipWriter::new(&mut stream);
                let options = SimpleFileOptions::default()
                    .compression_method(zip::CompressionMethod::Deflated);
                zip.start_file("JLSCircuit", options)?;
                diagram.save(zip, name.display())?;
            }
            stream.flush()?;
        }
        if args.diagram {
            output.diagram = Some(diagram);
        }
    }
    println!("{}", serde_json::to_string(&output)?);
    Ok(())
}

fn main() {
    if let Err(e) = inner_main() {
        eprintln!("{e}");
        std::process::exit(1);
    }
}
