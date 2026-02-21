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
    #[arg(short, long)]
    zip: Option<PathBuf>,
}

#[derive(Debug, Serialize, Default)]
struct Output {
    tex: Option<String>,
    logic: Option<String>,
    words: Option<String>,
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
    if args.diagram || args.zip.is_some() {
        let diagram = circuit.to_diagram();
        if let Some(zip_path) = args.zip {
            let file = std::fs::File::create(&zip_path)?;
            let mut stream = std::io::BufWriter::new(file);
            let mut zip = zip::ZipWriter::new(&mut stream);
            let options =
                SimpleFileOptions::default().compression_method(zip::CompressionMethod::Deflated);
            zip.start_file("JLSCircuit", options)?;
            diagram.save(zip)?;
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
