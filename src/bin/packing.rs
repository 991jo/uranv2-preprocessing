use clap::{Parser, ValueEnum};
use std::path::PathBuf;
use std::time::Instant;
use uranv2::packing::{save_lifetimes, GraphPacker, IterativeUnpacker, NaiveUnpacker};
use uranv2::utils::load_graph;

#[derive(Parser, Debug)]
struct Arguments {
    #[arg(short, long)]
    graph: PathBuf,

    #[arg(short, long)]
    output: PathBuf,

    #[arg(short, long)]
    packer: Packer,
}

#[derive(Copy, Clone, PartialEq, Eq, Ord, PartialOrd, ValueEnum, Debug)]
enum Packer {
    Iterative,
    Naive,
}

pub fn main() {
    let timer = Instant::now();
    let args = Arguments::parse();

    let graph = load_graph(&args.graph);

    let delta = timer.elapsed().as_secs_f32();

    println!("{} nodes, {} edges", graph.nodes.len(), graph.edges.len());
    println!("It took {} seconds to load the graph.", delta);

    let timer = Instant::now();

    let mut packer: Box<dyn GraphPacker> = match args.packer {
        Packer::Iterative => Box::new(IterativeUnpacker::new(&graph)),
        Packer::Naive => Box::new(NaiveUnpacker::new(&graph)),
    };

    let lifetimes = packer.pack();

    let delta = timer.elapsed().as_secs_f32();
    println!("It took {} seconds to pack the graph.", delta);

    match save_lifetimes(&lifetimes, &args.output) {
        Ok(_) => (),
        Err(e) => {
            println!("An error occured while saving the lifetimes: {}", e);
            std::process::exit(1);
        }
    }
}
