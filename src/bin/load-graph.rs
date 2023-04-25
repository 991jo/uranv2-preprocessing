use clap::Parser;
use std::path::PathBuf;
use uranv2::utils::load_graph;
use std::time::Instant;

#[derive(Parser, Debug)]
struct Arguments {
    #[arg(short, long)]
    graph: PathBuf,
}

pub fn main() {

    let timer = Instant::now();
    let args = Arguments::parse();

    let graph = load_graph(&args.graph);

    let delta = timer.elapsed().as_secs_f32();

    println!("{} nodes, {} edges", graph.nodes.len(), graph.edges.len());
    println!("It took {} seconds to load the graph.", delta);
}
