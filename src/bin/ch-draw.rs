use clap::Parser;
use std::path::PathBuf;
use std::process::exit;
use std::time::Instant;
use uranv2::graph::{Level, UranNode, UranEdge};
use uranv2::query::{QueryResponse};
use uranv2::utils::load_graph;

#[derive(Parser, Debug)]
struct Arguments {
    #[arg(short, long)]
    graph: PathBuf,

    #[arg(short, long)]
    level: Level,

    #[arg(short, long)]
    output: PathBuf,
}

pub fn main() {
    let args = Arguments::parse();

    let graph = load_graph(&args.graph);

    let level = args.level;

    let mut result = QueryResponse::<UranNode, UranEdge>::new();

    for edge in graph.edges.iter() {

        let edge_level = graph.nodes[edge.src as usize].level.min(graph.nodes[edge.dst as usize].level);

        if edge_level < level {
            continue
        }

        if !edge.is_shortcut() {
            result.edges.push(edge.clone());
            println!("adding original edge");
            continue;
        }

        let bridge_a = &graph.edges[edge.bridge_a as usize];

        let contracted_node_level = graph.nodes[bridge_a.dst as usize].level;

        if contracted_node_level >= level {
            continue;
        }
        result.edges.push(edge.clone());

    }

    result.build_response(&graph);
    println!("{} nodes and {} edges were collected", result.nodes.len(), result.edges.len());
    match result.save(&args.output) {
        Ok(_) => (),
        Err(e) => {
            println!("An error occured while writing the file: {}", e);
            exit(1);
        }
    }
}
