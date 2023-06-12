use clap::Parser;
use std::path::PathBuf;
use std::process::exit;
use std::time::Instant;
use uranv2::graph::Level;
use uranv2::query::NaiveQuery;
use uranv2::utils::{load_graph, load_lifetimes};

#[derive(Parser, Debug)]
struct Arguments {
    #[arg(short, long)]
    graph: PathBuf,

    #[arg(long)]
    lifetimes: PathBuf,

    #[arg(short, long)]
    level: Level,

    #[arg(short, long)]
    output: PathBuf,
}

pub fn main() {
    let timer = Instant::now();
    let args = Arguments::parse();

    let graph = load_graph(&args.graph);

    let delta = timer.elapsed().as_secs_f32();

    println!("{} nodes, {} edges", graph.nodes.len(), graph.edges.len());
    println!("It took {} seconds to load the graph.", delta);

    let lifetimes = load_lifetimes(&args.lifetimes);

    let mut counter = 0;
    for lifetime in lifetimes.iter() {
        if !lifetime.is_empty() {
            counter += 1;
            //dbg!(lifetime);
        };
    }

    println!("{} edges with non-empty lifetimes.", counter);

    assert!(lifetimes.len() == graph.edges.len());

    let query = NaiveQuery {
        graph: &graph,
        lifetimes,
    };

    let result = query.level_query(args.level);
    match result.save(&args.output) {
        Ok(_) => (),
        Err(e) => {
            println!("An error occured while writing the file: {}", e);
            exit(1);
        }
    }
}
