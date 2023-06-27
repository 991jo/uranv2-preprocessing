use clap::Parser;
use std::path::PathBuf;
use uranv2::utils::load_lifetimes;

#[derive(Parser, Debug)]
struct Arguments {
    #[arg(long)]
    lifetimes1: PathBuf,
    #[arg(long)]
    lifetimes2: PathBuf,
}

pub fn main() {
    let args = Arguments::parse();

    let lifetimes1 = load_lifetimes(&args.lifetimes1);
    let lifetimes2 = load_lifetimes(&args.lifetimes2);

    for (index, lt1) in lifetimes1.iter().enumerate() {
        let lt2 = lifetimes2[index];

        if *lt1 != lt2 {
            println!("{index}, {lt1:?}, {lt2:?}");
        }
    }
}
