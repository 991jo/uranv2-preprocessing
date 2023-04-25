use crate::graph::{Graph, UranEdge, UranNode};
use crate::packing::Lifetime;
use std::path::Path;
use std::process::exit;

/// Loads a graph from the given path.
///
/// If there is an error while loading the graph this function exits with
/// return code 1.
pub fn load_graph(filename: &Path) -> Graph<UranNode, UranEdge> {
    match Graph::<UranNode, UranEdge>::from_file(filename) {
        Ok(g) => g,
        Err(e) => {
            eprintln!(
                "Failed to open graph from '{}': {:?}",
                filename.display(),
                e
            );
            exit(1);
        }
    }
}

/// Loads lifetimes from the given path.
///
/// If there is an error while loading the lifetimes this function exits with
/// return code 1.
pub fn load_lifetimes(filename: &Path) -> Vec<Lifetime> {
    match crate::packing::load_lifetimes(filename) {
        Ok(g) => g,
        Err(e) => {
            eprintln!(
                "Failed to open lifetimes from '{}': {:?}",
                filename.display(),
                e
            );
            exit(1);
        }
    }
}
