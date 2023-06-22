use crate::graph::{EdgeTrait, Graph, Level, NodeId, NodeTrait, UranEdge, UranNode};
use crate::packing::Lifetime;
use std::io::{BufWriter, Write};
use std::path::Path;

pub struct NaiveQuery<'a> {
    pub graph: &'a Graph<UranNode, UranEdge>,
    pub lifetimes: Vec<Lifetime>,
}

pub struct QueryResponse<N: NodeTrait, E: EdgeTrait> {
    nodes: Vec<N>,
    edges: Vec<E>,
}

impl<'a> NaiveQuery<'a> {
    pub fn level_query(&self, level: Level) -> QueryResponse<UranNode, UranEdge> {
        let mut response = QueryResponse::<UranNode, UranEdge>::new();

        // collect the edges
        for edge in self.graph.edges.iter() {
            if self.lifetimes[edge.id as usize].lives_at(level) {
                response.edges.push(edge.clone());
            }
        }

        // mark all used nodes
        let mut used_nodes = vec![false; self.graph.nodes.len()];

        for edge in response.edges.iter() {
            used_nodes[edge.src as usize] = true;
            used_nodes[edge.dst as usize] = true;
        }

        // gather the edges
        let mut id_map: Vec<NodeId> = vec![NodeId::MAX; self.graph.nodes.len()];
        for (index, value) in used_nodes.iter().enumerate() {
            if *value {
                id_map[index] = response.nodes.len() as NodeId;
                response.nodes.push(self.graph.nodes[index].clone());
            }
        }

        // renumber the src and destination id's to the new positions of the
        // nodes
        for edge in response.edges.iter_mut() {
            edge.src = id_map[edge.src as usize];
            edge.dst = id_map[edge.dst as usize];
        }

        response
    }
}

impl<N: NodeTrait, E: EdgeTrait> QueryResponse<N, E> {
    pub fn new() -> QueryResponse<N, E> {
        QueryResponse {
            nodes: Vec::new(),
            edges: Vec::new(),
        }
    }
}
impl QueryResponse<UranNode, UranEdge> {
    pub fn save(&self, filename: &Path) -> Result<(), std::io::Error> {
        let file = std::fs::File::create(filename)?;
        let mut writer = BufWriter::new(file);

        writer.write_fmt(format_args!("{}\n", self.nodes.len()))?;
        writer.write_fmt(format_args!("{}\n", self.edges.len()))?;

        for (_index, node) in self.nodes.iter().enumerate() {
            writer.write_fmt(format_args!(
                "{} {}\n",
                node.position.latitude, node.position.longitude
            ))?;
        }

        for edge in self.edges.iter() {
            writer.write_fmt(format_args!("{} {} 8 1\n", edge.src, edge.dst))?;
        }

        Ok(())
    }
}
