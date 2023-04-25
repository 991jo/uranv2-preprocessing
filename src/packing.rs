use crate::graph::{EdgeId, Graph, Level, NodeId, UranEdge, UranNode};
use smallvec::SmallVec;
use std::fs;
use std::io::{BufRead, BufReader, BufWriter, Write};
use std::path::Path;

/// The lifetime of an edge.
/// An edge lives at a point t, when start <= t < end.
/// Therefore a lifetime from x to x is empty.
#[derive(Debug, Copy, Clone)]
pub struct Lifetime {
    pub start: Level,
    pub end: Level,
}

impl Lifetime {
    pub fn lives_at(&self, time: Level) -> bool {
        self.start <= time && time < self.end
    }

    pub fn is_empty(&self) -> bool {
        self.end <= self.start
    }

    /// creates an empty lifetime
    pub fn default() -> Lifetime {
        Lifetime {
            start: Level::MAX,
            end: Level::MAX,
        }
    }
}

pub trait GraphPacker {
    fn pack(&mut self) -> Vec<Lifetime>;
}

pub fn save_lifetimes(lifetimes: &Vec<Lifetime>, filename: &Path) -> Result<(), std::io::Error> {
    let file = std::fs::File::create(filename)?;
    let mut writer = BufWriter::new(file);

    for (index, lifetime) in lifetimes.iter().enumerate() {
        if lifetime.is_empty() {
            writer.write_fmt(format_args!("{} -1 -1\n", index))?;
        } else {
            writer.write_fmt(format_args!(
                "{} {} {}\n",
                index, lifetime.end, lifetime.start
            ))?;
        }
    }
    Ok(())
}

pub fn load_lifetimes(filename: &Path) -> Result<Vec<Lifetime>, LifetimeParsingError> {
    let file = fs::File::open(filename).map_err(LifetimeParsingError::ReadError)?;
    let reader = BufReader::new(file);

    let mut lifetimes = Vec::<Lifetime>::new();

    for (linenr, line) in reader.lines().enumerate() {
        let line = line.map_err(LifetimeParsingError::ReadError)?;
        let split = line.trim().split(" ");

        let mut split = split.skip(1);

        let end = split
            .next()
            .ok_or(LifetimeParsingError::ParsingError(format!(
                "could not find end value in line {linenr}"
            )))?;
        let end = end.parse::<i16>().map_err(|_| {
            LifetimeParsingError::ParsingError(format!("could not parse {end} as an integer"))
        })?;

        let end: Level = if end > 0 { end as Level } else { Level::MAX };

        let start = split
            .next()
            .ok_or(LifetimeParsingError::ParsingError(format!(
                "could not find start value in line {linenr}"
            )))?;
        let start = start.parse::<i16>().map_err(|_| {
            LifetimeParsingError::ParsingError(format!("could not parse {start} as an integer"))
        })?;

        let start: Level = if start > 0 {
            start as Level
        } else {
            Level::MAX
        };

        lifetimes.push(Lifetime { start, end });
    }

    Ok(lifetimes)
}

#[derive(Debug)]
pub enum LifetimeParsingError {
    ReadError(std::io::Error),
    ParsingError(String),
}

pub struct IterativeUnpacker<'a> {
    graph: &'a Graph<UranNode, UranEdge>,
    edges_by_level: Vec<Vec<EdgeId>>,
    max_level: Level,
    unpacked_edges: Vec<EdgeId>,
    unpack_offsets: Vec<usize>,
}

impl<'a> IterativeUnpacker<'a> {
    pub fn new(graph: &'a Graph<UranNode, UranEdge>) -> IterativeUnpacker {
        let max_level = graph.nodes.iter().map(|n| n.level).max().unwrap();

        let mut unpacker = IterativeUnpacker {
            graph,
            edges_by_level: vec![Vec::new(); (max_level + 1) as usize],
            max_level,
            unpacked_edges: vec![EdgeId::MAX; graph.edges.len()],
            unpack_offsets: Vec::with_capacity((max_level + 2) as usize),
        };

        // assign the edges to their levels
        for edge in graph.edges.iter() {
            let level = graph.node(edge.src).level.min(graph.node(edge.dst).level);
            unpacker.edges_by_level[level as usize].push(edge.id);
        }

        unpacker
    }

    pub fn unpack(&mut self) {
        let mut edge_unpacked: Vec<bool> = vec![false; self.graph.edges.len()];

        fn unpack_edge(
            graph: &Graph<UranNode, UranEdge>,
            edge_unpacked: &mut Vec<bool>,
            unpacked_edges: &mut Vec<EdgeId>,
            edge_idx: EdgeId,
        ) {
            if edge_unpacked[edge_idx as usize] {
                return;
            }

            edge_unpacked[edge_idx as usize] = true;
            let edge = graph.edge(edge_idx);
            if edge.is_shortcut() {
                unpack_edge(graph, edge_unpacked, unpacked_edges, edge.bridge_a);
                unpack_edge(graph, edge_unpacked, unpacked_edges, edge.bridge_b);
            } else {
                unpacked_edges.push(edge_idx)
            }
        }

        for level in (0..=self.max_level).rev() {
            for edge_idx in self.edges_by_level[level as usize].iter() {
                unpack_edge(
                    &self.graph,
                    &mut edge_unpacked,
                    &mut self.unpacked_edges,
                    *edge_idx,
                );
            }
            self.unpack_offsets[level as usize] = self.unpacked_edges.len();
        }
    }

    pub fn pack(&mut self) {
        let lifetimes = vec![Lifetime::default(); self.graph.edges.len()];

        let mut neighbor_counter = vec![NeighborCounter::new(); self.graph.nodes.len()];
        let mut contracted_node = vec![NodeId::MAX; self.graph.edges.len()];

        for edge in self.graph.edges.iter() {
            if edge.is_shortcut() {
                contracted_node[edge.id as usize] = self.graph.edge(edge.bridge_a).dst;
            }
        }

        for level in (0..=self.max_level).rev() {
            let level_offset = self.unpack_offsets[level as usize];
            let used_edges = &self.graph.edges[..level_offset];

            // calculate the neighbor count of edge node
            for counter in neighbor_counter.iter_mut() {
                counter.clear();
            }
            for edge in used_edges.iter() {
                neighbor_counter[edge.src as usize].add(edge.dst);
                neighbor_counter[edge.dst as usize].add(edge.src);
            }
        }
    }
}

/// A helper struct to count the amount of unique neighbors a node has.
///
/// This structure only answers the question if there are 2, less than 2
/// or more than 2 neighbors.
///
/// ```
/// use uranv2::packing::NeighborCounter;
/// let mut counter = NeighborCounter::new();
///
/// assert_eq!(counter.count, 0);
///
/// counter.add(1);
/// assert!(counter.count == 1);
///
/// counter.add(2);
/// counter.add(2);
/// assert!(counter.count == 2);
/// counter.add(3);
/// counter.add(4);
/// assert!(counter.count == 3);
/// ```
#[derive(Debug, Default, Copy, Clone)]
pub struct NeighborCounter {
    /// the amount of neighbors that have been counted
    pub count: u8,
    nodes: [NodeId; 2],
}

impl NeighborCounter {
    pub fn new() -> NeighborCounter {
        NeighborCounter {
            count: 0,
            nodes: [NodeId::MAX; 2],
        }
    }

    /// adds a node to the counter.
    /// If this was the third unique node that was added this returns true.
    /// Otherwise it returns false.
    pub fn add(&mut self, node_id: NodeId) -> bool {
        if self.count >= 3 {
            return false;
        }
        if self.nodes[0] == NodeId::MAX {
            self.nodes[0] = node_id;
            self.count += 1;
        }

        if self.nodes[0] == node_id {
            return false;
        }
        if self.nodes[1] == NodeId::MAX {
            self.nodes[1] = node_id;
            self.count += 1;
        }

        if self.nodes[1] == node_id {
            return false;
        }
        self.count = 3;
        return true;
    }

    /// removes all neighbors and resets the counter.
    pub fn clear(&mut self) {
        self.nodes[0] = NodeId::MAX;
        self.nodes[1] = NodeId::MAX;
        self.count = 0;
    }
}

#[derive(Debug, Default)]
/// A counter that allows to track the amount of neighbors and the edge via
/// which they are connected.
///
/// This also allows to remove edges.
///
/// ```
/// use uranv2::packing::EdgeNeighborCounter;
///
/// let mut counter = EdgeNeighborCounter::new();
/// assert!(counter.count() == 0);
///
/// counter.add(1, 2);
/// assert!(counter.count() == 1);
///
/// counter.add(2, 2);
/// assert!(counter.count() == 1);
///
/// counter.add(3, 42);
/// assert!(counter.count() == 2);
///
/// counter.del(2);
/// assert!(counter.count() == 2);
///
/// counter.del(1);
/// assert!(counter.count() == 1);
///
/// counter.add(4, 9001);
/// assert!(counter.count() == 2);
/// ```
pub struct EdgeNeighborCounter {
    edges: SmallVec<[(EdgeId, NodeId); 4]>,
    counter: NeighborCounter,
    counter_valid: bool,
}

impl EdgeNeighborCounter {
    pub fn new() -> EdgeNeighborCounter {
        EdgeNeighborCounter {
            edges: SmallVec::new(),
            counter: NeighborCounter::new(),
            counter_valid: true,
        }
    }

    /// Adds a given neighbor via the given edge.
    pub fn add(&mut self, edge: EdgeId, node: NodeId) {
        for (vec_edge, _) in self.edges.iter() {
            if *vec_edge == edge {
                return;
            }
        }

        if self.counter_valid {
            self.counter.add(node);
        }

        for (vec_edge, vec_node) in self.edges.iter_mut() {
            if *vec_edge == EdgeId::MAX {
                *vec_edge = edge;
                *vec_node = node;
                return;
            }
        }

        self.edges.push((edge, node));
    }

    /// Deletes an edge and the corresponding neighbor.
    pub fn del(&mut self, edge: EdgeId) {
        for (vec_edge, vec_node) in self.edges.iter_mut() {
            if *vec_edge == edge {
                *vec_edge = EdgeId::MAX;
                *vec_node = NodeId::MAX;
                self.counter_valid = false;
                return;
            }
        }
    }

    /// Returns the amount of neighbors.
    ///
    /// This counter is only accurate for up to 3 neighbors.
    /// If 3 is returned, there might be more neighbors.
    pub fn count(&mut self) -> u8 {
        if !self.counter_valid {
            self.counter.clear();
            for (_, neighbor) in self.edges.iter() {
                if *neighbor != NodeId::MAX {
                    self.counter.add(*neighbor);
                }
            }
        }

        self.counter.count
    }
}
