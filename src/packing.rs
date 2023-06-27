use crate::graph::{EdgeId, Graph, Level, NodeId, UranEdge, UranNode};
use smallvec::SmallVec;
use std::fs;
use std::io::{BufRead, BufReader, BufWriter, Write};
use std::path::Path;

/// Defines a function that determines whether or not an edge may be repacked
/// or not depending on aesthetic criteria.
pub type AestheticFunction = fn(EdgeId, &Graph<UranNode, UranEdge>, level: Level) -> bool;


/// An aesthetic function that simply allows repacking all edges.
pub fn accept_all(_edge_id: EdgeId, _graph: &Graph<UranNode, UranEdge>, _level: Level) -> bool {
    return true
}

/// allows only edges with a certain length
pub fn maximum_length_500(edge_id: EdgeId, graph: &Graph<UranNode, UranEdge>, _level: Level) -> bool {
    let edge = graph.edge(edge_id);
    !edge.is_shortcut() || edge.cost < 500
}

/// allows only edges with a certain length
pub fn maximum_length_50_level(edge_id: EdgeId, graph: &Graph<UranNode, UranEdge>, level: Level) -> bool {
    let edge = graph.edge(edge_id);
    !edge.is_shortcut() || edge.cost < 50 * (level as u32)
}

/// The lifetime of an edge.
/// An edge lives at a point t, when start <= t < end.
/// Therefore a lifetime from x to x is empty.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
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
    fn pack(&mut self, aesthetic: AestheticFunction) -> Vec<Lifetime>;
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
                index,
                lifetime.end - 1,
                lifetime.start
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
        // dbg!(linenr);
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

        // dbg!(end);

        let end: Level = if end >= 0 {
            (end + 1) as Level
        } else {
            Level::MAX
        };

        // dbg!(end);

        let start = split
            .next()
            .ok_or(LifetimeParsingError::ParsingError(format!(
                "could not find start value in line {linenr}"
            )))?;
        let start = start.parse::<i16>().map_err(|_| {
            LifetimeParsingError::ParsingError(format!("could not parse {start} as an integer"))
        })?;

        // dbg!(start);

        let start: Level = if start >= 0 {
            start as Level
        } else {
            Level::MAX
        };
        // dbg!(start);

        let lt = Lifetime { start, end };
        // dbg!(linenr, lt);
        lifetimes.push(lt);
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
    edges_by_level: Vec<Vec<EdgeId>>, // the edges sorted into buckets corresponding to the minimum
    // level of the two nodes they connect
    max_level: Level,
    unpacked_edges: Vec<EdgeId>, //
    unpack_offsets: Vec<usize>,
}

impl<'a> IterativeUnpacker<'a> {
    pub fn new(graph: &'a Graph<UranNode, UranEdge>) -> IterativeUnpacker {
        let max_level = graph.nodes.iter().map(|n| n.level).max().unwrap();

        let mut unpacker = IterativeUnpacker {
            graph,
            edges_by_level: vec![Vec::new(); (max_level + 1) as usize],
            max_level,
            unpacked_edges: Vec::new(),
            unpack_offsets: vec![0; (max_level + 1) as usize],
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

    pub fn repack(&mut self, aesthetic: AestheticFunction) -> Vec<Lifetime> {
        // dbg!(self.unpack_offsets.clone());
        // dbg!(self.unpacked_edges.clone());
        let mut lifetimes = vec![
            Lifetime {
                start: u16::MAX,
                end: u16::MIN
            };
            self.graph.edges.len()
        ];

        // calculate the node that was contracted to create a shortcut.
        let mut contracted_node = vec![NodeId::MAX; self.graph.edges.len()];
        for edge in self.graph.edges.iter() {
            if edge.is_shortcut() {
                contracted_node[edge.id as usize] = self.graph.edge(edge.bridge_a).dst;
            }
        }

        for level in (0..=self.max_level).rev() {
            println!("working on level {level}");
            let mut edge_used = vec![false; self.graph.edges.len()];
            let mut neighbor_counter = vec![NeighborCounter::new(); self.graph.nodes.len()];
            let level_offset = self.unpack_offsets[level as usize];
            let used_edges = &self.unpacked_edges[..level_offset];
            // dbg!(used_edges);
            for edge_idx in used_edges.iter() {
                let edge = &self.graph.edges[*edge_idx as usize];

                neighbor_counter[edge.src as usize].add(edge.dst);
                neighbor_counter[edge.dst as usize].add(edge.src);
                edge_used[*edge_idx as usize] = true;
            }

            // contract the edges
            for level in 0..=self.max_level {
                // println!("working on level {level} (inner loop)");
                for edge_idx in self.edges_by_level[level as usize].iter() {
                    let edge = &self.graph.edges[*edge_idx as usize];

                    if !edge.is_shortcut() {
                        continue;
                    }

                    if !aesthetic(edge.id, self.graph, level) {
                        continue;
                    }

                    if !(edge_used[edge.bridge_a as usize] && edge_used[edge.bridge_b as usize]) {
                        continue;
                    }

                    let node = contracted_node[edge.id as usize];

                    let neighbors = neighbor_counter[node as usize].count;

                    if neighbors >= 3 {
                        continue;
                    }
                    // dbg!(edge);

                    // remove the old edges
                    edge_used[edge.bridge_a as usize] = false;
                    edge_used[edge.bridge_b as usize] = false;

                    // add the new edge
                    edge_used[edge.id as usize] = true;
                }
            }

            for (index, used) in edge_used.iter().enumerate() {
                if !used {
                    continue;
                }
                let lifetime = &mut lifetimes[index as usize];

                lifetime.start = lifetime.start.min(level);
                lifetime.end = lifetime.end.max(level + 1);
            }
            // dbg!(lifetimes.clone());
        }

        lifetimes
    }
}

impl<'a> GraphPacker for IterativeUnpacker<'a> {
    fn pack(&mut self, aesthetic: AestheticFunction) -> Vec<Lifetime> {
        self.unpack();
        self.repack(aesthetic)
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
#[derive(Debug, Clone)]
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

pub struct NaiveUnpacker<'a> {
    graph: &'a Graph<UranNode, UranEdge>,
    edges_by_level: Vec<Vec<EdgeId>>, // the edges sorted into buckets corresponding to the minimum
    // level of the two nodes they connect
    max_level: Level,
}

impl<'a> NaiveUnpacker<'a> {
    pub fn new(graph: &'a Graph<UranNode, UranEdge>) -> Self {
        let max_level = graph.nodes.iter().map(|n| n.level).max().unwrap();

        let mut unpacker = NaiveUnpacker {
            graph,
            edges_by_level: vec![Vec::new(); (max_level + 1) as usize],
            max_level,
        };

        // assign the edges to their levels
        for edge in graph.edges.iter() {
            let level = graph.node(edge.src).level.min(graph.node(edge.dst).level);
            unpacker.edges_by_level[level as usize].push(edge.id);
        }

        unpacker
    }
}

impl<'a> GraphPacker for NaiveUnpacker<'a> {
    fn pack(&mut self, aesthetic: AestheticFunction) -> Vec<Lifetime> {
        let mut lifetimes = vec![Lifetime::default(); self.graph.edges.len()];

        for level in 0..=self.max_level {
            println!("unpacking {level}");

            // the unpack flag stores whether an edge needs to be unpacked
            let mut unpack_flag = vec![false; self.graph.edges.len()];
            // the used flag stores whether an edge is currently used
            // This changes, when a shortcut is used instead of it's child edges.
            // The shortcut becomes true, while the children become false
            let mut used_flag = vec![false; self.graph.edges.len()];

            let mut neighbor_counters = vec![NeighborCounter::new(); self.graph.nodes.len()];

            println!("marking the edges");

            // mark all edges that have to be unpacked
            for edges_by_level in
                self.edges_by_level[(level as usize)..=(self.max_level as usize)].iter()
            {
                for edge_id in edges_by_level.iter() {
                    let edge = &self.graph.edges[*edge_id as usize];
                    if edge.is_shortcut() {
                        unpack_flag[edge.id as usize] = true;
                    } else {
                        used_flag[edge.id as usize] = true;
                    }
                }
            }

            println!("unpacking the edges");
            // unpack the edges
            for edges_by_level in self.edges_by_level.iter().rev() {
                for edge_id in edges_by_level.iter() {
                    let unpack = unpack_flag[*edge_id as usize];
                    if unpack == false {
                        continue;
                    }

                    let edge = &self.graph.edges[*edge_id as usize];
                    if edge.is_shortcut() {
                        unpack_flag[edge.bridge_a as usize] = true;
                        unpack_flag[edge.bridge_b as usize] = true;
                    } else {
                        used_flag[*edge_id as usize] = true;
                    }
                }
            }

            // count the neighbors
            for (index, _used) in used_flag.iter().enumerate().filter(|d| *d.1) {
                let edge = &self.graph.edges[index];
                neighbor_counters[edge.src as usize].add(edge.dst);
                neighbor_counters[edge.dst as usize].add(edge.src);
            }

            println!("repacking the edges");
            // repack the edges
            for edges_by_level in self.edges_by_level.iter() {
                for edge_id in edges_by_level.iter() {
                    let edge = &self.graph.edges[*edge_id as usize];

                    if !edge.is_shortcut() {
                        continue;
                    }

                    if !aesthetic(edge.id, self.graph, level) {
                        continue;
                    }

                    let bridge_a = edge.bridge_a as usize;
                    let bridge_b = edge.bridge_b as usize;

                    // one of the child edges is not used, so we can't insert
                    // this shortcut
                    if !(used_flag[bridge_b] && used_flag[bridge_a]) {
                        continue;
                    }

                    let contracted_node = self.graph.edges[bridge_a].dst as usize;

                    if neighbor_counters[contracted_node].count <= 2 {
                        used_flag[*edge_id as usize] = true;
                        used_flag[bridge_a] = false;
                        used_flag[bridge_b] = false;
                    }
                }
            }

            println!("saving the lifetimes");
            // save the liftimes
            for (index, used) in used_flag.iter().enumerate() {
                if !used {
                    continue;
                };

                let mut lifetime = &mut lifetimes[index];

                lifetime.start = lifetime.start.min(level);
                lifetime.end = level + 1;
            }
        }

        return lifetimes;
    }
}

pub struct EfficientUnpacker<'a> {
    graph: &'a Graph<UranNode, UranEdge>,
    edges_by_level: Vec<Vec<EdgeId>>, // the edges sorted into buckets corresponding to the minimum
    // level of the two nodes they connect
    max_level: Level,
    three_neighbor_level: Vec<Level>, // contains the lowest level at which this node has at most
    // 2 neighbors
    neighbor_counter: Vec<NeighborCounter>,
    lifetimes: Vec<Lifetime>, // the lifetimes of the edges
    unpacked_edges: Vec<bool>,
    unpacked_by: Vec<EdgeId>, // the ID from which the edge was first unpacked
    aesthetic_criterium: Vec<Level> // the lowest level an edge may be used
}

impl<'a> EfficientUnpacker<'a> {
    pub fn new(graph: &'a Graph<UranNode, UranEdge>) -> EfficientUnpacker<'a> {
        let max_level = graph.nodes.iter().map(|n| n.level).max().unwrap();

        let mut unpacker = EfficientUnpacker {
            graph,
            max_level,
            edges_by_level: vec![Vec::new(); (max_level + 1) as usize],
            three_neighbor_level: vec![Level::MIN; graph.nodes.len()],
            lifetimes: vec![Lifetime::default(); graph.edges.len()],
            unpacked_edges: vec![false; graph.edges.len()],
            neighbor_counter: vec![NeighborCounter::new(); graph.nodes.len()],
            unpacked_by: vec![EdgeId::MAX; graph.edges.len()],
            aesthetic_criterium: vec![Level::MAX; graph.edges.len()],
        };
        // assign the edges to their levels
        for edge in graph.edges.iter() {
            let level = graph.node(edge.src).level.min(graph.node(edge.dst).level);
            unpacker.edges_by_level[level as usize].push(edge.id);
        }

        // set the level for the aesthetic criterium
        for edge in graph.edges.iter() {
            if maximum_length_500(edge.id, graph, 0) {
                unpacker.aesthetic_criterium[edge.id as usize] = 0;
            }
        }
        

        unpacker
    }

    fn unpack_edge(
        edge: &'a UranEdge,
        level: Level,
        lifetimes: &mut Vec<Lifetime>,
        graph: &'a Graph<UranNode, UranEdge>,
        unpacked_edges: &mut Vec<bool>,
        three_neighbor_level: &mut Vec<Level>,
        neighbor_counter: &mut Vec<NeighborCounter>,
        unpacked_by: &mut Vec<EdgeId>,
    ) {
        // early return for all edges that we already worked on
        if unpacked_edges[edge.id as usize] {
            return;
        };
        unpacked_edges[edge.id as usize] = true;

        // set the end of the lifetime at first introduction
        // +1 because end is non-inclusive
        lifetimes[edge.id as usize].end = level + 1;

        if edge.is_shortcut() {
            // the edge is a shortcut
            let bridge_a = graph.edge(edge.bridge_a);
            let bridge_b = graph.edge(edge.bridge_b);
            if unpacked_by[bridge_a.id as usize] == EdgeId::MAX {
                unpacked_by[bridge_a.id as usize] = edge.id;
            }
            if unpacked_by[bridge_b.id as usize] == EdgeId::MAX {
                unpacked_by[bridge_b.id as usize] = edge.id;
            }

            Self::unpack_edge(
                bridge_a,
                level,
                lifetimes,
                graph,
                unpacked_edges,
                three_neighbor_level,
                neighbor_counter,
                unpacked_by,
            );
            Self::unpack_edge(
                bridge_b,
                level,
                lifetimes,
                graph,
                unpacked_edges,
                three_neighbor_level,
                neighbor_counter,
                unpacked_by,
            );
        } else {
            // the edge is not a shortcut
            let src = edge.src;
            let dst = edge.dst;
            if neighbor_counter[src as usize].add(dst) {
                three_neighbor_level[src as usize] = level + 1;
            }
            if neighbor_counter[dst as usize].add(src) {
                three_neighbor_level[dst as usize] = level + 1;
            }
        }
    }

    fn calculate_edge_lifetime(
        edge: &UranEdge,
        graph: &'a Graph<UranNode, UranEdge>,
        lifetimes: &mut Vec<Lifetime>,
        three_neighbor_level: &Vec<Level>,
        aesthetic_criterium: &Vec<Level>,
    ) -> Level {
        // early return for edges that already have been done
        if lifetimes[edge.id as usize].start != Level::MAX {
            return lifetimes[edge.id as usize].start;
        };

        // default value for all original edges
        let mut lifetime_start: Level = 0;

        if edge.is_shortcut() {
            let bridge_a = graph.edge(edge.bridge_a);
            let bridge_b = graph.edge(edge.bridge_b);

            let contracted_node_id = bridge_a.dst;
            let node_lifetime = three_neighbor_level[contracted_node_id as usize];

            let lifetime_a =
                Self::calculate_edge_lifetime(bridge_a, graph, lifetimes, three_neighbor_level, aesthetic_criterium);
            let lifetime_b =
                Self::calculate_edge_lifetime(bridge_b, graph, lifetimes, three_neighbor_level, aesthetic_criterium);

            lifetime_start = node_lifetime.max(lifetime_a.max(lifetime_b.max(aesthetic_criterium[edge.id as usize])));
        }

        lifetimes[edge.id as usize].start = lifetime_start;
        return lifetime_start;
    }
}

impl<'a> GraphPacker for EfficientUnpacker<'a> {
    fn pack(&mut self, _aesthetic: AestheticFunction) -> Vec<Lifetime> {

        // first stage
        // - track introduction of the edges (highest possible lifetime)
        // - track the neighbor-counter (for the start of the lifetime of the edges)
        for level in (0..=self.max_level).rev() {
            for edge_id in self.edges_by_level[level as usize].iter() {
                let edge = self.graph.edge(*edge_id);
                Self::unpack_edge(
                    edge,
                    level,
                    &mut self.lifetimes,
                    self.graph,
                    &mut self.unpacked_edges,
                    &mut self.three_neighbor_level,
                    &mut self.neighbor_counter,
                    &mut self.unpacked_by,
                );
            }
        }

        // second stage
        // calculate start of the lifetime recursively
        // max(node, bridge_a, bridge_b)
        for level in (0..=self.max_level).rev() {
            for edge_id in self.edges_by_level[level as usize].iter() {
                let edge = self.graph.edge(*edge_id);
                Self::calculate_edge_lifetime(
                    edge,
                    self.graph,
                    &mut self.lifetimes,
                    &self.three_neighbor_level,
                    &self.aesthetic_criterium
                );
            }
        }

        // third stage
        // calculate the end of the lifetimes due to the lifetimes of higher
        // level shortcuts recursively
        for level in (0..=self.max_level).rev() {
            for edge_id in self.edges_by_level[level as usize].iter() {
                let edge = self.graph.edge(*edge_id);
                let lifetime_start = self.lifetimes[*edge_id as usize].start;

                if edge.is_shortcut() {
                    let bridge_a = self.graph.edge(edge.bridge_a);
                    let bridge_b = self.graph.edge(edge.bridge_b);

                    if self.unpacked_by[bridge_a.id as usize] == edge.id {
                        self.lifetimes[bridge_a.id as usize].end =
                            self.lifetimes[bridge_a.id as usize].end.min(lifetime_start);
                    }
                    if self.unpacked_by[bridge_b.id as usize] == edge.id {
                        self.lifetimes[bridge_b.id as usize].end =
                            self.lifetimes[bridge_b.id as usize].end.min(lifetime_start);
                    }
                }
            }
        }

        self.lifetimes.clone()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lifetimes() {
        let lifetime = Lifetime { start: 1, end: 5 };
        assert!(lifetime.lives_at(1));
        assert!(!lifetime.lives_at(0));
        assert!(lifetime.lives_at(4));
        assert!(!lifetime.lives_at(5));

        let lifetime = Lifetime { start: 0, end: 179 };
        assert!(lifetime.lives_at(0));
        assert!(!lifetime.lives_at(200));
    }
}
