use std::str::FromStr;
use std::path::Path;
use std::fs;
use std::io::{BufRead, BufReader};

pub type NodeId = u32;
pub type EdgeId = u32;
pub type EdgeCost = u32;
pub type Level = u16;

pub trait NodeTrait {
    fn id(&self) -> NodeId;
    fn from_line(line: &str) -> Result<Self, GraphParsingError> where Self: Sized;
}

pub trait EdgeTrait {
    fn src(&self) -> NodeId;
    fn dst(&self) -> NodeId;
    fn id(&self) -> EdgeId;
    fn set_id(&mut self, edge: EdgeId);
    fn from_line(line: &str) -> Result<Self, GraphParsingError> where Self: Sized;
}

pub struct Graph<N, E> where N:NodeTrait, E: EdgeTrait {
    pub nodes: Vec<N>,
    pub edges: Vec<E>,
}

impl<N, E> Graph<N, E> where N: NodeTrait, E: EdgeTrait {

    /// Initializes an empty graph without nodes and edges
    pub fn default() -> Self {
        Graph{nodes: Vec::<N>::new(), edges: Vec::<E>::new()}
    }

    pub fn from_file(filename: &Path) -> Result<Graph<N, E>, GraphParsingError> {
        let file = fs::File::open(filename).map_err(GraphParsingError::ReadError)?;
        let reader = BufReader::new(file);

        let mut graph = Graph::default();

        #[derive(PartialEq, Eq, Debug)]
        enum ParserState {
            FindNumNodes,
            FindNumEdges,
            ParseNodes,
            ParseEdges,
            End,
        }

        let mut state = ParserState::FindNumNodes;
        let mut num_nodes: usize = 0;
        let mut num_edges: usize = 0;
        let mut edge_counter: EdgeId = 0;

        for (index, line) in reader.lines().enumerate() {
            let line = line.map_err(GraphParsingError::ReadError)?;
            let line = line.trim();

            if line.starts_with("#") || line.is_empty() {
                continue;
            }

            match &state {
                ParserState::FindNumNodes => {
                    num_nodes = line.parse::<usize>().map_err(|_| GraphParsingError::NotAnUnsignedInteger(format!("Can not parse '{}' in line {} as an unsigned integer for the number of nodes in the graph.", line, index)))?;
                    graph.nodes.reserve_exact(num_nodes);
                    state = ParserState::FindNumEdges;
                }
                ParserState::FindNumEdges => {
                    num_edges = line.parse::<usize>().map_err(|_| GraphParsingError::NotAnUnsignedInteger(format!("Can not parse '{}' in line {} as an unsigned integer for the number of edges in the graph.", line, index)))?;
                    graph.edges.reserve_exact(num_edges);
                    state = ParserState::ParseNodes;
                }
                ParserState::ParseNodes => {
                    let node = N::from_line(line)?;
                    graph.nodes.push(node);
                    num_nodes = num_nodes - 1;
                    if num_nodes == 0 {
                        state = ParserState::ParseEdges;
                    }
                }
                ParserState::ParseEdges => {
                    let mut edge = E::from_line(line)?;
                    edge.set_id(edge_counter);
                    edge_counter += 1;
                    graph.edges.push(edge);

                    num_edges -= 1;
                    if num_edges == 0 {
                        state = ParserState::End;
                    }
                }
                ParserState::End => {
                    return Err(GraphParsingError::AdditionalElementsError);
                }
            }
        }

        if state != ParserState::End {
            return Err(GraphParsingError::TooManyElements(format!(
                "Parsing ended in state {:?}",
                state
            )));
        }

        return Ok(graph);
    }

    pub fn node<'a>(&'a self, id: NodeId) -> &'a N {
        &self.nodes[id as usize]
    }

    pub fn edge<'a>(&'a self, id: EdgeId) -> &'a E {
        &self.edges[id as usize]
    }
}

#[derive(Clone, Debug)]
pub struct Position {
    pub latitude: f32,
    pub longitude: f32,
}

#[derive(Clone, Debug)]
pub struct UranNode {
    pub id: NodeId,
    pub position: Position,
    pub level: Level,
}

#[derive(Clone, Debug)]
pub struct UranEdge {
    pub id: EdgeId,
    pub src: NodeId,
    pub dst: NodeId,
    pub cost: EdgeCost,
    pub bridge_a: EdgeId,
    pub bridge_b: EdgeId,
}

impl NodeTrait for UranNode {

    fn id(&self) -> NodeId {
        self.id
    }

    fn from_line(line: &str) -> Result<Self, GraphParsingError> {
        let mut split = line.split(" ");

        fn parse_next_as<T: FromStr>(
            split: &mut dyn Iterator<Item = &str>,
            name: &str,
            line: &str,
        ) -> Result<T, GraphParsingError> {
            let item = split.next();
            let item = item.ok_or(GraphParsingError::NodeParsingError(format!(
                "Cannot parse '{}' as a Node. The {} element is missing.",
                line, name
            )))?;
            let item = item.parse::<T>().map_err(|_| {
                GraphParsingError::NodeParsingError(format!(
                    "Cannot parse '{}' as a Node. '{}' is not a {:?}",
                    line,
                    name,
                    std::any::type_name::<T>()
                ))
            })?;
            return Ok(item);
        }

        let id = parse_next_as::<NodeId>(&mut split, "NodeID", line)?;
        let mut split = split.skip(1);
        let latitude = parse_next_as::<f32>(&mut split, "latitude", line)?;
        let longitude = parse_next_as::<f32>(&mut split, "latitude", line)?;
        let mut split = split.skip(1);
        let level = parse_next_as::<Level>(&mut split, "Level", line)?;

        if split.next().is_some() {
            return Err(GraphParsingError::NodeParsingError(format!(
                "Cannot parse '{}' as a Node. It contains more than 6 elements.",
                line
            )));
        }

        let position = Position {
            latitude,
            longitude,
        };

        return Ok(UranNode{
            id,
            position,
            level,
        });
    }
}

impl UranEdge {
    /// Returns whether an edge is a shortcut in the contraction hierarchy or not.
    #[inline(always)]
    pub fn is_shortcut(&self) -> bool {
        self.bridge_a != EdgeId::MAX || self.bridge_b != EdgeId::MAX
    }
}

impl EdgeTrait for UranEdge {
    /// Parses an Edge given a string as input.
    ///
    /// The format is seperated by spaces and as follows:
    /// `src dest cost edge_type max_speed birdge_a bridge_b`
    ///
    /// The `max_speed` is ignored.
    /// `bridge_a` and `bridge_b` refer to the implicit `EdgeIDs` of other
    /// edges that created this edge if it is a shortcut.
    /// If both values are `-1` then this edge is not a shortcut edge.
    fn from_line(line: &str) -> Result<UranEdge, GraphParsingError> {
        let mut split = line.split(" ");

        fn parse_next_as<T: FromStr>(
            split: &mut dyn Iterator<Item = &str>,
            name: &str,
            line: &str,
        ) -> Result<T, GraphParsingError> {
            let item = split.next();
            let item = item.ok_or(GraphParsingError::EdgeParsingError(format!(
                "Cannot parse '{}' as an Edge. The {} element is missing.",
                line, name
            )))?;
            let item = item.parse::<T>().map_err(|_| {
                GraphParsingError::EdgeParsingError(format!(
                    "Cannot parse '{}' as an Edge. '{}' is not a {:?}",
                    line,
                    name,
                    std::any::type_name::<T>()
                ))
            })?;
            return Ok(item);
        }

        let src = parse_next_as::<NodeId>(&mut split, "SrcID", line)?;
        let dst = parse_next_as::<NodeId>(&mut split, "DestID", line)?;

        let cost = parse_next_as::<EdgeCost>(&mut split, "EdgeCost", line)?;
        // let edge_type = parse_next_as::<EdgeType>(&mut split, "EdgeType", line)?;
        // let max_speed = parse_next_as::<u16>(&mut split, "max_speed", line)?;
        let mut split = split.skip(2);
        let bridge_a = parse_next_as::<i32>(&mut split, "bridge_a", line)?;
        let bridge_b = parse_next_as::<i32>(&mut split, "bridge_b", line)?;
        let bridge_a: EdgeId = if bridge_a >= 0 { bridge_a as EdgeId } else { EdgeId::MAX };
        let bridge_b: EdgeId = if bridge_b >= 0 { bridge_b as EdgeId } else { EdgeId::MAX };

        if split.next().is_some() {
            return Err(GraphParsingError::EdgeParsingError(format!(
                "Cannot parse '{}' as an Edge . It contains more than 7 elements.",
                line
            )));
        }

        return Ok(UranEdge {
            id: EdgeId::MAX,
            src,
            dst,
            cost,
            bridge_a,
            bridge_b,
        });
    }

    fn id(&self) -> EdgeId {
        self.id
    }

    fn set_id(&mut self, id: EdgeId) {
        self.id = id
    }

    fn src(&self) -> NodeId {
        self.src
    }

    fn dst(&self) -> NodeId {
        self.dst
    }

}

#[derive(Debug)]
pub enum GraphParsingError {
    ReadError(std::io::Error),
    AdditionalElementsError,
    TooManyElements(String),
    NotAnUnsignedInteger(String),
    NodeParsingError(String),
    EdgeParsingError(String),
}
