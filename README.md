# URANv2 - Preprocessing

This repository contains code for efficient unpacking/repacking of edges of
contraction hierarchies as a preprocessing step for an improved version
of URAN [^1].

## Build

To build the code in this repository the cargo and rustc with version >=1.66 is
needed.

To build the code simply run `cargo build --release`.
The binaries can then be found in `target/release`.

## Packing/Unpacking

Use the `packing` binary to pack/unpack the graph.

```console
$ packing --help
Usage: packing --graph <GRAPH> --output <OUTPUT> --packer <PACKER>

Options:
  -g, --graph <GRAPH>    
  -o, --output <OUTPUT>  
  -p, --packer <PACKER>  [possible values: iterative, naive, efficient]
  -h, --help             Print help

packing --graph=<your graph> --packer efficient --output=example.ranges
```

The CH graph has to be in the same format as described in [^3] with the addition
of the fields `bridge_a` and `bridge_b` for the edges that contain the id's of
the edges that are bridged by a shortcut or `-1` if the edge is an original
edge.

## Simple Query

A very simple and naive query programm allows to generate output for [^2]
to render the graph.

```console
simple_query --help

Usage: simple_query --graph <GRAPH> --lifetimes <LIFETIMES> --level <LEVEL> --output <OUTPUT>

Options:
  -g, --graph <GRAPH>          
      --lifetimes <LIFETIMES>  
  -l, --level <LEVEL>          
  -o, --output <OUTPUT>        
  -h, --help                   Print help

simple_query --graph=<your graph> --lifetimes=example.ranges --level=42 --output=example.draw
```

## References

[^1]: Funke, S., Schnelle, N., Storandt, S. (2017). URAN: A Unified Data Structure for Rendering and Navigation. In: Brosset, D., Claramunt, C., Li, X., Wang, T. (eds) Web and Wireless Geographical Information Systems. W2GIS 2017. Lecture Notes in Computer Science(), vol 10181. Springer, Cham. https://doi.org/10.1007/978-3-319-55998-8_5

[^2]: https://github.com/invor/simplestGraphRendering

[^3]: https://fmi.uni-stuttgart.de/alg/research/stuff/
