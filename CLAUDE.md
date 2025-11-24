# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Maelstrom is a distributed systems testing framework built on top of Jepsen. It allows users to write toy implementations of distributed systems in any language and tests them against various workloads (echo, broadcast, CRDTs, Raft, etc.). The framework simulates network conditions, partitions, and provides detailed analysis of correctness and performance.

## Architecture

- **Main Framework**: Clojure application packaged as `lib/maelstrom.jar`
- **Custom OCaml Server**: Located in `self/server/` - an OCaml implementation of a Maelstrom node for testing distributed algorithms
- **Demo Implementations**: Multi-language examples in `demo/` (C++, Clojure, Go, Java, JavaScript, Python, Ruby, Rust)
- **Documentation**: Comprehensive guides in `doc/` covering various distributed systems concepts
- **Test Results**: Stored in `store/` with detailed analysis, timelines, and visualizations

## Commands

### Running Maelstrom Tests
```bash
# Basic echo test
./maelstrom test -w echo --bin path/to/your/binary --node-count 1 --time-limit 10

# Broadcast test with topology
./maelstrom test -w broadcast --bin path/to/binary --node-count 5 --time-limit 20 --rate 10

# With debugging
./maelstrom test -w workload --bin binary --log-stderr --log-net-send --log-net-recv
```

### OCaml Server Development
```bash
# Build the OCaml server
cd self/server
dune build

# Run tests
dune test

# Build and test with Maelstrom
dune exec bin/main.exe  # This creates the binary for Maelstrom to run
```

### Development Environment
```bash
# Enter Nix development shell (provides JDK, GraphViz, GnuPlot, Ruby)
nix develop

# Or install dependencies manually:
# - Java 8+ (for running maelstrom.jar)
# - GraphViz (for network topology visualization)
# - GnuPlot (for performance graphs)
```

## OCaml Server Implementation Details

The `self/server/` contains a complete OCaml implementation of a Maelstrom node:

### Key Modules
- `Msg` (`msg.ml/.mli`): Message protocol definitions and JSON serialization
- `State` (`state.ml/.mli`): Node state management with thread-safe operations
- `Bag` (`bag.ml`): Core message handling and business logic
- `Higher` (`higher.ml/.mli`): Higher-order message construction utilities

### Message Flow
1. **Input**: JSON messages from STDIN (Maelstrom protocol)
2. **Processing**: Concurrent message handling with EIO fibers
3. **State Management**: Thread-safe state updates using mutexes
4. **Output**: JSON responses to STDOUT, logs to STDERR
5. **RPC System**: Callback-based RPC with timeout handling
6. **Gossip Protocol**: Async message propagation for broadcast workloads

### Supported Workloads
- **Echo**: Simple request-response
- **Generate**: Unique ID generation
- **Broadcast**: Message gossip with topology awareness
- **Topology**: Dynamic network topology configuration

## Testing Patterns

### Local Binary Testing
```bash
# Test your implementation
./maelstrom test -w echo --bin ./your-binary --node-count 1 --time-limit 10

# Stress testing
./maelstrom test -w broadcast --bin ./binary --node-count 25 --time-limit 60 --rate 100 --concurrency 4n
```

### Network Simulation
```bash
# Add latency and partitions
./maelstrom test -w workload --bin binary --latency 100 --nemesis partition --nemesis-interval 10
```

### Result Analysis
- Test results stored in `store/[workload]/[timestamp]/`
- `timeline.html`: Interactive visualization of operations
- `results.edn`: Detailed analysis data
- `*.png`: Performance graphs and network diagrams
- `jepsen.log`: Detailed execution log

## Development Workflow

1. **Implement Algorithm**: Write your distributed algorithm in any language
2. **Protocol Compliance**: Ensure JSON message format matches Maelstrom protocol
3. **Test Locally**: Use echo workload for basic functionality
4. **Scale Testing**: Progress through broadcast, CRDTs, and transactional workloads
5. **Analysis**: Use generated visualizations to understand behavior under various conditions

## File Structure Notes

- `/demo/`: Reference implementations in multiple languages
- `/doc/`: Step-by-step tutorials for building distributed systems
- `/lib/maelstrom.jar`: Main Maelstrom testing framework
- `/self/server/`: Custom OCaml node implementation
- `/store/`: Test result storage with detailed analysis
- `./maelstrom`: Shell script wrapper for running `java -jar lib/maelstrom.jar`