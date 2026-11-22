# gl_wat

[![Package Version](https://img.shields.io/hexpm/v/gl_wat)](https://hex.pm/packages/gl_wat)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/gl_wat/)

```sh
gleam add gl_wat@1
```
```gleam
import gl_wat

pub fn main() {
  // TODO: An example of the project in use
}
```

Further documentation can be found at <https://hexdocs.pm/gl_wat>.

## Development

```sh
gleam run   # Run the project
gleam test  # Run the tests
```

## Status

The goal is to support the full [WASM 3.0](https://webassembly.org/news/2025-09-17-wasm-3.0/) text format specification.

- [ ] Lexical format
  - [x] Keyword tokens
  - [x] Integer tokens
  - [x] Float tokens
  - [x] String tokens
  - [x] Regular ID tokens
  - [x] String ID tokens
  - [ ] White space
  - [x] Inline comments
  - [ ] Block comments
  - [ ] Annotations
- [ ] Values
  - [ ] Integer
  - [ ] Float
  - [x] String
  - [ ] Names
  - [ ] Identifiers
- [ ] Types
  - [x] Number types
  - [ ] Vector types
  - [x] Heap types
  - [x] Reference types
  - [x] Value types
  - [x] Composite types
  - [x] Recursive types
  - [x] Address types
  - [x] Limits
  - [x] Tag types
  - [x] Global types
  - [x] Memory types
  - [x] Table types
  - [x] External types
  - [x] Type uses
- [ ] Instructions
  - [ ] Labels
  - [x] Parametric instructions
  - [ ] Control instructions (missing: catch, throw)
  - [x] Variable instructions
  - [x] Table instructions
  - [ ] Memory instructions (missing: vector load/store)
  - [x] Reference instructions
  - [x] Aggregate instructions
  - [ ] Numeric instructions (missing: wrap, trunc, extend, demote, convert, reinterpret)
  - [ ] Vector instructions
  - [x] Folded instructions
  - [x] Expressions
- [ ] Modules
  - [ ] Indices (missing: some checks?)
    - [x] Resolve type, tag, global, memory, table, function, data, element indices
    - [x] Resolve local indices
    - [ ] Resolve label indices
    - [ ] Resolve field indices
  - [ ] Types (missing: implicitly created types)
  - [x] Tags
  - [x] Globals
  - [x] Memories
  - [x] Tables
  - [x] Functions
  - [x] Data
  - [x] Element
  - [x] Start
  - [x] Imports
  - [x] Exports
  - [x] Modules
