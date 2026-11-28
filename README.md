# gl_wat

[![Package Version](https://img.shields.io/hexpm/v/gl_wat)](https://hex.pm/packages/gl_wat)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/gl_wat/)

```gleam
import gl_wat as w
import gleam/result
import nibble
import nibble/lexer

pub fn main() {
  let src =
    "(func (export \"add\") (param i32 i32) (result i32) "
    <> "(i32.add (local.get 0) (local.get 1)))"
  // produce tokens for the parser
  let assert Ok(tokens) = lexer.run(src, w.lexer()) |> result.map(w.simplify)
  // run the parser itself
  let assert Ok(#(mod, ctx)) = nibble.run(tokens, w.module_parser())
  // resolve any cross-references to their respective indices
  let assert Ok(mod) = w.resolve_indices(mod, ctx)
  // go wild!
  echo mod
}
```

Further documentation can be found at <https://hexdocs.pm/gl_wat>.

## Development

```sh
gleam dev   # Run the above example
gleam test  # Run the tests
```

## Status

The goal is to support the full [WASM 3.0](https://webassembly.org/news/2025-09-17-wasm-3.0/) text format specification. SIMD/Vector support is a low priority.

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
  - [x] Integer
  - [x] Float (except: negative and non-canonical NaN)
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
- [x] Modules
  - [x] Indices
    - [x] Resolve type, tag, global, memory, table, function, data, element indices
    - [x] Resolve local indices
    - [x] Resolve label indices
    - [x] Resolve field indices
  - [x] Types
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
