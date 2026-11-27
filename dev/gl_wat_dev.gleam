import gl_wasm/wasm
import gl_wat as w
import gleam/bytes_tree
import gleam/result
import nibble
import nibble/lexer
import simplifile
import wat_to_wasm

fn memory_output_stream() {
  wasm.OutputStream(
    stream: bytes_tree.new(),
    write_bytes: fn(bytes_tree, bytes) {
      Ok(bytes_tree.append(to: bytes_tree, suffix: bytes))
    },
    close: fn(s) { Ok(s) },
  )
}

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
  // compile to binary
  let assert Ok(mb) = wat_to_wasm.compile_module(mod)
  let assert Ok(bytes) = wasm.emit_module(mb, memory_output_stream())
  let assert Ok(_) =
    simplifile.write_bits("test.wasm", bytes_tree.to_bit_array(bytes))
}
