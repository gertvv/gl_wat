import gl_wat as w
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import gleeunit
import nibble
import nibble/lexer

pub fn main() {
  gleeunit.main()
}

pub fn basic_lexer_test() {
  let assert Ok(tokens) =
    lexer.run("(module (func $0 (result i32) (i32.const 0xA)))", w.lexer())
  let tokens = list.map(tokens, fn(token) { token.value })
  assert tokens
    == [
      w.LParen,
      w.Keyword("module"),
      w.LParen,
      w.Keyword("func"),
      w.Id("$0"),
      w.LParen,
      w.Keyword("result"),
      w.Keyword("i32"),
      w.RParen,
      w.LParen,
      w.Keyword("i32.const"),
      w.Integer("0xA"),
      w.RParen,
      w.RParen,
      w.RParen,
    ]
}

pub fn lex_invalid_concat_test() {
  let assert Error(_) = lexer.run("0$x", w.lexer())
}

pub fn int_lexer_test() {
  let valid_ints = "-12 +1_000 -1_2_3 12_345 +0x1F -0x1e 0xaaa_bbbb"
  let assert Ok(res) = lexer.run(valid_ints, w.lexer())
  assert list.map(res, fn(token) { token.lexeme })
    == string.split(valid_ints, " ")
  assert list.all(res, fn(token) {
    case token.value {
      w.Integer(_) -> True
      _ -> False
    }
  })
}

pub fn str_lexer_test() {
  let l = w.lexer()
  assert_lex_self_as(l, "\"Hello, World!\"", w.String)
  let assert Error(_) = lexer.run("\"Hello", l)
  let assert Error(_) = lexer.run("\"Hello\t\"", l)
  assert_lex_self_as(l, "\"Hello\\tWorld\"", w.String)
  assert_lex_self_as(l, "\"Hello\\\"World\"", w.String)
  assert_lex_self_as(l, "\"Hello\\\\World\"", w.String)
  assert_lex_self_as(l, "\"\\u{0x7F_A2}\"", w.String)
  assert_lex_self_as(l, "\"\\5c\\00\"", w.String)
}

pub fn parse_func(str) {
  let assert Ok(tokens) = lexer.run(str, w.lexer())
  nibble.run(tokens, w.func_parser())
}

pub fn parse_func_empty_test() {
  let assert Ok(w.Func(None, [], [])) = parse_func("(func)")
}

pub fn parse_func_invalid_test() {
  let assert Error(_) = parse_func("(type)")
}

pub fn parse_func_one_param_test() {
  let assert Ok(w.Func(None, [w.Param(None, w.I32)], [])) =
    parse_func("(func (param i32))")
}

pub fn parse_func_two_params_test() {
  let assert Ok(w.Func(None, [w.Param(None, w.I32), w.Param(None, w.I32)], [])) =
    parse_func("(func (param i32) (param i32))")

  let assert Ok(w.Func(None, [w.Param(None, w.F32), w.Param(None, w.F32)], [])) =
    parse_func("(func (param f32 f32))")
}

pub fn parse_func_param_with_id_test() {
  let assert Ok(w.Func(None, [w.Param(Some("$n"), w.I64)], [])) =
    parse_func("(func (param $n i64))")

  let assert Error(_) = parse_func("(func (param $n i64 i64))")
}

pub fn parse_func_one_result_test() {
  let assert Ok(w.Func(None, [], [w.F64])) = parse_func("(func (result f64))")
}

pub fn parse_func_two_results_test() {
  let assert Ok(w.Func(None, [w.Param(None, w.I32)], [w.F64, w.I32])) =
    parse_func("(func (param i32) (result f64) (result i32))")
  let assert Error(_) =
    parse_func("(func (result f64) (param i32) (result i32))")
}

pub fn parse_func_with_id_test() {
  let assert Ok(w.Func(Some("$f"), [], [])) = parse_func("(func $f)")

  let assert Ok(w.Func(Some("$f"), [], [w.I32])) =
    parse_func("(func $f (result i32))")
}

fn assert_lex_self_as(l, input, fun) {
  let assert Ok([token]) = lexer.run(input, l)
  assert token.lexeme == input
  assert token.value == fun(input)
}
