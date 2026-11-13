import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/regexp.{type Regexp}
import nibble
import nibble/lexer

pub type Token {
  LParen
  RParen
  Keyword(String)
  Integer(String)
  Float(String)
  String(String)
  Id(String)
  // Reserved
}

const idchar = "[0-9A-Za-z!#$%&'*+-\\./:<=>?@\\^_`|~]"

pub fn main() {
  let lexer = lexer()

  echo lexer.run("0$x", lexer)
}

pub fn lexer() {
  let assert Ok(int_re) =
    regexp.from_string(
      "^[+-]?(0x[0-9A-Fa-f](_?[0-9A-Fa-f])*)|(^[+-]?[0-9](_?[0-9])*$)$",
    )
  let assert Ok(str_re) =
    regexp.from_string(
      "\"([^\\x00-\\x19\\x7f\"\\\\]|\\\\t|\\\\n|\\\\r|\\\\\"|\\\\'|\\\\|\\\\u{0x[0-9A-Fa-f](_?[0-9A-Fa-f])*}|\\\\([0-9A-Fa-f]{2}))*\"",
    )
  let assert Ok(keyword_re) = regexp.from_string("^[a-z]" <> idchar <> "*$")
  let assert Ok(id_re) = regexp.from_string("^[$]" <> idchar <> "+$")

  lexer.simple([
    separated_token(keyword_re, Keyword),
    // TODO: name-style IDs
    separated_token(id_re, Id),
    separated_token(int_re, Integer),
    // TODO: float
    separated_token(str_re, String),
    lexer.token("(", LParen),
    lexer.token(")", RParen),
    lexer.comment(";;", fn(_) { Nil }) |> lexer.ignore(),
    // TODO: the spec's definition of whitespace
    lexer.whitespace(Nil) |> lexer.ignore(),
  ])
}

fn is_separator(lookahead: String) {
  // TODO: expand!
  lookahead == "" || lookahead == " " || lookahead == "(" || lookahead == ")"
}

fn separated_token(
  re: Regexp,
  token: fn(String) -> Token,
) -> lexer.Matcher(Token, mode) {
  lexer.keep(fn(lexeme, lookahead) {
    case is_separator(lookahead) && regexp.check(re, lexeme) {
      True -> Ok(token(lexeme))
      False -> Error(Nil)
    }
  })
}

pub type ValueType {
  I32
  I64
  F32
  F64
}

pub type Param {
  Param(id: Option(String), typ: ValueType)
}

pub type Func {
  Func(params: List(Param))
}

pub fn func_parser() {
  use _ <- nibble.do(nibble.token(LParen))
  use _ <- nibble.do(nibble.token(Keyword("func")))
  // TOOD: id
  // TODO: explict func type
  use params <- nibble.do(nibble.many(param_parser()))
  use _ <- nibble.do(nibble.token(RParen))
  nibble.return(Func(list.flatten(params)))
}

fn param_parser() {
  use _ <- nibble.do(nibble.token(LParen))
  use _ <- nibble.do(nibble.token(Keyword("param")))
  use maybe_id <- nibble.do(nibble.optional(id_parser()))
  case maybe_id {
    Some(id) -> {
      use type_ <- nibble.do(value_type_parser())
      use _ <- nibble.do(nibble.token(RParen))
      nibble.return([Param(Some(id), type_)])
    }
    None -> {
      use types <- nibble.do(nibble.many(value_type_parser()))
      use _ <- nibble.do(nibble.token(RParen))
      nibble.return(list.map(types, Param(None, _)))
    }
  }
}

fn id_parser() {
  use t <- nibble.do(nibble.any())
  case t {
    Id(id) -> nibble.return(id)
    _ -> nibble.fail("expected an id")
  }
}

fn value_type_parser() {
  use t <- nibble.do(nibble.any())
  case t {
    Keyword(name) ->
      case name {
        "i32" -> nibble.return(I32)
        "i64" -> nibble.return(I64)
        "f32" -> nibble.return(F32)
        "f64" -> nibble.return(F64)
        _ -> nibble.fail("expeced a value type")
      }
    _ -> nibble.fail("expected a keyword")
  }
}
