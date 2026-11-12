import gleam/io
import gleam/list
import gleam/regexp.{type Regexp}
import gleam/result
import gleam/set
import nibble/lexer

pub type Token {
  LParen
  RParen
  Keyword
  Integer
  Float
  String
  Id
  // Reserved
}

const idchar = "[0-9A-Za-z!#$%&'*+-\\./:<=>?@\\^_`|~]"

const hexdigit = ""

pub fn main() {
  let assert Ok(int_re) =
    regexp.from_string(
      "^[+-]?(0x[0-9A-Fa-f](_?[0-9A-Fa-f])*)|(^[+-][0-9](_?[0-9])*$)$",
    )
  let assert Ok(keyword_re) = regexp.from_string("^[a-z]" <> idchar <> "*$")
  let assert Ok(id_re) = regexp.from_string("^[$]" <> idchar <> "+$")

  let lexer =
    lexer.simple([
      separated_token(keyword_re, Keyword),
      // TODO: name-style IDs
      separated_token(id_re, Id),
      separated_token(int_re, Integer),
      // TODO: float
      // TODO: string
      lexer.token("(", LParen),
      lexer.token(")", RParen),
      // TODO: the spec's definition of whitespace
      lexer.whitespace(Nil) |> lexer.ignore(),
    ])

  echo lexer.run("(module (func $0 (result i32) (i32.const 0xA)))", lexer)
    |> result.map(fn(tokens) {
      list.map(tokens, fn(token) { #(token.lexeme, token.value) })
    })

  echo lexer.run("0$x", lexer)
}

fn is_separator(lookahead: String) {
  // TODO: expand!
  lookahead == " " || lookahead == "(" || lookahead == ")"
}

fn separated_token(re: Regexp, token: Token) -> lexer.Matcher(Token, mode) {
  lexer.keep(fn(lexeme, lookahead) {
    case is_separator(lookahead) && regexp.check(re, lexeme) {
      True -> Ok(token)
      False -> Error(Nil)
    }
  })
}
