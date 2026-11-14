import gleam/bool
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
  Func(
    id: Option(String),
    type_: Option(Index),
    params: List(Param),
    results: List(ValueType),
    locals: List(Param),
  )
}

pub type Index {
  IndexInt(String)
  IndexId(String)
}

pub fn func_parser() {
  use _ <- nibble.do(nibble.token(LParen))
  use _ <- nibble.do(nibble.token(Keyword("func")))
  func_parser_internal()
}

type FuncParseContext {
  ConsumedNothing(func: Func)
  ConsumedType(func: Func)
  ConsumedParam(func: Func)
  ConsumedResult(func: Func)
  ConsumedLocal(func: Func)
}

fn func_parser_internal() {
  use id <- nibble.do(id_parser() |> nibble.optional)
  // TODO: exports
  // TODO: import
  use func <- nibble.do({
    use ctx <- nibble.loop(ConsumedNothing(Func(id, None, [], [], [])))
    nibble.one_of([
      nibble.token(RParen) |> nibble.replace(nibble.Break(ctx)),
      {
        use _ <- nibble.do(nibble.token(LParen))
        nibble.one_of([
          func_type_parser(ctx)
            |> nibble.map(nibble.Continue),
          param_parser(ctx)
            |> nibble.map(nibble.Continue),
          result_parser(ctx)
            |> nibble.map(nibble.Continue),
          local_parser(ctx)
            |> nibble.map(nibble.Continue),
        ])
      },
    ])
  })
  // TODO: expressions
  nibble.return(func.func)
}

fn func_type_parser(
  ctx: FuncParseContext,
) -> nibble.Parser(FuncParseContext, Token, a) {
  use <- bool.guard(
    case ctx {
      ConsumedNothing(..) -> False
      _ -> True
    },
    nibble.fail("out-of-place type use"),
  )
  let func = ctx.func
  use _ <- nibble.do(nibble.token(Keyword("type")))
  use type_use <- nibble.do(
    nibble.one_of([
      int_parser() |> nibble.map(IndexInt),
      id_parser() |> nibble.map(IndexId),
    ]),
  )
  use _ <- nibble.do(nibble.token(RParen))
  nibble.return(ConsumedType(Func(..func, type_: Some(type_use))))
}

fn int_parser() -> nibble.Parser(String, Token, a) {
  use t <- nibble.do(nibble.any())
  case t {
    Integer(str) -> nibble.return(str)
    _ -> nibble.fail("expected an integer")
  }
}

fn param_parser(ctx: FuncParseContext) {
  use <- bool.guard(
    case ctx {
      ConsumedNothing(..) | ConsumedType(..) | ConsumedParam(..) -> False
      _ -> True
    },
    nibble.fail("out-of-place param"),
  )
  let func = ctx.func
  use _ <- nibble.do(nibble.token(Keyword("param")))
  use params <- nibble.do(param_like_parser(func.params))
  nibble.return(Func(..func, params:) |> ConsumedParam)
}

fn result_parser(ctx: FuncParseContext) {
  use <- bool.guard(
    case ctx {
      ConsumedNothing(..)
      | ConsumedType(..)
      | ConsumedParam(..)
      | ConsumedResult(..) -> False
      _ -> True
    },
    nibble.fail("out-of-place result"),
  )
  let func = ctx.func
  use _ <- nibble.do(nibble.token(Keyword("result")))
  use types <- nibble.do(nibble.many(value_type_parser()))
  use _ <- nibble.do(nibble.token(RParen))
  nibble.return(ConsumedResult(
    Func(..func, results: list.append(func.results, types)),
  ))
}

fn local_parser(
  ctx: FuncParseContext,
) -> nibble.Parser(FuncParseContext, Token, a) {
  let func = ctx.func
  use _ <- nibble.do(nibble.token(Keyword("local")))
  use locals <- nibble.do(param_like_parser(func.locals))
  nibble.return(Func(..func, locals:) |> ConsumedLocal)
}

fn param_like_parser(previous: List(Param)) {
  use maybe_id <- nibble.do(nibble.optional(id_parser()))
  case maybe_id {
    Some(id) -> {
      use type_ <- nibble.do(value_type_parser())
      use _ <- nibble.do(nibble.token(RParen))
      nibble.return(list.append(previous, [Param(Some(id), type_)]))
    }
    None -> {
      use types <- nibble.do(nibble.many(value_type_parser()))
      use _ <- nibble.do(nibble.token(RParen))
      nibble.return(list.append(previous, list.map(types, Param(None, _))))
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
