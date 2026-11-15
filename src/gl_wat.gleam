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
    expr: List(Instruction),
  )
}

pub type BlockType {
  BlockTypeUse(
    type_: Option(Index),
    params: List(Param),
    results: List(ValueType),
  )
  BlockResult(ValueType)
  BlockEmpty
}

pub type HeapType

pub type RefType

pub type Instruction {
  // Control instructions
  Unreachable
  Nop
  Block(label: Option(String), block_type: BlockType)
  Loop(label: Option(String), block_type: BlockType)
  If(label: Option(String), block_type: BlockType)
  Else(label: Option(String))
  End(label: Option(String))
  // TODO: exceptions
  Branch(label_index: Index)
  BranchIf(label_index: Index)
  // TODO: BranchTable
  Return
  Call(function_index: Index)
  // TODO: CallIndirect
  ReturnCall(function_index: Index)
  // TODO: ReturnCallIndirect
  CallRef(type_index: Index)
  ReturnCallRef(type_index: Index)
  @deprecated("Use BranchOnNull instead")
  BreakOnNull(label_index: Index)
  BranchOnNull(label_index: Index)
  @deprecated("Use BranchOnNonNull instead")
  BreakOnNonNull(label_index: Index)
  BranchOnNonNull(label_index: Index)
  // TODO: BranchOnCast*
  //
  // Reference instructions
  RefNull(heap_type: HeapType)
  RefFunc(function_index: Index)
  RefIsNull
  RefAsNonNull
  RefEq
  RefTest(ref_type: RefType)
  RefCast(ref_type: RefType)
  StructNew(type_index: Index)
  StructNewDefault(type_index: Index)
  StructGet(type_index: Index, field_index: Index)
  StructGetS(type_index: Index, field_index: Index)
  StructGetU(type_index: Index, field_index: Index)
  StructSet(type_index: Index, field_index: Index)
  ArrayNew(type_index: Index)
  ArrayNewDefault(type_index: Index)
  ArrayNewFixed(type_index: Index, size: Int)
  ArrayNewData(type_index: Index, data_index: Index)
  // TODO: ArrayNewElem
  ArrayGet(type_index: Index)
  ArrayGetS(type_index: Index)
  ArrayGetU(type_index: Index)
  ArraySet(type_index: Index)
  ArrayLen
  ArrayFill(type_index: Index)
  ArrayCopy(type_index: Index, source_type_index: Index)
  ArrayInitData(type_index: Index, data_index: Index)
  // TODO: ArrayInitElem
  // TODO: remaining reference instructions
  //
  // Parametric instructions
  Drop
  Select(List(ValueType))
  //
  // Variable instructions
  LocalGet(local_index: Index)
  LocalSet(local_index: Index)
  LocalTee(local_index: Index)
  GlobalGet(global_index: Index)
  GlobalSet(global_index: Index)
  //
  // Table instructions
  TableGet(table_index: Index)
  TableSet(table_index: Index)
  TableSize(table_index: Index)
  TableGrow(table_index: Index)
  TableFill(table_index: Index)
  TableCopy(table_index: Index, source_table_index: Index)
  TableInit(table_index: Index, element_index: Index)
  ElemDrop(element_index: Index)
  //
  // Memory instructions
  I32Load(offset: Int, align: Int)
  I64Load(offset: Int, align: Int)
  F32Load(offset: Int, align: Int)
  F64Load(offset: Int, align: Int)
  I32Load8S(offset: Int, align: Int)
  I32Load8U(offset: Int, align: Int)
  I32Load16S(offset: Int, align: Int)
  I32Load16U(offset: Int, align: Int)
  I64Load8S(offset: Int, align: Int)
  I64Load8U(offset: Int, align: Int)
  I64Load16S(offset: Int, align: Int)
  I64Load16U(offset: Int, align: Int)
  I64Load32S(offset: Int, align: Int)
  I64Load32U(offset: Int, align: Int)
  I32Store(offset: Int, align: Int)
  I64Store(offset: Int, align: Int)
  F32Store(offset: Int, align: Int)
  F64Store(offset: Int, align: Int)
  I32Store8(offset: Int, align: Int)
  I32Store16(offset: Int, align: Int)
  I64Store8(offset: Int, align: Int)
  I64Store16(offset: Int, align: Int)
  I64Store32(offset: Int, align: Int)
  MemorySize
  MemoryGrow
  MemoryFill
  MemoryCopy
  MemoryInit(data_index: Index)
  DataDrop(data_index: Index)
  //
  // Numeric instructions
  I32Const(value: String)
  I64Const(value: String)
  F32Const(value: String)
  F64Const(value: String)
  I32EqZ
  I32Eq
  I32NE
  I32LtS
  I32LtU
  I32GtS
  I32GtU
  I32LeS
  I32LeU
  I32GeS
  I32GeU
  I64EqZ
  I64Eq
  I64NE
  I64LtS
  I64LtU
  I64GtS
  I64GtU
  I64LeS
  I64LeU
  I64GeS
  I64GeU
  F32Eq
  F32NE
  F32Lt
  F32Gt
  F32Le
  F32Ge
  F64Eq
  F64NE
  F64Lt
  F64Gt
  F64Le
  F64Ge
  I32CntLZ
  I32CntTZ
  I32PopCnt
  I32Add
  I32Sub
  I32Mul
  I32DivS
  I32DivU
  I32RemS
  I32RemU
  I32And
  I32Or
  I32Xor
  I32ShL
  I32ShRS
  I32ShLU
  I32RotL
  I32RotR
  I64CntLZ
  I64CntTZ
  I64PopCnt
  I64Add
  I64Sub
  I64Mul
  I64DivS
  I64DivU
  I64RemS
  I64RemU
  I64And
  I64Or
  I64Xor
  I64ShL
  I64ShRS
  I64ShLU
  I64RotL
  I64RotR
  F32Abs
  F32Neg
  F32Ceil
  F32Floor
  F32Trunc
  F32Nearest
  F32Sqrt
  F32Add
  F32Sub
  F32Mul
  F32Div
  F32Min
  F32Max
  F32CopySign
  F64Abs
  F64Neg
  F64Ceil
  F64Floor
  F64Trunc
  F64Nearest
  F64Sqrt
  F64Add
  F64Sub
  F64Mul
  F64Div
  F64Min
  F64Max
  F64CopySign
  // TODO: conversions / truncations / etc.
  //
  // Vector instructions
  // TODO: vector instructions
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

fn func_parser_internal() {
  use id <- nibble.do(id_parser() |> nibble.optional)
  // TODO: exports
  // TODO: import
  use #(_, func) <- nibble.do({
    use ctx <- nibble.loop(#(0, Func(id, None, [], [], [], [])))
    nibble.one_of([
      nibble.token(RParen) |> nibble.replace(nibble.Break(ctx)),
      {
        use _ <- nibble.do(nibble.token(LParen))
        nibble.one_of([
          staged_optional(func_type_parser, ctx, 1)
            |> nibble.map(nibble.Continue),
          staged_many(param_parser, ctx, 2)
            |> nibble.map(nibble.Continue),
          staged_many(result_parser, ctx, 3)
            |> nibble.map(nibble.Continue),
          staged_many(local_parser, ctx, 4)
            |> nibble.map(nibble.Continue),
          staged_many(func_folded_expr_parser, ctx, 5)
            |> nibble.map(nibble.Continue),
        ])
      },
      staged_many(func_flat_expr_parser, ctx, 5) |> nibble.map(nibble.Continue),
    ])
  })
  nibble.return(func)
}

fn staged_optional(parser, ctx, n) {
  let #(m, ctx) = ctx
  use <- bool.guard(n <= m, nibble.fail("out-of-place"))

  use ctx <- nibble.do(parser(ctx))
  nibble.return(#(n, ctx))
}

fn staged_many(parser, ctx, n) {
  let #(m, ctx) = ctx
  use <- bool.guard(n < m, nibble.fail("out-of-place"))

  use ctx <- nibble.do(parser(ctx))
  nibble.return(#(n, ctx))
}

fn func_folded_expr_parser(func: Func) -> nibble.Parser(Func, Token, a) {
  use expr <- nibble.do(folded_instr_parser_internal())
  nibble.return(Func(..func, expr: list.append(func.expr, expr)))
}

fn instr_parser() -> nibble.Parser(List(Instruction), Token, a) {
  nibble.one_of([
    flat_instr_parser(),
    folded_instr_parser(),
  ])
}

fn func_flat_expr_parser(func: Func) -> nibble.Parser(Func, Token, a) {
  use expr <- nibble.do(flat_instr_parser())
  nibble.return(Func(..func, expr: list.append(func.expr, expr)))
}

fn flat_instr_parser() {
  use expr <- nibble.do(basic_instr_parser())
  case expr {
    Block(..) | Loop(..) | If(..) -> {
      use #(block_type, arg) <- nibble.do(block_type_parser())
      let block = case expr {
        Block(label:, block_type: _) -> Block(label:, block_type:)
        Loop(label:, block_type: _) -> Loop(label:, block_type:)
        If(label:, block_type: _) -> If(label:, block_type:)
        _ -> panic
      }
      nibble.return([block, ..arg])
    }
    _ -> {
      nibble.return([expr])
    }
  }
}

fn folded_instr_parser() -> nibble.Parser(List(Instruction), Token, a) {
  use _ <- nibble.do(nibble.token(LParen))
  folded_instr_parser_internal()
}

fn folded_instr_parser_internal() {
  use expr <- nibble.do(basic_instr_parser())
  use expr <- nibble.do(case expr {
    Block(label:, ..) -> folded_block_parser(label, Block)
    Loop(label:, ..) -> folded_block_parser(label, Loop)
    If(label:, ..) -> folded_if_parser(label)
    _ -> {
      use args <- nibble.do(nibble.many(instr_parser()))
      nibble.return(list.append(list.flatten(args), [expr]))
    }
  })
  use _ <- nibble.do(nibble.token(RParen))
  nibble.return(expr)
}

fn folded_if_parser(
  label: Option(String),
) -> nibble.Parser(List(Instruction), Token, a) {
  use #(block_type, arg) <- nibble.do(block_type_parser())
  // TODO: we might already have eaten the then clause...
  let if_ = If(label:, block_type:)
  // TODO: this will eat the then clause + only folded instr are allowed
  use args <- nibble.do(nibble.many(instr_parser()))
  let args = list.flatten([arg, ..args])
  use then <- nibble.do(todo)
  use else_ <- nibble.do(todo |> nibble.optional)
  let else_ = case else_ {
    Some(expr) -> [Else(None), ..expr]
    None -> []
  }
  nibble.return(list.flatten([args, [if_], then, else_, [End(None)]]))
}

fn folded_block_parser(
  label: Option(String),
  constr: fn(Option(String), BlockType) -> Instruction,
) -> nibble.Parser(List(Instruction), Token, a) {
  use #(block_type, arg) <- nibble.do(block_type_parser())
  let block = constr(label, block_type)
  use args <- nibble.do(nibble.many(instr_parser()))
  let args = list.flatten([arg, ..args])
  nibble.return(list.flatten([[block], args, [End(None)]]))
}

fn block_type_parser() {
  // treat block as a special case of func
  use #(_, func) <- nibble.do({
    use ctx <- nibble.loop(#(0, Func(None, None, [], [], [], [])))
    use maybe <- nibble.do(nibble.token(LParen) |> nibble.optional)
    use <- bool.guard(maybe == None, nibble.return(nibble.Break(ctx)))
    use maybe_ctx <- nibble.do(
      nibble.one_of([
        staged_many(func_type_parser, ctx, 1),
        staged_many(param_parser, ctx, 2),
        staged_many(result_parser, ctx, 3),
        staged_many(func_folded_expr_parser, ctx, 4),
      ])
      |> nibble.optional,
    )
    case maybe_ctx {
      Some(#(4, _) as ctx) -> nibble.Break(ctx)
      Some(ctx) -> nibble.Continue(ctx)
      None -> nibble.Break(ctx)
    }
    |> nibble.return
  })
  // map the func type to the appropriate block type
  case func {
    Func(type_: None, params: [], results: [], expr:, ..) -> #(BlockEmpty, expr)
    Func(type_: None, params: [], results: [result], expr:, ..) -> #(
      BlockResult(result),
      expr,
    )
    Func(type_:, params:, results:, expr:, ..) -> #(
      BlockTypeUse(type_:, params:, results:),
      expr,
    )
  }
  |> nibble.return
}

fn basic_instr_parser() -> nibble.Parser(Instruction, Token, a) {
  use name <- nibble.do(keyword())
  let r = nibble.return
  case name {
    // parametric instructions
    "nop" -> r(Nop)
    "unreachable" -> r(Unreachable)
    "drop" -> r(Drop)
    "select" -> todo
    // control instructions
    // NOTE: further parsing of block instructions is handled by the caller
    "block" ->
      one_arg_parser(nibble.optional(id_parser()), Block(_, BlockEmpty))
    "loop" -> one_arg_parser(nibble.optional(id_parser()), Loop(_, BlockEmpty))
    "if" -> one_arg_parser(nibble.optional(id_parser()), If(_, BlockEmpty))
    "else" -> one_arg_parser(nibble.optional(id_parser()), Else)
    "end" -> one_arg_parser(nibble.optional(id_parser()), End)
    // TODO: try/catch
    "br" -> one_arg_parser(index_parser(), Branch)
    "br_if" -> one_arg_parser(index_parser(), BranchIf)
    "br_table" -> todo
    "br_on_null" -> one_arg_parser(index_parser(), BranchOnNull)
    "br_on_cast" -> todo
    "br_on_cast_fail" -> todo
    "call" -> one_arg_parser(index_parser(), Call)
    "call_ref" -> one_arg_parser(index_parser(), CallRef)
    "call_indirect" -> todo
    "return" -> r(Return)
    "return_call" -> one_arg_parser(index_parser(), ReturnCall)
    "return_call_ref" -> one_arg_parser(index_parser(), ReturnCallRef)
    "return_call_indirect" -> todo
    "throw" -> todo
    "throw_ref" -> todo
    // variable instructions
    "local.get" -> one_arg_parser(index_parser(), LocalGet)
    "local.set" -> one_arg_parser(index_parser(), LocalSet)
    "local.tee" -> one_arg_parser(index_parser(), LocalTee)
    "global.get" -> one_arg_parser(index_parser(), GlobalGet)
    "global.set" -> one_arg_parser(index_parser(), GlobalSet)
    // table instructions
    "table.get" ->
      one_arg_parser_optional(index_parser(), IndexInt("0"), TableGet)
    "table.set" ->
      one_arg_parser_optional(index_parser(), IndexInt("0"), TableSet)
    "table.size" ->
      one_arg_parser_optional(index_parser(), IndexInt("0"), TableSize)
    "table.grow" ->
      one_arg_parser_optional(index_parser(), IndexInt("0"), TableGrow)
    "table.copy" ->
      // TODO: could possibly be more robust...
      nibble.one_of([
        two_arg_parser(index_parser(), index_parser(), TableCopy),
        r(TableCopy(IndexInt("0"), IndexInt("0"))),
      ])
    "table.init" -> todo
    "elem.drop" ->
      one_arg_parser_optional(index_parser(), IndexInt("0"), ElemDrop)
    // TODO: memory instructions
    // TODO: reference instructions
    // numeric instructions
    "i32.const" -> one_arg_parser(int_parser(), I32Const)
    "i64.const" -> one_arg_parser(int_parser(), I64Const)
    "f32.const" -> todo
    "f64.const" -> todo
    "i32.eqz" -> r(I32EqZ)
    "i32.eq" -> r(I32Eq)
    "i32.ne" -> r(I32NE)
    "i32.lt_s" -> r(I32LtS)
    "i32.lt_u" -> r(I32LtU)
    "i32.gt_s" -> r(I32GtS)
    "i32.gt_u" -> r(I32GtU)
    "i32.le_s" -> r(I32LeS)
    "i32.le_u" -> r(I32LeU)
    "i32.ge_s" -> r(I32GeS)
    "i32.ge_u" -> r(I32GeU)
    "i32.clz" -> r(I32CntLZ)
    "i32.ctz" -> r(I32CntTZ)
    "i32.popcnt" -> r(I32PopCnt)
    "i32.extend8_s" -> todo
    "i32.extend16_s" -> todo
    "i32.add" -> r(I32Add)
    "i32.sub" -> r(I32Sub)
    "i32.mul" -> r(I32Mul)
    "i32.div_s" -> r(I32DivS)
    "i32.div_u" -> r(I32DivU)
    "i32.rem_s" -> r(I32RemS)
    "i32.rem_u" -> r(I32RemU)
    "i32.and" -> r(I32And)
    "i32.or" -> r(I32Or)
    "i32.xor" -> r(I32Xor)
    "i32.shl" -> r(I32ShL)
    "i32.shr_s" -> r(I32ShRS)
    "i32.shr_u" -> todo
    "i32.rotl" -> r(I32RotL)
    "i32.rotr" -> r(I32RotR)
    "i64.eqz" -> r(I64EqZ)
    "i64.eq" -> r(I64Eq)
    "i64.ne" -> r(I64NE)
    "i64.lt_s" -> r(I64LtS)
    "i64.lt_u" -> r(I64LtU)
    "i64.gt_s" -> r(I64GtS)
    "i64.gt_u" -> r(I64GtU)
    "i64.le_s" -> r(I64LeS)
    "i64.le_u" -> r(I64LeU)
    "i64.ge_s" -> r(I64GeS)
    "i64.ge_u" -> r(I64GeU)
    "i64.clz" -> r(I64CntLZ)
    "i64.ctz" -> r(I64CntTZ)
    "i64.popcnt" -> r(I64PopCnt)
    "i64.extend8_s" -> todo
    "i64.extend16_s" -> todo
    "i64.extend32_s" -> todo
    "i64.add" -> r(I64Add)
    "i64.sub" -> r(I64Sub)
    "i64.mul" -> r(I64Mul)
    "i64.div_s" -> r(I64DivS)
    "i64.div_u" -> r(I64DivU)
    "i64.rem_s" -> r(I64RemS)
    "i64.rem_u" -> r(I64RemU)
    "i64.and" -> r(I64And)
    "i64.or" -> r(I64Or)
    "i64.xor" -> r(I64Xor)
    "i64.shl" -> r(I64ShL)
    "i64.shr_s" -> r(I64ShRS)
    "i64.shr_u" -> todo
    "i64.rotl" -> r(I64RotL)
    "i64.rotr" -> r(I64RotR)
    "f32.eq" -> r(F32Eq)
    "f32.ne" -> r(F32NE)
    "f32.lt" -> r(F32Lt)
    "f32.gt" -> r(F32Gt)
    "f32.le" -> r(F32Le)
    "f32.ge" -> r(F32Ge)
    "f32.abs" -> r(F32Abs)
    "f32.neg" -> r(F32Neg)
    "f32.sqrt" -> r(F32Sqrt)
    "f32.ceil" -> r(F32Ceil)
    "f32.floor" -> r(F32Floor)
    "f32.trunc" -> r(F32Trunc)
    "f32.nearest" -> r(F32Nearest)
    "f32.add" -> r(F32Add)
    "f32.sub" -> r(F32Sub)
    "f32.mul" -> r(F32Mul)
    "f32.div" -> r(F32Div)
    "f32.min" -> r(F32Min)
    "f32.max" -> r(F32Max)
    "f32.copysign" -> r(F32CopySign)
    "f64.eq" -> r(F64Eq)
    "f64.ne" -> r(F64NE)
    "f64.lt" -> r(F64Lt)
    "f64.gt" -> r(F64Gt)
    "f64.le" -> r(F64Le)
    "f64.ge" -> r(F64Ge)
    "f64.abs" -> r(F64Abs)
    "f64.neg" -> r(F64Neg)
    "f64.sqrt" -> r(F64Sqrt)
    "f64.ceil" -> r(F64Ceil)
    "f64.floor" -> r(F64Floor)
    "f64.trunc" -> r(F64Trunc)
    "f64.nearest" -> r(F64Nearest)
    "f64.add" -> r(F64Add)
    "f64.sub" -> r(F64Sub)
    "f64.mul" -> r(F64Mul)
    "f64.div" -> r(F64Div)
    "f64.min" -> r(F64Min)
    "f64.max" -> r(F64Max)
    "f64.copysign" -> r(F64CopySign)
    // TODO: wrap/trunc/convert/etc.
    // TODO: vector instructions
    _ -> nibble.fail("Unrecognized instruction")
  }
}

fn one_arg_parser(arg1, constr) {
  use arg1 <- nibble.do(arg1)
  nibble.return(constr(arg1))
}

fn two_arg_parser(arg1, arg2, constr) {
  use arg1 <- nibble.do(arg1)
  use arg2 <- nibble.do(arg2)
  nibble.return(constr(arg1, arg2))
}

fn one_arg_parser_optional(arg1, def1, constr) {
  use arg1 <- nibble.do(nibble.optional(arg1))
  let arg1 = option.unwrap(arg1, def1)
  nibble.return(constr(arg1))
}

fn index_parser() {
  nibble.one_of([
    int_parser() |> nibble.map(IndexInt),
    id_parser() |> nibble.map(IndexId),
  ])
}

fn keyword() -> nibble.Parser(String, Token, a) {
  use t <- nibble.do(nibble.any())
  case t {
    Keyword(name) -> nibble.return(name)
    _ -> nibble.fail("Expected a keyword")
  }
}

fn func_type_parser(func: Func) -> nibble.Parser(Func, Token, a) {
  use _ <- nibble.do(nibble.token(Keyword("type")))
  use type_use <- nibble.do(index_parser())
  use _ <- nibble.do(nibble.token(RParen))
  nibble.return(Func(..func, type_: Some(type_use)))
}

fn int_parser() -> nibble.Parser(String, Token, a) {
  use t <- nibble.do(nibble.any())
  case t {
    Integer(str) -> nibble.return(str)
    _ -> nibble.fail("expected an integer")
  }
}

fn param_parser(func: Func) {
  use _ <- nibble.do(nibble.token(Keyword("param")))
  use params <- nibble.do(param_like_parser(func.params))
  nibble.return(Func(..func, params:))
}

fn result_parser(func: Func) {
  use _ <- nibble.do(nibble.token(Keyword("result")))
  use types <- nibble.do(nibble.many(value_type_parser()))
  use _ <- nibble.do(nibble.token(RParen))
  nibble.return(Func(..func, results: list.append(func.results, types)))
}

fn local_parser(func: Func) -> nibble.Parser(Func, Token, a) {
  use _ <- nibble.do(nibble.token(Keyword("local")))
  use locals <- nibble.do(param_like_parser(func.locals))
  nibble.return(Func(..func, locals:))
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
