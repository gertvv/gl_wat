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
  Ref(RefType)
}

pub type HeapType {
  HeapAny
  HeapEq
  HeapI31
  HeapStruct
  HeapArray
  HeapNone
  HeapFunc
  HeapNoFunc
  // TODO: exn, noexn
  HeapExtern
  HeapNoExtern
  // TODO: call this TypeUse?
  HeapIndex(Index)
}

pub type RefType {
  NonNull(heap_type: HeapType)
  Nullable(heap_type: HeapType)
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

type IfElseState {
  IfElseState(
    label: Option(String),
    type_: Option(Index),
    params: List(Param),
    results: List(ValueType),
    args: List(Instruction),
    then: List(Instruction),
    else_: List(Instruction),
  )
}

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
  BranchTable(label_indices: List(Index), default_label_index: Index)
  Return
  Call(function_index: Index)
  CallIndirect(
    table_index: Index,
    type_: Option(Index),
    params: List(ValueType),
    results: List(ValueType),
  )
  ReturnCall(function_index: Index)
  ReturnCallIndirect(
    table_index: Index,
    type_: Option(Index),
    params: List(ValueType),
    results: List(ValueType),
  )
  CallRef(type_index: Index)
  ReturnCallRef(type_index: Index)
  BranchOnNull(label_index: Index)
  BranchOnNonNull(label_index: Index)
  BranchOnCast(label_index: Index, from_ref_type: RefType, to_ref_type: RefType)
  BranchOnCastFail(
    label_index: Index,
    from_ref_type: RefType,
    to_ref_type: RefType,
  )
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
  I32Extend8S
  I32Extend16S
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
  I32ShRU
  I32RotL
  I32RotR
  I64CntLZ
  I64CntTZ
  I64PopCnt
  I64Extend8S
  I64Extend16S
  I64Extend32S
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
  I64ShRU
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
  use #(stage, func) <- nibble.loop(#(0, Func(id, None, [], [], [], [])))
  nibble.one_of([
    nibble.token(RParen) |> nibble.replace(nibble.Break(func)),
    {
      use _ <- nibble.do(nibble.token(LParen))
      nibble.one_of([
        staged_optional(func_type_parser(func, update_func_type), stage, 1)
          |> nibble.map(nibble.Continue),
        staged_many(param_parser(func, append_func_params), stage, 2)
          |> nibble.map(nibble.Continue),
        staged_many(result_parser(func, append_func_results), stage, 3)
          |> nibble.map(nibble.Continue),
        staged_many(local_parser(func, append_func_locals), stage, 4)
          |> nibble.map(nibble.Continue),
        staged_many(folded_expr_parser(func, append_func_expr), stage, 5)
          |> nibble.map(nibble.Continue),
      ])
    },
    staged_many(flat_expr_parser(func, append_func_expr), stage, 5)
      |> nibble.map(nibble.Continue),
  ])
}

fn update_func_type(func: Func, type_: Option(Index)) -> Func {
  Func(..func, type_:)
}

fn append_func_params(func: Func, params: List(Param)) -> Func {
  Func(..func, params: list.append(func.params, params))
}

fn append_func_results(func: Func, results: List(ValueType)) -> Func {
  Func(..func, results: list.append(func.results, results))
}

fn append_func_locals(func: Func, locals: List(Param)) -> Func {
  Func(..func, locals: list.append(func.locals, locals))
}

fn append_func_expr(func: Func, expr: List(Instruction)) -> Func {
  Func(..func, expr: list.append(func.expr, expr))
}

fn staged_optional(parser, m, n) {
  use <- bool.guard(n <= m, nibble.fail("out-of-place"))

  use ctx <- nibble.do(parser)
  nibble.return(#(n, ctx))
}

fn staged_many(parser, m, n) {
  use <- bool.guard(n < m, nibble.fail("out-of-place"))

  use ctx <- nibble.do(parser)
  nibble.return(#(n, ctx))
}

fn folded_expr_parser(
  ctx: context,
  update: fn(context, List(Instruction)) -> context,
) -> nibble.Parser(context, Token, a) {
  use expr <- nibble.do(folded_instr_parser_internal())
  nibble.return(update(ctx, expr))
}

fn instr_parser() -> nibble.Parser(List(Instruction), Token, a) {
  nibble.one_of([
    flat_instr_parser(),
    folded_instr_parser(),
  ])
}

fn flat_expr_parser(
  ctx: context,
  update: fn(context, List(Instruction)) -> context,
) -> nibble.Parser(context, Token, a) {
  use expr <- nibble.do(flat_instr_parser())
  nibble.return(update(ctx, expr))
}

fn flat_instr_parser() {
  use expr <- nibble.do(basic_instr_parser())
  case expr {
    Select(..) -> {
      flat_select_parser()
    }
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
    CallIndirect(table_index:, ..) -> {
      flat_call_indirect_parser(table_index, None, [], [], CallIndirect)
    }
    ReturnCallIndirect(table_index:, ..) -> {
      flat_call_indirect_parser(table_index, None, [], [], ReturnCallIndirect)
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
    Select(..) -> folded_select_parser()
    Block(label:, ..) -> folded_block_parser(label, Block)
    Loop(label:, ..) -> folded_block_parser(label, Loop)
    If(label:, ..) -> folded_if_parser(label)
    CallIndirect(table_index:, ..) ->
      folded_call_indirect_parser(table_index, None, [], [], CallIndirect)
    ReturnCallIndirect(table_index:, ..) ->
      folded_call_indirect_parser(table_index, None, [], [], ReturnCallIndirect)
    _ -> {
      use args <- nibble.do(nibble.many(instr_parser()))
      nibble.return(list.append(list.flatten(args), [expr]))
    }
  })
  use _ <- nibble.do(nibble.token(RParen))
  nibble.return(expr)
}

type CallIndirectState {
  CallIndirectState(
    type_: Option(Index),
    params: List(ValueType),
    results: List(ValueType),
    args: List(Instruction),
  )
}

type SelectState {
  SelectState(results: List(ValueType), args: List(Instruction))
}

fn flat_select_parser() {
  use #(stage, state) <- nibble.loop(#(0, SelectState([], [])))
  use maybe_state <- nibble.do(select_parser_internal(state, stage))
  case maybe_state {
    Some(#(2, state)) ->
      nibble.Break(list.append(state.args, [Select(state.results)]))
    Some(state) -> nibble.Continue(state)
    None -> nibble.Break(list.append(state.args, [Select(state.results)]))
  }
  |> nibble.return
}

fn folded_select_parser() {
  use #(stage, state) <- nibble.loop(#(0, SelectState([], [])))
  use maybe_state <- nibble.do(select_parser_internal(state, stage))
  case maybe_state {
    Some(state) -> nibble.Continue(state)
    None -> nibble.Break(list.append(state.args, [Select(state.results)]))
  }
  |> nibble.return
}

fn select_parser_internal(
  state: SelectState,
  stage: Int,
) -> nibble.Parser(Option(#(Int, SelectState)), Token, a) {
  nibble.optional(
    nibble.one_of([
      {
        use _ <- nibble.do(nibble.token(LParen))
        nibble.one_of([
          staged_many(result_parser(state, append_select_results), stage, 1),
          staged_many(folded_expr_parser(state, append_select_args), stage, 2),
        ])
      },
      staged_many(flat_expr_parser(state, append_select_args), stage, 2),
    ]),
  )
}

fn append_select_results(
  state: SelectState,
  results: List(ValueType),
) -> SelectState {
  SelectState(..state, results: list.append(state.results, results))
}

fn append_select_args(
  state: SelectState,
  args: List(Instruction),
) -> SelectState {
  SelectState(..state, args: list.append(state.args, args))
}

fn folded_call_indirect_parser(
  table_index: Index,
  type_: Option(Index),
  params: List(ValueType),
  results: List(ValueType),
  constr: fn(Index, Option(Index), List(ValueType), List(ValueType)) ->
    Instruction,
) -> nibble.Parser(List(Instruction), Token, a) {
  use CallIndirectState(type_:, params:, results:, args:) <- nibble.do({
    use #(stage, state) <- nibble.loop(#(
      0,
      CallIndirectState(type_:, params:, results:, args: []),
    ))
    use maybe_state <- nibble.do(call_indirect_parser_internal(state, stage))
    case maybe_state {
      Some(state) -> nibble.Continue(state)
      None -> nibble.Break(state)
    }
    |> nibble.return
  })
  nibble.return(
    list.append(args, [constr(table_index, type_, params, results)]),
  )
}

fn flat_call_indirect_parser(
  table_index: Index,
  type_: Option(Index),
  params: List(ValueType),
  results: List(ValueType),
  constr: fn(Index, Option(Index), List(ValueType), List(ValueType)) ->
    Instruction,
) -> nibble.Parser(List(Instruction), Token, a) {
  use CallIndirectState(type_:, params:, results:, args:) <- nibble.do({
    use #(stage, state) <- nibble.loop(#(
      0,
      CallIndirectState(type_:, params:, results:, args: []),
    ))
    use maybe_state <- nibble.do(call_indirect_parser_internal(state, stage))
    case maybe_state {
      Some(#(4, state)) -> nibble.Break(state)
      Some(state) -> nibble.Continue(state)
      None -> nibble.Break(state)
    }
    |> nibble.return
  })
  nibble.return([constr(table_index, type_, params, results), ..args])
}

fn call_indirect_parser_internal(
  state: CallIndirectState,
  stage: Int,
) -> nibble.Parser(Option(#(Int, CallIndirectState)), Token, a) {
  nibble.optional(
    nibble.one_of([
      {
        use _ <- nibble.do(nibble.token(LParen))
        nibble.one_of([
          staged_optional(
            func_type_parser(state, update_call_indirect_type),
            stage,
            1,
          ),
          staged_many(
            result_like_parser("param", state, append_call_indirect_params),
            stage,
            2,
          ),
          staged_many(
            result_parser(state, append_call_indirect_results),
            stage,
            3,
          ),
          staged_many(
            folded_expr_parser(state, append_call_indirect_args),
            stage,
            4,
          ),
        ])
      },
      staged_many(flat_expr_parser(state, append_call_indirect_args), stage, 4),
    ]),
  )
}

fn update_call_indirect_type(
  state: CallIndirectState,
  type_: Option(Index),
) -> CallIndirectState {
  CallIndirectState(..state, type_:)
}

fn append_call_indirect_params(
  state: CallIndirectState,
  params: List(ValueType),
) {
  CallIndirectState(..state, params: list.append(state.params, params))
}

fn append_call_indirect_results(
  state: CallIndirectState,
  results: List(ValueType),
) {
  CallIndirectState(..state, results: list.append(state.results, results))
}

fn append_call_indirect_args(
  state: CallIndirectState,
  args: List(Instruction),
) {
  CallIndirectState(..state, args: list.append(state.args, args))
}

fn folded_if_parser(
  label: Option(String),
) -> nibble.Parser(List(Instruction), Token, a) {
  use state <- nibble.do({
    use #(stage, state) <- nibble.loop(#(
      0,
      IfElseState(label, None, [], [], [], [], []),
    ))
    use maybe <- nibble.do(nibble.token(LParen) |> nibble.optional)
    use <- bool.guard(maybe == None, nibble.return(nibble.Break(state)))
    nibble.one_of([
      // TODO: should be staged_optional?
      staged_many(func_type_parser(state, update_if_else_type), stage, 1),
      staged_many(param_parser(state, append_if_else_params), stage, 2),
      staged_many(result_parser(state, append_if_else_results), stage, 3),
      staged_many(folded_expr_parser(state, append_if_else_args), stage, 4),
      // TODO: then is not truly optional
      staged_optional(then_parser(state), stage, 5),
      staged_optional(else_parser(state), stage, 6),
    ])
    |> nibble.map(nibble.Continue)
  })
  let block_type = block_type(state.type_, state.params, state.results)
  let if_ = If(label: state.label, block_type:)
  let else_ = case state.else_ {
    [] -> []
    expr -> [Else(None), ..expr]
  }
  nibble.return(
    list.flatten([state.args, [if_], state.then, else_, [End(None)]]),
  )
}

fn then_parser(state: IfElseState) -> nibble.Parser(IfElseState, Token, a) {
  use _ <- nibble.do(nibble.token(Keyword("then")))
  use instr <- nibble.do(nibble.many(instr_parser()))
  use _ <- nibble.do(nibble.token(RParen))
  nibble.return(IfElseState(..state, then: list.flatten(instr)))
}

fn else_parser(state: IfElseState) -> nibble.Parser(IfElseState, Token, a) {
  use _ <- nibble.do(nibble.token(Keyword("else")))
  use instr <- nibble.do(nibble.many(instr_parser()))
  use _ <- nibble.do(nibble.token(RParen))
  nibble.return(IfElseState(..state, else_: list.flatten(instr)))
}

fn update_if_else_type(
  state: IfElseState,
  type_: Option(Index),
) -> IfElseState {
  IfElseState(..state, type_:)
}

fn append_if_else_params(
  state: IfElseState,
  params: List(Param),
) -> IfElseState {
  IfElseState(..state, params: list.append(state.params, params))
}

fn append_if_else_results(
  state: IfElseState,
  results: List(ValueType),
) -> IfElseState {
  IfElseState(..state, results: list.append(state.results, results))
}

fn append_if_else_args(
  state: IfElseState,
  args: List(Instruction),
) -> IfElseState {
  IfElseState(..state, args: list.append(state.args, args))
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
  use func <- nibble.do({
    use #(stage, func) <- nibble.loop(#(0, Func(None, None, [], [], [], [])))
    use maybe <- nibble.do(nibble.token(LParen) |> nibble.optional)
    use <- bool.guard(maybe == None, nibble.return(nibble.Break(func)))
    use maybe_ctx <- nibble.do(
      nibble.one_of([
        staged_many(func_type_parser(func, update_func_type), stage, 1),
        staged_many(param_parser(func, append_func_params), stage, 2),
        staged_many(result_parser(func, append_func_results), stage, 3),
        staged_many(folded_expr_parser(func, append_func_expr), stage, 4),
      ])
      |> nibble.optional,
    )
    case maybe_ctx {
      Some(#(4, func)) -> nibble.Break(func)
      Some(ctx) -> nibble.Continue(ctx)
      None -> nibble.Break(func)
    }
    |> nibble.return
  })
  nibble.return(#(block_type(func.type_, func.params, func.results), func.expr))
}

fn block_type(
  type_: Option(Index),
  params: List(Param),
  results: List(ValueType),
) -> BlockType {
  case type_, params, results {
    None, [], [] -> BlockEmpty
    None, [], [result] -> BlockResult(result)
    _, _, _ -> BlockTypeUse(type_:, params:, results:)
  }
}

/// Parses instructions at a basic level. Independent of folded or flat,
/// so never consumes a closing RParen.
fn basic_instr_parser() -> nibble.Parser(Instruction, Token, a) {
  use name <- nibble.do(keyword())
  let r = nibble.return
  case name {
    // parametric instructions
    "nop" -> r(Nop)
    "unreachable" -> r(Unreachable)
    "drop" -> r(Drop)
    // NOTE: further parsing of result types is handled by the caller
    "select" -> r(Select([]))
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
    "br_table" -> {
      use labels <- nibble.do(nibble.many1(index_parser()))
      let assert [default, ..labels] = list.reverse(labels)
      let labels = list.reverse(labels)
      nibble.return(BranchTable(labels, default))
    }
    "br_on_null" -> one_arg_parser(index_parser(), BranchOnNull)
    "br_on_cast" ->
      three_arg_parser(
        index_parser(),
        ref_type_parser(),
        ref_type_parser(),
        BranchOnCast,
      )
    "br_on_cast_fail" ->
      three_arg_parser(
        index_parser(),
        ref_type_parser(),
        ref_type_parser(),
        BranchOnCastFail,
      )
    "call" -> one_arg_parser(index_parser(), Call)
    "call_ref" -> one_arg_parser(index_parser(), CallRef)
    "call_indirect" -> {
      // NOTE: parsing the type use is up to the caller
      use table_index <- nibble.do(index_parser() |> nibble.optional)
      let table_index = option.unwrap(table_index, IndexInt("0"))
      nibble.return(
        CallIndirect(table_index:, type_: None, params: [], results: []),
      )
    }
    "return" -> r(Return)
    "return_call" -> one_arg_parser(index_parser(), ReturnCall)
    "return_call_ref" -> one_arg_parser(index_parser(), ReturnCallRef)
    "return_call_indirect" -> {
      // NOTE: parsing the type use is up to the caller
      use table_index <- nibble.do(index_parser() |> nibble.optional)
      let table_index = option.unwrap(table_index, IndexInt("0"))
      nibble.return(
        ReturnCallIndirect(table_index:, type_: None, params: [], results: []),
      )
    }
    // "throw" -> todo
    // "throw_ref" -> todo
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
      nibble.one_of([
        two_arg_parser(index_parser(), index_parser(), TableCopy),
        r(TableCopy(IndexInt("0"), IndexInt("0"))),
      ])
    "table.init" -> {
      use idx1 <- nibble.do(index_parser())
      use idx2 <- nibble.do(index_parser() |> nibble.optional)
      case idx2 {
        Some(idx2) -> TableInit(idx1, idx2)
        None -> TableInit(IndexInt("0"), idx1)
      }
      |> nibble.return
    }
    "elem.drop" ->
      one_arg_parser_optional(index_parser(), IndexInt("0"), ElemDrop)
    // TODO: memory instructions
    // TODO: reference instructions
    // numeric instructions
    "i32.const" -> one_arg_parser(int_parser(), I32Const)
    "i64.const" -> one_arg_parser(int_parser(), I64Const)
    "f32.const" -> one_arg_parser(float_parser(), F32Const)
    "f64.const" -> one_arg_parser(float_parser(), F64Const)
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
    "i32.extend8_s" -> r(I32Extend8S)
    "i32.extend16_s" -> r(I32Extend16S)
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
    "i32.shr_u" -> r(I32ShRU)
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
    "i64.extend8_s" -> r(I64Extend8S)
    "i64.extend16_s" -> r(I64Extend16S)
    "i64.extend32_s" -> r(I64Extend32S)
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
    "i64.shr_u" -> r(I64ShRU)
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

fn three_arg_parser(arg1, arg2, arg3, constr) {
  use arg1 <- nibble.do(arg1)
  use arg2 <- nibble.do(arg2)
  use arg3 <- nibble.do(arg3)
  nibble.return(constr(arg1, arg2, arg3))
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

fn func_type_parser(
  ctx: context,
  update: fn(context, Option(Index)) -> context,
) -> nibble.Parser(context, Token, a) {
  use _ <- nibble.do(nibble.token(Keyword("type")))
  use type_use <- nibble.do(index_parser())
  use _ <- nibble.do(nibble.token(RParen))
  nibble.return(update(ctx, Some(type_use)))
}

fn int_parser() -> nibble.Parser(String, Token, a) {
  use t <- nibble.do(nibble.any())
  case t {
    Integer(str) -> nibble.return(str)
    _ -> nibble.fail("expected an integer")
  }
}

fn float_parser() -> nibble.Parser(String, Token, a) {
  use t <- nibble.do(nibble.any())
  case t {
    Float(str) -> nibble.return(str)
    _ -> nibble.fail("expected a float")
  }
}

fn param_parser(ctx: context, update: fn(context, List(Param)) -> context) {
  param_like_parser("param", ctx, update)
}

fn result_parser(
  ctx: context,
  update: fn(context, List(ValueType)) -> context,
) {
  result_like_parser("result", ctx, update)
}

fn result_like_parser(
  name: String,
  ctx: context,
  update: fn(context, List(ValueType)) -> context,
) {
  use _ <- nibble.do(nibble.token(Keyword(name)))
  use types <- nibble.do(nibble.many(value_type_parser()))
  use _ <- nibble.do(nibble.token(RParen))
  nibble.return(update(ctx, types))
}

fn local_parser(
  ctx: context,
  update: fn(context, List(Param)) -> context,
) -> nibble.Parser(context, Token, a) {
  param_like_parser("local", ctx, update)
}

fn param_like_parser(
  name: String,
  ctx: context,
  update: fn(context, List(Param)) -> context,
) -> nibble.Parser(context, Token, a) {
  use _ <- nibble.do(nibble.token(Keyword(name)))
  use maybe_id <- nibble.do(nibble.optional(id_parser()))
  use params <- nibble.do(case maybe_id {
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
  })
  nibble.return(update(ctx, params))
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

fn ref_type_parser() -> nibble.Parser(RefType, Token, a) {
  nibble.one_of([
    keyword_value_parser("anyref", Nullable(HeapAny)),
    keyword_value_parser("eqref", Nullable(HeapEq)),
    keyword_value_parser("i31ref", Nullable(HeapI31)),
    keyword_value_parser("structref", Nullable(HeapStruct)),
    keyword_value_parser("arrayref", Nullable(HeapArray)),
    keyword_value_parser("nullref", Nullable(HeapNone)),
    keyword_value_parser("funcref", Nullable(HeapFunc)),
    keyword_value_parser("nullfuncref", Nullable(HeapNoFunc)),
    keyword_value_parser("externref", Nullable(HeapExtern)),
    keyword_value_parser("nullexternref", Nullable(HeapNoExtern)),
    full_ref_type_parser(),
  ])
}

fn full_ref_type_parser() {
  use _ <- nibble.do(nibble.token(LParen))
  use _ <- nibble.do(nibble.token(Keyword("ref")))
  use null <- nibble.do(nibble.token(Keyword("null")) |> nibble.optional)
  use ht <- nibble.do(heap_type_parser())
  case null {
    Some(_) -> Nullable(ht)
    None -> NonNull(ht)
  }
  |> nibble.return
}

fn heap_type_parser() -> nibble.Parser(HeapType, Token, a) {
  nibble.one_of([
    keyword_value_parser("any", HeapAny),
    keyword_value_parser("eq", HeapEq),
    keyword_value_parser("i31", HeapI31),
    keyword_value_parser("struct", HeapStruct),
    keyword_value_parser("array", HeapArray),
    keyword_value_parser("none", HeapNone),
    keyword_value_parser("func", HeapFunc),
    keyword_value_parser("nofunc", HeapNoFunc),
    keyword_value_parser("extern", HeapExtern),
    keyword_value_parser("noextern", HeapNoExtern),
    index_parser() |> nibble.map(HeapIndex),
  ])
}

fn keyword_value_parser(
  keyword: String,
  value: v,
) -> nibble.Parser(v, Token, a) {
  use _ <- nibble.do(nibble.token(Keyword(keyword)))
  nibble.return(value)
}
