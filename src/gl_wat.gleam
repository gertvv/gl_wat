import gleam/bit_array
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/regexp.{type Regexp}
import gleam/string
import nibble
import nibble/lexer

pub type Token {
  LParen
  RParen
  Keyword(String)
  LKeyword(String)
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
    // TODO: block comments
    // TODO: annotations
    // TODO: the spec's definition of whitespace
    lexer.whitespace(Nil) |> lexer.ignore(),
  ])
}

// lexes the content of a string
pub fn string_lexer() {
  let assert Ok(ucp_re) =
    regexp.from_string("\\\\u{0x[0-9A-Fa-f](_?[0-9A-Fa-f])*}")
  let assert Ok(hex_re) = regexp.from_string("\\\\([0-9A-Fa-f]{2})")
  lexer.simple([
    lexer.token("\\t", <<"\t">>),
    lexer.token("\\n", <<"\n">>),
    lexer.token("\\r", <<"\r">>),
    lexer.token("\\\"", <<"\"">>),
    lexer.token("\\'", <<"'">>),
    lexer.token("\\\\", <<"\\">>),
    lexer.custom(fn(mode, lexeme, _) {
      case regexp.check(ucp_re, lexeme) {
        True -> {
          let assert Ok(bytes) =
            lexeme
            |> string.drop_start(5)
            |> string.drop_end(1)
            |> bit_array.base16_decode
          lexer.Keep(bytes, mode)
        }
        False -> lexer.NoMatch
      }
    }),
    lexer.custom(fn(mode, lexeme, _) {
      case regexp.check(hex_re, lexeme) {
        True -> {
          let assert Ok(bytes) =
            lexeme |> string.drop_start(1) |> bit_array.base16_decode
          lexer.Keep(bytes, mode)
        }
        False -> lexer.NoMatch
      }
    }),
    lexer.custom(fn(mode, lexeme, _) {
      case lexeme {
        "\\" <> _ -> lexer.NoMatch
        str -> lexer.Keep(bit_array.from_string(str), mode)
      }
    }),
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

pub fn simplify(tokens: List(lexer.Token(Token))) -> List(lexer.Token(Token)) {
  list.fold(tokens, [], fn(acc, token) {
    case acc, token {
      [lexer.Token(_, _, LParen), ..rest],
        lexer.Token(span, lexeme, Keyword(keyword))
      -> [lexer.Token(span, lexeme, LKeyword(keyword)), ..rest]
      _, _ -> [token, ..acc]
    }
  })
  |> list.reverse
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

pub type Function {
  Function(
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

pub type RecType {
  RecType(List(TypeDef))
}

pub type TypeDef {
  TypeDef(id: Option(String), sub: SubType, super: List(Index), def: CompType)
}

pub type SubType {
  Open
  Final
}

pub type CompType {
  Struct(fields: List(Field))
  Array(FieldType)
  Func(params: List(Param), result: List(ValueType))
}

pub type Field {
  Field(id: Option(String), field_type: FieldType)
}

pub type FieldType {
  ValueField(mutability: Mutability, value_type: ValueType)
  PackedField(mutability: Mutability, packed_type: PackedType)
}

pub type PackedType {
  I8
  I16
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
  //
  // Aggregate instructions
  RefI31
  I31GetS
  I31GetU
  StructNew(type_index: Index)
  StructNewDefault(type_index: Index)
  StructGet(type_index: Index, field_index: Index)
  StructGetS(type_index: Index, field_index: Index)
  StructGetU(type_index: Index, field_index: Index)
  StructSet(type_index: Index, field_index: Index)
  ArrayNew(type_index: Index)
  ArrayNewDefault(type_index: Index)
  ArrayNewFixed(type_index: Index, size: String)
  ArrayNewData(type_index: Index, data_index: Index)
  ArrayNewElem(type_index: Index, element_index: Index)
  ArrayGet(type_index: Index)
  ArrayGetS(type_index: Index)
  ArrayGetU(type_index: Index)
  ArraySet(type_index: Index)
  ArrayLen
  ArrayFill(type_index: Index)
  ArrayCopy(type_index: Index, source_type_index: Index)
  ArrayInitData(type_index: Index, data_index: Index)
  ArrayInitElem(type_index: Index, element_index: Index)
  AnyConvertExtern
  ExternConvertAny
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
  I32Load(memory_index: Index, offset: String, align: String)
  I64Load(memory_index: Index, offset: String, align: String)
  F32Load(memory_index: Index, offset: String, align: String)
  F64Load(memory_index: Index, offset: String, align: String)
  I32Load8S(memory_index: Index, offset: String, align: String)
  I32Load8U(memory_index: Index, offset: String, align: String)
  I32Load16S(memory_index: Index, offset: String, align: String)
  I32Load16U(memory_index: Index, offset: String, align: String)
  I64Load8S(memory_index: Index, offset: String, align: String)
  I64Load8U(memory_index: Index, offset: String, align: String)
  I64Load16S(memory_index: Index, offset: String, align: String)
  I64Load16U(memory_index: Index, offset: String, align: String)
  I64Load32S(memory_index: Index, offset: String, align: String)
  I64Load32U(memory_index: Index, offset: String, align: String)
  // TODO: vector load instructions
  I32Store(memory_index: Index, offset: String, align: String)
  I64Store(memory_index: Index, offset: String, align: String)
  F32Store(memory_index: Index, offset: String, align: String)
  F64Store(memory_index: Index, offset: String, align: String)
  I32Store8(memory_index: Index, offset: String, align: String)
  I32Store16(memory_index: Index, offset: String, align: String)
  I64Store8(memory_index: Index, offset: String, align: String)
  I64Store16(memory_index: Index, offset: String, align: String)
  I64Store32(memory_index: Index, offset: String, align: String)
  // TODO: vector store instructions
  MemorySize(memory_index: Index)
  MemoryGrow(memory_index: Index)
  MemoryFill(memory_index: Index)
  MemoryCopy(memory_index: Index)
  MemoryInit(memory_index: Index, data_index: Index)
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

type TypeUse {
  TypeUse(type_: Option(Index), params: List(Param), results: List(ValueType))
}

pub type Global {
  Global(
    id: Option(String),
    mutability: Mutability,
    type_: ValueType,
    expr: List(Instruction),
  )
}

pub type Mutability {
  Mutable
  Immutable
}

pub type Memory {
  Memory(id: Option(String), address_type: AddressType, limits: Limits)
}

pub type Table {
  Table(
    id: Option(String),
    address_type: AddressType,
    limits: Limits,
    ref_type: RefType,
    expr: List(Instruction),
  )
}

pub type AddressType {
  Address32
  Address64
}

pub type Limits {
  Limits(min: String, max: Option(String))
}

pub type Data {
  PassiveData(id: Option(String), data: BitArray)
  ActiveData(
    id: Option(String),
    data: BitArray,
    memory_index: Index,
    offset: List(Instruction),
  )
}

pub type Element {
  PassiveElement(
    id: Option(String),
    ref_type: RefType,
    elems: List(List(Instruction)),
  )
  ActiveElement(
    id: Option(String),
    ref_type: RefType,
    elems: List(List(Instruction)),
    table_index: Index,
    offset: List(Instruction),
  )
  DeclarativeElement(
    id: Option(String),
    ref_type: RefType,
    elems: List(List(Instruction)),
  )
}

pub type Start {
  Start(function_index: Index)
}

pub type Import {
  ImportGlobal(
    module_name: String,
    import_name: String,
    id: Option(String),
    mutability: Mutability,
    type_: ValueType,
  )
  ImportMemory(
    module_name: String,
    import_name: String,
    id: Option(String),
    address_type: AddressType,
    limits: Limits,
  )
  ImportTable(
    module_name: String,
    import_name: String,
    id: Option(String),
    address_type: AddressType,
    limits: Limits,
    ref_type: RefType,
  )
  ImportFunction(
    module_name: String,
    import_name: String,
    id: Option(String),
    type_: Option(Index),
    params: List(Param),
    results: List(ValueType),
  )
}

pub type Export {
  ExportGlobal(name: String, global_index: Index)
  ExportMemory(name: String, memory_index: Index)
  ExportTable(name: String, table_index: Index)
  ExportFunction(name: String, function_index: Index)
}

pub type Decl {
  TypeDecl(RecType)
  ImportDecl(Import)
  GlobalDecl(Global)
  MemoryDecl(Memory)
  TableDecl(Table)
  FunctionDecl(Function)
  DataDecl(Data)
  ElementDecl(Element)
  StartDecl(Start)
  ExportDecl(Export)
}

pub type Module {
  Module(
    id: Option(String),
    types: List(RecType),
    imports: List(Import),
    globals: List(Global),
    memories: List(Memory),
    tables: List(Table),
    functions: List(Function),
    datas: List(Data),
    elements: List(Element),
    start: Option(Start),
    exports: List(Export),
  )
}

type ParseContext {
  ParseContext(
    types: Int,
    globals: Int,
    memories: Int,
    tables: Int,
    functions: Int,
    datas: Int,
    elements: Int,
    start: Bool,
    // TODO: prevent imports after non-import tags, globals, memories, tables, or functions
    non_import_defs: Bool,
  )
}

pub fn module_parser() {
  use mod <- nibble.do(nibble.optional(nibble.token(LKeyword("module"))))
  use #(id, decl) <- nibble.do(case mod {
    Some(_) -> {
      use id <- nibble.do(nibble.optional(id_parser()))
      use decl <- nibble.do(module_declarations_parser())
      use _ <- nibble.do(nibble.token(RParen))
      nibble.return(#(id, decl))
    }
    None -> {
      use decl <- nibble.do(module_declarations_parser())
      nibble.return(#(None, decl))
    }
  })
  let mod =
    list.fold(
      decl,
      Module(id, [], [], [], [], [], [], [], [], None, []),
      fn(mod, decl) {
        case decl {
          TypeDecl(rec_type) -> Module(..mod, types: [rec_type, ..mod.types])
          ImportDecl(import_) ->
            Module(..mod, imports: [import_, ..mod.imports])
          GlobalDecl(global) -> Module(..mod, globals: [global, ..mod.globals])
          MemoryDecl(mem) -> Module(..mod, memories: [mem, ..mod.memories])
          TableDecl(table) -> Module(..mod, tables: [table, ..mod.tables])
          FunctionDecl(func) ->
            Module(..mod, functions: [func, ..mod.functions])
          DataDecl(data) -> Module(..mod, datas: [data, ..mod.datas])
          ElementDecl(element) ->
            Module(..mod, elements: [element, ..mod.elements])
          StartDecl(start) -> Module(..mod, start: Some(start))
          ExportDecl(export) -> Module(..mod, exports: [export, ..mod.exports])
        }
      },
    )
  nibble.return(mod)
}

/// Returns all module declarations in reverse order
fn module_declarations_parser() -> nibble.Parser(List(Decl), Token, b) {
  let ctx = ParseContext(0, 0, 0, 0, 0, 0, 0, False, False)
  use #(ctx, decl) <- nibble.loop(#(ctx, []))
  use res <- nibble.do(decl_parser(ctx) |> nibble.optional)
  case res {
    Some(#(ctx, new_decl)) ->
      nibble.Continue(#(ctx, rev_prepend(new_decl, decl)))
    None -> nibble.Break(decl)
  }
  |> nibble.return
}

/// Prepends the items to the list in reverse order: rev_prepend([2, 1], [3, 4]) -> [1, 2, 3, 4]
fn rev_prepend(items: List(Decl), list: List(Decl)) -> List(Decl) {
  case items {
    [] -> list
    [head, ..rest] -> rev_prepend(rest, [head, ..list])
  }
}

fn decl_parser(
  ctx: ParseContext,
) -> nibble.Parser(#(ParseContext, List(Decl)), Token, b) {
  nibble.one_of([
    rec_type_parser() |> nibble.map(fn(rec) { #(ctx, [TypeDecl(rec)]) }),
    // import
    global_parser() |> nibble.map(fn(glob) { #(ctx, glob) }),
    memory_parser(ctx),
    table_parser(ctx),
    func_parser(ctx),
    data_parser(ctx),
    // element
  // start
  // export
  ])
}

fn table_parser(
  ctx: ParseContext,
) -> nibble.Parser(#(ParseContext, List(Decl)), Token, b) {
  use _ <- nibble.do(nibble.token(LKeyword("table")))
  use id <- nibble.do(id_parser() |> nibble.optional)
  use exports <- nibble.do(nibble.many(
    inline_export_parser()
    |> nibble.map(fn(name) {
      ExportDecl(ExportTable(name, IndexInt(int.to_string(ctx.tables))))
    }),
  ))
  nibble.one_of([
    {
      // inline import case
      use #(module_name, import_name) <- nibble.do(inline_import_parser())
      use address_type <- nibble.do(address_type_parser())
      use limits <- nibble.do(limits_parser())
      use ref_type <- nibble.do(ref_type_parser())
      use _ <- nibble.do(nibble.token(RParen))
      nibble.return(
        #(ParseContext(..ctx, tables: ctx.tables + 1), [
          ImportDecl(ImportTable(
            module_name:,
            import_name:,
            id:,
            address_type:,
            limits:,
            ref_type:,
          )),
          ..exports
        ]),
      )
    },
    {
      use address_type <- nibble.do(address_type_parser())
      nibble.one_of([
        {
          // inline elem case
          use ref_type <- nibble.do(ref_type_parser())
          use _ <- nibble.do(nibble.token(LKeyword("elem")))
          use #(elem_ref_type, elems) <- nibble.do(parse_elem_list())
          let n = list.length(elems) |> int.to_string
          let limits = Limits(n, Some(n))
          use expr <- nibble.do(case ref_type {
            Nullable(ht) -> nibble.return([RefNull(ht)])
            _ ->
              nibble.fail("elem initialization expression for non-null reftype")
          })
          let offset = [
            case address_type {
              Address32 -> I32Const("0")
              Address64 -> I64Const("0")
            },
          ]
          use _ <- nibble.do(nibble.token(RParen))
          use _ <- nibble.do(nibble.token(RParen))
          nibble.return(
            #(
              ParseContext(
                ..ctx,
                tables: ctx.tables + 1,
                elements: ctx.elements + 1,
              ),
              [
                TableDecl(Table(id:, address_type:, limits:, ref_type:, expr:)),
                ElementDecl(ActiveElement(
                  id: None,
                  ref_type: elem_ref_type,
                  elems:,
                  table_index: IndexInt(int.to_string(ctx.tables)),
                  offset:,
                )),
                ..exports
              ],
            ),
          )
        },
        {
          // plain table case
          use limits <- nibble.do(limits_parser())
          use ref_type <- nibble.do(ref_type_parser())
          use expr <- nibble.do(expr_parser())
          use expr <- nibble.do(case expr, ref_type {
            [], Nullable(ht) -> nibble.return([RefNull(ht)])
            [], _ ->
              nibble.fail(
                "missing initialization expression for non-null reftype",
              )
            _, _ -> nibble.return(expr)
          })
          use _ <- nibble.do(nibble.token(RParen))
          nibble.return(
            #(ParseContext(..ctx, tables: ctx.tables + 1), [
              TableDecl(Table(id:, address_type:, limits:, ref_type:, expr:)),
              ..exports
            ]),
          )
        },
      ])
    },
  ])
}

fn parse_elem_list() -> nibble.Parser(
  #(RefType, List(List(Instruction))),
  Token,
  b,
) {
  nibble.one_of([
    {
      // explicit ref_type and items case
      use ref_type <- nibble.do(ref_type_parser())
      use items <- nibble.do(
        nibble.many({
          nibble.one_of([
            {
              use _ <- nibble.do(nibble.token(LKeyword("item")))
              use expr <- nibble.do(expr_parser())
              use _ <- nibble.do(nibble.token(RParen))
              nibble.return(expr)
            },
            // single folded instruction instead of (item expr)
            folded_instr_parser(),
          ])
        }),
      )
      nibble.return(#(ref_type, items))
    },
    {
      // function indices case
      use _ <- nibble.do(nibble.token(Keyword("func")))
      use indices <- nibble.do(nibble.many(index_parser()))
      let items = list.map(indices, fn(index) { [RefFunc(index)] })
      nibble.return(#(NonNull(HeapFunc), items))
    },
  ])
}

fn expr_parser() -> nibble.Parser(List(Instruction), Token, b) {
  nibble.many(instr_parser()) |> nibble.map(list.flatten)
}

fn data_parser(ctx: ParseContext) {
  use _ <- nibble.do(nibble.token(LKeyword("data")))
  use id <- nibble.do(id_parser() |> nibble.optional)
  nibble.one_of([
    {
      // passive data
      use data <- nibble.do(string_bytes_parser())
      use _ <- nibble.do(nibble.token(RParen))
      nibble.return(
        #(ParseContext(..ctx, datas: ctx.datas + 1), [
          DataDecl(PassiveData(id:, data:)),
        ]),
      )
    },
    {
      // active data
      use memory_index <- nibble.do(nibble.or(
        memory_use_parser(),
        // abbreviation: 𝜖 -> (memory 0)
        IndexInt("0"),
      ))
      use offset <- nibble.do(
        nibble.one_of([
          {
            use _ <- nibble.do(nibble.token(LKeyword("offset")))
            use expr <- nibble.do(expr_parser())
            use _ <- nibble.do(nibble.token(RParen))
            nibble.return(expr)
          },
          // abbreviation: single folded instruction instead of (offset expr)
          folded_instr_parser(),
        ]),
      )
      use data <- nibble.do(string_bytes_parser())
      use _ <- nibble.do(nibble.token(RParen))
      nibble.return(
        #(ParseContext(..ctx, datas: ctx.datas + 1), [
          DataDecl(ActiveData(id:, data:, memory_index:, offset:)),
        ]),
      )
    },
  ])
}

fn memory_use_parser() -> nibble.Parser(Index, Token, b) {
  use _ <- nibble.do(nibble.token(LKeyword("memory")))
  use index <- nibble.do(index_parser())
  use _ <- nibble.do(nibble.token(RParen))
  nibble.return(index)
}

const page_size = 65_536

fn memory_parser(ctx: ParseContext) {
  use _ <- nibble.do(nibble.token(LKeyword("memory")))
  use id <- nibble.do(id_parser() |> nibble.optional)
  use exports <- nibble.do(nibble.many(
    inline_export_parser()
    |> nibble.map(fn(name) {
      ExportDecl(ExportMemory(name, IndexInt(int.to_string(ctx.memories))))
    }),
  ))
  nibble.one_of([
    {
      // inline import case
      use #(module_name, import_name) <- nibble.do(inline_import_parser())
      use address_type <- nibble.do(address_type_parser())
      use limits <- nibble.do(limits_parser())
      nibble.return(
        #(ParseContext(..ctx, memories: ctx.memories + 1), [
          ImportDecl(ImportMemory(
            module_name:,
            import_name:,
            id:,
            address_type:,
            limits:,
          )),
          ..exports
        ]),
      )
    },
    {
      use address_type <- nibble.do(address_type_parser())
      nibble.one_of([
        {
          // inline data case
          use _ <- nibble.do(nibble.token(LKeyword("data")))
          use data <- nibble.do(string_bytes_parser())
          let n = bit_array.byte_size(data)
          let n =
            { { n + page_size - 1 } / page_size }
            |> int.to_string
          use _ <- nibble.do(nibble.token(RParen))
          use _ <- nibble.do(nibble.token(RParen))
          nibble.return(
            #(
              ParseContext(
                ..ctx,
                memories: ctx.memories + 1,
                datas: ctx.datas + 1,
              ),
              [
                MemoryDecl(Memory(
                  id:,
                  address_type:,
                  limits: Limits(n, Some(n)),
                )),
                DataDecl(
                  ActiveData(
                    id: None,
                    data:,
                    memory_index: IndexInt(ctx.memories |> int.to_string),
                    offset: [
                      I32Const("0"),
                    ],
                  ),
                ),
                ..exports
              ],
            ),
          )
        },
        {
          // plain memory case
          use limits <- nibble.do(limits_parser())
          use _ <- nibble.do(nibble.token(RParen))
          nibble.return(
            #(ParseContext(..ctx, memories: ctx.memories + 1), [
              MemoryDecl(Memory(id:, address_type:, limits:)),
            ]),
          )
        },
      ])
    },
  ])
}

fn limits_parser() -> nibble.Parser(Limits, Token, b) {
  use min <- nibble.do(int_parser())
  use max <- nibble.do(int_parser() |> nibble.optional)
  nibble.return(Limits(min:, max:))
}

fn inline_export_parser() -> nibble.Parser(String, Token, b) {
  use _ <- nibble.do(nibble.token(LKeyword("export")))
  use name <- nibble.do(string_parser())
  use _ <- nibble.do(nibble.token(RParen))
  nibble.return(name)
}

fn inline_import_parser() -> nibble.Parser(#(String, String), Token, b) {
  use _ <- nibble.do(nibble.token(LKeyword("import")))
  use mod_name <- nibble.do(string_parser())
  use imp_name <- nibble.do(string_parser())
  use _ <- nibble.do(nibble.token(RParen))
  nibble.return(#(mod_name, imp_name))
}

fn string_parser() -> nibble.Parser(String, Token, b) {
  use bytes <- nibble.do(string_bytes_parser())
  case bit_array.to_string(bytes) {
    Ok(str) -> nibble.return(str)
    Error(_) -> nibble.fail("Invalid UTF-8")
  }
}

fn string_bytes_parser() -> nibble.Parser(BitArray, Token, b) {
  use token <- nibble.do(nibble.any())
  use str <- nibble.do(case token {
    String(str) -> nibble.return(str)
    _ -> nibble.fail("Expected a String token")
  })
  let bytes = decode_string(str)
  nibble.return(bytes)
}

fn decode_string(str: String) -> BitArray {
  let assert Ok(data) =
    str
    |> string.drop_start(1)
    |> string.drop_end(1)
    |> lexer.run(string_lexer())
  list.map(data, fn(t) { t.value })
  |> bit_array.concat
}

fn address_type_parser() -> nibble.Parser(AddressType, Token, b) {
  nibble.or(
    nibble.one_of([
      nibble.token(Keyword("i32")) |> nibble.replace(Address32),
      nibble.token(Keyword("i64")) |> nibble.replace(Address64),
    ]),
    Address32,
  )
}

fn global_parser() -> nibble.Parser(List(Decl), Token, b) {
  use _ <- nibble.do(nibble.token(LKeyword("global")))
  use id <- nibble.do(id_parser() |> nibble.optional)
  use #(mutability, type_) <- nibble.do(global_type_parser())
  use expr <- nibble.do(expr_parser())
  use _ <- nibble.do(nibble.token(RParen))
  nibble.return([GlobalDecl(Global(id:, mutability:, type_:, expr:))])
}

fn global_type_parser() -> nibble.Parser(#(Mutability, ValueType), Token, b) {
  nibble.one_of([
    {
      use _ <- nibble.do(nibble.token(LKeyword("mut")))
      use type_ <- nibble.do(value_type_parser())
      use _ <- nibble.do(nibble.token(RParen))
      nibble.return(#(Mutable, type_))
    },
    {
      use type_ <- nibble.do(value_type_parser())
      nibble.return(#(Immutable, type_))
    },
  ])
}

pub fn rec_type_parser() {
  nibble.one_of([
    {
      use _ <- nibble.do(nibble.token(LKeyword("rec")))
      use typedefs <- nibble.do(nibble.many(type_def_parser()))
      use _ <- nibble.do(nibble.token(RParen))
      nibble.return(typedefs)
    },
    type_def_parser() |> nibble.map(fn(t) { [t] }),
  ])
  |> nibble.map(RecType)
}

fn type_def_parser() -> nibble.Parser(TypeDef, Token, b) {
  use _ <- nibble.do(nibble.token(LKeyword("type")))
  use id <- nibble.do(id_parser() |> nibble.optional)
  use #(sub, super, def) <- nibble.do(sub_type_parser())
  use _ <- nibble.do(nibble.token(RParen))
  nibble.return(TypeDef(id:, sub:, super:, def:))
}

fn sub_type_parser() -> nibble.Parser(
  #(SubType, List(Index), CompType),
  Token,
  b,
) {
  nibble.one_of([
    {
      use _ <- nibble.do(nibble.token(LKeyword("sub")))
      use sub <- nibble.do(nibble.or(
        nibble.token(Keyword("final"))
          |> nibble.replace(Final),
        Open,
      ))
      use super <- nibble.do(nibble.many(index_parser()))
      use def <- nibble.do(comp_type_parser())
      nibble.return(#(sub, super, def))
    },
    {
      use def <- nibble.do(comp_type_parser())
      nibble.return(#(Final, [], def))
    },
  ])
}

fn comp_type_parser() -> nibble.Parser(CompType, Token, b) {
  nibble.one_of([
    def_struct_parser(),
    def_array_parser(),
    def_func_parser(),
  ])
}

fn def_struct_parser() -> nibble.Parser(CompType, Token, b) {
  use _ <- nibble.do(nibble.token(LKeyword("struct")))
  use fields <- nibble.do(nibble.many(field_parser()))
  use _ <- nibble.do(nibble.token(RParen))
  nibble.return(Struct(fields))
}

fn def_array_parser() -> nibble.Parser(CompType, Token, b) {
  use _ <- nibble.do(nibble.token(LKeyword("array")))
  use ft <- nibble.do(field_type_parser())
  use _ <- nibble.do(nibble.token(RParen))
  nibble.return(Array(ft))
}

fn def_func_parser() -> nibble.Parser(CompType, Token, b) {
  use _ <- nibble.do(nibble.token(LKeyword("func")))
  use params <- nibble.do(
    nibble.many(param_parser()) |> nibble.map(list.flatten),
  )
  use results <- nibble.do(
    nibble.many(result_parser()) |> nibble.map(list.flatten),
  )
  use _ <- nibble.do(nibble.token(RParen))
  nibble.return(Func(params, results))
}

fn field_parser() -> nibble.Parser(Field, Token, b) {
  // TODO: abbreviation for anonymous fields (multiple in one)
  use _ <- nibble.do(nibble.token(LKeyword("field")))
  use id <- nibble.do(id_parser() |> nibble.optional)
  use ft <- nibble.do(field_type_parser())
  use _ <- nibble.do(nibble.token(RParen))
  nibble.return(Field(id, ft))
}

fn field_type_parser() -> nibble.Parser(FieldType, Token, b) {
  nibble.one_of([
    {
      use _ <- nibble.do(nibble.token(LKeyword("mut")))
      use st <- nibble.do(storage_type_parser())
      use _ <- nibble.do(nibble.token(RParen))
      nibble.return(case st {
        ValueField(mutability: _, value_type:) ->
          ValueField(Mutable, value_type)
        PackedField(mutability: _, packed_type:) ->
          PackedField(Mutable, packed_type)
      })
    },
    storage_type_parser(),
  ])
}

fn storage_type_parser() -> nibble.Parser(FieldType, Token, b) {
  nibble.one_of([
    nibble.token(Keyword("i8")) |> nibble.replace(PackedField(Immutable, I8)),
    nibble.token(Keyword("i16")) |> nibble.replace(PackedField(Immutable, I16)),
    value_type_parser() |> nibble.map(ValueField(Immutable, _)),
  ])
}

fn func_parser(ctx: ParseContext) {
  use _ <- nibble.do(nibble.token(LKeyword("func")))
  use id <- nibble.do(id_parser() |> nibble.optional)
  use exports <- nibble.do(nibble.many(
    inline_export_parser()
    |> nibble.map(fn(name) {
      ExportDecl(ExportFunction(name, IndexInt(int.to_string(ctx.functions))))
    }),
  ))
  nibble.one_of([
    {
      // import case
      use #(module_name, import_name) <- nibble.do(inline_import_parser())
      use TypeUse(type_, params, results) <- nibble.do(type_use_parser())
      use _ <- nibble.do(nibble.token(RParen))
      nibble.return(
        #(ParseContext(..ctx, functions: ctx.functions + 1), [
          ImportDecl(ImportFunction(
            module_name:,
            import_name:,
            id:,
            type_:,
            params:,
            results:,
          )),
          ..exports
        ]),
      )
    },
    {
      // inline function
      use TypeUse(type_, params, results) <- nibble.do(type_use_parser())
      use locals <- nibble.do(
        nibble.many(local_parser()) |> nibble.map(list.flatten),
      )
      use expr <- nibble.do(expr_parser())
      use _ <- nibble.do(nibble.token(RParen))
      nibble.return(
        #(ParseContext(..ctx, functions: ctx.functions + 1), [
          FunctionDecl(Function(id:, type_:, params:, results:, locals:, expr:)),
          ..exports
        ]),
      )
    },
  ])
}

fn instr_parser() -> nibble.Parser(List(Instruction), Token, a) {
  basic_instr_parser(AnyInstr)
}

fn folded_instr_parser() -> nibble.Parser(List(Instruction), Token, a) {
  basic_instr_parser(FoldedInstr)
}

type InstructionKind {
  AnyInstr
  FoldedInstr
  FlatInstr
}

fn basic_instr_parser(
  kind: InstructionKind,
) -> nibble.Parser(List(Instruction), Token, a) {
  use kw <- nibble.do(
    nibble.take_if("instr", fn(token) {
      case token, kind {
        Keyword(..), AnyInstr | Keyword(..), FlatInstr -> True
        LKeyword(..), AnyInstr | LKeyword(..), FoldedInstr -> True
        _, _ -> False
      }
    }),
  )
  let name = case kw {
    Keyword(name) | LKeyword(name) -> name
    _ -> panic
  }
  let r = fn(instr) { nibble.return([instr]) }
  use expr <- nibble.do(case name {
    // parametric instructions
    "nop" -> r(Nop)
    "unreachable" -> r(Unreachable)
    "drop" -> r(Drop)
    "select" -> {
      use results <- nibble.do(nibble.many(result_parser()))
      r(Select(list.flatten(results)))
    }
    // control instructions
    "block" -> {
      use label <- nibble.do(nibble.optional(id_parser()))
      use block_type <- nibble.do(block_type_parser())
      r(Block(label, block_type))
    }
    "loop" -> {
      use label <- nibble.do(nibble.optional(id_parser()))
      use block_type <- nibble.do(block_type_parser())
      r(Loop(label, block_type))
    }
    "if" -> {
      use label <- nibble.do(nibble.optional(id_parser()))
      case kw {
        LKeyword(..) -> folded_if_parser(label)
        _ -> {
          use block_type <- nibble.do(block_type_parser())
          r(If(label, block_type))
        }
      }
    }
    "else" -> one_arg_parser(nibble.optional(id_parser()), Else)
    "end" -> one_arg_parser(nibble.optional(id_parser()), End)
    // TODO: try/catch
    "br" -> one_arg_parser(index_parser(), Branch)
    "br_if" -> one_arg_parser(index_parser(), BranchIf)
    "br_table" -> {
      use labels <- nibble.do(nibble.many1(index_parser()))
      let assert [default, ..labels] = list.reverse(labels)
      let labels = list.reverse(labels)
      r(BranchTable(labels, default))
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
    "call_indirect" -> call_indirect_parser(CallIndirect)
    "return" -> r(Return)
    "return_call" -> one_arg_parser(index_parser(), ReturnCall)
    "return_call_ref" -> one_arg_parser(index_parser(), ReturnCallRef)
    "return_call_indirect" -> call_indirect_parser(ReturnCallIndirect)
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
        Some(idx2) -> r(TableInit(idx1, idx2))
        None -> r(TableInit(IndexInt("0"), idx1))
      }
    }
    "elem.drop" ->
      one_arg_parser_optional(index_parser(), IndexInt("0"), ElemDrop)
    // memory instructions
    "i32.load" -> mem_instr_parser(I32Load)
    "i64.load" -> mem_instr_parser(I64Load)
    "f32.load" -> mem_instr_parser(F32Load)
    "f64.load" -> mem_instr_parser(F64Load)
    "i32.load8_s" -> mem_instr_parser(I32Load8S)
    "i32.load8_u" -> mem_instr_parser(I32Load8U)
    "i32.load16_s" -> mem_instr_parser(I32Load16S)
    "i32.load16_u" -> mem_instr_parser(I32Load16U)
    "i64.load8_s" -> mem_instr_parser(I64Load8S)
    "i64.load8_u" -> mem_instr_parser(I64Load8U)
    "i64.load16_s" -> mem_instr_parser(I64Load16S)
    "i64.load16_u" -> mem_instr_parser(I64Load16U)
    "i64.load32_s" -> mem_instr_parser(I64Load32S)
    "i64.load32_u" -> mem_instr_parser(I64Load32U)
    // TODO: vector load instructions
    "i32.store" -> mem_instr_parser(I32Store)
    "i64.store" -> mem_instr_parser(I64Store)
    "f32.store" -> mem_instr_parser(F32Store)
    "f64.store" -> mem_instr_parser(F64Store)
    "i32.store8" -> mem_instr_parser(I32Store8)
    "i32.store16" -> mem_instr_parser(I32Store16)
    "i64.store8" -> mem_instr_parser(I64Store8)
    "i64.store16" -> mem_instr_parser(I64Store16)
    "i64.store32" -> mem_instr_parser(I64Store32)
    // TODO: vector store instructions
    "memory.size" ->
      one_arg_parser_optional(index_parser(), IndexInt("0"), MemorySize)
    "memory.grow" ->
      one_arg_parser_optional(index_parser(), IndexInt("0"), MemoryGrow)
    "memory.fill" ->
      one_arg_parser_optional(index_parser(), IndexInt("0"), MemoryFill)
    "memory.copy" -> todo as "two optional index args"
    "memory.init" -> todo as "memory index optional, data index required"
    "data.drop" -> one_arg_parser(index_parser(), DataDrop)
    // reference instructions
    "ref.null" -> one_arg_parser(heap_type_parser(), RefNull)
    "ref.func" -> one_arg_parser(index_parser(), RefFunc)
    "ref.is_null" -> r(RefIsNull)
    "ref.as_non_null" -> r(RefAsNonNull)
    "ref.eq" -> r(RefEq)
    "ref.test" -> one_arg_parser(ref_type_parser(), RefTest)
    "ref.cast" -> one_arg_parser(ref_type_parser(), RefCast)
    // aggregate instructions
    "ref.i31" -> r(RefI31)
    "i31.get_s" -> r(I31GetS)
    "i31.get_u" -> r(I31GetU)
    "struct.new" -> one_arg_parser(index_parser(), StructNew)
    "struct.new_default" -> one_arg_parser(index_parser(), StructNewDefault)
    "struct.get" -> two_arg_parser(index_parser(), index_parser(), StructGet)
    "struct.get_s" -> two_arg_parser(index_parser(), index_parser(), StructGetS)
    "struct.get_u" -> two_arg_parser(index_parser(), index_parser(), StructGetU)
    "struct.set" -> two_arg_parser(index_parser(), index_parser(), StructSet)
    "array.new" -> one_arg_parser(index_parser(), ArrayNew)
    "array.new_default" -> one_arg_parser(index_parser(), ArrayNewDefault)
    "array.new_fixed" ->
      two_arg_parser(index_parser(), int_parser(), ArrayNewFixed)
    "array.new_data" ->
      two_arg_parser(index_parser(), index_parser(), ArrayNewData)
    "array.new_elem" ->
      two_arg_parser(index_parser(), index_parser(), ArrayNewElem)
    "array.get" -> one_arg_parser(index_parser(), ArrayGet)
    "array.get_s" -> one_arg_parser(index_parser(), ArrayGetS)
    "array.get_u" -> one_arg_parser(index_parser(), ArrayGetU)
    "array.set" -> one_arg_parser(index_parser(), ArraySet)
    "array.len" -> r(ArrayLen)
    "array.fill" -> one_arg_parser(index_parser(), ArrayFill)
    "array.copy" -> two_arg_parser(index_parser(), index_parser(), ArrayCopy)
    "array.init_data" ->
      two_arg_parser(index_parser(), index_parser(), ArrayInitData)
    "array.init_elem" ->
      two_arg_parser(index_parser(), index_parser(), ArrayInitElem)
    "any.convert_extern" -> r(AnyConvertExtern)
    "extern.convert_any" -> r(ExternConvertAny)
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
    _ -> nibble.fail("Unrecognized instruction: " <> name)
  })
  case kw {
    LKeyword("if") -> {
      // folded if has args before the then block
      use _ <- nibble.do(nibble.token(RParen))
      nibble.return(expr)
    }
    LKeyword("block") | LKeyword("loop") -> {
      // folded blocks have nested instructions, not arguments
      use instrs <- nibble.do(expr_parser())
      use _ <- nibble.do(nibble.token(RParen))
      nibble.return(list.flatten([expr, instrs, [End(None)]]))
    }
    LKeyword(_) -> {
      // plain folded instructions have their arguments prepended
      use args <- nibble.do(expr_parser())
      use _ <- nibble.do(nibble.token(RParen))
      nibble.return(list.append(args, expr))
    }
    _ -> nibble.return(expr)
  }
}

fn call_indirect_parser(constr) {
  use table_index <- nibble.do(index_parser() |> nibble.optional)
  let table_index = option.unwrap(table_index, IndexInt("0"))
  use type_ <- nibble.do(nibble.optional(func_type_parser()))
  use params <- nibble.do(nibble.many(result_like_parser("param")))
  use results <- nibble.do(nibble.many(result_parser()))
  nibble.return([
    constr(table_index, type_, list.flatten(params), list.flatten(results)),
  ])
}

fn folded_if_parser(
  label: Option(String),
) -> nibble.Parser(List(Instruction), Token, a) {
  use block_type <- nibble.do(block_type_parser())
  use args <- nibble.do(nibble.many(folded_instr_parser()))
  use then <- nibble.do(then_parser())
  use else_ <- nibble.do(nibble.optional(else_parser()))
  let if_ = If(label: label, block_type:)
  let else_ = case else_ {
    None -> []
    Some(expr) -> [Else(None), ..expr]
  }
  nibble.return(
    list.flatten([list.flatten(args), [if_], then, else_, [End(None)]]),
  )
}

fn then_parser() {
  use _ <- nibble.do(nibble.token(LKeyword("then")))
  use expr <- nibble.do(expr_parser())
  use _ <- nibble.do(nibble.token(RParen))
  nibble.return(expr)
}

fn else_parser() {
  use _ <- nibble.do(nibble.token(LKeyword("else")))
  use expr <- nibble.do(expr_parser())
  use _ <- nibble.do(nibble.token(RParen))
  nibble.return(expr)
}

fn block_type_parser() {
  use t <- nibble.do(type_use_parser())
  nibble.return(block_type(t))
}

fn type_use_parser() {
  use type_ <- nibble.do(nibble.optional(func_type_parser()))
  use params <- nibble.do(nibble.many(param_parser()))
  use results <- nibble.do(nibble.many(result_parser()))
  nibble.return(TypeUse(type_, list.flatten(params), list.flatten(results)))
}

fn block_type(t: TypeUse) -> BlockType {
  case t {
    TypeUse(None, [], []) -> BlockEmpty
    TypeUse(None, [], [result]) -> BlockResult(result)
    TypeUse(type_:, params:, results:) ->
      BlockTypeUse(type_:, params:, results:)
  }
}

fn mem_instr_parser(
  constr: fn(Index, String, String) -> Instruction,
) -> nibble.Parser(List(Instruction), Token, a) {
  use idx <- nibble.do(nibble.optional(index_parser()))
  let idx = option.unwrap(idx, IndexInt("0"))
  use offset <- nibble.do(
    nibble.optional({
      use t <- nibble.do(nibble.any())
      case t {
        Keyword("offset=" <> offset) -> nibble.return(offset)
        _ -> nibble.fail("expected offset=")
      }
    }),
  )
  let offset = option.unwrap(offset, "0")
  use align <- nibble.do(
    nibble.optional({
      use t <- nibble.do(nibble.any())
      case t {
        Keyword("align=" <> align) -> nibble.return(align)
        _ -> nibble.fail("expected align=")
      }
    }),
  )
  let align = option.unwrap(align, "0")
  nibble.return([constr(idx, offset, align)])
}

fn one_arg_parser(arg1, constr) {
  use arg1 <- nibble.do(arg1)
  nibble.return([constr(arg1)])
}

fn two_arg_parser(arg1, arg2, constr) {
  use arg1 <- nibble.do(arg1)
  use arg2 <- nibble.do(arg2)
  nibble.return([constr(arg1, arg2)])
}

fn three_arg_parser(arg1, arg2, arg3, constr) {
  use arg1 <- nibble.do(arg1)
  use arg2 <- nibble.do(arg2)
  use arg3 <- nibble.do(arg3)
  nibble.return([constr(arg1, arg2, arg3)])
}

fn one_arg_parser_optional(arg1, def1, constr) {
  use arg1 <- nibble.do(nibble.optional(arg1))
  let arg1 = option.unwrap(arg1, def1)
  nibble.return([constr(arg1)])
}

fn index_parser() {
  nibble.one_of([
    int_parser() |> nibble.map(IndexInt),
    id_parser() |> nibble.map(IndexId),
  ])
}

fn func_type_parser() -> nibble.Parser(Index, Token, a) {
  use _ <- nibble.do(nibble.token(LKeyword("type")))
  use type_use <- nibble.do(index_parser())
  use _ <- nibble.do(nibble.token(RParen))
  nibble.return(type_use)
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

fn param_parser() {
  param_like_parser("param")
}

fn result_parser() {
  result_like_parser("result")
}

fn local_parser() {
  param_like_parser("local")
}

fn param_like_parser(name: String) {
  use _ <- nibble.do(nibble.token(LKeyword(name)))
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

fn result_like_parser(name: String) {
  use _ <- nibble.do(nibble.token(LKeyword(name)))
  use types <- nibble.do(nibble.many(value_type_parser()))
  use _ <- nibble.do(nibble.token(RParen))
  nibble.return(types)
}

fn id_parser() {
  use t <- nibble.do(nibble.any())
  case t {
    Id(id) -> nibble.return(id)
    _ -> nibble.fail("expected an id")
  }
}

fn value_type_parser() {
  nibble.one_of([
    nibble.token(Keyword("i32")) |> nibble.replace(I32),
    nibble.token(Keyword("i64")) |> nibble.replace(I64),
    nibble.token(Keyword("f32")) |> nibble.replace(F32),
    nibble.token(Keyword("f64")) |> nibble.replace(F64),
    ref_type_parser() |> nibble.map(Ref),
  ])
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
  use _ <- nibble.do(nibble.token(LKeyword("ref")))
  use null <- nibble.do(nibble.token(Keyword("null")) |> nibble.optional)
  use ht <- nibble.do(heap_type_parser())
  use _ <- nibble.do(nibble.token(RParen))
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
