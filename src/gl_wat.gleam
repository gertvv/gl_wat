import bigi.{type BigInt}
import gleam/bit_array
import gleam/bool
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/pair
import gleam/regexp.{type Regexp}
import gleam/result
import gleam/string
import ieee_float.{type IEEEFloat}
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

const idchar = "[0-9A-Za-z!#$%&'*+-\\./:<=>?@\\\\^_`|~]"

const strpat =
  "\"([^\\x00-\\x19\\x7f\"\\\\]|\\\\t|\\\\n|\\\\r|\\\\\"|\\\\'|\\\\|\\\\u{0x[0-9A-Fa-f](_?[0-9A-Fa-f])*}|\\\\([0-9A-Fa-f]{2}))*\""

const hexnum = "[0-9A-Fa-f](_?[0-9A-Fa-f])*"

const decnum = "[0-9](_?[0-9])*"

pub fn lexer() {
  let assert Ok(int_re) =
    regexp.from_string("^[+-]?((0x" <> hexnum <> ")|(" <> decnum <> "))$")
  let assert Ok(decfloat_re) =
    regexp.from_string(
      "^[+-]?"
      <> decnum
      <> "(\\.("
      <> decnum
      <> ")?)?([Ee][+-]?"
      <> decnum
      <> ")?$",
    )
  let assert Ok(hexfloat_re) =
    regexp.from_string(
      "^[+-]?0x"
      <> hexnum
      <> "(\\.("
      <> hexnum
      <> ")?)?([Pp][+-]?"
      <> decnum
      <> ")?$",
    )
  let assert Ok(nanfloat_re) =
    regexp.from_string("^[+-]?(inf|nan(:0x" <> hexnum <> ")?)")
  let assert Ok(str_re) = regexp.from_string(strpat)
  let assert Ok(keyword_re) = regexp.from_string("^[a-z]" <> idchar <> "*$")
  let assert Ok(id_re) = regexp.from_string("^[$]" <> idchar <> "+$")
  let assert Ok(name_id_re) = regexp.from_string("^[$]" <> strpat)

  lexer.simple([
    separated_token(id_re, Id),
    separated_token(name_id_re, Id),
    separated_token(int_re, Integer),
    separated_token(decfloat_re, Float),
    separated_token(hexfloat_re, Float),
    // TODO: worry about overlap between inf/nan and keywords?
    separated_token(nanfloat_re, Float),
    separated_token(keyword_re, Keyword),
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

/// Parse a WebAssembly Text float representation
fn parse_float(str: String) -> Result(IEEEFloat, String) {
  case str {
    "+inf" | "inf" -> Ok(ieee_float.positive_infinity())
    "-inf" -> Ok(ieee_float.negative_infinity())
    "+nan" | "nan" -> Ok(ieee_float.nan())
    "-nan" -> Error("Negative NaN not supported")
    "+nan:" <> _val | "-nan:" <> _val | "nan:" <> _val ->
      Error("Non-canonical NaN not supported")
    "+0x" <> _ | "0x" <> _ | "-0x" <> _ ->
      parse_hexfloat(string.replace(str, "_", ""))
    str -> {
      let f = ieee_float.parse(string.replace(str, "_", ""))
      case ieee_float.is_nan(f) {
        True -> Error("Invalid float value: " <> str)
        False -> Ok(f)
      }
    }
  }
}

/// Parse a C99-style hexadecimal float: `0x0.8b2 -> 2.0`.
pub fn parse_hexfloat(str: String) -> Result(IEEEFloat, String) {
  let err = fn() { "Invalid hexfloat: " <> str }
  // detect the sign (0 = positive, 1 = negative)
  use #(sign, hex) <- result.try(case str {
    "+0x" <> hex | "0x" <> hex -> Ok(#(0, hex))
    "-0x" <> hex -> Ok(#(1, hex))
    _ -> Error(err())
  })
  // separate mantissa and exponent
  let #(mant, exp) =
    string.split_once(hex, "p")
    |> result.or(string.split_once(hex, "P"))
    |> result.unwrap(#(hex, "0"))
  // separate whole and fractional parts
  let #(whole, frac) =
    string.split_once(mant, ".") |> result.unwrap(#(mant, ""))
  let frac_digits = string.length(frac)
  let mant = whole <> frac
  // pad to an even number of digits for base16_decode, which expects whole bytes
  let mant = case int.is_odd(string.length(mant)) {
    True -> "0" <> mant
    False -> mant
  }
  // convert hex to binary
  use mant <- result.try(
    bit_array.base16_decode(mant) |> result.replace_error(err()),
  )
  // the exponent is a decimal integer
  use exp <- result.try(int.parse(exp) |> result.replace_error(err()))
  // count fractional digits into the exponent (each hex digit is 4 bits)
  let exp = exp - 4 * frac_digits
  let msb = msb(mant)
  // early return for zero
  use <- bool.guard(
    msb == -1,
    Ok(ieee_float.from_bytes_64_be(<<sign:size(1), 0:size(63)>>)),
  )
  // the actual exponent increases by the number of significant bits
  let exp = exp + msb
  use <- bool.guard(exp < -1074, Error("Too small: " <> str))
  use <- bool.guard(exp > 1023, Error("Too large: " <> str))
  // drop the leading zeros
  let lead = bit_array.bit_size(mant) - { msb + 1 }
  let assert <<_:size(lead), mant:bits>> = mant
  // add bias to exponent
  let exp = exp + 1023
  let #(mant, exp) = case exp <= 0 {
    True -> {
      // subnormal - add the needed number of leading zeros
      let mant = <<0:size(-exp), mant:bits>>
      #(mant, 0)
    }
    False -> {
      // normal - drop the leading one
      let assert <<1:size(1), mant:bits>> = mant
      #(mant, exp)
    }
  }
  // add trailing zeros if needed and drop least significant bits beyond 52
  let tail = 52 - bit_array.bit_size(mant)
  let mant = case tail >= 0 {
    True -> {
      <<mant:bits, 0:size(tail)>>
    }
    False -> {
      let assert <<mant:bits-size(52), _:bits>> = mant
      mant
    }
  }
  Ok(ieee_float.from_bytes_64_be(<<sign:size(1), exp:size(11), mant:bits>>))
}

/// Returns the index of the most significant bit, counted from the end
fn msb(mant: BitArray) -> Int {
  find_msb(mant, bit_array.bit_size(mant) - 1)
}

fn find_msb(mant: BitArray, idx: Int) -> Int {
  case mant {
    <<1:size(1), _:bits>> -> idx
    <<_:size(1), r:bits>> -> find_msb(r, idx - 1)
    _ -> -1
  }
}

/// Lexes the content of a string
fn string_lexer() {
  let assert Ok(ucp_re) =
    regexp.from_string("\\\\u{x[0-9A-Fa-f](_?[0-9A-Fa-f])*}")
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

/// Simplifies the output of the lexer for use by the parser.
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

pub type ValueType(i) {
  I32
  I64
  F32
  F64
  Ref(RefType(i))
}

pub type HeapType(i) {
  HeapAny
  HeapEq
  HeapI31
  HeapStruct
  HeapArray
  HeapNone
  HeapFunc
  HeapNoFunc
  HeapExn
  HeapNoExn
  HeapExtern
  HeapNoExtern
  HeapIndex(i)
}

pub type RefType(i) {
  NonNull(heap_type: HeapType(i))
  Nullable(heap_type: HeapType(i))
}

pub type Param(i) {
  Param(id: Option(String), typ: ValueType(i))
}

pub type Function(i) {
  Function(
    id: Option(String),
    type_: i,
    params: List(Param(i)),
    results: List(ValueType(i)),
    locals: List(Param(i)),
    expr: List(Instruction(i)),
  )
}

pub type BlockType(i) {
  BlockTypeUse(type_: i, params: List(Param(i)), results: List(ValueType(i)))
  BlockResult(ValueType(i))
  BlockEmpty
}

pub type RecType(i) {
  RecType(defs: List(TypeDef(i)))
}

pub type TypeDef(i) {
  TypeDef(id: Option(String), sub: SubType, super: List(i), def: CompType(i))
}

pub type SubType {
  Open
  Final
}

pub type CompType(i) {
  Struct(fields: List(Field(i)))
  Array(item_type: FieldType(i))
  Func(params: List(Param(i)), results: List(ValueType(i)))
}

pub fn func_type(
  params: List(Param(i)),
  results: List(ValueType(i)),
) -> RecType(i) {
  RecType([TypeDef(None, Final, [], Func(params, results))])
}

pub type Field(i) {
  Field(id: Option(String), field_type: FieldType(i))
}

pub type FieldType(i) {
  ValueField(mutability: Mutability, value_type: ValueType(i))
  PackedField(mutability: Mutability, packed_type: PackedType)
}

pub type PackedType {
  I8
  I16
}

pub type Instruction(i) {
  // Control instructions
  Unreachable
  Nop
  Block(label: Option(String), block_type: BlockType(i))
  Loop(label: Option(String), block_type: BlockType(i))
  If(label: Option(String), block_type: BlockType(i))
  Else(label: Option(String))
  End(label: Option(String))
  // TODO: exceptions
  Branch(label_index: i)
  BranchIf(label_index: i)
  BranchTable(label_indices: List(i), default_label_index: i)
  Return
  Call(function_index: i)
  CallIndirect(
    table_index: i,
    type_: i,
    params: List(ValueType(i)),
    results: List(ValueType(i)),
  )
  ReturnCall(function_index: i)
  ReturnCallIndirect(
    table_index: i,
    type_: i,
    params: List(ValueType(i)),
    results: List(ValueType(i)),
  )
  CallRef(type_index: i)
  ReturnCallRef(type_index: i)
  BranchOnNull(label_index: i)
  BranchOnNonNull(label_index: i)
  BranchOnCast(
    label_index: i,
    from_ref_type: RefType(i),
    to_ref_type: RefType(i),
  )
  BranchOnCastFail(
    label_index: i,
    from_ref_type: RefType(i),
    to_ref_type: RefType(i),
  )
  //
  // Reference instructions
  RefNull(heap_type: HeapType(i))
  RefFunc(function_index: i)
  RefIsNull
  RefAsNonNull
  RefEq
  RefTest(ref_type: RefType(i))
  RefCast(ref_type: RefType(i))
  //
  // Aggregate instructions
  RefI31
  I31GetS
  I31GetU
  StructNew(type_index: i)
  StructNewDefault(type_index: i)
  StructGet(type_index: i, field_index: i)
  StructGetS(type_index: i, field_index: i)
  StructGetU(type_index: i, field_index: i)
  StructSet(type_index: i, field_index: i)
  ArrayNew(type_index: i)
  ArrayNewDefault(type_index: i)
  ArrayNewFixed(type_index: i, size: Int)
  ArrayNewData(type_index: i, data_index: i)
  ArrayNewElem(type_index: i, element_index: i)
  ArrayGet(type_index: i)
  ArrayGetS(type_index: i)
  ArrayGetU(type_index: i)
  ArraySet(type_index: i)
  ArrayLen
  ArrayFill(type_index: i)
  ArrayCopy(type_index: i, source_type_index: i)
  ArrayInitData(type_index: i, data_index: i)
  ArrayInitElem(type_index: i, element_index: i)
  AnyConvertExtern
  ExternConvertAny
  //
  // Parametric instructions
  Drop
  Select(List(ValueType(i)))
  //
  // Variable instructions
  LocalGet(local_index: i)
  LocalSet(local_index: i)
  LocalTee(local_index: i)
  GlobalGet(global_index: i)
  GlobalSet(global_index: i)
  //
  // Table instructions
  TableGet(table_index: i)
  TableSet(table_index: i)
  TableSize(table_index: i)
  TableGrow(table_index: i)
  TableFill(table_index: i)
  TableCopy(table_index: i, source_table_index: i)
  TableInit(table_index: i, element_index: i)
  ElemDrop(element_index: i)
  //
  // Memory instructions
  I32Load(memory_index: i, offset: String, align: String)
  I64Load(memory_index: i, offset: String, align: String)
  F32Load(memory_index: i, offset: String, align: String)
  F64Load(memory_index: i, offset: String, align: String)
  I32Load8S(memory_index: i, offset: String, align: String)
  I32Load8U(memory_index: i, offset: String, align: String)
  I32Load16S(memory_index: i, offset: String, align: String)
  I32Load16U(memory_index: i, offset: String, align: String)
  I64Load8S(memory_index: i, offset: String, align: String)
  I64Load8U(memory_index: i, offset: String, align: String)
  I64Load16S(memory_index: i, offset: String, align: String)
  I64Load16U(memory_index: i, offset: String, align: String)
  I64Load32S(memory_index: i, offset: String, align: String)
  I64Load32U(memory_index: i, offset: String, align: String)
  // TODO: vector load instructions
  I32Store(memory_index: i, offset: String, align: String)
  I64Store(memory_index: i, offset: String, align: String)
  F32Store(memory_index: i, offset: String, align: String)
  F64Store(memory_index: i, offset: String, align: String)
  I32Store8(memory_index: i, offset: String, align: String)
  I32Store16(memory_index: i, offset: String, align: String)
  I64Store8(memory_index: i, offset: String, align: String)
  I64Store16(memory_index: i, offset: String, align: String)
  I64Store32(memory_index: i, offset: String, align: String)
  // TODO: vector store instructions
  MemorySize(memory_index: i)
  MemoryGrow(memory_index: i)
  MemoryFill(memory_index: i)
  MemoryCopy(memory_index: i)
  MemoryInit(memory_index: i, data_index: i)
  DataDrop(data_index: i)
  //
  // Numeric instructions
  I32Const(value: Int)
  I64Const(value: BigInt)
  F32Const(value: IEEEFloat)
  F64Const(value: IEEEFloat)
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

pub type ImplicitFunc {
  ImplicitFunc(params: List(ValueType(Index)), results: List(ValueType(Index)))
}

fn implicit_func(params: List(Param(Index)), results: List(ValueType(Index))) {
  ImplicitFunc(list.map(params, fn(param) { param.typ }), results)
}

pub type Index {
  IndexInt(Int)
  IndexId(String)
  IndexImplicit(ImplicitFunc)
}

type TypeUse(i) {
  TypeUse(type_: i, params: List(Param(i)), results: List(ValueType(i)))
}

pub type Tag(i) {
  Tag(
    id: Option(String),
    type_: i,
    params: List(Param(i)),
    results: List(ValueType(i)),
  )
}

pub type Global(i) {
  Global(
    id: Option(String),
    mutability: Mutability,
    value_type: ValueType(i),
    expr: List(Instruction(i)),
  )
}

pub type Mutability {
  Mutable
  Immutable
}

pub type Memory {
  Memory(id: Option(String), address_type: AddressType, limits: Limits)
}

pub type Table(i) {
  Table(
    id: Option(String),
    address_type: AddressType,
    limits: Limits,
    ref_type: RefType(i),
    expr: List(Instruction(i)),
  )
}

pub type AddressType {
  Address32
  Address64
}

pub type Limits {
  Limits(min: BigInt, max: Option(BigInt))
}

pub type Data(i) {
  PassiveData(id: Option(String), data: BitArray)
  ActiveData(
    id: Option(String),
    data: BitArray,
    memory_index: i,
    offset: List(Instruction(i)),
  )
}

pub type Element(i) {
  PassiveElement(
    id: Option(String),
    ref_type: RefType(i),
    elems: List(List(Instruction(i))),
  )
  ActiveElement(
    id: Option(String),
    ref_type: RefType(i),
    elems: List(List(Instruction(i))),
    table_index: i,
    offset: List(Instruction(i)),
  )
  DeclarativeElement(
    id: Option(String),
    ref_type: RefType(i),
    elems: List(List(Instruction(i))),
  )
}

pub type Start(i) {
  Start(function_index: i)
}

pub type Import(i) {
  ImportTag(
    module_name: String,
    import_name: String,
    id: Option(String),
    type_: i,
    params: List(Param(i)),
    results: List(ValueType(i)),
  )
  ImportGlobal(
    module_name: String,
    import_name: String,
    id: Option(String),
    mutability: Mutability,
    value_type: ValueType(i),
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
    ref_type: RefType(i),
  )
  ImportFunction(
    module_name: String,
    import_name: String,
    id: Option(String),
    type_: i,
    params: List(Param(i)),
    results: List(ValueType(i)),
  )
}

pub type Export(i) {
  ExportTag(name: String, tag_index: i)
  ExportGlobal(name: String, global_index: i)
  ExportMemory(name: String, memory_index: i)
  ExportTable(name: String, table_index: i)
  ExportFunction(name: String, function_index: i)
}

pub type Decl {
  TypeDecl(RecType(Index))
  ImportDecl(Import(Index))
  TagDecl(Tag(Index))
  GlobalDecl(Global(Index))
  MemoryDecl(Memory)
  TableDecl(Table(Index))
  FunctionDecl(Function(Index))
  DataDecl(Data(Index))
  ElementDecl(Element(Index))
  StartDecl(Start(Index))
  ExportDecl(Export(Index))
}

pub type Module(i) {
  Module(
    id: Option(String),
    types: List(RecType(i)),
    imports: List(Import(i)),
    tags: List(Tag(i)),
    globals: List(Global(i)),
    memories: List(Memory),
    tables: List(Table(i)),
    functions: List(Function(i)),
    datas: List(Data(i)),
    elements: List(Element(i)),
    start: Option(Start(i)),
    exports: List(Export(i)),
  )
}

pub opaque type IdContext {
  IdContext(
    types: Dict(String, Int),
    tags: Dict(String, Int),
    globals: Dict(String, Int),
    memories: Dict(String, Int),
    tables: Dict(String, Int),
    functions: Dict(String, Int),
    datas: Dict(String, Int),
    elements: Dict(String, Int),
    locals: Dict(String, Int),
    labels: List(String),
    fields: Dict(#(Int, String), Int),
    func_types: Dict(ImplicitFunc, Int),
  )
}

fn empty_id_context() {
  let e = dict.new()
  IdContext(e, e, e, e, e, e, e, e, e, [], dict.new(), dict.new())
}

type ParseContext {
  ParseContext(
    types: Int,
    tags: Int,
    globals: Int,
    memories: Int,
    tables: Int,
    functions: Int,
    datas: Int,
    elements: Int,
    start: Bool,
    non_import_defs: Bool,
    ids: IdContext,
    impl: List(ImplicitFunc),
  )
}

type ParseReturn {
  ParseReturn(impl: List(ImplicitFunc), decl: List(Decl))
}

type ExprReturn {
  ExprReturn(impl: List(ImplicitFunc), instr: List(Instruction(Index)))
}

fn ret_one(decl: Decl) {
  ParseReturn([], [decl])
}

fn ret_many(decl: List(Decl)) {
  ParseReturn([], decl)
}

pub fn module_parser() {
  use mod <- nibble.do(nibble.optional(nibble.token(LKeyword("module"))))
  use #(id, #(ctx, decl)) <- nibble.do(case mod {
    Some(_) -> {
      use id <- nibble.do(nibble.optional(id_parser()))
      use res <- nibble.do(module_declarations_parser())
      use _ <- nibble.do(nibble.token(RParen))
      nibble.return(#(id, res))
    }
    None -> {
      use res <- nibble.do(module_declarations_parser())
      nibble.return(#(None, res))
    }
  })
  let #(ctx, new_decl) = create_implicit_func_types(ctx)
  let decl = rev_prepend(new_decl, decl)
  let mod =
    list.fold(
      decl,
      Module(id, [], [], [], [], [], [], [], [], [], None, []),
      fn(mod, decl) {
        case decl {
          TypeDecl(rec_type) -> Module(..mod, types: [rec_type, ..mod.types])
          ImportDecl(import_) ->
            Module(..mod, imports: [import_, ..mod.imports])
          TagDecl(tag) -> Module(..mod, tags: [tag, ..mod.tags])
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
  nibble.return(#(mod, ctx))
}

fn create_implicit_func_types(ctx: ParseContext) -> #(IdContext, List(Decl)) {
  let #(_, func_types, decl) =
    list.fold(ctx.impl, #(ctx.types, ctx.ids.func_types, []), fn(acc, impl) {
      let #(idx, func_types, decl) = acc
      use <- bool.guard(dict.has_key(func_types, impl), acc)
      let t = func_type(list.map(impl.params, Param(None, _)), impl.results)
      let func_types = dict.insert(func_types, impl, idx)
      #(idx + 1, func_types, [TypeDecl(t), ..decl])
    })
  #(IdContext(..ctx.ids, func_types:), list.reverse(decl))
}

/// Returns all module declarations in reverse order
fn module_declarations_parser() {
  let ctx =
    ParseContext(0, 0, 0, 0, 0, 0, 0, 0, False, False, empty_id_context(), [])
  use #(ctx, decl) <- nibble.loop(#(ctx, []))
  use res <- nibble.do(decl_parser(ctx) |> nibble.optional)
  case res {
    Some(#(ctx, new_decl)) ->
      nibble.Continue(#(ctx, rev_prepend(new_decl, decl)))
    None -> nibble.Break(#(ctx, decl))
  }
  |> nibble.return
}

/// Prepends the items to the list in reverse order: rev_prepend([2, 1], [3, 4]) -> [1, 2, 3, 4]
fn rev_prepend(items: List(a), list: List(a)) -> List(a) {
  case items {
    [] -> list
    [head, ..rest] -> rev_prepend(rest, [head, ..list])
  }
}

fn decl_parser(
  ctx: ParseContext,
) -> nibble.Parser(#(ParseContext, List(Decl)), Token, b) {
  use ParseReturn(impl:, decl:) <- nibble.do(
    nibble.one_of([
      rec_type_parser(),
      import_parser(ctx),
      tag_parser(ctx),
      global_parser(ctx),
      memory_parser(ctx),
      table_parser(ctx),
      function_parser(ctx),
      data_parser(ctx),
      element_parser(ctx),
      start_parser(ctx),
      export_parser(),
    ]),
  )
  nibble.return(#(update_context(ctx, impl, decl), decl))
}

fn update_context(
  ctx: ParseContext,
  impl: List(ImplicitFunc),
  decls: List(Decl),
) -> ParseContext {
  let ctx = ParseContext(..ctx, impl: rev_prepend(impl, ctx.impl))
  list.fold(decls, ctx, fn(ctx, decl) {
    case decl {
      TypeDecl(rec_type) -> {
        // for func_types
        let new_type_idx = ctx.types
        let ctx =
          list.fold(rec_type.defs, ctx, fn(ctx, t) {
            let id = ctx.types
            let ctx = new_type(ctx, t.id)
            case t.def {
              Struct(fields:) -> new_field(ctx, id, fields)
              _ -> ctx
            }
          })
        // if a singular, final function type is registered, track this to later
        // resolve implicit references via function type uses.
        case rec_type {
          RecType(defs: [
            TypeDef(sub: Final, super: [], def: Func(params:, results:), ..),
          ]) -> {
            let params = list.map(params, fn(param) { param.typ })
            case dict.get(ctx.ids.func_types, ImplicitFunc(params, results)) {
              Ok(_) -> ctx
              Error(_) ->
                ParseContext(
                  ..ctx,
                  ids: IdContext(
                    ..ctx.ids,
                    func_types: dict.insert(
                      ctx.ids.func_types,
                      ImplicitFunc(params, results),
                      new_type_idx,
                    ),
                  ),
                )
            }
          }
          _ -> ctx
        }
      }
      ImportDecl(value) ->
        case value {
          ImportTag(id:, ..) -> new_tag(ctx, id)
          ImportGlobal(id:, ..) -> new_global(ctx, id)
          ImportMemory(id:, ..) -> new_memory(ctx, id)
          ImportTable(id:, ..) -> new_table(ctx, id)
          ImportFunction(id:, ..) -> new_function(ctx, id)
        }
      TagDecl(tag) -> new_tag(ctx, tag.id) |> non_import
      GlobalDecl(global) -> new_global(ctx, global.id) |> non_import
      MemoryDecl(memory) -> new_memory(ctx, memory.id) |> non_import
      TableDecl(table) -> new_table(ctx, table.id) |> non_import
      FunctionDecl(function) -> new_function(ctx, function.id) |> non_import
      DataDecl(data) -> new_data(ctx, data.id)
      ElementDecl(element) -> new_element(ctx, element.id)
      StartDecl(_) -> ParseContext(..ctx, start: True)
      ExportDecl(_) -> ctx
    }
  })
}

fn new_type(ctx: ParseContext, id: Option(String)) -> ParseContext {
  ParseContext(
    ..ctx,
    types: ctx.types + 1,
    ids: IdContext(..ctx.ids, types: assign_id(ctx.ids.types, id, ctx.types)),
  )
}

fn new_tag(ctx: ParseContext, id: Option(String)) -> ParseContext {
  ParseContext(
    ..ctx,
    tags: ctx.tags + 1,
    ids: IdContext(..ctx.ids, tags: assign_id(ctx.ids.tags, id, ctx.tags)),
  )
}

fn new_global(ctx: ParseContext, id: Option(String)) -> ParseContext {
  ParseContext(
    ..ctx,
    globals: ctx.globals + 1,
    ids: IdContext(
      ..ctx.ids,
      globals: assign_id(ctx.ids.globals, id, ctx.globals),
    ),
  )
}

fn new_memory(ctx: ParseContext, id: Option(String)) -> ParseContext {
  ParseContext(
    ..ctx,
    memories: ctx.memories + 1,
    ids: IdContext(
      ..ctx.ids,
      memories: assign_id(ctx.ids.memories, id, ctx.memories),
    ),
  )
}

fn new_table(ctx: ParseContext, id: Option(String)) -> ParseContext {
  ParseContext(
    ..ctx,
    tables: ctx.tables + 1,
    ids: IdContext(..ctx.ids, tables: assign_id(ctx.ids.tables, id, ctx.tables)),
  )
}

fn new_function(ctx: ParseContext, id: Option(String)) -> ParseContext {
  ParseContext(
    ..ctx,
    functions: ctx.functions + 1,
    ids: IdContext(
      ..ctx.ids,
      functions: assign_id(ctx.ids.functions, id, ctx.functions),
    ),
  )
}

fn new_data(ctx: ParseContext, id: Option(String)) -> ParseContext {
  ParseContext(
    ..ctx,
    datas: ctx.datas + 1,
    ids: IdContext(..ctx.ids, datas: assign_id(ctx.ids.datas, id, ctx.datas)),
  )
}

fn new_element(ctx: ParseContext, id: Option(String)) -> ParseContext {
  ParseContext(
    ..ctx,
    elements: ctx.elements + 1,
    ids: IdContext(
      ..ctx.ids,
      elements: assign_id(ctx.ids.elements, id, ctx.elements),
    ),
  )
}

fn new_field(
  ctx: ParseContext,
  type_idx: Int,
  fields: List(Field(Index)),
) -> ParseContext {
  list.index_map(fields, fn(field, idx) { #(idx, field) })
  |> list.fold(ctx, fn(ctx, field) {
    let #(field_idx, field) = field
    case field.id {
      Some(id) ->
        ParseContext(
          ..ctx,
          ids: IdContext(
            ..ctx.ids,
            fields: dict.insert(ctx.ids.fields, #(type_idx, id), field_idx),
          ),
        )
      None -> ctx
    }
  })
}

fn non_import(ctx: ParseContext) -> ParseContext {
  ParseContext(..ctx, non_import_defs: True)
}

fn assign_id(
  ids: Dict(String, Int),
  id: Option(String),
  idx: Int,
) -> Dict(String, Int) {
  case id {
    Some(id) -> dict.insert(ids, id, idx)
    None -> ids
  }
}

fn tag_parser(ctx: ParseContext) {
  use _ <- nibble.do(nibble.token(LKeyword("tag")))
  use id <- nibble.do(id_parser() |> nibble.optional)
  use exports <- nibble.do(inline_exports_parser(ctx.tables, ExportTable))
  nibble.one_of([
    {
      use #(module_name, import_name) <- nibble.do(inline_import_parser(ctx))
      use #(impl, TypeUse(type_:, params:, results:)) <- nibble.do(
        type_use_parser(),
      )
      use _ <- nibble.do(nibble.token(RParen))
      nibble.return(
        ParseReturn(impl, [
          ImportDecl(ImportTag(
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
      use #(impl, TypeUse(type_:, params:, results:)) <- nibble.do(
        type_use_parser(),
      )
      use _ <- nibble.do(nibble.token(RParen))
      nibble.return(
        ParseReturn(impl, [
          TagDecl(Tag(id:, type_:, params:, results:)),
          ..exports
        ]),
      )
    },
  ])
}

fn export_parser() {
  use _ <- nibble.do(nibble.token(LKeyword("export")))
  use export_name <- nibble.do(string_parser())
  use export <- nibble.do(nibble.one_of(
    [
      #("tag", ExportTag),
      #("global", ExportGlobal),
      #("memory", ExportMemory),
      #("table", ExportTable),
      #("func", ExportFunction),
    ]
    |> list.map(fn(p) {
      let #(name, constr) = p
      use _ <- nibble.do(nibble.token(LKeyword(name)))
      use index <- nibble.do(index_parser())
      use _ <- nibble.do(nibble.token(RParen))
      nibble.return(constr(export_name, index))
    }),
  ))
  use _ <- nibble.do(nibble.token(RParen))
  nibble.return(ExportDecl(export) |> ret_one)
}

fn import_parser(ctx: ParseContext) {
  use _ <- nibble.do(nibble.token(LKeyword("import")))
  use _ <- nibble.do(nibble.guard(
    !ctx.non_import_defs,
    "imports must occur before all non-import definitions",
  ))
  use module_name <- nibble.do(string_parser())
  use import_name <- nibble.do(string_parser())
  use import_ <- nibble.do(
    nibble.one_of([
      {
        use _ <- nibble.do(nibble.token(LKeyword("tag")))
        use id <- nibble.do(id_parser() |> nibble.optional)
        use #(impl, TypeUse(type_:, params:, results:)) <- nibble.do(
          type_use_parser(),
        )
        use _ <- nibble.do(nibble.token(RParen))
        nibble.return(
          ParseReturn(impl, [
            ImportDecl(ImportTag(
              module_name:,
              import_name:,
              id:,
              type_:,
              params:,
              results:,
            )),
          ]),
        )
      },
      {
        use _ <- nibble.do(nibble.token(LKeyword("global")))
        use id <- nibble.do(id_parser() |> nibble.optional)
        use #(mutability, value_type) <- nibble.do(global_type_parser())
        use _ <- nibble.do(nibble.token(RParen))
        nibble.return(
          ImportDecl(ImportGlobal(
            module_name:,
            import_name:,
            id:,
            mutability:,
            value_type:,
          ))
          |> ret_one,
        )
      },
      {
        use _ <- nibble.do(nibble.token(LKeyword("memory")))
        use id <- nibble.do(id_parser() |> nibble.optional)
        use #(address_type, limits) <- nibble.do(memory_type_parser())
        use _ <- nibble.do(nibble.token(RParen))
        nibble.return(
          ImportDecl(ImportMemory(
            module_name:,
            import_name:,
            id:,
            address_type:,
            limits:,
          ))
          |> ret_one,
        )
      },
      {
        use _ <- nibble.do(nibble.token(LKeyword("table")))
        use id <- nibble.do(id_parser() |> nibble.optional)
        use #(address_type, limits, ref_type) <- nibble.do(table_type_parser())
        use _ <- nibble.do(nibble.token(RParen))
        nibble.return(
          ImportDecl(ImportTable(
            module_name:,
            import_name:,
            id:,
            address_type:,
            limits:,
            ref_type:,
          ))
          |> ret_one,
        )
      },
      {
        use _ <- nibble.do(nibble.token(LKeyword("func")))
        use id <- nibble.do(id_parser() |> nibble.optional)
        use #(impl, TypeUse(type_:, params:, results:)) <- nibble.do(
          type_use_parser(),
        )
        use _ <- nibble.do(nibble.token(RParen))
        nibble.return(
          ParseReturn(impl, [
            ImportDecl(ImportFunction(
              module_name:,
              import_name:,
              id:,
              type_:,
              params:,
              results:,
            )),
          ]),
        )
      },
    ]),
  )
  use _ <- nibble.do(nibble.token(RParen))
  nibble.return(import_)
}

fn start_parser(ctx: ParseContext) {
  use _ <- nibble.do(nibble.token(LKeyword("start")))
  use function_index <- nibble.do(index_parser())
  use _ <- nibble.do(nibble.token(RParen))
  case ctx.start {
    True -> nibble.fail("at most one start function may occur in a module")
    False -> nibble.return(StartDecl(Start(function_index:)) |> ret_one)
  }
}

fn element_parser(ctx: ParseContext) {
  use _ <- nibble.do(nibble.token(LKeyword("elem")))
  use id <- nibble.do(id_parser() |> nibble.optional)
  nibble.one_of([
    {
      // declarative element
      use _ <- nibble.do(nibble.token(Keyword("declare")))
      use #(ref_type, impl, elems) <- nibble.do(elem_list_parser(False, ctx))
      use _ <- nibble.do(nibble.token(RParen))
      nibble.return(
        ParseReturn(impl, [
          ElementDecl(DeclarativeElement(id:, ref_type:, elems:)),
        ]),
      )
    },
    {
      // passive element
      use #(ref_type, impl, elems) <- nibble.do(elem_list_parser(False, ctx))
      use _ <- nibble.do(nibble.token(RParen))
      nibble.return(
        ParseReturn(impl, [
          ElementDecl(PassiveElement(id:, ref_type:, elems:)),
        ]),
      )
    },
    {
      // active element
      use table_index <- nibble.do(use_parser("table") |> nibble.optional)
      use ExprReturn(impl: impl1, instr: offset) <- nibble.do(offset_parser(ctx))
      // if table use is omitted, the func keyword can be omitted as well
      use #(ref_type, impl2, elems) <- nibble.do(elem_list_parser(
        option.is_none(table_index),
        ctx,
      ))
      use _ <- nibble.do(nibble.token(RParen))
      // abbreviation: 𝜖 -> (table 0)
      let table_index = option.unwrap(table_index, IndexInt(0))
      nibble.return(
        ParseReturn(list.append(impl1, impl2), [
          ElementDecl(ActiveElement(
            id:,
            ref_type:,
            elems:,
            table_index:,
            offset:,
          )),
        ]),
      )
    },
  ])
}

fn inline_exports_parser(new_index: Int, constr) {
  nibble.many(
    inline_export_parser()
    |> nibble.map(fn(name) { ExportDecl(constr(name, IndexInt(new_index))) }),
  )
}

fn table_parser(ctx: ParseContext) {
  use _ <- nibble.do(nibble.token(LKeyword("table")))
  use id <- nibble.do(id_parser() |> nibble.optional)
  use exports <- nibble.do(inline_exports_parser(ctx.tables, ExportTable))
  nibble.one_of([
    {
      // inline import case
      use #(module_name, import_name) <- nibble.do(inline_import_parser(ctx))
      use #(address_type, limits, ref_type) <- nibble.do(table_type_parser())
      use _ <- nibble.do(nibble.token(RParen))
      nibble.return(
        [
          ImportDecl(ImportTable(
            module_name:,
            import_name:,
            id:,
            address_type:,
            limits:,
            ref_type:,
          )),
          ..exports
        ]
        |> ret_many,
      )
    },
    {
      use address_type <- nibble.do(address_type_parser())
      nibble.one_of([
        {
          // inline elem case
          use ref_type <- nibble.do(ref_type_parser())
          use _ <- nibble.do(nibble.token(LKeyword("elem")))
          use #(elem_ref_type, impl, elems) <- nibble.do(elem_list_parser(
            False,
            ctx,
          ))
          let n = list.length(elems) |> bigi.from_int
          let limits = Limits(n, Some(n))
          use expr <- nibble.do(case ref_type {
            Nullable(ht) -> nibble.return([RefNull(ht)])
            _ ->
              nibble.fail("elem initialization expression for non-null reftype")
          })
          let offset = [
            zero_address(address_type),
          ]
          use _ <- nibble.do(nibble.token(RParen))
          use _ <- nibble.do(nibble.token(RParen))
          nibble.return(
            ParseReturn(impl, [
              TableDecl(Table(id:, address_type:, limits:, ref_type:, expr:)),
              ElementDecl(ActiveElement(
                id: None,
                ref_type: elem_ref_type,
                elems:,
                table_index: IndexInt(ctx.tables),
                offset:,
              )),
              ..exports
            ]),
          )
        },
        {
          // plain table case
          use limits <- nibble.do(limits_parser())
          use ref_type <- nibble.do(ref_type_parser())
          use ExprReturn(impl:, instr: expr) <- nibble.do(expr_parser(ctx))
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
            ParseReturn(impl, [
              TableDecl(Table(id:, address_type:, limits:, ref_type:, expr:)),
              ..exports
            ]),
          )
        },
      ])
    },
  ])
}

fn zero_address(address_type: AddressType) -> Instruction(i) {
  case address_type {
    Address32 -> I32Const(0)
    Address64 -> I64Const(bigi.zero())
  }
}

fn table_type_parser() -> nibble.Parser(
  #(AddressType, Limits, RefType(Index)),
  Token,
  b,
) {
  use address_type <- nibble.do(address_type_parser())
  use limits <- nibble.do(limits_parser())
  use ref_type <- nibble.do(ref_type_parser())
  nibble.return(#(address_type, limits, ref_type))
}

fn elem_list_parser(
  omit_func: Bool,
  ctx: ParseContext,
) -> nibble.Parser(
  #(RefType(Index), List(ImplicitFunc), List(List(Instruction(Index)))),
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
              use expr <- nibble.do(expr_parser(ctx))
              use _ <- nibble.do(nibble.token(RParen))
              nibble.return(expr)
            },
            // single folded instruction instead of (item expr)
            folded_instr_parser(ctx),
          ])
        }),
      )
      let impl = list.flat_map(items, fn(item) { item.impl })
      let items = list.map(items, fn(item) { item.instr })
      nibble.return(#(ref_type, impl, items))
    },
    {
      // function indices case
      // for backwards compatibility, if the table use is omitted in an active element section, the 'func' keyword can
      // be omitted as well
      use _ <- nibble.do(case omit_func {
        True ->
          nibble.token(Keyword("func"))
          |> nibble.optional
          |> nibble.map(option.unwrap(_, Nil))
        False -> nibble.token(Keyword("func"))
      })
      use indices <- nibble.do(nibble.many(index_parser()))
      let items = list.map(indices, fn(index) { [RefFunc(index)] })
      nibble.return(#(NonNull(HeapFunc), [], items))
    },
  ])
}

fn expr_parser(ctx: ParseContext) -> nibble.Parser(ExprReturn, Token, b) {
  nibble.many(instr_parser(ctx)) |> nibble.map(flatten_expr)
}

fn flatten_expr(vals: List(ExprReturn)) -> ExprReturn {
  ExprReturn(
    impl: list.flat_map(vals, fn(val) { val.impl }),
    instr: list.flat_map(vals, fn(val) { val.instr }),
  )
}

fn data_parser(ctx: ParseContext) {
  use _ <- nibble.do(nibble.token(LKeyword("data")))
  use id <- nibble.do(id_parser() |> nibble.optional)
  nibble.one_of([
    {
      // passive data
      use data <- nibble.do(string_bytes_parser())
      use _ <- nibble.do(nibble.token(RParen))
      nibble.return(DataDecl(PassiveData(id:, data:)) |> ret_one)
    },
    {
      // active data
      use memory_index <- nibble.do(nibble.or(
        use_parser("memory"),
        // abbreviation: 𝜖 -> (memory 0)
        IndexInt(0),
      ))
      use ExprReturn(impl:, instr: offset) <- nibble.do(offset_parser(ctx))
      use data <- nibble.do(string_bytes_parser())
      use _ <- nibble.do(nibble.token(RParen))
      nibble.return(
        ParseReturn(impl, [
          DataDecl(ActiveData(id:, data:, memory_index:, offset:)),
        ]),
      )
    },
  ])
}

fn offset_parser(ctx: ParseContext) -> nibble.Parser(ExprReturn, Token, b) {
  nibble.one_of([
    {
      use _ <- nibble.do(nibble.token(LKeyword("offset")))
      use expr <- nibble.do(expr_parser(ctx))
      use _ <- nibble.do(nibble.token(RParen))
      nibble.return(expr)
    },
    // abbreviation: single folded instruction instead of (offset expr)
    folded_instr_parser(ctx),
  ])
}

fn use_parser(name: String) -> nibble.Parser(Index, Token, b) {
  use _ <- nibble.do(nibble.token(LKeyword(name)))
  use index <- nibble.do(index_parser())
  use _ <- nibble.do(nibble.token(RParen))
  nibble.return(index)
}

const page_size = 65_536

fn memory_parser(ctx: ParseContext) {
  use _ <- nibble.do(nibble.token(LKeyword("memory")))
  use id <- nibble.do(id_parser() |> nibble.optional)
  use exports <- nibble.do(inline_exports_parser(ctx.memories, ExportMemory))
  nibble.one_of([
    {
      // inline import case
      use #(module_name, import_name) <- nibble.do(inline_import_parser(ctx))
      use #(address_type, limits) <- nibble.do(memory_type_parser())
      nibble.return(
        [
          ImportDecl(ImportMemory(
            module_name:,
            import_name:,
            id:,
            address_type:,
            limits:,
          )),
          ..exports
        ]
        |> ret_many,
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
            |> bigi.from_int
          use _ <- nibble.do(nibble.token(RParen))
          use _ <- nibble.do(nibble.token(RParen))
          nibble.return(
            [
              MemoryDecl(Memory(id:, address_type:, limits: Limits(n, Some(n)))),
              DataDecl(
                ActiveData(
                  id: None,
                  data:,
                  memory_index: IndexInt(ctx.memories),
                  offset: [
                    zero_address(address_type),
                  ],
                ),
              ),
              ..exports
            ]
            |> ret_many,
          )
        },
        {
          // plain memory case
          use limits <- nibble.do(limits_parser())
          use _ <- nibble.do(nibble.token(RParen))
          nibble.return(
            [
              MemoryDecl(Memory(id:, address_type:, limits:)),
            ]
            |> ret_many,
          )
        },
      ])
    },
  ])
}

fn memory_type_parser() -> nibble.Parser(#(AddressType, Limits), Token, b) {
  use address_type <- nibble.do(address_type_parser())
  use limits <- nibble.do(limits_parser())
  nibble.return(#(address_type, limits))
}

fn limits_parser() -> nibble.Parser(Limits, Token, b) {
  use min <- nibble.do(int64_parser())
  use max <- nibble.do(int64_parser() |> nibble.optional)
  nibble.return(Limits(min:, max:))
}

fn inline_export_parser() -> nibble.Parser(String, Token, b) {
  use _ <- nibble.do(nibble.token(LKeyword("export")))
  use name <- nibble.do(string_parser())
  use _ <- nibble.do(nibble.token(RParen))
  nibble.return(name)
}

fn inline_import_parser(
  ctx: ParseContext,
) -> nibble.Parser(#(String, String), Token, b) {
  use _ <- nibble.do(nibble.token(LKeyword("import")))
  use _ <- nibble.do(nibble.guard(
    !ctx.non_import_defs,
    "imports must occur before all non-import definitions",
  ))
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

fn global_parser(ctx: ParseContext) {
  use _ <- nibble.do(nibble.token(LKeyword("global")))
  use id <- nibble.do(id_parser() |> nibble.optional)
  use exports <- nibble.do(inline_exports_parser(ctx.globals, ExportGlobal))
  nibble.one_of([
    {
      // inline import case
      use #(module_name, import_name) <- nibble.do(inline_import_parser(ctx))
      use #(mutability, value_type) <- nibble.do(global_type_parser())
      nibble.return(
        [
          ImportDecl(ImportGlobal(
            module_name:,
            import_name:,
            id:,
            mutability:,
            value_type:,
          )),
          ..exports
        ]
        |> ret_many,
      )
    },
    {
      use #(mutability, value_type) <- nibble.do(global_type_parser())
      use ExprReturn(impl, instr: expr) <- nibble.do(expr_parser(ctx))
      use _ <- nibble.do(nibble.token(RParen))
      nibble.return(
        ParseReturn(impl, [
          GlobalDecl(Global(id:, mutability:, value_type:, expr:)),
          ..exports
        ]),
      )
    },
  ])
}

fn global_type_parser() -> nibble.Parser(
  #(Mutability, ValueType(Index)),
  Token,
  b,
) {
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

fn rec_type_parser() {
  nibble.one_of([
    {
      use _ <- nibble.do(nibble.token(LKeyword("rec")))
      use typedefs <- nibble.do(nibble.many(type_def_parser()))
      use _ <- nibble.do(nibble.token(RParen))
      nibble.return(typedefs)
    },
    type_def_parser() |> nibble.map(fn(t) { [t] }),
  ])
  |> nibble.map(fn(t) { TypeDecl(RecType(t)) |> ret_one })
}

fn type_def_parser() -> nibble.Parser(TypeDef(Index), Token, b) {
  use _ <- nibble.do(nibble.token(LKeyword("type")))
  use id <- nibble.do(id_parser() |> nibble.optional)
  use #(sub, super, def) <- nibble.do(sub_type_parser())
  use _ <- nibble.do(nibble.token(RParen))
  nibble.return(TypeDef(id:, sub:, super:, def:))
}

fn sub_type_parser() -> nibble.Parser(
  #(SubType, List(Index), CompType(Index)),
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

fn comp_type_parser() -> nibble.Parser(CompType(Index), Token, b) {
  nibble.one_of([
    def_struct_parser(),
    def_array_parser(),
    def_func_parser(),
  ])
}

fn def_struct_parser() -> nibble.Parser(CompType(Index), Token, b) {
  use _ <- nibble.do(nibble.token(LKeyword("struct")))
  use fields <- nibble.do(
    nibble.many(field_parser()) |> nibble.map(list.flatten),
  )
  use _ <- nibble.do(nibble.token(RParen))
  nibble.return(Struct(fields))
}

fn def_array_parser() -> nibble.Parser(CompType(Index), Token, b) {
  use _ <- nibble.do(nibble.token(LKeyword("array")))
  use ft <- nibble.do(field_type_parser())
  use _ <- nibble.do(nibble.token(RParen))
  nibble.return(Array(ft))
}

fn def_func_parser() -> nibble.Parser(CompType(Index), Token, b) {
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

fn field_parser() -> nibble.Parser(List(Field(Index)), Token, b) {
  use _ <- nibble.do(nibble.token(LKeyword("field")))
  use fields <- nibble.do(
    nibble.one_of([
      {
        use id <- nibble.do(id_parser())
        use ft <- nibble.do(field_type_parser())
        nibble.return([Field(Some(id), ft)])
      },
      {
        use fts <- nibble.do(nibble.many(field_type_parser()))
        nibble.return(list.map(fts, Field(None, _)))
      },
    ]),
  )
  use _ <- nibble.do(nibble.token(RParen))
  nibble.return(fields)
}

fn field_type_parser() -> nibble.Parser(FieldType(Index), Token, b) {
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

fn storage_type_parser() -> nibble.Parser(FieldType(Index), Token, b) {
  nibble.one_of([
    nibble.token(Keyword("i8")) |> nibble.replace(PackedField(Immutable, I8)),
    nibble.token(Keyword("i16")) |> nibble.replace(PackedField(Immutable, I16)),
    value_type_parser() |> nibble.map(ValueField(Immutable, _)),
  ])
}

fn function_parser(ctx: ParseContext) {
  use _ <- nibble.do(nibble.token(LKeyword("func")))
  use id <- nibble.do(id_parser() |> nibble.optional)
  use exports <- nibble.do(inline_exports_parser(ctx.functions, ExportFunction))
  nibble.one_of([
    {
      // import case
      use #(module_name, import_name) <- nibble.do(inline_import_parser(ctx))
      use #(impl, TypeUse(type_, params, results)) <- nibble.do(
        type_use_parser(),
      )
      use _ <- nibble.do(nibble.token(RParen))
      nibble.return(
        ParseReturn(impl, [
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
      use #(impl1, TypeUse(type_, params, results)) <- nibble.do(
        type_use_parser(),
      )
      use locals <- nibble.do(
        nibble.many(local_parser()) |> nibble.map(list.flatten),
      )
      use ExprReturn(impl: impl2, instr: expr) <- nibble.do(expr_parser(ctx))
      let impl = list.append(impl1, impl2)
      use _ <- nibble.do(nibble.token(RParen))
      nibble.return(
        ParseReturn(impl, [
          FunctionDecl(Function(id:, type_:, params:, results:, locals:, expr:)),
          ..exports
        ]),
      )
    },
  ])
}

fn instr_parser(ctx: ParseContext) -> nibble.Parser(ExprReturn, Token, a) {
  basic_instr_parser(AnyInstr, ctx)
}

fn folded_instr_parser(
  ctx: ParseContext,
) -> nibble.Parser(ExprReturn, Token, a) {
  basic_instr_parser(FoldedInstr, ctx)
}

type InstructionKind {
  AnyInstr
  FoldedInstr
  FlatInstr
}

fn basic_instr_parser(
  kind: InstructionKind,
  ctx: ParseContext,
) -> nibble.Parser(ExprReturn, Token, a) {
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
  let r = fn(instr) { nibble.return(ExprReturn([], [instr])) }
  let m = fn(parser) { nibble.map(parser, ExprReturn([], _)) }
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
      use #(impl, block_type) <- nibble.do(block_type_parser())
      ExprReturn(impl, [Block(label, block_type)]) |> nibble.return
    }
    "loop" -> {
      use label <- nibble.do(nibble.optional(id_parser()))
      use #(impl, block_type) <- nibble.do(block_type_parser())
      ExprReturn(impl, [Loop(label, block_type)]) |> nibble.return
    }
    "if" -> {
      use label <- nibble.do(nibble.optional(id_parser()))
      case kw {
        LKeyword(..) -> folded_if_parser(label, ctx)
        _ -> {
          use #(impl, block_type) <- nibble.do(block_type_parser())
          ExprReturn(impl, [If(label, block_type)]) |> nibble.return
        }
      }
    }
    "else" -> one_arg_parser(nibble.optional(id_parser()), Else) |> m
    "end" -> one_arg_parser(nibble.optional(id_parser()), End) |> m
    // TODO: try/catch
    "br" -> one_arg_parser(index_parser(), Branch) |> m
    "br_if" -> one_arg_parser(index_parser(), BranchIf) |> m
    "br_table" -> {
      use labels <- nibble.do(nibble.many1(index_parser()))
      let assert [default, ..labels] = list.reverse(labels)
      let labels = list.reverse(labels)
      r(BranchTable(labels, default))
    }
    "br_on_null" -> one_arg_parser(index_parser(), BranchOnNull) |> m
    "br_on_cast" ->
      three_arg_parser(
        index_parser(),
        ref_type_parser(),
        ref_type_parser(),
        BranchOnCast,
      )
      |> m
    "br_on_cast_fail" ->
      three_arg_parser(
        index_parser(),
        ref_type_parser(),
        ref_type_parser(),
        BranchOnCastFail,
      )
      |> m
    "call" -> one_arg_parser(index_parser(), Call) |> m
    "call_ref" -> one_arg_parser(index_parser(), CallRef) |> m
    "call_indirect" -> call_indirect_parser(CallIndirect) |> m
    "return" -> r(Return)
    "return_call" -> one_arg_parser(index_parser(), ReturnCall) |> m
    "return_call_ref" -> one_arg_parser(index_parser(), ReturnCallRef) |> m
    "return_call_indirect" -> call_indirect_parser(ReturnCallIndirect) |> m
    // "throw" -> todo
    // "throw_ref" -> todo
    // variable instructions
    "local.get" -> one_arg_parser(index_parser(), LocalGet) |> m
    "local.set" -> one_arg_parser(index_parser(), LocalSet) |> m
    "local.tee" -> one_arg_parser(index_parser(), LocalTee) |> m
    "global.get" -> one_arg_parser(index_parser(), GlobalGet) |> m
    "global.set" -> one_arg_parser(index_parser(), GlobalSet) |> m
    // table instructions
    "table.get" ->
      one_arg_parser_optional(index_parser(), IndexInt(0), TableGet) |> m
    "table.set" ->
      one_arg_parser_optional(index_parser(), IndexInt(0), TableSet) |> m
    "table.size" ->
      one_arg_parser_optional(index_parser(), IndexInt(0), TableSize) |> m
    "table.grow" ->
      one_arg_parser_optional(index_parser(), IndexInt(0), TableGrow) |> m
    "table.copy" ->
      nibble.one_of([
        two_arg_parser(index_parser(), index_parser(), TableCopy) |> m,
        r(TableCopy(IndexInt(0), IndexInt(0))),
      ])
    "table.init" -> {
      use idx1 <- nibble.do(index_parser())
      use idx2 <- nibble.do(index_parser() |> nibble.optional)
      case idx2 {
        Some(idx2) -> r(TableInit(idx1, idx2))
        None -> r(TableInit(IndexInt(0), idx1))
      }
    }
    "elem.drop" ->
      one_arg_parser_optional(index_parser(), IndexInt(0), ElemDrop) |> m
    // memory instructions
    "i32.load" -> mem_instr_parser(I32Load) |> m
    "i64.load" -> mem_instr_parser(I64Load) |> m
    "f32.load" -> mem_instr_parser(F32Load) |> m
    "f64.load" -> mem_instr_parser(F64Load) |> m
    "i32.load8_s" -> mem_instr_parser(I32Load8S) |> m
    "i32.load8_u" -> mem_instr_parser(I32Load8U) |> m
    "i32.load16_s" -> mem_instr_parser(I32Load16S) |> m
    "i32.load16_u" -> mem_instr_parser(I32Load16U) |> m
    "i64.load8_s" -> mem_instr_parser(I64Load8S) |> m
    "i64.load8_u" -> mem_instr_parser(I64Load8U) |> m
    "i64.load16_s" -> mem_instr_parser(I64Load16S) |> m
    "i64.load16_u" -> mem_instr_parser(I64Load16U) |> m
    "i64.load32_s" -> mem_instr_parser(I64Load32S) |> m
    "i64.load32_u" -> mem_instr_parser(I64Load32U) |> m
    // TODO: vector load instructions
    "i32.store" -> mem_instr_parser(I32Store) |> m
    "i64.store" -> mem_instr_parser(I64Store) |> m
    "f32.store" -> mem_instr_parser(F32Store) |> m
    "f64.store" -> mem_instr_parser(F64Store) |> m
    "i32.store8" -> mem_instr_parser(I32Store8) |> m
    "i32.store16" -> mem_instr_parser(I32Store16) |> m
    "i64.store8" -> mem_instr_parser(I64Store8) |> m
    "i64.store16" -> mem_instr_parser(I64Store16) |> m
    "i64.store32" -> mem_instr_parser(I64Store32) |> m
    // TODO: vector store instructions
    "memory.size" ->
      one_arg_parser_optional(index_parser(), IndexInt(0), MemorySize) |> m
    "memory.grow" ->
      one_arg_parser_optional(index_parser(), IndexInt(0), MemoryGrow) |> m
    "memory.fill" ->
      one_arg_parser_optional(index_parser(), IndexInt(0), MemoryFill) |> m
    "memory.copy" -> todo as "two optional index args"
    "memory.init" -> todo as "memory index optional, data index required"
    "data.drop" -> one_arg_parser(index_parser(), DataDrop) |> m
    // reference instructions
    "ref.null" -> one_arg_parser(heap_type_parser(), RefNull) |> m
    "ref.func" -> one_arg_parser(index_parser(), RefFunc) |> m
    "ref.is_null" -> r(RefIsNull)
    "ref.as_non_null" -> r(RefAsNonNull)
    "ref.eq" -> r(RefEq)
    "ref.test" -> one_arg_parser(ref_type_parser(), RefTest) |> m
    "ref.cast" -> one_arg_parser(ref_type_parser(), RefCast) |> m
    // aggregate instructions
    "ref.i31" -> r(RefI31)
    "i31.get_s" -> r(I31GetS)
    "i31.get_u" -> r(I31GetU)
    "struct.new" -> one_arg_parser(index_parser(), StructNew) |> m
    "struct.new_default" ->
      one_arg_parser(index_parser(), StructNewDefault) |> m
    "struct.get" ->
      two_arg_parser(index_parser(), index_parser(), StructGet) |> m
    "struct.get_s" ->
      two_arg_parser(index_parser(), index_parser(), StructGetS) |> m
    "struct.get_u" ->
      two_arg_parser(index_parser(), index_parser(), StructGetU) |> m
    "struct.set" ->
      two_arg_parser(index_parser(), index_parser(), StructSet) |> m
    "array.new" -> one_arg_parser(index_parser(), ArrayNew) |> m
    "array.new_default" -> one_arg_parser(index_parser(), ArrayNewDefault) |> m
    "array.new_fixed" ->
      two_arg_parser(index_parser(), int32_parser(), ArrayNewFixed) |> m
    "array.new_data" ->
      two_arg_parser(index_parser(), index_parser(), ArrayNewData) |> m
    "array.new_elem" ->
      two_arg_parser(index_parser(), index_parser(), ArrayNewElem) |> m
    "array.get" -> one_arg_parser(index_parser(), ArrayGet) |> m
    "array.get_s" -> one_arg_parser(index_parser(), ArrayGetS) |> m
    "array.get_u" -> one_arg_parser(index_parser(), ArrayGetU) |> m
    "array.set" -> one_arg_parser(index_parser(), ArraySet) |> m
    "array.len" -> r(ArrayLen)
    "array.fill" -> one_arg_parser(index_parser(), ArrayFill) |> m
    "array.copy" ->
      two_arg_parser(index_parser(), index_parser(), ArrayCopy) |> m
    "array.init_data" ->
      two_arg_parser(index_parser(), index_parser(), ArrayInitData) |> m
    "array.init_elem" ->
      two_arg_parser(index_parser(), index_parser(), ArrayInitElem) |> m
    "any.convert_extern" -> r(AnyConvertExtern)
    "extern.convert_any" -> r(ExternConvertAny)
    // numeric instructions
    "i32.const" -> one_arg_parser(int32_parser(), I32Const) |> m
    "i64.const" -> one_arg_parser(int64_parser(), I64Const) |> m
    "f32.const" -> one_arg_parser(float_parser(), F32Const) |> m
    "f64.const" -> one_arg_parser(float_parser(), F64Const) |> m
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
      let ExprReturn(impl: impl1, instr: expr) = expr
      use ExprReturn(impl: impl2, instr:) <- nibble.do(expr_parser(ctx))
      use _ <- nibble.do(nibble.token(RParen))
      nibble.return(ExprReturn(
        list.append(impl1, impl2),
        list.flatten([expr, instr, [End(None)]]),
      ))
    }
    LKeyword(_) -> {
      // plain folded instructions have their arguments prepended
      let ExprReturn(impl: impl2, instr: expr) = expr
      use ExprReturn(impl: impl1, instr: args) <- nibble.do(expr_parser(ctx))
      use _ <- nibble.do(nibble.token(RParen))
      nibble.return(ExprReturn(
        list.append(impl1, impl2),
        list.append(args, expr),
      ))
    }
    _ -> nibble.return(expr)
  }
}

fn call_indirect_parser(constr) {
  use table_index <- nibble.do(index_parser() |> nibble.optional)
  let table_index = option.unwrap(table_index, IndexInt(0))
  use type_ <- nibble.do(nibble.optional(func_type_parser()))
  use params <- nibble.do(
    nibble.many(result_like_parser("param")) |> nibble.map(list.flatten),
  )
  use results <- nibble.do(
    nibble.many(result_parser()) |> nibble.map(list.flatten),
  )
  let type_ = option.unwrap(type_, IndexImplicit(ImplicitFunc(params, results)))
  nibble.return([
    constr(table_index, type_, params, results),
  ])
}

fn folded_if_parser(
  label: Option(String),
  ctx: ParseContext,
) -> nibble.Parser(ExprReturn, Token, a) {
  use #(impl0, block_type) <- nibble.do(block_type_parser())
  use ExprReturn(impl: impl1, instr: args) <- nibble.do(
    nibble.many(folded_instr_parser(ctx)) |> nibble.map(flatten_expr),
  )
  use ExprReturn(impl: impl2, instr: then) <- nibble.do(then_parser(ctx))
  use else_ <- nibble.do(nibble.optional(else_parser(ctx)))
  let if_ = If(label: label, block_type:)
  let #(impl3, else_) = case else_ {
    None -> #([], [])
    Some(ExprReturn(impl:, instr:)) -> #(impl, [Else(None), ..instr])
  }
  nibble.return(ExprReturn(
    list.flatten([impl0, impl1, impl2, impl3]),
    list.flatten([args, [if_], then, else_, [End(None)]]),
  ))
}

fn then_parser(ctx: ParseContext) {
  use _ <- nibble.do(nibble.token(LKeyword("then")))
  use expr <- nibble.do(expr_parser(ctx))
  use _ <- nibble.do(nibble.token(RParen))
  nibble.return(expr)
}

fn else_parser(ctx: ParseContext) {
  use _ <- nibble.do(nibble.token(LKeyword("else")))
  use expr <- nibble.do(expr_parser(ctx))
  use _ <- nibble.do(nibble.token(RParen))
  nibble.return(expr)
}

fn block_type_parser() {
  use #(impl, t) <- nibble.do(type_use_parser())
  let #(impl, bt) = case t {
    TypeUse(IndexImplicit(_), [], []) -> #([], BlockEmpty)
    TypeUse(IndexImplicit(_), [], [result]) -> #([], BlockResult(result))
    TypeUse(type_:, params:, results:) -> #(
      impl,
      BlockTypeUse(type_:, params:, results:),
    )
  }
  nibble.return(#(impl, bt))
}

fn type_use_parser() {
  use type_ <- nibble.do(nibble.optional(func_type_parser()))
  use params <- nibble.do(
    nibble.many(param_parser()) |> nibble.map(list.flatten),
  )
  use results <- nibble.do(
    nibble.many(result_parser()) |> nibble.map(list.flatten),
  )
  let #(impl, type_) = case type_ {
    Some(type_) -> #([], type_)
    None -> {
      let impl = implicit_func(params, results)
      #([impl], IndexImplicit(impl))
    }
  }
  nibble.return(#(impl, TypeUse(type_, params, results)))
}

fn mem_instr_parser(
  constr: fn(Index, String, String) -> Instruction(Index),
) -> nibble.Parser(List(Instruction(Index)), Token, a) {
  use idx <- nibble.do(nibble.optional(index_parser()))
  let idx = option.unwrap(idx, IndexInt(0))
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
    int32_parser() |> nibble.map(IndexInt),
    id_parser() |> nibble.map(IndexId),
  ])
}

fn func_type_parser() -> nibble.Parser(Index, Token, a) {
  use _ <- nibble.do(nibble.token(LKeyword("type")))
  use type_use <- nibble.do(index_parser())
  use _ <- nibble.do(nibble.token(RParen))
  nibble.return(type_use)
}

fn int32_parser() -> nibble.Parser(Int, Token, a) {
  use str <- nibble.do(int_parser())
  let res = case string.replace(str, "_", "") {
    "-0x" <> hex -> int.base_parse("-" <> hex, 16)
    "+0x" <> hex | "0x" <> hex -> int.base_parse(hex, 16)
    _ -> int.parse(str)
  }
  case res {
    Ok(i) -> nibble.return(i)
    Error(_) -> nibble.fail("Invalid i32: " <> str)
  }
}

fn int64_parser() -> nibble.Parser(BigInt, Token, a) {
  use str <- nibble.do(int_parser())
  let res = case string.replace(str, "_", "") {
    "-0x" <> hex -> bigi.from_base16("-" <> hex)
    "+0x" <> hex | "0x" <> hex -> bigi.from_base16(hex)
    _ -> bigi.from_string(str)
  }
  case res {
    Ok(i) -> nibble.return(i)
    Error(_) -> nibble.fail("Invalid i64: " <> str)
  }
}

fn int_parser() -> nibble.Parser(String, Token, a) {
  use t <- nibble.do(nibble.any())
  case t {
    Integer(str) -> nibble.return(str)
    _ -> nibble.fail("expected an integer")
  }
}

fn float_parser() -> nibble.Parser(IEEEFloat, Token, a) {
  use t <- nibble.do(nibble.any())
  case t {
    Float(str) ->
      case parse_float(str) {
        Ok(val) -> nibble.return(val)
        Error(err) -> nibble.fail(err)
      }
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

fn ref_type_parser() -> nibble.Parser(RefType(Index), Token, a) {
  nibble.one_of([
    keyword_value_parser("anyref", Nullable(HeapAny)),
    keyword_value_parser("eqref", Nullable(HeapEq)),
    keyword_value_parser("i31ref", Nullable(HeapI31)),
    keyword_value_parser("structref", Nullable(HeapStruct)),
    keyword_value_parser("arrayref", Nullable(HeapArray)),
    keyword_value_parser("nullref", Nullable(HeapNone)),
    keyword_value_parser("funcref", Nullable(HeapFunc)),
    keyword_value_parser("nullfuncref", Nullable(HeapNoFunc)),
    keyword_value_parser("exnref", Nullable(HeapExn)),
    keyword_value_parser("nullexnref", Nullable(HeapNoExn)),
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

fn heap_type_parser() -> nibble.Parser(HeapType(Index), Token, a) {
  nibble.one_of([
    keyword_value_parser("any", HeapAny),
    keyword_value_parser("eq", HeapEq),
    keyword_value_parser("i31", HeapI31),
    keyword_value_parser("struct", HeapStruct),
    keyword_value_parser("array", HeapArray),
    keyword_value_parser("none", HeapNone),
    keyword_value_parser("func", HeapFunc),
    keyword_value_parser("nofunc", HeapNoFunc),
    keyword_value_parser("exn", HeapExn),
    keyword_value_parser("noexn", HeapNoExn),
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

/// Replace all occurrences of `IndexId` and `IndexImplicit` by `IndexInt`.
pub fn resolve_indices(
  mod: Module(Index),
  ctx: IdContext,
) -> Result(Module(Int), String) {
  use types <- result.try(
    list.try_map(mod.types, fn(rec) {
      use defs <- result.map(list.try_map(rec.defs, resolve_ids_type(_, ctx)))
      RecType(defs)
    }),
  )
  use imports <- result.try(
    list.try_map(mod.imports, resolve_ids_import(_, ctx)),
  )
  use tags <- result.try(list.try_map(mod.tags, resolve_ids_tag(_, ctx)))
  use globals <- result.try(
    list.try_map(mod.globals, resolve_ids_global(_, ctx)),
  )
  use tables <- result.try(list.try_map(mod.tables, resolve_ids_table(_, ctx)))
  use functions <- result.try(
    list.try_map(mod.functions, resolve_ids_function(_, ctx)),
  )
  use datas <- result.try(list.try_map(mod.datas, resolve_ids_data(_, ctx)))
  use elements <- result.try(
    list.try_map(mod.elements, resolve_ids_element(_, ctx)),
  )
  use start <- result.try(resolve_ids_start(mod.start, ctx))
  use exports <- result.map(
    list.try_map(mod.exports, resolve_ids_export(_, ctx)),
  )
  Module(
    ..mod,
    types:,
    imports:,
    tags:,
    globals:,
    tables:,
    functions:,
    datas:,
    elements:,
    start:,
    exports:,
  )
}

fn resolve_ids_type(
  t: TypeDef(Index),
  ctx: IdContext,
) -> Result(TypeDef(Int), String) {
  use super <- result.try(list.try_map(t.super, resolve_type(_, ctx)))
  use def <- result.map(resolve_ids_comp_type(t.def, ctx))
  TypeDef(..t, super:, def:)
}

fn resolve_ids_comp_type(
  def: CompType(Index),
  ctx: IdContext,
) -> Result(CompType(Int), String) {
  case def {
    Struct(fields:) -> {
      use fields <- result.map(list.try_map(fields, resolve_ids_field(_, ctx)))
      Struct(fields:)
    }
    Array(item_type:) -> {
      use item_type <- result.map(resolve_ids_field_type(item_type, ctx))
      Array(item_type:)
    }
    Func(params:, results:) -> {
      use params <- result.try(list.try_map(params, resolve_ids_param(_, ctx)))
      use results <- result.map(
        list.try_map(results, resolve_ids_value_type(_, ctx)),
      )
      Func(params:, results:)
    }
  }
}

fn resolve_ids_import(
  value: Import(Index),
  ctx: IdContext,
) -> Result(Import(Int), String) {
  case value {
    ImportTag(type_:, params:, results:, ..) -> {
      use TypeUse(type_:, params:, results:) <- result.map(resolve_ids_type_use(
        TypeUse(type_:, params:, results:),
        ctx,
      ))
      ImportTag(..value, type_:, params:, results:)
    }
    ImportGlobal(value_type:, ..) -> {
      use value_type <- result.map(resolve_ids_value_type(value_type, ctx))
      ImportGlobal(..value, value_type:)
    }
    ImportMemory(module_name:, import_name:, id:, address_type:, limits:) ->
      Ok(ImportMemory(module_name:, import_name:, id:, address_type:, limits:))
    ImportTable(ref_type:, ..) -> {
      use ref_type <- result.map(resolve_ids_ref_type(ref_type, ctx))
      ImportTable(..value, ref_type:)
    }
    ImportFunction(type_:, params:, results:, ..) -> {
      use TypeUse(type_:, params:, results:) <- result.map(resolve_ids_type_use(
        TypeUse(type_:, params:, results:),
        ctx,
      ))
      ImportFunction(..value, type_:, params:, results:)
    }
  }
}

fn resolve_ids_tag(
  tag: Tag(Index),
  ctx: IdContext,
) -> Result(Tag(Int), String) {
  let Tag(type_:, params:, results:, ..) = tag
  use TypeUse(type_:, params:, results:) <- result.map(resolve_ids_type_use(
    TypeUse(type_:, params:, results:),
    ctx,
  ))
  Tag(..tag, type_:, params:, results:)
}

fn resolve_ids_global(
  global: Global(Index),
  ctx: IdContext,
) -> Result(Global(Int), String) {
  use value_type <- result.try(resolve_ids_value_type(global.value_type, ctx))
  use expr <- result.map(list.try_map(global.expr, resolve_ids_instr(_, ctx)))
  Global(..global, value_type:, expr:)
}

fn resolve_ids_table(
  table: Table(Index),
  ctx: IdContext,
) -> Result(Table(Int), String) {
  use ref_type <- result.try(resolve_ids_ref_type(table.ref_type, ctx))
  use expr <- result.map(list.try_map(table.expr, resolve_ids_instr(_, ctx)))
  Table(..table, ref_type:, expr:)
}

fn resolve_ids_function(
  function: Function(Index),
  ctx: IdContext,
) -> Result(Function(Int), String) {
  let Function(type_:, params:, results:, locals:, expr:, ..) = function
  use TypeUse(type_:, params:, results:) <- result.try(resolve_ids_type_use(
    TypeUse(type_:, params:, results:),
    ctx,
  ))
  use locals <- result.try(list.try_map(locals, resolve_ids_param(_, ctx)))
  let local_indices =
    list.append(params, locals)
    |> list.index_map(fn(param, idx) {
      option.map(param.id, fn(id) { #(id, idx) })
    })
    |> option.values
    |> dict.from_list
  use expr <- result.map(resolve_ids_expr(
    expr,
    IdContext(..ctx, locals: local_indices),
  ))
  Function(..function, type_:, params:, results:, locals:, expr:)
}

fn resolve_ids_data(
  data: Data(Index),
  ctx: IdContext,
) -> Result(Data(Int), String) {
  case data {
    PassiveData(id:, data:) -> Ok(PassiveData(id:, data:))
    ActiveData(memory_index:, offset:, ..) -> {
      use memory_index <- result.try(resolve_memory(memory_index, ctx))
      use offset <- result.map(resolve_ids_expr(offset, ctx))
      ActiveData(..data, memory_index:, offset:)
    }
  }
}

fn resolve_ids_element(
  element: Element(Index),
  ctx: IdContext,
) -> Result(Element(Int), String) {
  use ref_type <- result.try(resolve_ids_ref_type(element.ref_type, ctx))
  use elems <- result.try(list.try_map(element.elems, resolve_ids_expr(_, ctx)))
  case element {
    PassiveElement(..) -> Ok(PassiveElement(..element, ref_type:, elems:))
    ActiveElement(table_index:, offset:, ..) -> {
      use table_index <- result.try(resolve_table(table_index, ctx))
      use offset <- result.map(resolve_ids_expr(offset, ctx))
      ActiveElement(..element, ref_type:, elems:, table_index:, offset:)
    }
    DeclarativeElement(..) ->
      Ok(DeclarativeElement(..element, ref_type:, elems:))
  }
}

fn resolve_ids_start(
  start: Option(Start(Index)),
  ctx: IdContext,
) -> Result(Option(Start(Int)), String) {
  case start {
    Some(Start(function_index:)) -> {
      use function_index <- result.map(resolve_function(function_index, ctx))
      Some(Start(function_index:))
    }
    None -> Ok(None)
  }
}

fn resolve_ids_export(
  export: Export(Index),
  ctx: IdContext,
) -> Result(Export(Int), String) {
  case export {
    ExportTag(name:, tag_index:) -> {
      use tag_index <- result.map(resolve_tag(tag_index, ctx))
      ExportTag(name:, tag_index:)
    }
    ExportGlobal(name:, global_index:) -> {
      use global_index <- result.map(resolve_global(global_index, ctx))
      ExportGlobal(name:, global_index:)
    }
    ExportMemory(name:, memory_index:) -> {
      use memory_index <- result.map(resolve_memory(memory_index, ctx))
      ExportMemory(name:, memory_index:)
    }
    ExportTable(name:, table_index:) -> {
      use table_index <- result.map(resolve_table(table_index, ctx))
      ExportTable(name:, table_index:)
    }
    ExportFunction(name:, function_index:) -> {
      use function_index <- result.map(resolve_function(function_index, ctx))
      ExportFunction(name:, function_index:)
    }
  }
}

fn resolve_ids_type_use(
  tu: TypeUse(Index),
  ctx: IdContext,
) -> Result(TypeUse(Int), String) {
  use type_ <- result.try(resolve_type(tu.type_, ctx))
  use params <- result.try(list.try_map(tu.params, resolve_ids_param(_, ctx)))
  use results <- result.map(
    list.try_map(tu.results, resolve_ids_value_type(_, ctx)),
  )
  TypeUse(type_:, params:, results:)
}

fn resolve_ids_param(
  param: Param(Index),
  ctx: IdContext,
) -> Result(Param(Int), String) {
  use typ <- result.map(resolve_ids_value_type(param.typ, ctx))
  Param(..param, typ:)
}

fn resolve_ids_ref_type(
  ref_type: RefType(Index),
  ctx: IdContext,
) -> Result(RefType(Int), String) {
  use heap_type <- result.map(resolve_ids_heap_type(ref_type.heap_type, ctx))
  case ref_type {
    NonNull(..) -> NonNull(heap_type:)
    Nullable(..) -> Nullable(heap_type:)
  }
}

fn resolve_ids_heap_type(
  heap_type: HeapType(Index),
  ctx: IdContext,
) -> Result(HeapType(Int), String) {
  case heap_type {
    HeapIndex(type_index) ->
      result.map(resolve_type(type_index, ctx), HeapIndex)
    HeapAny -> HeapAny |> Ok
    HeapEq -> HeapEq |> Ok
    HeapI31 -> HeapI31 |> Ok
    HeapStruct -> HeapStruct |> Ok
    HeapArray -> HeapArray |> Ok
    HeapNone -> HeapNone |> Ok
    HeapFunc -> HeapFunc |> Ok
    HeapNoFunc -> HeapNoFunc |> Ok
    HeapExn -> HeapExn |> Ok
    HeapNoExn -> HeapNoExn |> Ok
    HeapExtern -> HeapExtern |> Ok
    HeapNoExtern -> HeapNoExtern |> Ok
  }
}

fn resolve_ids_value_type(
  value_type: ValueType(Index),
  ctx: IdContext,
) -> Result(ValueType(Int), String) {
  case value_type {
    Ref(ref_type) -> result.map(resolve_ids_ref_type(ref_type, ctx), Ref)
    I32 -> Ok(I32)
    I64 -> Ok(I64)
    F32 -> Ok(F32)
    F64 -> Ok(F64)
  }
}

fn resolve_ids_field(
  field: Field(Index),
  ctx: IdContext,
) -> Result(Field(Int), String) {
  result.map(resolve_ids_field_type(field.field_type, ctx), Field(field.id, _))
}

fn resolve_ids_field_type(
  field_type: FieldType(Index),
  ctx: IdContext,
) -> Result(FieldType(Int), String) {
  case field_type {
    ValueField(mutability:, value_type:) ->
      result.map(resolve_ids_value_type(value_type, ctx), ValueField(
        mutability,
        _,
      ))
    PackedField(mutability:, packed_type:) ->
      Ok(PackedField(mutability:, packed_type:))
  }
}

fn resolve_ids_expr(expr: List(Instruction(Index)), ctx: IdContext) {
  list.try_fold(expr, #(ctx, []), fn(acc, instr) {
    let #(ctx, instrs) = acc
    use instr <- result.map(resolve_ids_instr(instr, ctx))
    let ctx = case instr {
      Block(label:, ..) | Loop(label:, ..) | If(label:, ..) ->
        IdContext(..ctx, labels: [option.unwrap(label, ""), ..ctx.labels])
      End(..) -> IdContext(..ctx, labels: list.drop(ctx.labels, 1))
      _ -> ctx
    }
    #(ctx, [instr, ..instrs])
  })
  |> result.map(pair.second)
  |> result.map(list.reverse)
}

fn resolve_ids_instr(
  instr: Instruction(Index),
  ctx: IdContext,
) -> Result(Instruction(Int), String) {
  case instr {
    Unreachable -> Unreachable |> Ok
    Nop -> Nop |> Ok
    Block(label:, block_type:) ->
      result.map(resolve_ids_block_type(block_type, ctx), Block(label, _))
    Loop(label:, block_type:) ->
      result.map(resolve_ids_block_type(block_type, ctx), Loop(label, _))
    If(label:, block_type:) ->
      result.map(resolve_ids_block_type(block_type, ctx), If(label, _))
    Else(label:) ->
      result.replace(check_block_label(label, ctx, "else"), Else(label:))
    End(label:) ->
      result.replace(check_block_label(label, ctx, "end"), End(label:))
    Branch(label_index:) -> result.map(resolve_label(label_index, ctx), Branch)
    BranchIf(label_index:) ->
      result.map(resolve_label(label_index, ctx), BranchIf)
    BranchTable(label_indices:, default_label_index:) -> {
      use label_indices <- result.try(
        list.try_map(label_indices, resolve_label(_, ctx)),
      )
      use default_label_index <- result.map(resolve_label(
        default_label_index,
        ctx,
      ))
      BranchTable(label_indices:, default_label_index:)
    }
    Return -> Return |> Ok
    Call(function_index:) ->
      result.map(resolve_function(function_index, ctx), Call)
    CallIndirect(table_index:, type_:, params:, results:) -> {
      use table_index <- result.try(resolve_table(table_index, ctx))
      use type_ <- result.try(resolve_type(type_, ctx))
      use params <- result.try(
        list.try_map(params, resolve_ids_value_type(_, ctx)),
      )
      use results <- result.map(
        list.try_map(results, resolve_ids_value_type(_, ctx)),
      )
      CallIndirect(table_index:, type_:, params:, results:)
    }
    ReturnCall(function_index:) ->
      result.map(resolve_function(function_index, ctx), ReturnCall)
    ReturnCallIndirect(table_index:, type_:, params:, results:) -> {
      use table_index <- result.try(resolve_table(table_index, ctx))
      use type_ <- result.try(resolve_type(type_, ctx))
      use params <- result.try(
        list.try_map(params, resolve_ids_value_type(_, ctx)),
      )
      use results <- result.map(
        list.try_map(results, resolve_ids_value_type(_, ctx)),
      )
      ReturnCallIndirect(table_index:, type_:, params:, results:)
    }
    CallRef(type_index:) -> result.map(resolve_type(type_index, ctx), CallRef)
    ReturnCallRef(type_index:) ->
      result.map(resolve_type(type_index, ctx), ReturnCallRef)
    BranchOnNull(label_index:) ->
      result.map(resolve_label(label_index, ctx), BranchOnNull)
    BranchOnNonNull(label_index:) ->
      result.map(resolve_label(label_index, ctx), BranchOnNonNull)
    BranchOnCast(label_index:, from_ref_type:, to_ref_type:) -> {
      use label_index <- result.try(resolve_label(label_index, ctx))
      use from_ref_type <- result.try(resolve_ids_ref_type(from_ref_type, ctx))
      use to_ref_type <- result.map(resolve_ids_ref_type(to_ref_type, ctx))
      BranchOnCast(label_index:, from_ref_type:, to_ref_type:)
    }
    BranchOnCastFail(label_index:, from_ref_type:, to_ref_type:) -> {
      use label_index <- result.try(resolve_label(label_index, ctx))
      use from_ref_type <- result.try(resolve_ids_ref_type(from_ref_type, ctx))
      use to_ref_type <- result.map(resolve_ids_ref_type(to_ref_type, ctx))
      BranchOnCast(label_index:, from_ref_type:, to_ref_type:)
    }
    RefNull(heap_type:) ->
      result.map(resolve_ids_heap_type(heap_type, ctx), RefNull)
    RefFunc(function_index:) ->
      result.map(resolve_function(function_index, ctx), RefFunc)
    RefIsNull -> RefIsNull |> Ok
    RefAsNonNull -> RefAsNonNull |> Ok
    RefEq -> RefEq |> Ok
    RefTest(ref_type:) ->
      result.map(resolve_ids_ref_type(ref_type, ctx), RefTest)
    RefCast(ref_type:) ->
      result.map(resolve_ids_ref_type(ref_type, ctx), RefCast)
    RefI31 -> RefI31 |> Ok
    I31GetS -> I31GetS |> Ok
    I31GetU -> I31GetU |> Ok
    StructNew(type_index:) ->
      result.map(resolve_type(type_index, ctx), StructNew)
    StructNewDefault(type_index:) ->
      result.map(resolve_type(type_index, ctx), StructNewDefault)
    StructGet(type_index:, field_index:) ->
      resolve_ids_field_access(type_index, field_index, ctx, StructGet)
    StructGetS(type_index:, field_index:) ->
      resolve_ids_field_access(type_index, field_index, ctx, StructGetS)
    StructGetU(type_index:, field_index:) ->
      resolve_ids_field_access(type_index, field_index, ctx, StructGetU)
    StructSet(type_index:, field_index:) ->
      resolve_ids_field_access(type_index, field_index, ctx, StructSet)
    ArrayNew(type_index:) -> result.map(resolve_type(type_index, ctx), ArrayNew)
    ArrayNewDefault(type_index:) ->
      result.map(resolve_type(type_index, ctx), ArrayNewDefault)
    ArrayNewFixed(type_index:, size:) ->
      result.map(resolve_type(type_index, ctx), ArrayNewFixed(_, size))
    ArrayNewData(type_index:, data_index:) -> {
      use type_index <- result.try(resolve_type(type_index, ctx))
      use data_index <- result.map(resolve_data(data_index, ctx))
      ArrayNewData(type_index, data_index)
    }
    ArrayNewElem(type_index:, element_index:) -> {
      use type_index <- result.try(resolve_type(type_index, ctx))
      use element_index <- result.map(resolve_element(element_index, ctx))
      ArrayNewElem(type_index, element_index)
    }
    ArrayGet(type_index:) -> result.map(resolve_type(type_index, ctx), ArrayGet)
    ArrayGetS(type_index:) ->
      result.map(resolve_type(type_index, ctx), ArrayGetS)
    ArrayGetU(type_index:) ->
      result.map(resolve_type(type_index, ctx), ArrayGetU)
    ArraySet(type_index:) -> result.map(resolve_type(type_index, ctx), ArraySet)
    ArrayLen -> ArrayLen |> Ok
    ArrayFill(type_index:) ->
      result.map(resolve_type(type_index, ctx), ArrayFill)
    ArrayCopy(type_index:, source_type_index:) -> {
      use type_index <- result.try(resolve_type(type_index, ctx))
      use source_type_index <- result.map(resolve_type(source_type_index, ctx))
      ArrayCopy(type_index:, source_type_index:)
    }
    ArrayInitData(type_index:, data_index:) -> {
      use type_index <- result.try(resolve_type(type_index, ctx))
      use data_index <- result.map(resolve_data(data_index, ctx))
      ArrayInitData(type_index, data_index)
    }
    ArrayInitElem(type_index:, element_index:) -> {
      use type_index <- result.try(resolve_type(type_index, ctx))
      use element_index <- result.map(resolve_element(element_index, ctx))
      ArrayInitElem(type_index, element_index)
    }
    AnyConvertExtern -> AnyConvertExtern |> Ok
    ExternConvertAny -> ExternConvertAny |> Ok
    Drop -> Drop |> Ok
    Select(value_types) ->
      result.map(
        list.try_map(value_types, resolve_ids_value_type(_, ctx)),
        Select,
      )
    LocalGet(local_index:) ->
      result.map(resolve_local(local_index, ctx), LocalGet)
    LocalSet(local_index:) ->
      result.map(resolve_local(local_index, ctx), LocalSet)
    LocalTee(local_index:) ->
      result.map(resolve_local(local_index, ctx), LocalTee)
    GlobalGet(global_index:) ->
      result.map(resolve_global(global_index, ctx), GlobalGet)
    GlobalSet(global_index:) ->
      result.map(resolve_global(global_index, ctx), GlobalSet)
    TableGet(table_index:) ->
      result.map(resolve_table(table_index, ctx), TableGet)
    TableSet(table_index:) ->
      result.map(resolve_table(table_index, ctx), TableSet)
    TableSize(table_index:) ->
      result.map(resolve_table(table_index, ctx), TableSize)
    TableGrow(table_index:) ->
      result.map(resolve_table(table_index, ctx), TableGrow)
    TableFill(table_index:) ->
      result.map(resolve_table(table_index, ctx), TableFill)
    TableCopy(table_index:, source_table_index:) -> {
      use table_index <- result.try(resolve_table(table_index, ctx))
      use source_table_index <- result.map(resolve_table(
        source_table_index,
        ctx,
      ))
      TableCopy(table_index:, source_table_index:)
    }
    TableInit(table_index:, element_index:) -> {
      use table_index <- result.try(resolve_table(table_index, ctx))
      use element_index <- result.map(resolve_element(element_index, ctx))
      TableInit(table_index, element_index)
    }
    ElemDrop(element_index:) ->
      result.map(resolve_element(element_index, ctx), ElemDrop)
    I32Load(memory_index:, offset:, align:) ->
      result.map(resolve_memory(memory_index, ctx), I32Load(_, offset, align))
    I64Load(memory_index:, offset:, align:) ->
      result.map(resolve_memory(memory_index, ctx), I64Load(_, offset, align))
    F32Load(memory_index:, offset:, align:) ->
      result.map(resolve_memory(memory_index, ctx), F32Load(_, offset, align))
    F64Load(memory_index:, offset:, align:) ->
      result.map(resolve_memory(memory_index, ctx), F64Load(_, offset, align))
    I32Load8S(memory_index:, offset:, align:) ->
      result.map(resolve_memory(memory_index, ctx), I32Load8S(_, offset, align))
    I32Load8U(memory_index:, offset:, align:) ->
      result.map(resolve_memory(memory_index, ctx), I32Load8U(_, offset, align))
    I32Load16S(memory_index:, offset:, align:) ->
      result.map(resolve_memory(memory_index, ctx), I32Load16S(_, offset, align))
    I32Load16U(memory_index:, offset:, align:) ->
      result.map(resolve_memory(memory_index, ctx), I32Load16U(_, offset, align))
    I64Load8S(memory_index:, offset:, align:) ->
      result.map(resolve_memory(memory_index, ctx), I64Load8S(_, offset, align))
    I64Load8U(memory_index:, offset:, align:) ->
      result.map(resolve_memory(memory_index, ctx), I64Load8U(_, offset, align))
    I64Load16S(memory_index:, offset:, align:) ->
      result.map(resolve_memory(memory_index, ctx), I64Load16S(_, offset, align))
    I64Load16U(memory_index:, offset:, align:) ->
      result.map(resolve_memory(memory_index, ctx), I64Load16U(_, offset, align))
    I64Load32S(memory_index:, offset:, align:) ->
      result.map(resolve_memory(memory_index, ctx), I64Load32S(_, offset, align))
    I64Load32U(memory_index:, offset:, align:) ->
      result.map(resolve_memory(memory_index, ctx), I64Load32U(_, offset, align))
    I32Store(memory_index:, offset:, align:) ->
      result.map(resolve_memory(memory_index, ctx), I32Store(_, offset, align))
    I64Store(memory_index:, offset:, align:) ->
      result.map(resolve_memory(memory_index, ctx), I64Store(_, offset, align))
    F32Store(memory_index:, offset:, align:) ->
      result.map(resolve_memory(memory_index, ctx), F32Store(_, offset, align))
    F64Store(memory_index:, offset:, align:) ->
      result.map(resolve_memory(memory_index, ctx), F64Store(_, offset, align))
    I32Store8(memory_index:, offset:, align:) ->
      result.map(resolve_memory(memory_index, ctx), I32Store8(_, offset, align))
    I32Store16(memory_index:, offset:, align:) ->
      result.map(resolve_memory(memory_index, ctx), I32Store16(_, offset, align))
    I64Store8(memory_index:, offset:, align:) ->
      result.map(resolve_memory(memory_index, ctx), I64Store8(_, offset, align))
    I64Store16(memory_index:, offset:, align:) ->
      result.map(resolve_memory(memory_index, ctx), I64Store16(_, offset, align))
    I64Store32(memory_index:, offset:, align:) ->
      result.map(resolve_memory(memory_index, ctx), I64Store32(_, offset, align))
    MemorySize(memory_index:) ->
      result.map(resolve_memory(memory_index, ctx), MemorySize)
    MemoryGrow(memory_index:) ->
      result.map(resolve_memory(memory_index, ctx), MemoryGrow)
    MemoryFill(memory_index:) ->
      result.map(resolve_memory(memory_index, ctx), MemoryFill)
    MemoryCopy(memory_index:) ->
      result.map(resolve_memory(memory_index, ctx), MemoryCopy)
    MemoryInit(memory_index:, data_index:) -> {
      use memory_index <- result.try(resolve_memory(memory_index, ctx))
      use data_index <- result.map(resolve_data(data_index, ctx))
      MemoryInit(memory_index:, data_index:)
    }
    DataDrop(data_index:) -> result.map(resolve_data(data_index, ctx), DataDrop)
    I32Const(val) -> I32Const(val) |> Ok
    I64Const(val) -> I64Const(val) |> Ok
    F32Const(val) -> F32Const(val) |> Ok
    F64Const(val) -> F64Const(val) |> Ok
    I32EqZ -> I32EqZ |> Ok
    I32Eq -> I32Eq |> Ok
    I32NE -> I32NE |> Ok
    I32LtS -> I32LtS |> Ok
    I32LtU -> I32LtU |> Ok
    I32GtS -> I32GtS |> Ok
    I32GtU -> I32GtU |> Ok
    I32LeS -> I32LeS |> Ok
    I32LeU -> I32LeU |> Ok
    I32GeS -> I32GeS |> Ok
    I32GeU -> I32GeU |> Ok
    I64EqZ -> I64EqZ |> Ok
    I64Eq -> I64Eq |> Ok
    I64NE -> I64NE |> Ok
    I64LtS -> I64LtS |> Ok
    I64LtU -> I64LtU |> Ok
    I64GtS -> I64GtS |> Ok
    I64GtU -> I64GtU |> Ok
    I64LeS -> I64LeS |> Ok
    I64LeU -> I64LeU |> Ok
    I64GeS -> I64GeS |> Ok
    I64GeU -> I64GeU |> Ok
    F32Eq -> F32Eq |> Ok
    F32NE -> F32NE |> Ok
    F32Lt -> F32Lt |> Ok
    F32Gt -> F32Gt |> Ok
    F32Le -> F32Le |> Ok
    F32Ge -> F32Ge |> Ok
    F64Eq -> F64Eq |> Ok
    F64NE -> F64NE |> Ok
    F64Lt -> F64Lt |> Ok
    F64Gt -> F64Gt |> Ok
    F64Le -> F64Le |> Ok
    F64Ge -> F64Ge |> Ok
    I32CntLZ -> I32CntLZ |> Ok
    I32CntTZ -> I32CntTZ |> Ok
    I32PopCnt -> I32PopCnt |> Ok
    I32Extend8S -> I32Extend8S |> Ok
    I32Extend16S -> I32Extend16S |> Ok
    I32Add -> I32Add |> Ok
    I32Sub -> I32Sub |> Ok
    I32Mul -> I32Mul |> Ok
    I32DivS -> I32DivS |> Ok
    I32DivU -> I32DivU |> Ok
    I32RemS -> I32RemS |> Ok
    I32RemU -> I32RemU |> Ok
    I32And -> I32And |> Ok
    I32Or -> I32Or |> Ok
    I32Xor -> I32Xor |> Ok
    I32ShL -> I32ShL |> Ok
    I32ShRS -> I32ShRS |> Ok
    I32ShRU -> I32ShRU |> Ok
    I32RotL -> I32RotL |> Ok
    I32RotR -> I32RotR |> Ok
    I64CntLZ -> I64CntLZ |> Ok
    I64CntTZ -> I64CntTZ |> Ok
    I64PopCnt -> I64PopCnt |> Ok
    I64Extend8S -> I64Extend8S |> Ok
    I64Extend16S -> I64Extend16S |> Ok
    I64Extend32S -> I64Extend32S |> Ok
    I64Add -> I64Add |> Ok
    I64Sub -> I64Sub |> Ok
    I64Mul -> I64Mul |> Ok
    I64DivS -> I64DivS |> Ok
    I64DivU -> I64DivU |> Ok
    I64RemS -> I64RemS |> Ok
    I64RemU -> I64RemU |> Ok
    I64And -> I64And |> Ok
    I64Or -> I64Or |> Ok
    I64Xor -> I64Xor |> Ok
    I64ShL -> I64ShL |> Ok
    I64ShRS -> I64ShRS |> Ok
    I64ShRU -> I64ShRU |> Ok
    I64RotL -> I64RotL |> Ok
    I64RotR -> I64RotR |> Ok
    F32Abs -> F32Abs |> Ok
    F32Neg -> F32Neg |> Ok
    F32Ceil -> F32Ceil |> Ok
    F32Floor -> F32Floor |> Ok
    F32Trunc -> F32Trunc |> Ok
    F32Nearest -> F32Nearest |> Ok
    F32Sqrt -> F32Sqrt |> Ok
    F32Add -> F32Add |> Ok
    F32Sub -> F32Sub |> Ok
    F32Mul -> F32Mul |> Ok
    F32Div -> F32Div |> Ok
    F32Min -> F32Min |> Ok
    F32Max -> F32Max |> Ok
    F32CopySign -> F32CopySign |> Ok
    F64Abs -> F64Abs |> Ok
    F64Neg -> F64Neg |> Ok
    F64Ceil -> F64Ceil |> Ok
    F64Floor -> F64Floor |> Ok
    F64Trunc -> F64Trunc |> Ok
    F64Nearest -> F64Nearest |> Ok
    F64Sqrt -> F64Sqrt |> Ok
    F64Add -> F64Add |> Ok
    F64Sub -> F64Sub |> Ok
    F64Mul -> F64Mul |> Ok
    F64Div -> F64Div |> Ok
    F64Min -> F64Min |> Ok
    F64Max -> F64Max |> Ok
    F64CopySign -> F64CopySign |> Ok
  }
}

fn resolve_ids_field_access(
  type_index: Index,
  field_index: Index,
  ctx: IdContext,
  constr: fn(Int, Int) -> Instruction(Int),
) -> Result(Instruction(Int), String) {
  use type_index <- result.try(resolve_type(type_index, ctx))
  use field_index <- result.map(resolve_field(
    IndexInt(type_index),
    field_index,
    ctx,
  ))
  constr(type_index, field_index)
}

fn check_block_label(
  label: Option(String),
  ctx: IdContext,
  instr_name: String,
) -> Result(Nil, String) {
  case label, ctx.labels {
    _, [] -> Error("unexpected " <> instr_name)
    Some(label), [top, ..] if label != top ->
      Error(
        instr_name <> " label " <> label <> " does not match top label " <> top,
      )
    _, _ -> Ok(Nil)
  }
}

fn resolve_ids_block_type(
  block_type: BlockType(Index),
  ctx: IdContext,
) -> Result(BlockType(Int), String) {
  case block_type {
    BlockTypeUse(type_:, params:, results:) -> {
      use TypeUse(type_:, params:, results:) <- result.map(resolve_ids_type_use(
        TypeUse(type_:, params:, results:),
        ctx,
      ))
      BlockTypeUse(type_:, params:, results:)
    }
    BlockResult(value_type) ->
      result.map(resolve_ids_value_type(value_type, ctx), BlockResult)
    BlockEmpty -> Ok(BlockEmpty)
  }
}

fn resolve_type(type_index: Index, ctx: IdContext) -> Result(Int, String) {
  case type_index {
    IndexInt(i) -> Ok(i)
    IndexId(_) -> resolve_id(type_index, ctx.types, "type")
    IndexImplicit(i) ->
      dict.get(ctx.func_types, i)
      |> result.replace_error("missing implicit func type")
  }
}

fn resolve_tag(tag_index: Index, ctx: IdContext) -> Result(Int, String) {
  resolve_id(tag_index, ctx.tags, "tag")
}

fn resolve_global(global_index: Index, ctx: IdContext) -> Result(Int, String) {
  resolve_id(global_index, ctx.globals, "global")
}

fn resolve_memory(memory_index: Index, ctx: IdContext) -> Result(Int, String) {
  resolve_id(memory_index, ctx.memories, "memory")
}

fn resolve_table(table_index: Index, ctx: IdContext) -> Result(Int, String) {
  resolve_id(table_index, ctx.tables, "table")
}

fn resolve_function(
  function_index: Index,
  ctx: IdContext,
) -> Result(Int, String) {
  resolve_id(function_index, ctx.functions, "function")
}

fn resolve_data(data_index: Index, ctx: IdContext) -> Result(Int, String) {
  resolve_id(data_index, ctx.datas, "data")
}

fn resolve_element(
  element_index: Index,
  ctx: IdContext,
) -> Result(Int, String) {
  resolve_id(element_index, ctx.elements, "element")
}

fn resolve_local(local_index: Index, ctx: IdContext) -> Result(Int, String) {
  resolve_id(local_index, ctx.locals, "local")
}

fn resolve_label(label_index: Index, ctx: IdContext) -> Result(Int, String) {
  case label_index {
    IndexInt(idx) -> Ok(idx)
    IndexId(id) ->
      list.index_map(ctx.labels, fn(l, i) { #(i, l) })
      |> list.find(fn(p) { pair.second(p) == id })
      |> result.map(pair.first)
      |> result.replace_error("label not found: " <> id)
    IndexImplicit(_) -> panic
  }
}

fn resolve_field(
  type_index: Index,
  field_index: Index,
  ctx: IdContext,
) -> Result(Int, String) {
  let assert IndexInt(type_index) = type_index
  case field_index {
    IndexInt(idx) -> Ok(idx)
    IndexId(id) ->
      case dict.get(ctx.fields, #(type_index, id)) {
        Ok(idx) -> Ok(idx)
        Error(_) -> Error("field not found: " <> id)
      }
    IndexImplicit(_) -> panic
  }
}

fn resolve_id(
  index: Index,
  ids: Dict(String, Int),
  where: String,
) -> Result(Int, String) {
  case index {
    IndexInt(idx) -> Ok(idx)
    IndexId(id) ->
      case dict.get(ids, id) {
        Ok(index) -> Ok(index)
        Error(_) -> Error(where <> " not found: " <> id)
      }
    IndexImplicit(_) -> panic as { "implicit index for " <> where }
  }
}
