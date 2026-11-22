import gl_wat as w
import gleam/list
import gleam/option.{None, Some}
import gleam/pair
import gleam/result
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
  let expected = string.split(valid_ints, " ") |> list.map(w.Integer)
  let actual = list.map(res, fn(token) { token.value })
  assert actual == expected
}

pub fn float_lexer_test() {
  let l = w.lexer()
  let valid_floats =
    "0.0 1_000.000_1 3. 4e-7 0xf.0e -0xAAA_BBBp-3 +inf -nan nan:0x1F"
  let assert Ok(res) = lexer.run(valid_floats, l)
  let expected = string.split(valid_floats, " ") |> list.map(w.Float)
  let actual = list.map(res, fn(token) { token.value })
  assert expected == actual
  let assert Error(_) = lexer.run("0x0.eP", l)
  let assert Error(_) = lexer.run(".3e3", l)
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

pub fn id_lexer_test() {
  let l = w.lexer()
  assert_lex_self_as(l, "$id", w.Id)
  assert_lex_self_as(l, "$info+test@example.com", w.Id)
  assert_lex_self_as(l, "$a/b#c\\d|c^$", w.Id)
  assert_lex_self_as(l, "$\"\"", w.Id)
  assert_lex_self_as(l, "$\"\\u{0x263a}\"", w.Id)
}

pub fn parse_func_empty_test() {
  let assert Ok(w.Function(
    None,
    w.IndexImplicit(w.ImplicitFunc([], [])),
    [],
    [],
    [],
    [],
  )) = parse_func("(func)")
}

pub fn parse_func_invalid_test() {
  let assert Error(_) = parse_func("(type)")
}

pub fn parse_func_one_param_test() {
  let assert Ok(w.Function(params: [w.Param(None, w.I32)], ..)) =
    parse_func("(func (param i32))")
}

pub fn parse_func_two_params_test() {
  let assert Ok(w.Function(
    params: [w.Param(None, w.I32), w.Param(None, w.I32)],
    ..,
  )) = parse_func("(func (param i32) (param i32))")

  let assert Ok(w.Function(
    params: [w.Param(None, w.F32), w.Param(None, w.F32)],
    ..,
  )) = parse_func("(func (param f32 f32))")
}

pub fn parse_func_param_with_id_test() {
  let assert Ok(w.Function(params: [w.Param(Some("$n"), w.I64)], ..)) =
    parse_func("(func (param $n i64))")

  let assert Error(_) = parse_func("(func (param $n i64 i64))")
}

pub fn parse_func_one_result_test() {
  let assert Ok(w.Function(results: [w.F64], ..)) =
    parse_func("(func (result f64))")
}

pub fn parse_func_two_results_test() {
  let assert Ok(w.Function(
    params: [w.Param(None, w.I32)],
    results: [w.F64, w.I32],
    ..,
  )) = parse_func("(func (param i32) (result f64) (result i32))")
  let assert Error(_) =
    parse_func("(func (result f64) (param i32) (result i32))")
}

pub fn parse_func_with_id_test() {
  let assert Ok(w.Function(id: Some("$f"), ..)) = parse_func("(func $f)")

  let assert Ok(w.Function(id: Some("$f"), results: [w.I32], ..)) =
    parse_func("(func $f (result i32))")
}

pub fn parse_func_with_local_test() {
  let assert Ok(w.Function(locals: [w.Param(Some("$i"), w.I32)], ..)) =
    parse_func("(func (local $i i32))")
}

pub fn parse_func_with_two_locals_test() {
  let assert Ok(w.Function(
    locals: [w.Param(Some("$i"), w.I32), w.Param(Some("$j"), w.I32)],
    ..,
  )) = parse_func("(func (local $i i32) (local $j i32))")

  let assert Ok(w.Function(
    locals: [w.Param(None, w.I32), w.Param(None, w.I32)],
    ..,
  )) = parse_func("(func (local i32 i32))")

  let assert Error(_) = parse_func("(func (local $i i32 i32))")
}

pub fn parse_func_with_type_test() {
  let assert Ok(w.Function(type_: w.IndexInt("3"), ..)) =
    parse_func("(func (type 3))")

  let assert Ok(w.Function(type_: w.IndexId("$ft"), ..)) =
    parse_func("(func (type $ft))")

  let assert Error(_) = parse_func("(func (type 3 4))")
  let assert Error(_) = parse_func("(func (param i32) (type 3))")
  let assert Error(_) = parse_func("(func (type $ft) (type 3))")
}

pub fn parse_func_single_instr_test() {
  let assert Ok(w.Function(expr: [w.Unreachable], ..)) =
    parse_func("(func (unreachable))")

  let assert Ok(w.Function(expr: [w.Unreachable], ..)) =
    parse_func("(func unreachable)")
}

pub fn parse_func_multiple_instr_test() {
  let assert Ok(w.Function(expr: [w.Unreachable, w.Unreachable], ..)) =
    parse_func("(func (unreachable) (unreachable))")

  let assert Ok(w.Function(expr: [w.Unreachable, w.Unreachable], ..)) =
    parse_func("(func unreachable unreachable)")
}

pub fn parse_func_instr_immediate_arg_test() {
  let assert Ok(w.Function(expr: [w.LocalGet(w.IndexId("$n"))], ..)) =
    parse_func("(func (local.get $n))")
}

pub fn parse_func_instr_folded_arg_test() {
  let assert Ok(w.Function(
    expr: [w.LocalGet(w.IndexId("$m")), w.LocalSet(w.IndexId("$n"))],
    ..,
  )) = parse_func("(func (local.set $n (local.get $m)))")
}

pub fn parse_func_nested_folded_instr_test() {
  let assert Ok(w.Function(
    expr: [
      w.I32Const("2"),
      w.I32Const("3"),
      w.I32Mul,
      w.I32Const("5"),
      w.I32Add,
    ],
    ..,
  )) =
    parse_func(
      "(func (i32.add (i32.mul (i32.const 2) (i32.const 3)) (i32.const 5)))",
    )
}

pub fn parse_empty_block_test() {
  let assert Ok(w.Function(expr: [w.Block(None, w.BlockEmpty)], ..)) =
    parse_func("(func block)")
}

pub fn parse_block_result_test() {
  let assert Ok(w.Function(
    expr: [w.Block(Some("$b"), w.BlockResult(w.I32)), w.End(None)],
    ..,
  )) = parse_func("(func (block $b (result i32)))")
}

pub fn parse_block_multiple_results_test() {
  let assert Ok(w.Function(
    expr: [
      w.Block(
        Some("$b"),
        w.BlockTypeUse(
          w.IndexImplicit(w.ImplicitFunc([], [w.I32, w.F32])),
          [],
          [w.I32, w.F32],
        ),
      ),
      w.End(None),
    ],
    ..,
  )) = parse_func("(func (block $b (result i32) (result f32)))")
}

pub fn parse_block_params_test() {
  let assert Ok(w.Function(
    expr: [
      w.Block(
        Some("$b"),
        w.BlockTypeUse(
          w.IndexImplicit(w.ImplicitFunc([w.I32, w.I32], [w.F32])),
          [w.Param(None, w.I32), w.Param(None, w.I32)],
          [w.F32],
        ),
      ),
      w.End(None),
    ],
    ..,
  )) = parse_func("(func (block $b (param i32 i32) (result f32)))")
}

pub fn parse_block_type_test() {
  let assert Ok(w.Function(
    expr: [w.Block(None, w.BlockTypeUse(w.IndexInt("0"), [], [])), w.End(None)],
    ..,
  )) = parse_func("(func (block (type 0)))")
}

pub fn parse_block_child_exprs_test() {
  let assert Ok(w.Function(
    expr: [w.Block(None, w.BlockEmpty), w.I32Const("3"), w.Drop, w.End(None)],
    ..,
  )) = parse_func("(func (block (i32.const 3) drop))")
}

pub fn parse_block_unfolded_test() {
  let assert Ok(w.Function(
    expr: [
      w.Block(None, w.BlockResult(w.I32)),
      w.I32Const("3"),
      w.Drop,
      w.End(None),
    ],
    ..,
  )) = parse_func("(func block (result i32) i32.const 3 drop end)")
}

pub fn if_else_test() {
  let assert Ok(w.Function(
    expr: [
      w.I32Const("0"),
      w.If(None, w.BlockResult(w.I32)),
      w.I32Const("1"),
      w.Else(None),
      w.I32Const("2"),
      w.End(None),
    ],
    ..,
  )) =
    parse_func(
      "(func (if (result i32) (i32.const 0) (then (i32.const 1)) (else (i32.const 2))))",
    )
}

pub fn if_no_else_test() {
  let assert Ok(w.Function(
    expr: [w.If(None, w.BlockEmpty), w.Unreachable, w.End(None)],
    ..,
  )) = parse_func("(func (if (then (unreachable))))")
}

pub fn if_else_flat_test() {
  let assert Ok(w.Function(
    expr: [
      w.If(Some("$a"), w.BlockResult(w.I32)),
      w.I32Const("3"),
      w.Else(Some("$a")),
      w.I32Const("4"),
      w.End(Some("$a")),
    ],
    ..,
  )) =
    parse_func(
      "(func if $a (result i32) (i32.const 3) else $a i32.const 4 end $a)",
    )
}

pub fn br_on_cast_test() {
  let assert Ok(w.Function(
    expr: [
      w.BranchOnCast(
        w.IndexInt("1"),
        w.Nullable(w.HeapAny),
        w.Nullable(w.HeapIndex(w.IndexInt("0"))),
      ),
    ],
    ..,
  )) = parse_func("(func br_on_cast 1 anyref (ref null 0))")
}

pub fn call_indirect_test() {
  let assert Ok(w.Function(
    expr: [
      w.I32Const("0"),
      w.CallIndirect(w.IndexInt("0"), w.IndexId("$t"), [w.I32, w.I32], [w.I32]),
    ],
    ..,
  )) =
    parse_func(
      "(func (call_indirect (type $t) (param i32 i32) (result i32) (i32.const 0)))",
    )
}

pub fn flat_call_indirect_test() {
  let assert Ok(w.Function(
    expr: [
      w.I32Const("0"),
      w.CallIndirect(w.IndexInt("0"), w.IndexId("$t"), [w.I32, w.I32], [w.I32]),
      w.I32Const("1"),
      w.I32Const("2"),
    ],
    ..,
  )) =
    parse_func(
      "(func i32.const 0 call_indirect 0 (type $t) (param i32 i32) (result i32) i32.const 1 i32.const 2)",
    )
}

pub fn call_indirect_disallows_param_labels_test() {
  let assert Error(_) = parse_func("(func call_indirect (param $a i32))")
}

pub fn select_test() {
  let assert Ok(w.Function(
    expr: [w.I32Const("5"), w.Select([w.I32, w.F64])],
    ..,
  )) = parse_func("(func (select (result i32) (result f64) (i32.const 5)))")
}

pub fn parse_module_test() {
  let assert Ok(w.Module(Some("$mod"), functions: [], ..)) =
    parse_module("(module $mod)")
}

pub fn parse_module_global_test() {
  let assert Ok(w.Module(
    globals: [
      w.Global(Some("$g"), w.Mutable, w.I32, [w.I32Const("1")]),
      w.Global(None, w.Immutable, w.I64, [w.I64Const("3")]),
    ],
    ..,
  )) =
    parse_module(
      "(module (global $g (mut i32) (i32.const 1)) (global i64 i64.const 3))",
    )
}

pub fn parse_module_global_inline_import_export_test() {
  let assert Ok(w.Module(
    imports: [w.ImportGlobal("mod", "glob", None, w.Immutable, w.F64)],
    exports: [w.ExportGlobal("ex", w.IndexInt("0"))],
    ..,
  )) = parse_module("(global (export \"ex\") (import \"mod\" \"glob\") f64)")
}

pub fn parse_module_type_test() {
  let assert Ok(w.Module(
    types: [
      w.RecType([
        w.TypeDef(
          id: Some("$s"),
          sub: w.Final,
          super: [],
          def: w.Struct([
            w.Field(
              id: Some("$a"),
              field_type: w.ValueField(w.Immutable, w.I32),
            ),
            w.Field(
              id: Some("$b"),
              field_type: w.ValueField(w.Immutable, w.F32),
            ),
          ]),
        ),
      ]),
    ],
    ..,
  )) = parse_module("(type $s (struct (field $a i32) (field $b f32)))")
}

pub fn parse_module_func_implicit_type_test() {
  let assert Ok(w.Module(
    types: [
      w.RecType([
        w.TypeDef(
          id: None,
          sub: w.Final,
          super: [],
          def: w.Func([w.Param(None, w.I32)], [w.I32]),
        ),
      ]),
    ],
    functions: [
      w.Function(type_: w.IndexImplicit(w.ImplicitFunc([w.I32], [w.I32])), ..),
    ],
    ..,
  )) = parse_module("(func (param $a i32) (result i32))")
}

pub fn parse_module_memory_test() {
  let assert Ok(w.Module(
    memories: [w.Memory(None, w.Address32, w.Limits("4", Some("16")))],
    ..,
  )) = parse_module("(memory 4 16)")
}

pub fn parse_module_memory_with_data_test() {
  let assert Ok(w.Module(
    memories: [w.Memory(Some("$d"), w.Address32, w.Limits("1", Some("1")))],
    datas: [
      w.ActiveData(None, <<"hello there">>, w.IndexInt("0"), [w.I32Const("0")]),
    ],
    ..,
  )) = parse_module("(memory $d i32 (data \"hello there\"))")
}

pub fn parse_module_second_memory_with_data_test() {
  let assert Ok(w.Module(
    memories: [
      w.Memory(None, w.Address64, w.Limits("4", None)),
      w.Memory(Some("$d"), w.Address32, w.Limits("1", Some("1"))),
    ],
    datas: [
      w.ActiveData(None, <<"hello there">>, w.IndexInt("1"), [w.I32Const("0")]),
    ],
    ..,
  )) = parse_module("(memory i64 4) (memory $d i32 (data \"hello there\"))")
}

pub fn parse_module_memory_inline_import_export_test() {
  let assert Ok(w.Module(
    imports: [
      w.ImportMemory(
        module_name: "mod",
        import_name: "imp",
        id: Some("$mem"),
        address_type: w.Address32,
        limits: w.Limits("4", Some("16")),
      ),
    ],
    exports: [w.ExportMemory(name: "exp", memory_index: w.IndexInt("0"))],
    ..,
  )) =
    parse_module("(memory $mem (export \"exp\") (import \"mod\" \"imp\") 4 16)")
}

pub fn parse_module_data_test() {
  let assert Ok(w.Module(
    memories: [w.Memory(None, w.Address32, w.Limits("1", None))],
    datas: [w.ActiveData(None, <<"data">>, w.IndexInt("0"), [w.I32Const("12")])],
    ..,
  )) = parse_module("(memory 1) (data (memory 0) (i32.const 12) \"data\")")
}

pub fn parse_module_table_test() {
  let assert Ok(w.Module(
    tables: [
      w.Table(
        Some("$t"),
        w.Address32,
        w.Limits("8", None),
        w.Nullable(w.HeapAny),
        [w.RefNull(w.HeapAny)],
      ),
    ],
    ..,
  )) = parse_module("(table $t 8 anyref)")
}

pub fn parse_module_table_non_null_requires_initializer_test() {
  let assert Ok(_) =
    parse_module("(table $t 8 (ref any))")
    |> errors_include("missing initialization expression for non-null reftype")
}

pub fn parse_module_table_initializer_test() {
  let assert Ok(w.Module(
    tables: [
      w.Table(
        None,
        w.Address64,
        w.Limits("8", Some("16")),
        w.NonNull(w.HeapFunc),
        [w.RefFunc(w.IndexId("$f"))],
      ),
    ],
    ..,
  )) = parse_module("(table i64 8 16 (ref func) ref.func $f)")
}

pub fn parse_module_table_inline_elem_test() {
  let assert Ok(w.Module(
    tables: [
      _,
      w.Table(
        None,
        w.Address32,
        w.Limits("2", Some("2")),
        w.Nullable(w.HeapFunc),
        [w.RefNull(w.HeapFunc)],
      ),
    ],
    elements: [
      w.ActiveElement(
        None,
        w.NonNull(w.HeapFunc),
        [[w.RefFunc(w.IndexId("$f"))], [w.RefFunc(w.IndexId("$g"))]],
        w.IndexInt("1"),
        [w.I32Const("0")],
      ),
    ],
    ..,
  )) = parse_module("(table 8 anyref) (table funcref (elem func $f $g))")
}

pub fn parse_module_table_inline_import_export_test() {
  let assert Ok(w.Module(
    imports: [
      w.ImportTable(
        module_name: "mod",
        import_name: "imp",
        id: Some("$tbl"),
        address_type: w.Address32,
        limits: w.Limits("4", Some("16")),
        ref_type: w.Nullable(w.HeapFunc),
      ),
    ],
    exports: [w.ExportTable(name: "exp", table_index: w.IndexInt("0"))],
    ..,
  )) =
    parse_module(
      "(table $tbl (export \"exp\") (import \"mod\" \"imp\") 4 16 funcref)",
    )
}

pub fn parse_module_function_inline_import_export_test() {
  let assert Ok(w.Module(
    imports: [
      w.ImportFunction(
        module_name: "mod",
        import_name: "func",
        id: None,
        type_: w.IndexImplicit(w.ImplicitFunc([], [])),
        params: [],
        results: [],
      ),
    ],
    functions: [
      w.Function(
        id: None,
        type_: w.IndexImplicit(w.ImplicitFunc([], [w.I32])),
        params: [],
        results: [w.I32],
        locals: [],
        expr: [w.I32Const("0")],
      ),
    ],
    exports: [
      w.ExportFunction(name: "a", function_index: w.IndexInt("0")),
      w.ExportFunction(name: "b", function_index: w.IndexInt("1")),
    ],
    ..,
  )) =
    parse_module(
      "(func (export \"a\") (import \"mod\" \"func\")) (func (export \"b\") (result i32) i32.const 0)",
    )
}

pub fn parse_module_declarative_element_test() {
  let assert Ok(w.Module(
    elements: [
      w.DeclarativeElement(
        id: Some("$e"),
        ref_type: w.NonNull(w.HeapFunc),
        elems: [[w.RefFunc(w.IndexId("$f"))], [w.RefFunc(w.IndexId("$g"))]],
      ),
    ],
    ..,
  )) =
    parse_module(
      "(elem $e declare (ref func) (item ref.func $f) (item ref.func $g))",
    )
}

pub fn parse_module_legacy_active_element_test() {
  let assert Ok(w.Module(
    elements: [
      w.ActiveElement(
        id: None,
        ref_type: w.NonNull(w.HeapFunc),
        elems: [
          [w.RefFunc(w.IndexInt("0"))],
          [w.RefFunc(w.IndexInt("1"))],
          [w.RefFunc(w.IndexInt("2"))],
        ],
        table_index: w.IndexInt("0"),
        offset: [w.I32Const("8")],
      ),
    ],
    ..,
  )) = parse_module("(elem (i32.const 8) 0 1 2)")

  let assert Error(_) = parse_module("(elem (table 1) (i32.const 8) 0 1 2)")
  let assert Ok(_) = parse_module("(elem (table 1) (i32.const 8) func 0 1 2)")
}

pub fn parse_module_start_function_test() {
  let assert Ok(w.Module(start: Some(w.Start(w.IndexId("$start"))), ..)) =
    parse_module("(start $start)")
  let assert Error(_) = parse_module("(start $start) (start $again)")
}

pub fn parse_module_exports_test() {
  let assert Ok(w.Module(
    exports: [
      w.ExportMemory("mem", w.IndexInt("0")),
      w.ExportFunction("fn", w.IndexId("$f")),
    ],
    ..,
  )) = parse_module("(export \"mem\" (memory 0)) (export \"fn\" (func $f))")
}

pub fn parse_module_tags_test() {
  let assert Ok(w.Module(
    types: [
      w.RecType([
        w.TypeDef(None, w.Final, [], w.Func([w.Param(None, w.I32)], [])),
      ]),
    ],
    tags: [
      w.Tag(
        Some("$t"),
        w.IndexImplicit(w.ImplicitFunc([w.I32], [])),
        [w.Param(None, w.I32)],
        [],
      ),
    ],
    ..,
  )) = parse_module("(tag $t (param i32))")
}

pub fn no_imports_after_non_import_declarations_test() {
  let msg = "imports must occur before all non-import definitions"
  let assert Ok(_) =
    parse_module("(tag $t) (import \"mod\" \"tag\" (tag $u))")
    |> errors_include(msg)
  let assert Ok(_) =
    parse_module("(tag $t) (tag $u (import \"mod\" \"tag\"))")
    |> errors_include(msg)
  let assert Ok(_) =
    parse_module("(table 3 funcref) (func $u (import \"mod\" \"fun\"))")
    |> errors_include(msg)
}

pub fn resolve_func_ids_test() {
  let assert Ok(w.Module(
    functions: [
      w.Function(id: Some("$f"), expr: [w.Call(w.IndexInt("1"))], ..),
      w.Function(id: Some("$g"), expr: [w.Call(w.IndexInt("0"))], ..),
    ],
    ..,
  )) = parse_and_resolve_module("(func $f (call $g)) (func $g (call $f))")
  let assert Error("function not found: $g") =
    parse_and_resolve_module("(func $f (call $g))")
}

pub fn parse_module_func_implicit_type_matches_existing_test() {
  let assert Ok(w.Module(
    types: [
      w.RecType([
        w.TypeDef(
          id: None,
          sub: w.Final,
          super: [],
          def: w.Func([w.Param(Some("$x"), w.I32)], [w.I32]),
        ),
      ]),
    ],
    functions: [w.Function(type_: w.IndexInt("0"), ..)],
    ..,
  )) =
    parse_and_resolve_module(
      "(type (func (param $x i32) (result i32)))(func (param $a i32) (result i32))",
    )
}

pub fn resolve_local_ids_test() {
  let assert Ok(w.Module(
    functions: [
      w.Function(
        expr: [w.LocalGet(w.IndexInt("0")), w.LocalSet(w.IndexInt("1"))],
        ..,
      ),
    ],
    ..,
  )) =
    parse_and_resolve_module(
      "(func $f (param $a i32) (local $b i32) (local.get $a) (local.set $b))",
    )
}

pub fn resolve_branch_ids_test() {
  let assert Ok(w.Module(
    functions: [
      w.Function(
        expr: [
          w.Loop(Some("$outer"), w.BlockEmpty),
          w.Block(Some("$inner"), w.BlockEmpty),
          w.Branch(w.IndexInt("1")),
          w.Branch(w.IndexInt("0")),
          w.End(None),
          w.Branch(w.IndexInt("0")),
          w.End(None),
        ],
        ..,
      ),
    ],
    ..,
  )) =
    parse_and_resolve_module(
      "(func (loop $outer (block $inner (br $outer) (br $inner)) (br $outer)))",
    )
}

pub fn resolve_struct_field_ids_test() {
  let assert Ok(w.Module(
    functions: [
      w.Function(expr: [w.StructGet(w.IndexInt("0"), w.IndexInt("2"))], ..),
    ],
    ..,
  )) =
    parse_and_resolve_module(
      "(type (struct (field i32 i32) (field $c i32))) (func (struct.get 0 $c))",
    )
}

pub fn resolve_block_implicit_type_test() {
  let assert Ok(w.Module(
    types: [_, t1, t2],
    functions: [
      w.Function(
        expr: [
          w.Block(
            Some("$b"),
            w.BlockTypeUse(
              w.IndexInt(idx),
              [w.Param(Some("$a"), w.I32), w.Param(Some("$b"), w.I32)],
              [w.F32],
            ),
          ),
          w.End(None),
        ],
        ..,
      ),
    ],
    ..,
  )) =
    parse_and_resolve_module(
      "(type (func (param i64))) (func (block $b (param $a i32) (param $b i32) (result f32)))",
    )
  case idx {
    "1" -> {
      assert w.func_type([], []) == t2
      assert w.func_type([w.Param(None, w.I32), w.Param(None, w.I32)], [w.F32])
        == t1
    }
    "2" -> {
      assert w.func_type([], []) == t1
      assert w.func_type([w.Param(None, w.I32), w.Param(None, w.I32)], [w.F32])
        == t2
    }
    _ -> panic as { "invalid type index " <> idx }
  }
}

pub fn resolve_nested_block_implicit_type_test() {
  let assert Ok(w.Module(
    types: [_, t1, t2],
    functions: [
      w.Function(
        expr: [
          w.Block(
            Some("$b"),
            w.BlockTypeUse(
              w.IndexInt(idx0),
              [w.Param(None, w.I32), w.Param(None, w.I32)],
              [],
            ),
          ),
          w.Loop(
            Some("$l"),
            w.BlockTypeUse(
              w.IndexInt(idx1),
              [w.Param(None, w.F32), w.Param(None, w.F32)],
              [],
            ),
          ),
          w.End(None),
          w.End(None),
        ],
        ..,
      ),
    ],
    ..,
  )) =
    parse_and_resolve_module(
      "(type (func (param i64) (result i64))) (func (type 0) (block $b (param i32 i32) (loop $l (param f32 f32))))",
    )
  case idx0 {
    "1" -> {
      assert idx1 == "2"
      assert w.func_type([w.Param(None, w.I32), w.Param(None, w.I32)], []) == t1
      assert w.func_type([w.Param(None, w.F32), w.Param(None, w.F32)], []) == t2
    }
    "2" -> {
      assert idx1 == "1"
      assert w.func_type([w.Param(None, w.I32), w.Param(None, w.I32)], []) == t2
      assert w.func_type([w.Param(None, w.F32), w.Param(None, w.F32)], []) == t1
    }
    _ -> panic as { "invalid type index " <> idx0 }
  }
}

fn assert_lex_self_as(l, input, fun) {
  let assert Ok([token]) = lexer.run(input, l)
  assert token.lexeme == input
  assert token.value == fun(input)
}

fn errors_include(res: Result(r, List(nibble.DeadEnd(a, b))), str) {
  case res {
    Error(errors) ->
      list.find(errors, fn(err) { err.problem == nibble.Custom(str) })
    _ -> Error(Nil)
  }
}

fn parse_func(str) {
  let assert Ok(tokens) = lexer.run(str, w.lexer())
  let tokens = w.simplify(tokens)
  nibble.run(tokens, w.module_parser())
  |> result.map(pair.first)
  |> result.map(fn(mod) {
    let assert w.Module(
      id: None,
      types: _,
      imports: [],
      tags: [],
      globals: [],
      memories: [],
      tables: [],
      functions: [func],
      datas: [],
      elements: [],
      start: None,
      exports: [],
    ) = mod
    func
  })
}

fn parse_module(str: String) {
  let assert Ok(tokens) = lexer.run(str, w.lexer())
  let tokens = w.simplify(tokens)
  nibble.run(tokens, w.module_parser()) |> result.map(pair.first)
}

fn parse_and_resolve_module(str: String) {
  let assert Ok(tokens) = lexer.run(str, w.lexer())
  let tokens = w.simplify(tokens)
  let assert Ok(#(mod, ctx)) = nibble.run(tokens, w.module_parser())
  w.resolve_indices(mod, ctx)
}
