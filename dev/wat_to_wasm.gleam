import gl_wasm/wasm as w
import gl_wat as t
import gleam/list
import gleam/option.{type Option, None}
import gleam/pair
import gleam/result

pub fn compile_module(mod: t.Module(Int)) -> Result(w.ModuleBuilder, String) {
  let mb = w.create_module_builder(mod.id)
  // register types
  use mb <- result.try(
    list.map(mod.types, map_rec_type)
    |> list.try_fold(mb, fn(mb, group) {
      w.add_sub_type_group(mb, group) |> result.map(pair.first)
    }),
  )
  // add functions
  let sigs = list.map(mod.functions, map_function_signature)
  use #(mb, builders) <- result.try(w.create_function_builders(mb, sigs))
  let builders = list.zip(mod.functions, builders)
  use mb <- result.try(list.try_fold(builders, mb, compile_function))
  // add exports
  use mb <- result.try(
    list.map(mod.exports, map_export)
    |> list.try_fold(mb, fn(mb, export) { w.add_export(mb, export) }),
  )
  Ok(mb)
}

fn map_export(export: t.Export(Int)) -> w.Export {
  case export {
    t.ExportTag(name:, tag_index:) -> todo
    t.ExportGlobal(name:, global_index:) -> todo
    t.ExportMemory(name:, memory_index:) -> todo
    t.ExportTable(name:, table_index:) -> todo
    t.ExportFunction(name:, function_index:) ->
      w.ExportFunction(name, function_index)
  }
}

fn compile_function(
  mb: w.ModuleBuilder,
  func: #(t.Function(Int), w.CodeBuilder),
) -> Result(w.ModuleBuilder, String) {
  let #(func, fb) = func
  use fb <- result.try(
    list.try_fold(func.locals, fb, fn(fb, local) {
      w.add_local(fb, map_param(local), local.id) |> result.map(pair.first)
    }),
  )
  use fb <- result.try(
    list.try_fold(func.expr, fb, fn(fb, instr) {
      w.add_instruction(fb, map_instruction(instr))
    }),
  )
  use fb <- result.try(w.add_instruction(fb, w.End))
  w.finalize_function(mb, fb)
}

fn map_instruction(instr: t.Instruction(Int)) -> w.Instruction {
  case instr {
    t.Unreachable -> todo
    t.Nop -> todo
    t.Block(label:, block_type:) -> todo
    t.Loop(label:, block_type:) -> todo
    t.If(label:, block_type:) -> todo
    t.Else(label:) -> todo
    t.End(label:) -> w.End
    t.Branch(label_index:) -> todo
    t.BranchIf(label_index:) -> todo
    t.BranchTable(label_indices:, default_label_index:) -> todo
    t.Return -> todo
    t.Call(function_index:) -> todo
    t.CallIndirect(table_index:, type_:, params:, results:) -> todo
    t.ReturnCall(function_index:) -> todo
    t.ReturnCallIndirect(table_index:, type_:, params:, results:) -> todo
    t.CallRef(type_index:) -> todo
    t.ReturnCallRef(type_index:) -> todo
    t.BranchOnNull(label_index:) -> todo
    t.BranchOnNonNull(label_index:) -> todo
    t.BranchOnCast(label_index:, from_ref_type:, to_ref_type:) -> todo
    t.BranchOnCastFail(label_index:, from_ref_type:, to_ref_type:) -> todo
    t.RefNull(heap_type:) -> todo
    t.RefFunc(function_index:) -> todo
    t.RefIsNull -> todo
    t.RefAsNonNull -> todo
    t.RefEq -> todo
    t.RefTest(ref_type:) -> todo
    t.RefCast(ref_type:) -> todo
    t.RefI31 -> todo
    t.I31GetS -> todo
    t.I31GetU -> todo
    t.StructNew(type_index:) -> todo
    t.StructNewDefault(type_index:) -> todo
    t.StructGet(type_index:, field_index:) -> todo
    t.StructGetS(type_index:, field_index:) -> todo
    t.StructGetU(type_index:, field_index:) -> todo
    t.StructSet(type_index:, field_index:) -> todo
    t.ArrayNew(type_index:) -> todo
    t.ArrayNewDefault(type_index:) -> todo
    t.ArrayNewFixed(type_index:, size:) -> todo
    t.ArrayNewData(type_index:, data_index:) -> todo
    t.ArrayNewElem(type_index:, element_index:) -> todo
    t.ArrayGet(type_index:) -> todo
    t.ArrayGetS(type_index:) -> todo
    t.ArrayGetU(type_index:) -> todo
    t.ArraySet(type_index:) -> todo
    t.ArrayLen -> todo
    t.ArrayFill(type_index:) -> todo
    t.ArrayCopy(type_index:, source_type_index:) -> todo
    t.ArrayInitData(type_index:, data_index:) -> todo
    t.ArrayInitElem(type_index:, element_index:) -> todo
    t.AnyConvertExtern -> todo
    t.ExternConvertAny -> todo
    t.Drop -> todo
    t.Select(value_types) -> todo
    t.LocalGet(local_index:) -> w.LocalGet(local_index)
    t.LocalSet(local_index:) -> todo
    t.LocalTee(local_index:) -> todo
    t.GlobalGet(global_index:) -> todo
    t.GlobalSet(global_index:) -> todo
    t.TableGet(table_index:) -> todo
    t.TableSet(table_index:) -> todo
    t.TableSize(table_index:) -> todo
    t.TableGrow(table_index:) -> todo
    t.TableFill(table_index:) -> todo
    t.TableCopy(table_index:, source_table_index:) -> todo
    t.TableInit(table_index:, element_index:) -> todo
    t.ElemDrop(element_index:) -> todo
    t.I32Load(memory_index:, offset:, align:) -> todo
    t.I64Load(memory_index:, offset:, align:) -> todo
    t.F32Load(memory_index:, offset:, align:) -> todo
    t.F64Load(memory_index:, offset:, align:) -> todo
    t.I32Load8S(memory_index:, offset:, align:) -> todo
    t.I32Load8U(memory_index:, offset:, align:) -> todo
    t.I32Load16S(memory_index:, offset:, align:) -> todo
    t.I32Load16U(memory_index:, offset:, align:) -> todo
    t.I64Load8S(memory_index:, offset:, align:) -> todo
    t.I64Load8U(memory_index:, offset:, align:) -> todo
    t.I64Load16S(memory_index:, offset:, align:) -> todo
    t.I64Load16U(memory_index:, offset:, align:) -> todo
    t.I64Load32S(memory_index:, offset:, align:) -> todo
    t.I64Load32U(memory_index:, offset:, align:) -> todo
    t.I32Store(memory_index:, offset:, align:) -> todo
    t.I64Store(memory_index:, offset:, align:) -> todo
    t.F32Store(memory_index:, offset:, align:) -> todo
    t.F64Store(memory_index:, offset:, align:) -> todo
    t.I32Store8(memory_index:, offset:, align:) -> todo
    t.I32Store16(memory_index:, offset:, align:) -> todo
    t.I64Store8(memory_index:, offset:, align:) -> todo
    t.I64Store16(memory_index:, offset:, align:) -> todo
    t.I64Store32(memory_index:, offset:, align:) -> todo
    t.MemorySize(memory_index:) -> todo
    t.MemoryGrow(memory_index:) -> todo
    t.MemoryFill(memory_index:) -> todo
    t.MemoryCopy(memory_index:) -> todo
    t.MemoryInit(memory_index:, data_index:) -> todo
    t.DataDrop(data_index:) -> todo
    t.I32Const(value:) -> todo
    t.I64Const(value:) -> todo
    t.F32Const(value:) -> todo
    t.F64Const(value:) -> todo
    t.I32EqZ -> todo
    t.I32Eq -> todo
    t.I32NE -> todo
    t.I32LtS -> todo
    t.I32LtU -> todo
    t.I32GtS -> todo
    t.I32GtU -> todo
    t.I32LeS -> todo
    t.I32LeU -> todo
    t.I32GeS -> todo
    t.I32GeU -> todo
    t.I64EqZ -> todo
    t.I64Eq -> todo
    t.I64NE -> todo
    t.I64LtS -> todo
    t.I64LtU -> todo
    t.I64GtS -> todo
    t.I64GtU -> todo
    t.I64LeS -> todo
    t.I64LeU -> todo
    t.I64GeS -> todo
    t.I64GeU -> todo
    t.F32Eq -> todo
    t.F32NE -> todo
    t.F32Lt -> todo
    t.F32Gt -> todo
    t.F32Le -> todo
    t.F32Ge -> todo
    t.F64Eq -> todo
    t.F64NE -> todo
    t.F64Lt -> todo
    t.F64Gt -> todo
    t.F64Le -> todo
    t.F64Ge -> todo
    t.I32CntLZ -> todo
    t.I32CntTZ -> todo
    t.I32PopCnt -> todo
    t.I32Extend8S -> todo
    t.I32Extend16S -> todo
    t.I32Add -> w.I32Add
    t.I32Sub -> todo
    t.I32Mul -> todo
    t.I32DivS -> todo
    t.I32DivU -> todo
    t.I32RemS -> todo
    t.I32RemU -> todo
    t.I32And -> todo
    t.I32Or -> todo
    t.I32Xor -> todo
    t.I32ShL -> todo
    t.I32ShRS -> todo
    t.I32ShRU -> todo
    t.I32RotL -> todo
    t.I32RotR -> todo
    t.I64CntLZ -> todo
    t.I64CntTZ -> todo
    t.I64PopCnt -> todo
    t.I64Extend8S -> todo
    t.I64Extend16S -> todo
    t.I64Extend32S -> todo
    t.I64Add -> todo
    t.I64Sub -> todo
    t.I64Mul -> todo
    t.I64DivS -> todo
    t.I64DivU -> todo
    t.I64RemS -> todo
    t.I64RemU -> todo
    t.I64And -> todo
    t.I64Or -> todo
    t.I64Xor -> todo
    t.I64ShL -> todo
    t.I64ShRS -> todo
    t.I64ShRU -> todo
    t.I64RotL -> todo
    t.I64RotR -> todo
    t.F32Abs -> todo
    t.F32Neg -> todo
    t.F32Ceil -> todo
    t.F32Floor -> todo
    t.F32Trunc -> todo
    t.F32Nearest -> todo
    t.F32Sqrt -> todo
    t.F32Add -> todo
    t.F32Sub -> todo
    t.F32Mul -> todo
    t.F32Div -> todo
    t.F32Min -> todo
    t.F32Max -> todo
    t.F32CopySign -> todo
    t.F64Abs -> todo
    t.F64Neg -> todo
    t.F64Ceil -> todo
    t.F64Floor -> todo
    t.F64Trunc -> todo
    t.F64Nearest -> todo
    t.F64Sqrt -> todo
    t.F64Add -> todo
    t.F64Sub -> todo
    t.F64Mul -> todo
    t.F64Div -> todo
    t.F64Min -> todo
    t.F64Max -> todo
    t.F64CopySign -> todo
  }
}

fn map_function_signature(f: t.Function(Int)) -> w.FunctionSignature {
  w.FunctionSignature(type_index: f.type_, name: f.id, param_names: None)
}

fn map_rec_type(rec_type: t.RecType(Int)) -> List(w.SubType) {
  list.map(rec_type.defs, map_sub_type)
}

fn map_sub_type(def: t.TypeDef(Int)) -> w.SubType {
  case def.sub {
    t.Open -> w.SubOpen(def.super, map_composite_type(def.def, def.id))
    t.Final -> w.SubFinal(def.super, map_composite_type(def.def, def.id))
  }
}

fn map_composite_type(
  def: t.CompType(Int),
  id: Option(String),
) -> w.CompositeType {
  case def {
    t.Struct(fields:) -> todo
    t.Array(item_type:) -> todo
    t.Func(params:, results:) ->
      w.Func(id, list.map(params, map_param), list.map(results, map_value_type))
  }
}

fn map_param(p: t.Param(Int)) -> w.ValueType {
  map_value_type(p.typ)
}

fn map_value_type(vt: t.ValueType(Int)) -> w.ValueType {
  case vt {
    t.I32 -> w.I32
    t.I64 -> w.I64
    t.F32 -> w.F32
    t.F64 -> w.F64
    t.Ref(rt) -> w.Ref(map_ref_type(rt))
  }
}

fn map_ref_type(rt: t.RefType(Int)) -> w.RefType {
  case rt {
    t.NonNull(ht) -> w.NonNull(map_heap_type(ht))
    t.Nullable(ht) -> w.Nullable(map_heap_type(ht))
  }
}

fn map_heap_type(ht: t.HeapType(Int)) -> w.HeapType {
  case ht {
    t.HeapAny -> w.AbstractAny
    t.HeapEq -> w.AbstractEq
    t.HeapI31 -> w.AbstractI31
    t.HeapStruct -> w.AbstractStruct
    t.HeapArray -> w.AbstractArray
    t.HeapNone -> w.AbstractNone
    t.HeapFunc -> w.AbstractFunc
    t.HeapNoFunc -> w.AbstractNoFunc
    t.HeapExn -> todo
    t.HeapNoExn -> todo
    t.HeapExtern -> w.AbstractExtern
    t.HeapNoExtern -> w.AbstractNoExtern
    t.HeapIndex(i) -> w.ConcreteType(i)
  }
}
