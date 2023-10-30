module Brainfuck.Codegen

open System
open System.Collections.Generic
open System.Reflection
open System.Reflection.Emit
open System.Runtime.CompilerServices
open Brainfuck.Primitives

module IncrementPointer =
    let inline value (il: ILGenerator) (cellPtr: LocalBuilder) =
        il.Emit(OpCodes.Ldloc_S, cellPtr)
        il.Emit OpCodes.Ldc_I4_4
        il.Emit OpCodes.Conv_I
        il.Emit OpCodes.Add
        il.Emit(OpCodes.Stloc, cellPtr)

module DecrementPointer =
    let inline value (il: ILGenerator) (cellPtr: LocalBuilder) =
        il.Emit(OpCodes.Ldloc_S, cellPtr)
        il.Emit OpCodes.Ldc_I4_4
        il.Emit OpCodes.Conv_I
        il.Emit OpCodes.Sub
        il.Emit(OpCodes.Stloc, cellPtr)

module IncrementCell =
    let inline i1 (il: ILGenerator) (cellPtr: LocalBuilder) =
        il.Emit(OpCodes.Ldloc_S, cellPtr)
        il.Emit(OpCodes.Ldloc_S, cellPtr)
        il.Emit OpCodes.Ldind_I1
        il.Emit OpCodes.Ldc_I4_1
        il.Emit OpCodes.Conv_I1
        il.Emit OpCodes.Add
        il.Emit OpCodes.Stind_I1

    let inline i2 (il: ILGenerator) (cellPtr: LocalBuilder) =
        il.Emit(OpCodes.Ldloc_S, cellPtr)
        il.Emit(OpCodes.Ldloc_S, cellPtr)
        il.Emit OpCodes.Ldind_I2
        il.Emit OpCodes.Ldc_I4_1
        il.Emit OpCodes.Conv_I2
        il.Emit OpCodes.Add
        il.Emit OpCodes.Stind_I2

    let inline i4 (il: ILGenerator) (cellPtr: LocalBuilder) =
        il.Emit(OpCodes.Ldloc_S, cellPtr)
        il.Emit(OpCodes.Ldloc_S, cellPtr)
        il.Emit OpCodes.Ldind_I4
        il.Emit OpCodes.Ldc_I4_1
        il.Emit OpCodes.Add
        il.Emit OpCodes.Stind_I4

module DecrementCell =
    let inline i1 (il: ILGenerator) (cellPtr: LocalBuilder) =
        il.Emit(OpCodes.Ldloc_S, cellPtr)
        il.Emit(OpCodes.Ldloc_S, cellPtr)
        il.Emit OpCodes.Ldind_I1
        il.Emit OpCodes.Ldc_I4_1
        il.Emit OpCodes.Conv_I1
        il.Emit OpCodes.Sub
        il.Emit OpCodes.Stind_I1

    let inline i2 (il: ILGenerator) (cellPtr: LocalBuilder) =
        il.Emit(OpCodes.Ldloc_S, cellPtr)
        il.Emit(OpCodes.Ldloc_S, cellPtr)
        il.Emit OpCodes.Ldind_I2
        il.Emit OpCodes.Ldc_I4_1
        il.Emit OpCodes.Conv_I2
        il.Emit OpCodes.Sub
        il.Emit OpCodes.Stind_I2

    let inline i4 (il: ILGenerator) (cellPtr: LocalBuilder) =
        il.Emit(OpCodes.Ldloc_S, cellPtr)
        il.Emit(OpCodes.Ldloc_S, cellPtr)
        il.Emit OpCodes.Ldind_I4
        il.Emit OpCodes.Ldc_I4_1
        il.Emit OpCodes.Sub
        il.Emit OpCodes.Stind_I4

module GetChar =
    let read = typeof<Console>.GetMethod "Read"

    let inline i1 (il: ILGenerator) (cellPtr: LocalBuilder) =
        il.Emit(OpCodes.Ldloc_S, cellPtr)
        il.EmitCall(OpCodes.Call, read, null)
        il.Emit OpCodes.Conv_I1
        il.Emit OpCodes.Stind_I1

    let inline i2 (il: ILGenerator) (cellPtr: LocalBuilder) =
        il.Emit(OpCodes.Ldloc_S, cellPtr)
        il.EmitCall(OpCodes.Call, read, null)
        il.Emit OpCodes.Conv_I2
        il.Emit OpCodes.Stind_I2

    let inline i4 (il: ILGenerator) (cellPtr: LocalBuilder) =
        il.Emit(OpCodes.Ldloc_S, cellPtr)
        il.EmitCall(OpCodes.Call, read, null)
        il.Emit OpCodes.Stind_I4

module PutChar =
    let write = typeof<Console>.GetMethod ("Write", [| typeof<char> |])

    let inline i1 (il: ILGenerator) (cellPtr: LocalBuilder) =
        il.Emit(OpCodes.Ldloc_S, cellPtr)
        il.Emit OpCodes.Ldind_I1
        il.EmitCall(OpCodes.Call, write, null)

    let inline i2 (il: ILGenerator) (cellPtr: LocalBuilder) =
        il.Emit(OpCodes.Ldloc_S, cellPtr)
        il.Emit OpCodes.Ldind_I2
        il.EmitCall(OpCodes.Call, write, null)

    let inline i4 (il: ILGenerator) (cellPtr: LocalBuilder) =
        il.Emit(OpCodes.Ldloc_S, cellPtr)
        il.Emit OpCodes.Ldind_I4
        il.EmitCall(OpCodes.Call, write, null)

module JumpIfEqual =
    let inline i1 (il: ILGenerator) (cellPtr: LocalBuilder) (labels: Stack<Label>) =
        il.Emit(OpCodes.Ldloc_S, cellPtr)
        il.Emit OpCodes.Ldind_I1
        let je = il.DefineLabel()
        il.Emit(OpCodes.Brfalse, je)
        labels.Push je
        let jne = il.DefineLabel()
        il.MarkLabel jne
        labels.Push jne

    let inline i2 (il: ILGenerator) (cellPtr: LocalBuilder) (labels: Stack<Label>) =
        il.Emit(OpCodes.Ldloc_S, cellPtr)
        il.Emit OpCodes.Ldind_I2
        let je = il.DefineLabel()
        il.Emit(OpCodes.Brfalse, je)
        labels.Push je
        let jne = il.DefineLabel()
        il.MarkLabel jne
        labels.Push jne

    let inline i4 (il: ILGenerator) (cellPtr: LocalBuilder) (labels: Label Stack) =
        il.Emit(OpCodes.Ldloc_S, cellPtr)
        il.Emit OpCodes.Ldind_I4
        let je = il.DefineLabel()
        il.Emit(OpCodes.Brfalse, je)
        labels.Push je
        let jne = il.DefineLabel()
        il.MarkLabel jne
        labels.Push jne

module JumpIfNotEqual =
    let inline i1 (il: ILGenerator) (cellPtr: LocalBuilder) (labels: Stack<Label>) =
        il.Emit(OpCodes.Ldloc_S, cellPtr)
        il.Emit OpCodes.Ldind_I1
        il.Emit(OpCodes.Brtrue, labels.Pop())
        il.MarkLabel(labels.Pop())

    let inline i2 (il: ILGenerator) (cellPtr: LocalBuilder) (labels: Stack<Label>) =
        il.Emit(OpCodes.Ldloc_S, cellPtr)
        il.Emit OpCodes.Ldind_I2
        il.Emit(OpCodes.Brtrue, labels.Pop())
        il.MarkLabel(labels.Pop())

    let inline i4 (il: ILGenerator) (cellPtr: LocalBuilder) (labels: Label Stack) =
        il.Emit(OpCodes.Ldloc_S, cellPtr)
        il.Emit OpCodes.Ldind_I4
        il.Emit(OpCodes.Brtrue, labels.Pop())
        il.MarkLabel(labels.Pop())

[<IsReadOnly; Struct>]
type Props =
    { CellType: Type
      IncrementPointer: ILGenerator -> LocalBuilder -> unit
      DecrementPointer: ILGenerator -> LocalBuilder -> unit
      IncrementCell: ILGenerator -> LocalBuilder -> unit
      DecrementCell: ILGenerator -> LocalBuilder -> unit
      GetChar: ILGenerator -> LocalBuilder -> unit
      PutChar: ILGenerator -> LocalBuilder -> unit
      JumpIfEqual: ILGenerator -> LocalBuilder -> Stack<Label> -> unit
      JumpIfNotEqual: ILGenerator -> LocalBuilder -> Stack<Label> -> unit }

module Props =
    let i1 =
        { CellType = typeof<int8>
          IncrementPointer = IncrementPointer.value
          DecrementPointer = DecrementPointer.value
          IncrementCell = IncrementCell.i1
          DecrementCell = DecrementCell.i1
          GetChar = GetChar.i1
          PutChar = PutChar.i1
          JumpIfEqual = JumpIfEqual.i1
          JumpIfNotEqual = JumpIfNotEqual.i1 }

    let i2 =
        { CellType = typeof<int16>
          IncrementPointer = IncrementPointer.value
          DecrementPointer = DecrementPointer.value
          IncrementCell = IncrementCell.i2
          DecrementCell = DecrementCell.i2
          GetChar = GetChar.i2
          PutChar = PutChar.i2
          JumpIfEqual = JumpIfEqual.i2
          JumpIfNotEqual = JumpIfNotEqual.i2 }

    let i4 =
        { CellType = typeof<int32>
          IncrementPointer = IncrementPointer.value
          DecrementPointer = DecrementPointer.value
          IncrementCell = IncrementCell.i4
          DecrementCell = DecrementCell.i4
          GetChar = GetChar.i4
          PutChar = PutChar.i4
          JumpIfEqual = JumpIfEqual.i4
          JumpIfNotEqual = JumpIfNotEqual.i4 }

    let create =
        function
        | Single -> i1
        | Double -> i2
        | Quad -> i4

let createAssembly props commands =
    let assemblyBuilder =
        AssemblyBuilder.DefineDynamicAssembly(AssemblyName "Brainfuck", AssemblyBuilderAccess.Run)

    let moduleBuilder = assemblyBuilder.DefineDynamicModule "Brainfuck"

    let typeBuilder =
        moduleBuilder.DefineType("Program", TypeAttributes.Public ||| TypeAttributes.Class)

    let entryPoint =
        typeBuilder.DefineMethod(
            "Main",
            MethodAttributes.Public
            ||| MethodAttributes.Static
            ||| MethodAttributes.HideBySig,
            typeof<int>,
            [||]
        )

    let il = entryPoint.GetILGenerator()
    let cellPtr = il.DeclareLocal typeof<nativeint>

    il.Emit(OpCodes.Ldc_I4, 10_000)
    il.Emit(OpCodes.Sizeof, props.CellType)
    il.Emit OpCodes.Mul
    il.Emit OpCodes.Localloc
    il.Emit(OpCodes.Stloc, cellPtr)

    let labels = Stack()

    let emit =
        function
        | IncrementPointer -> props.IncrementPointer il cellPtr
        | DecrementPointer -> props.DecrementPointer il cellPtr
        | IncrementCell -> props.IncrementCell il cellPtr
        | DecrementCell -> props.DecrementCell il cellPtr
        | GetChar -> props.GetChar il cellPtr
        | PutChar -> props.PutChar il cellPtr
        | JumpIfEqual -> props.JumpIfEqual il cellPtr labels
        | JumpIfNotEqual -> props.JumpIfNotEqual il cellPtr labels

    Seq.iter emit commands
    il.Emit(OpCodes.Ldloc_S, cellPtr)

    if props.CellType = typeof<int8> then
        il.Emit OpCodes.Ldind_I1
    elif props.CellType = typeof<int16> then
        il.Emit OpCodes.Ldind_I2
    else
        il.Emit OpCodes.Ldind_I4

    il.Emit OpCodes.Ret
    Assembly.GetAssembly(typeBuilder.CreateType())
