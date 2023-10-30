module Brainfuck.Primitives

open System
open System.Runtime.CompilerServices

type SyntaxError(error, line, col) =
    inherit Exception $"%s{error} at line %i{line} col %i{col}"

[<IsReadOnly; Struct>]
type CellSize =
    | Single // 8-bit
    | Double // 16-bit
    | Quad // 32-bit

module CellSize =
    let tryParse =
        function
        | "single" -> ValueSome Single
        | "double" -> ValueSome Double
        | "quad" -> ValueSome Quad
        | _ -> ValueNone

[<IsReadOnly; Struct>]
type Command =
    | IncrementPointer
    | DecrementPointer
    | IncrementCell
    | DecrementCell
    | GetChar
    | PutChar
    | JumpIfEqual
    | JumpIfNotEqual
