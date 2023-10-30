module Brainfuck.Reader

open System.IO
open Brainfuck.Primitives

let read (reader: TextReader) =
    let mutable line = 1
    let mutable col = 1
    let locations = ResizeArray()

    [ while reader.Peek() <> -1 do
          match char (reader.Read()) with
          | '>' ->
              yield IncrementPointer
              col <- col + 1
          | '<' ->
              yield DecrementPointer
              col <- col + 1
          | '+' ->
              yield IncrementCell
              col <- col + 1
          | '-' ->
              yield DecrementCell
              col <- col + 1
          | ',' ->
              yield GetChar
              col <- col + 1
          | '.' ->
              yield PutChar
              col <- col + 1
          | '[' ->
              locations.Add struct (line, col)
              yield JumpIfEqual
              col <- col + 1
          | ']' ->
              if Seq.isEmpty locations then
                  raise (SyntaxError("Unexpected ']'", line, col))

              locations.RemoveAt(locations.Count - 1)
              yield JumpIfNotEqual
              col <- col + 1
          | '\n' ->
              line <- line + 1
              col <- 1
          | _ -> col <- col + 1

      if not (Seq.isEmpty locations) then
          let struct (line, col) = locations[0]
          raise (SyntaxError("Unmatched '['", line, col)) ]
