module Brainfuck.Program

open System.IO
open System.Reflection
open Argu
open Brainfuck.Primitives

type CommandLineArgs =
    | [<AltCommandLine("-s"); Unique>] Size of string
    | [<MainCommand; ExactlyOnce; Last>] Path of string

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Size _ -> "specify either 'single' (8-bit), 'double' (16-bit) or 'quad' (32-bit)."
            | Path _ -> "path to a brainfuck source file."

[<EntryPoint>]
let main argv =
    let parser = ArgumentParser.Create<CommandLineArgs>(programName = "brainfuck.exe")
    let mutable cellSize = Quad
    let mutable path = null

    try
        for arg in parser.ParseCommandLine(inputs = argv, raiseOnUsage = true).GetAllResults() do
            match arg with
            | Size size ->
                match CellSize.tryParse size with
                | ValueNone -> eprintfn "Invalid cell size. Defaulting to 'quad' (32-bit)."
                | ValueSome cellSize' -> cellSize <- cellSize'
            | Path path' -> path <- path'

        let commands = Reader.read (new StreamReader(path))
        let props = Codegen.Props.create cellSize
        let assembly = Codegen.createAssembly props commands

        assembly
            .GetType("Program")
            .GetMethod("Main", BindingFlags.Public ||| BindingFlags.Static)
            .Invoke(null, [||])
        :?> int

    with e ->
        eprintfn $"{e.Message}"
        1
