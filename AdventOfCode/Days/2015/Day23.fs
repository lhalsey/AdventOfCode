namespace AdventOfCode.Days.Y2015

open AdventOfCode.Shared.Utility

/// Day 23: Opening the Turing Lock
/// https://adventofcode.com/2015/day/23
/// Little Jane Marie just got her very first computer for Christmas from some unknown benefactor.
module Day23 =

    type Register = string
    type Offset = int

    type Op =
        | Half of Register
        | Triple of Register
        | Increment of Register
        | Jump of Offset
        | JumpIfEven of Register * Offset
        | JumpIfOne of Register * Offset

    type State = { Index: int; Registers: Map<string, int>; Code: Op[] } with
        member __.Item with get(reg: string) = __.Registers.TryFind reg |> Option.defaultValue 0

    let setReg reg f (state: State) =
        { state with Registers = state.Registers.Add (reg, f state.[reg]) }

    let jump offset state = { state with Index = state.Index + offset }

    let parse (s: string) =
        let tokens = s |> split ' ' |> Array.toList

        let (|Int|_|) = tryParseAsInt

        let l = [ yield! split ' ' s ]

        match tokens with
        | "hlf"::reg::[] -> Half reg
        | "tpl"::reg::[] -> Triple reg
        | "inc"::reg::[] -> Increment reg
        | "jmp"::Int offset::[] -> Jump offset
        | "jie"::reg::Int offset::[] -> JumpIfEven (reg.TrimEnd(','), offset)
        | "jio"::reg::Int offset::[] -> JumpIfOne (reg.TrimEnd(','), offset)
        | x -> failwithf "Invalid input: %A" x

    let parseInput() = getFile (2015, 23) |> readLinesAs parse |> Seq.toArray

    let runWith registers = 
        let rec runWithR state =
            let execute (s: State) =
                match s.Code.[s.Index] with
                | Half reg -> s |> setReg reg (fun x -> x / 2) |> jump 1
                | Triple reg -> s |> setReg reg ((*) 3) |> jump 1
                | Increment reg -> s |> setReg reg ((+) 1) |> jump 1
                | Jump offset -> s |> jump offset
                | JumpIfEven (reg, offset) -> s |> jump (if s.[reg] % 2 = 0 then offset else 1)
                | JumpIfOne (reg, offset) -> s |> jump (if s.[reg] = 1 then offset else 1)

            if state.Index >= state.Code.Length then state
            else execute state |> runWithR

        { Index = 0; Registers = Map registers; Code = parseInput() }
        |> runWithR
        |> fun s -> s.["b"]

    // What is the value in register b when the program in your puzzle input is finished executing?
    let Part1() = runWith [] 

    // What is the value in register b after the program is finished executing if register a starts
    // as 1 instead?
    let Part2() = runWith ["a", 1]