namespace AdventOfCode.Days.Y2020.Shared

open AdventOfCode.Shared.Utility
open ImmutableHashCollections

module BootCodeInterpreter =

    type Op = Acc of int | Jmp of int | NoOp of int
    
    type Status = Running | Terminated | Looping

    type MemoryMap = HashMapOkasaki<int, Op>

    let parse (s: string) =
        match s |> split ' ' |> Array.toList with
        | "acc"::Int i::[] -> Acc i
        | "jmp"::Int i::[] -> Jmp i
        | "nop"::Int i::[] -> NoOp i
        | x -> failwithf "Invalid input: %A" x

    let parseBootCode filename = filename |> readLinesAs parse |> Seq.indexed |> HashMapOkasaki.ofSeq
    
    type Interpreter =
        private
            { index: int
              memory: MemoryMap
              acc: int
              status: Status
              visited: int Set } with

          static member Create file =
            { index = 0
              acc = 0
              status = Running
              visited = Set.empty
              memory = parseBootCode file }

          member __.Status = __.status

    // Try replacing each single instance of op with replacement op and return clone of interpreter
    let getPotentialFixes replaceOp (i: Interpreter) =
        i.memory
        |> Seq.choose replaceOp
        |> Seq.map (fun op -> { i with memory = i.memory.Add op })

    let getAccumulator (i: Interpreter) = i.acc

    let private (==>) i n = { i with index = i.index + n }

    // nop stands for No OPeration - it does nothing
    let private noop i = i ==> 1

    // acc increases or decreases the accumulator by the value given in the argument
    let private acc x i = { i with acc = i.acc + x } ==> 1

    // jmp jumps to a new instruction relative to itself
    let private jmp x i = i ==> x

    let (| Visited |_|) i = if i.visited.Contains i.index then Some Visited else None

    let (| ValidOp | IndexOutOfRange |) i =
        match i.memory.TryFind i.index with Some x -> ValidOp x | None -> IndexOutOfRange

    // Run all instructions until termination or infinite loop
    let rec run (i: Interpreter) =
        match i with
        | Visited -> { i with status = Looping }
        | IndexOutOfRange -> { i with status = Terminated }
        | ValidOp op -> 
            let i = { i with visited = i.visited.Add i.index }

            match op with
            | NoOp _ -> noop i
            | Acc x -> acc x i
            | Jmp x -> jmp x i
            |> run

        | x -> failwithf "Invalid input: %A" x