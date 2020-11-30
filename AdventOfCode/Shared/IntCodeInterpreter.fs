namespace AdventOfCode.Shared

open Utility
open System
open ImmutableHashCollections

module IntCodeInterpreter =

    // Originally used FSharp.HashCollections.HashMap for improved lookup performance
    // but Okasaki version gives up to 100% improvement over F# Map
    type MemoryMap = HashMapOkasaki<int64, int64> 

    type ParameterMode = Position | Immediate | Relative

    let private getParamMode num index =
        num
        |> Seq.unfold (fun x -> Some (x % 10L |> int, x / 10L))
        |> Seq.skip 2 // Skip 2 digits for opcode
        |> Seq.item index
        |> function
            | 0 -> Position
            | 1 -> Immediate
            | 2 -> Relative
            | x -> failwithf "Invalid parameter mode: %i" x

    let private readIntCode filename = 
        readCsv filename
        |> List.mapi (fun i x -> (int64 i, Int64.Parse x))
        |> HashMapOkasaki.ofSeq

    type Interpreter =
        { Index: int64
          RelativeBase: int64
          Memory: MemoryMap
          IsTerminated: bool } with

            static member Create file =
                { Index = 0L
                  RelativeBase = 0L
                  Memory = readIntCode file
                  IsTerminated = false }
          
            member private __.CurrentOp = __.Get __.Index
        
            member private __.Parameter (paramIndex: int) =
                let p = __.Get (__.Index + 1L + (int64 paramIndex))
                let mode = getParamMode __.CurrentOp paramIndex
                mode, p
            
            member __.CurrentOpCode = __.CurrentOp % 100L |> int
        
            member __.Get index = // Default to zero if not initialised
                 __.Memory |> HashMapOkasaki.tryFind index |> Option.defaultValue 0L 
        
            member __.ReadParameter (paramIndex: int) =
                match __.Parameter paramIndex with
                | Immediate, p -> p
                | Position,  p -> __.Get p
                | Relative,  p -> __.Get (p + __.RelativeBase)
            
            member __.WriteToMemory (paramIndex: int) value =
                        
                let address = 
                    match __.Parameter paramIndex with
                    | Immediate, _ -> failwith "Immediate mode invalid for write"
                    | Position,  p -> p
                    | Relative,  p -> p + __.RelativeBase
            
                { __ with Memory = __.Memory |> HashMapOkasaki.add address value }
            
            member __.RelativeBaseOffset offset =
                { __ with RelativeBase = __.RelativeBase + offset }

    // Move to memory address offset
    let private (+=>) (s: Interpreter) (offset: int) =
        { s with Index = s.Index + (int64 offset) }
    
    // Move to memory address
    let private (==>) (s: Interpreter) index = { s with Index = index }

    let private add (s: Interpreter) = // p0 + p1 -> p2
        let (p0, p1) = s.ReadParameter 0, s.ReadParameter 1
        let value = p0 + p1
        s.WriteToMemory 2 value +=> 4
    
    let private multiply (s: Interpreter) = // p0 * p1 -> p2
        let (p0, p1) = s.ReadParameter 0, s.ReadParameter 1
        let value = p0 * p1
        s.WriteToMemory 2 value +=> 4
          
    let private lessThan (s: Interpreter) = // p0 < p1 -> p2
        let (p0, p1) = s.ReadParameter 0, s.ReadParameter 1
        let value = if p0 < p1 then 1L else 0L
        s.WriteToMemory 2 value +=> 4

    let private equal (s: Interpreter) = // p0 = p1 -> p2
        let (p0, p1) = s.ReadParameter 0, s.ReadParameter 1
        let value = if p0 = p1 then 1L else 0L
        s.WriteToMemory 2 value +=> 4

    let private output (s: Interpreter) = // p0 -> output
        let p0 = s.ReadParameter 0
        let s = s +=> 2
        (p0, s)
    
    let private input (s: Interpreter) = // input -> p0 
        fun value -> s.WriteToMemory 0 value +=> 2
    
    let private relOffset (s: Interpreter) = // p0 -> RelOffset
        let p0 = s.ReadParameter 0
        s.RelativeBaseOffset p0 +=> 2
       
    let private jumpIfTrue (s: Interpreter) = // p0 = true -> jump p1
        let (p0, p1) = s.ReadParameter 0, s.ReadParameter 1
        if p0 <> 0L then s ==> p1 else s +=> 3

    let private jumpIfFalse (s: Interpreter) = // p0 = false -> jump p1
        let (p0, p1) = s.ReadParameter 0, s.ReadParameter 1
        if p0 = 0L then s ==> p1 else s +=> 3

    // Credit to Cameron Aavik for the elegant modelling of program state
    // https://github.com/CameronAavik/AdventOfCode/blob/master/AdventOfCode.2019/Common/Intcode.fs
    type ProgramState =
        | Terminated of Interpreter
        | Output of int64 list * ProgramState
        | Input of (int64 -> ProgramState)

    type private Operation =
        | Apply of (Interpreter -> Interpreter)
        | Terminated
        | HasOutput of int64 * Interpreter
        | RequiresInput of (int64 -> Interpreter)

    let rec private getInstruction (i: Interpreter) =
        
        match i.CurrentOpCode with
        | 1 -> add |> Apply
        | 2 -> multiply |> Apply
        | 3 -> input i |> RequiresInput
        | 4 -> output i |> HasOutput
        | 5 -> jumpIfTrue |> Apply
        | 6 -> jumpIfFalse |> Apply
        | 7 -> lessThan |> Apply
        | 8 -> equal |> Apply
        | 9 -> relOffset |> Apply
        | 99 -> Terminated
        | _ -> failwithf "Unexpected input: %A" i

    // Continue to run instructions until we have Input, Output or Termination
    let rec run (i: Interpreter) =
        match getInstruction i with
        | Apply op -> run (op i)
        | Terminated -> ProgramState.Terminated i
        | RequiresInput f -> Input (f >> run) // Function to resume running when input is provided
        | HasOutput (o, i) ->
            match run i with // Check if there is more output or get next state
            | Output(o', i') -> Output(o::o', i')
            | i' -> Output([o], i')
       
    let provideInput input = function
        | Input f -> f input
        | state -> failwithf "Expected input, but got %A" state

    let readOutput = function
        | Output (output, state) -> (output, state)
        | state -> failwithf "Expected output, but got %A" state

    let readFinalOutput = readOutput >> fun (o, _) -> List.last o
        
    let getMemory address (interpreter: Interpreter) = interpreter.Get address

    let setMemory (overrides: (int64 * int64) list) (interpreter: Interpreter) =
        let memory =
            (interpreter.Memory, overrides)
            ||> List.fold (fun acc (address, value) -> acc |> HashMapOkasaki.add address value)

        { interpreter with Memory = memory }