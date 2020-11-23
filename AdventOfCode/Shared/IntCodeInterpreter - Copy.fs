namespace AdventOfCode.Shared

open Utility
open System

module IntCodeInterpreter =

    type CodeMap = Map<int64, int64>

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

    type State =
        { Input: int64 seq
          Output: int64 list
          Index: int64
          RelativeBase: int64
          Memory: CodeMap
          IsTerminated: bool } with
      
            member private __.CurrentOp =  __.Memory.[__.Index]
        
            member private __.Parameter (paramIndex: int) =
                let p = __.Memory.[__.Index + 1L + (int64 paramIndex)]
                let mode = getParamMode __.CurrentOp paramIndex
                mode, p
            
            member __.CurrentOpCode = __.CurrentOp % 100L |> int
        
            member __.TryGet index = // Default to zero if not initialised
                __.Memory.TryFind index |? 0L 
        
            member __.ReadParameter (paramIndex: int) =
                match __.Parameter paramIndex with
                | Immediate, p -> p
                | Position,  p -> __.TryGet p
                | Relative,  p -> __.TryGet (p + __.RelativeBase)
            
            member __.WriteToMemory (paramIndex: int) value =
                        
                let address = 
                    match __.Parameter paramIndex with
                    | Immediate, _ -> failwith "Immediate mode invalid for write"
                    | Position,  p -> p
                    | Relative,  p -> p + __.RelativeBase
            
                { __ with Memory = __.Memory.Add (address, value) }
            
            member __.ReadInput() =  Seq.head __.Input, { __ with Input = Seq.tail __.Input }           
         
            member __.WriteToOutput value = { __ with Output = (value::__.Output) }
            
            member __.RelativeBaseOffset offset =
                { __ with RelativeBase = __.RelativeBase + offset }

    // Move to memory address offset
    let private (+=>) (s: State) (offset: int) =
        { s with Index = s.Index + (int64 offset) }
    
    // Move to memory address
    let private (==>) (s: State) index = { s with Index = index }

    let private add (s: State) = // p0 + p1 -> p2
        let (p0, p1) = s.ReadParameter 0, s.ReadParameter 1
        let value = p0 + p1
        s.WriteToMemory 2 value +=> 4
    
    let private multiply (s: State) = // p0 * p1 -> p2
        let (p0, p1) = s.ReadParameter 0, s.ReadParameter 1
        let value = p0 * p1
        s.WriteToMemory 2 value +=> 4
          
    let private lessThan (s: State) = // p0 < p1 -> p2
        let (p0, p1) = s.ReadParameter 0, s.ReadParameter 1
        let value = if p0 < p1 then 1L else 0L
        s.WriteToMemory 2 value +=> 4

    let private equal (s: State) = // p0 = p1 -> p2
        let (p0, p1) = s.ReadParameter 0, s.ReadParameter 1
        let value = if p0 = p1 then 1L else 0L
        s.WriteToMemory 2 value +=> 4

    let private output (s: State) = // p0 -> output
        let p0 = s.ReadParameter 0
        s.WriteToOutput p0 +=> 2
    
    let private input (s: State) = // input -> p0 
        let value, s = s.ReadInput()
        s.WriteToMemory 0 value +=> 2
    
    let private relOffset (s: State) = // p0 -> RelOffset
        let p0 = s.ReadParameter 0
        s.RelativeBaseOffset p0 +=> 2
       
    let private jumpIfTrue (s: State) = // p0 = true -> jump p1
        let (p0, p1) = s.ReadParameter 0, s.ReadParameter 1
        if p0 <> 0L then s ==> p1 else s +=> 3

    let private jumpIfFalse (s: State) = // p0 = false -> jump p1
        let (p0, p1) = s.ReadParameter 0, s.ReadParameter 1
        if p0 = 0L then s ==> p1 else s +=> 3

    let private terminate (s: State) = { s with IsTerminated = true }

    let private executeOp (s: State) =
    
        let op =
            match s.CurrentOpCode with
            | 1 -> add
            | 2 -> multiply
            | 3 -> input
            | 4 -> output
            | 5 -> jumpIfTrue
            | 6 -> jumpIfFalse
            | 7 -> lessThan
            | 8 -> equal
            | 9 -> relOffset
            | 99 -> terminate
            | _ -> failwithf "Unexpected input: %A" s
        
        op s

    let private readIntCode filename = 
        readCsv filename
        |> List.mapi (fun i x -> int64 i, Int64.Parse x)
        |> CodeMap


    // TODO: Revisit when have added all IntCode days
    type Event =
        | Terminated of State
        | Output of int64
        | RequestInput


    type Interpreter = { Code: CodeMap; Input: int64 seq } with

        static member Create file = { Code = readIntCode file; Input = [] }

        member __.Run() =
            let state =
                 { Input = __.Input
                   Output = []
                   Index = 0L
                   RelativeBase = 0L
                   Memory = __.Code
                   IsTerminated = false }

            state
            |> Seq.unfold (fun x -> if x.IsTerminated then None else Some (x, executeOp x))


    let setOverrides (overrides: (int64 * int64) list) interpreter =
        let code =
            (interpreter.Code, overrides)
            ||> List.fold (fun acc x -> acc.Add x)

        { interpreter with Code = code }