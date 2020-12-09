namespace AdventOfCode.Days.Y2020

open AdventOfCode.Shared.Utility
open FSharp.Collections.ParallelSeq
open AdventOfCode.Days.Y2020.Shared.BootCodeInterpreter

/// Day 8: Handheld Halting
/// https://adventofcode.com/2020/day/8
/// Your flight to the major airline hub reaches cruising altitude without incident.
module Day08 =
    let getInterpreter() = getFile(2020, 8) |> Interpreter.Create
        
    // Run your copy of the boot code. Immediately before any instruction is executed a
    // second time, what value is in the accumulator?
    let Part1() =
        getInterpreter()
        |> run
        |> getAccumulator

    // Fix the program so that it terminates normally by changing exactly one jmp (to nop)
    // or nop (to jmp). What is the value of the accumulator after the program terminates?
    let Part2() =
        let replaceOp (index, op) =
            match op with
            | Jmp x -> Some (index, NoOp x)
            | NoOp x -> Some (index, Jmp x)
            | _ -> None // Acc not corrupted

        getInterpreter()
        |> getPotentialFixes replaceOp
        |> PSeq.map run
        |> PSeq.find (fun x -> x.Status = Terminated)
        |> getAccumulator