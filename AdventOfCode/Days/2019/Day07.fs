namespace AdventOfCode.Days.Y2019

open AdventOfCode.Shared.Utility
open AdventOfCode.Days.Y2019.Shared.IntCodeInterpreter
open FSharp.Collections.ParallelSeq

/// Day 7: Amplification Circuit
/// https://adventofcode.com/2019/day/7
/// Based on the navigational maps, you're going to need to send more power
/// to your ship's thrusters to reach Santa in time.
module Day07 =

    let readOutput = function
        | Output (output::[], state) -> (output, state)
        | state -> failwithf "Expected output, but got %A" state

    // Provide input and take output unless program terminates
    let runWithInput (input: int64) = function
        | Input f -> f input |> readOutput |> Some
        | Terminated _ -> None
        | state -> failwithf "Expected input or termination, but got %A" state

    // Run amplifier with output from previous amplifier (or zero for first)
    // and then add to end of "queue"
    let rec tryPhaseSettings input interpreters  = 
        match interpreters with
        | h::t -> 
            match runWithInput input h with
            | Some (output, amp) -> tryPhaseSettings output (t @ [amp])
            | None -> input
        | [] -> failwith "No amplifiers"

    let getInterpreter() = getFile(2019, 7) |> Interpreter.Create

    let runAmps settings =
        let interpreter = getInterpreter()
        
        // Provide phase setting to each amplifier
        let initInterpreter setting =
            interpreter
            |> run
            |> provideInput setting
        
        permutations settings
        |> PSeq.map (List.map initInterpreter)
        |> PSeq.map (tryPhaseSettings 0L)
        |> PSeq.max

    // Try every combination of phase settings on the amplifiers.
    // What is the highest signal that can be sent to the thrusters?
    let Part1() = runAmps [0L..4L]

    // Try every combination of the new phase settings on the amplifier feedback loop.
    // What is the highest signal that can be sent to the thrusters?
    let Part2() = runAmps [5L..9L]