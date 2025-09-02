namespace AdventOfCode.Days.Y2024

open AdventOfCode.Shared.Utility
open FSharp.Collections.ParallelSeq

/// Day 7: Bridge Repair
/// https://adventofcode.com/2024/day/7
/// The Historians take you to a familiar rope bridge over a river in the middle of a jungle.
module Day07 =

    type Equation = { TestValue: int64; Numbers: int64 list }

    let parse (s: string) =
        let tokens = s |> split ':'
        let numbers = tokens[1] |> split ' ' |> Array.map int64 |> Array.toList

        { TestValue = int64 tokens[0]; Numbers = numbers }

    let parseInput() = getFile (2024, 7) |> readLinesAs parse

    let canCalibrate  operations (e: Equation) =
        let rec canCalibrateR numbers acc =
            match numbers with
            | [] -> acc = e.TestValue
            | h::t -> operations |> List.exists (fun op -> canCalibrateR t (op acc h))

        canCalibrateR e.Numbers.Tail e.Numbers.Head

    let getCalibrationResult operations =
        parseInput()
        |> PSeq.filter (canCalibrate operations)
        |> PSeq.sumBy (fun x -> x.TestValue)

    let combine (x: int64) (y: int64) = (string x + string y) |> int64

    // Determine which equations could possibly be true. What is their total calibration result?
    let Part1() = getCalibrationResult [(+); (*)]

    // Using your new knowledge of elephant hiding spots, determine which equations could possibly be true.
    // What is their total calibration result?
    let Part2() = getCalibrationResult [(+); (*); combine]