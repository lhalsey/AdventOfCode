namespace AdventOfCode.Days.Y2017

open AdventOfCode.Shared.Utility

/// Day 7: Recursive Circus
/// https://adventofcode.com/2017/day/7
/// Wandering further through the circuits of the computer, you come upon a tower of programs that have gotten themselves into a bit of trouble.
module Day07 =

    let parse = function // E.g. rdrad (6) -> gwyfm, fozyip, uotzz, fmkkz
        | Regex "(\w+) \((\d+)\) -> (.*)" [source; weight; targets] -> (source, weight, targets |> splitOn ", ")
        | Regex "(\w+) \((\d+)\)" [source; weight] -> (source, weight, [||])
        | x -> failwithf "Invalid input: %s" x

    let parseInput() = getFile (2017, 7) |> readLinesAs parse |> Seq.toArray

    let Part1() =
        let input = parseInput()

        let allTargets = input |> Array.collect (fun (_, _, t) -> t) |> toHashSet
        let allSources = input |> Array.map (fun (s, _, _) -> s)
        
        allSources |> Array.find (allTargets.Contains >> not)

    let Part2() =
        0