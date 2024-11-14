namespace AdventOfCode.Days.Y2023

open AdventOfCode.Shared.Utility
open System.Collections.Generic
open System

/// Day 2: Cube Conundrum
/// https://adventofcode.com/2023/day/2
/// You're launched high into the atmosphere! The apex of your trajectory just barely reaches
/// the surface of a large island floating in the sky.
module Day02 =

    type CubeCount = IReadOnlyDictionary<string, int>
    type Game = { Id: int; MaxCubeCount: CubeCount }

    let getCount = function
        | Regex "(\d+) (\w+)" [Int count; colour] -> (colour, count)
        | x -> failwithf "Invalid input: %s" x

    let getMaxCubeCount (s: string) =
        s.Split([| ';'; ',' |], StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (trim >> getCount)
        |> groupBy fst (fun _ v -> Seq.max v)
        |> readOnlyDict

    let parse = function
        | Regex "Game (\d+): (.+)" [Int id; cubes] -> { Id = id; MaxCubeCount = getMaxCubeCount cubes }
        | x -> failwithf "Invalid input: %s" x

    let parseInput() = getFile (2023, 2) |> readLinesAs parse

    // Determine which games would have been possible if the bag had been loaded with only 
    // 12 red cubes, 13 green cubes, and 14 blue cubes. What is the sum of the IDs of those games?
    let Part1() =
        let thresholds = [("red", 12); ("green", 13); ("blue", 14)] |> readOnlyDict

        parseInput()
        |> Seq.filter (_.MaxCubeCount >> Seq.forall (fun x -> x.Value <= thresholds[x.Key]))
        |> Seq.sumBy (_.Id)

    // For each game, find the minimum set of cubes that must have been present.
    // What is the sum of the power of these sets?
    let Part2() =
        parseInput()
        |> Seq.sumBy (_.MaxCubeCount.Values >> Seq.reduce (*))