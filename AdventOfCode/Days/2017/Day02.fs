namespace AdventOfCode.Days.Y2017

open AdventOfCode.Shared.Utility

/// Day 2: Corruption Checksum
/// https://adventofcode.com/2017/day/2
/// As you walk through the door, a glowing humanoid shape yells in your direction.
module Day02 =

    let parseInput() =
        getFile (2017, 2)
        |> readLines
        |> Seq.map (split '\t' >> Array.map System.Int32.Parse)

    let getDivision (nums: int[]) =
        Array.allPairs nums nums
        |> Array.find (fun (x, y) -> x > y && x % y = 0)
        |> fun (x, y) -> x / y

    let Part1() =
        parseInput()
        |> Seq.sumBy (fun x -> Array.max x - Array.min x)

    let Part2() =
        parseInput()
        |> Seq.sumBy getDivision