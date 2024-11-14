namespace AdventOfCode.Days.Y2023

open AdventOfCode.Shared.Utility

/// Day 9: Mirage Maintenance
/// https://adventofcode.com/2023/day/9
/// You ride the camel through the sandstorm and stop where the ghost's maps told you to stop.
module Day09 =

    let parse = split ' ' >> Array.map int

    let parseInput() = getFile (2023, 9) |> readLinesAs parse

    let getDiffs = Array.pairwise >> Array.map (fun (x, y) -> y - x)

    let rec getNextNum (nums: int array) =
        if nums |> Array.forall ((=) 0)
            then 0
            else nums[nums.Length - 1] + getNextNum (getDiffs nums)

    // Analyze your OASIS report and extrapolate the next value for each history.
    // What is the sum of these extrapolated values?
    let Part1() = parseInput() |> Seq.sumBy getNextNum

    // Analyze your OASIS report again, this time extrapolating the previous value for each history.
    // What is the sum of these extrapolated values?
    let Part2() = parseInput() |> Seq.sumBy (Array.rev >> getNextNum)