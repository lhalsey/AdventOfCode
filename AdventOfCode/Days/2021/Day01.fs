namespace AdventOfCode.Days.Y2021

open AdventOfCode.Shared.Utility

/// Day 1: Sonar Sweep
/// https://adventofcode.com/2021/day/1
/// You're minding your own business on a ship at sea when the overboard alarm goes off!
module Day01 =

    let parseInput() = getFile (2021, 1) |> readLinesAs int

    let countIncreases getMeasurements =
        parseInput()
        |> getMeasurements
        |> Seq.pairwise
        |> countIf (fun (x, y) -> y > x)

    // How many measurements are larger than the previous measurement?
    let Part1() = countIncreases id

    // Consider sums of a three-measurement sliding window. How many sums are larger than the previous sum?
    let Part2() = countIncreases (Seq.windowed 3 >> Seq.map Array.sum)