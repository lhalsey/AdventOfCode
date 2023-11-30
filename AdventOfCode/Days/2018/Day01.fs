namespace AdventOfCode.Days.Y2018

open AdventOfCode.Shared.Utility

/// Day 1: Chronal Calibration
/// https://adventofcode.com/2018/day/1
/// We've detected some temporal anomalies, one of Santa's Elves at the Temporal Anomaly Research and
/// Detection Instrument Station tells you.
module Day01 =

    let parseInput() = getFile (2018, 1) |> readLinesAs int

    // Starting with a frequency of zero, what is the resulting frequency after all of the changes in
    // frequency have been applied?
    let Part1() = parseInput() |> Seq.sum

    // What is the first frequency your device reaches twice?
    let Part2() =
        parseInput()
        |> repeatInfinite
        |> Seq.scan (+) 0
        |> findDuplicate
