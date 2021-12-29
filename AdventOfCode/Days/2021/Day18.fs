namespace AdventOfCode.Days.Y2021

open AdventOfCode.Shared.Utility

/// Day 18: Snailfish
/// https://adventofcode.com/2021/day/18
/// You descend into the ocean trench and encounter some snailfish.
module Day18 =

    type SnailNumber = Single of int | Pair of SnailNumber * SnailNumber

    let parseInput() = getFile (2021, 18) |> readLines

    // Add up all of the snailfish numbers from the homework assignment in the order they appear.
    // What is the magnitude of the final sum?
    let Part1() =
        let s1 = Pair (Single 1, Single 2)
        let s2 = Pair (Pair (Single 1, Single 2), Single 3)

        0

    let Part2() =
        0