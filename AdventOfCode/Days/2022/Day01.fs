namespace AdventOfCode.Days.Y2022

open AdventOfCode.Shared.Utility

/// Day 1: Calorie Counting
/// https://adventofcode.com/2022/day/1
/// Santa's reindeer typically eat regular reindeer food, but they need a lot of magical energy to deliver presents on Christmas.
module Day01 =

    let parseInput() = getFile (2022, 1) |> readLines

    let getElfCalories() =
        parseInput()
        |> splitBy ""
        |> Seq.map (Seq.sumBy int)

    // Find the Elf carrying the most Calories. How many total Calories is that Elf carrying?
    let Part1() = getElfCalories() |> Seq.max    

    // Find the top three Elves carrying the most Calories. How many Calories are those Elves carrying in total?
    let Part2() = getElfCalories() |> partialSortDesc 3 |> Seq.sum