namespace AdventOfCode.Days.Y2020

open AdventOfCode.Shared.Utility

/// Day 1: Repair Report
/// https://adventofcode.com/2020/day/1
/// After saving Christmas five years in a row, you've decided to take a vacation at a
/// nice resort on a tropical island.
module Day01 =

    let parseInput() = getFile (2020, 1) |> readLinesAs int |> toReadOnlyHashSet

    let findProductOfTwoNumsSummingTo target =
        let input = parseInput()

        input
        |> Seq.find (fun x -> input.Contains (target - x))
        |> fun x -> x * (target - x)

    let findProductOfThreeNumsSummingTo target =
        let input = parseInput()

        input
        |> Seq.toList
        |> uniquePairs
        |> Seq.find (fun (x, y) -> input.Contains (target - (x + y)))
        |> fun (x, y) -> x * y * (target - (x + y))


    // Find the two entries that sum to 2020; what do you get if you multiply them together?
    let Part1() = findProductOfTwoNumsSummingTo 2020

    // In your expense report, what is the product of the three entries that sum to 2020?
    let Part2() = findProductOfThreeNumsSummingTo 2020
        