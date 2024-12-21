namespace AdventOfCode.Days.Y2024

open AdventOfCode.Shared.Utility

/// Day 2: Red-Nosed Reports
/// https://adventofcode.com/2024/day/2
/// Fortunately, the first location The Historians want to search isn't a long walk from the Chief Historian's office.
module Day02 =

    let parse (s: string) = s |> split ' ' |> Array.map int |> Array.toList

    let parseInput() = getFile (2024, 2) |> readLinesAs parse

    let isSteadyDecrease = List.pairwise >> List.forall (fun (x, y) -> x - y <= 3 && x > y)

    let isSteadyIncrease = List.pairwise >> List.forall (fun (x, y) -> y - x <= 3 && y > x)

    let isStable x = isSteadyIncrease x || isSteadyDecrease x

    let getCombos (elems: int list) = // Remove one element for each combo
        [0 .. elems.Length - 1]
        |> List.map (fun x -> elems[0..(x - 1)] @ elems[(x + 1)..])

    // Analyze the unusual data from the engineers. How many reports are safe?
    let Part1() = parseInput() |> countIf isStable

    // Update your analysis by handling situations where the Problem Dampener can remove a single level from
    // unsafe reports. How many reports are now safe?
    let Part2() = parseInput() |> countIf (getCombos >> List.exists isStable)