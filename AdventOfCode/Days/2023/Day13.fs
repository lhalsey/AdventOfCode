namespace AdventOfCode.Days.Y2023

open AdventOfCode.Shared.Utility

/// Day 13: Point of Incidence
/// https://adventofcode.com/2023/day/13
/// With your help, the hot springs team locates an appropriate spring which launches
/// you neatly and precisely up to the edge of Lava Island.
module Day13 =

    let parseInput() = getFile (2023, 13) |> readLines |> splitBy ""

    let getReflectionPoint (lines: string seq) =
        let row =
            lines
            |> Seq.pairwise
            |> Seq.tryFindIndex (fun (x, y) -> x = y)

        let col = 
            lines
            |> Seq.transpose
            |> Seq.pairwise
            |> Seq.tryFindIndex (fun (x, y) -> x = y)

        match row, col with
        | Some row, _ -> row * 100
        | _, Some col -> col
        | x -> failwithf "Invalid input: %A" x

    // Find the line of reflection in each of the patterns in your notes.
    // What number do you get after summarizing all of your notes?
    let Part1() =
        let input = parseInput()

        input |> Seq.sumBy getReflectionPoint

    let Part2() =
        0