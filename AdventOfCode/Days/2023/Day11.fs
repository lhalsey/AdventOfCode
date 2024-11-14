namespace AdventOfCode.Days.Y2023

open AdventOfCode.Shared.Utility
open AdventOfCode.Shared

/// Day 11: Cosmic Expansion
/// https://adventofcode.com/2023/day/11
/// You continue following signs for "Hot Springs" and eventually come across an observatory.
module Day11 =

    let parse = function '#' -> true | _ -> false

    let parseInput() = getFile (2023, 11) |> parseGrid parse |> Seq.filter snd |> Seq.map fst |> Seq.toList

    let getDistances (multiplier: int) =
        let input = parseInput()

        let maxX = input |> List.map (fun x -> x.X) |> List.max

        let emptyColumns =
            [0 .. maxX]
            |> List.filter (fun x -> input |> List.exists (fun y -> y.X = x) |> not)

        let maxY = input |> List.map (fun x -> x.Y) |> List.max

        let emptyRows =
            [0 .. maxY]
            |> List.filter (fun x -> input |> List.exists (fun y -> y.Y = x) |> not)

        let getNewPos (point: Point2d) =
            let dx = emptyColumns |> countIf (fun x -> point.X > x)
            let dy = emptyRows |> countIf (fun x -> point.Y > x)

            { X = point.X + (dx * (multiplier - 1)); Y = point.Y + (dy * (multiplier - 1)) }

        input
        |> List.map getNewPos
        |> uniquePairs
        |> Seq.map (fun (x, y) -> x.ManhattanDistanceTo y |> int64)
        |> Seq.reduce (+)


    // Expand the universe, then find the length of the shortest path between every pair of galaxies.
    // What is the sum of these lengths?
    let Part1() = getDistances 2

    // Starting with the same initial image, expand the universe according to these new rules,
    // then find the length of the shortest path between every pair of galaxies.
    // What is the sum of these lengths?
    let Part2() = getDistances 1_000_000