namespace AdventOfCode.Days.Y2022

open AdventOfCode.Shared.Utility
open System
open MoreLinq

/// Day 8: Treetop Tree House
/// https://adventofcode.com/2022/day/8
/// The expedition comes across a peculiar patch of tall trees all planted carefully in a grid.
module Day08 =

    let parseInput() =
        getFile (2022, 8)
        |> readLinesAs (fun x -> x |> Seq.map (Char.GetNumericValue >> int) |> Seq.toArray)
        |> Seq.toArray

    let getResults() =
        let input = parseInput()
        let (maxRow, maxCol) = input.Length - 1, input[0].Length - 1

        let getResult (row, col) =
            let height = input[row][col]
            let treesUp = [row - 1..-1..0] |> List.map (fun r -> input[r][col])
            let treesDown = [row + 1..maxRow] |> List.map (fun r -> input[r][col])
            let treesLeft = [col - 1..-1..0] |> List.map (fun c -> input[row][c])
            let treesRight = [col + 1..maxCol] |> List.map (fun c -> input[row][c])

            height, [ treesUp; treesDown; treesLeft; treesRight ]

        List.allPairs [0..maxRow] [0..maxCol] |> List.map getResult

    let Part1() =
        let input = parseInput()
        let (maxRow, maxCol) = input.Length - 1, input[0].Length - 1

        let isHidden (row, col) =
            let height = input[row][col]
            let hiddenFromTop = [0..row - 1] |> List.exists (fun r -> input[r][col] >= height)
            let hiddenFromBottom = [row + 1..maxRow] |> List.exists (fun r -> input[r][col] >= height)
            let hiddenFromLeft = [0..col - 1] |> List.exists (fun c -> input[row][c] >= height)
            let hiddenFromRight = [col + 1..maxCol] |> List.exists (fun c -> input[row][c] >= height)

            hiddenFromTop && hiddenFromBottom && hiddenFromLeft && hiddenFromRight

        List.allPairs [0..maxRow] [0..maxCol] |> countIf (isHidden >> not)


    let Part2() =
        let getScore (height: int) (trees: int list) = trees |> takeUntil (fun x -> x >= height) |> Seq.length

        getResults()
        |> List.map (fun (h, ts) -> ts |> List.map (getScore h) |> List.reduce (*))
        |> List.max
