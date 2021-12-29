namespace AdventOfCode.Days.Y2021

open AdventOfCode.Shared.Utility
open AdventOfCode.Shared

/// Day 13: Transparent Origami
/// https://adventofcode.com/2021/day/13
/// You reach another volcanically active part of the cave.
module Day13 =

    type Fold = FoldAlongX of int | FoldAlongY of int

    type Input = Dot of Point2d | Fold of Fold | Blank

    let parse = function
        | Regex "fold along x=(\d+)" [Int x] -> Fold (FoldAlongX x)
        | Regex "fold along y=(\d+)" [Int y] -> Fold (FoldAlongY y)
        | Regex "(\d+),(\d+)" [Int x; Int y] -> Dot { X = x; Y = y; }
        | _ -> Blank

    let parseInput() =
        let input = getFile (2021, 13) |> readLinesAs parse

        let dots = input |> Seq.choose (function Dot p -> Some p | _ -> None) |> Seq.toList
        let folds = input |> Seq.choose (function Fold f -> Some f | _ -> None) |> Seq.toList

        dots, folds

    let foldAlongY (y: int) (dots: Point2d list) = 
        dots
        |> List.map (fun p -> { p with Y = y - (abs (y - p.Y))})
        |> List.distinct

    let foldAlongX (x: int) (dots: Point2d list) = 
        dots
        |> List.map (fun p -> { p with X = x - (abs (x - p.X))})
        |> List.distinct

    let foldAlong (dots: Point2d list) = function
        | FoldAlongY y -> foldAlongY y dots
        | FoldAlongX x -> foldAlongX x dots

    let drawImage (points: Point2d list) =
        let pointsSet = set points

        let maxX = pointsSet |> Set.map (fun p -> p.X) |> Set.maxElement
        let maxY = pointsSet |> Set.map (fun p -> p.Y) |> Set.maxElement

        List.allPairs [0..maxY] [0..maxX]
        |> List.map (fun (y, x) -> if pointsSet.Contains { X = x; Y = y } then '█' else ' ')
        |> List.chunkBySize (maxX + 1)
        |> List.map (List.toArray >> System.String)
        |> String.concat "\n"

    // How many dots are visible after completing just the first fold instruction on your transparent paper?
    let Part1() =
        let (dots, folds) = parseInput()

        folds.Head
        |> foldAlong dots
        |> List.length

    // Finish folding the transparent paper according to the instructions. The manual says the code is always
    // eight capital letters.
    // What code do you use to activate the infrared thermal imaging camera system?
    let Part2() =
        parseInput()
        ||> List.fold foldAlong
        |> drawImage