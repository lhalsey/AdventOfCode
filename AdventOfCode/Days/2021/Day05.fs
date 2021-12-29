namespace AdventOfCode.Days.Y2021

open AdventOfCode.Shared.Utility
open AdventOfCode.Shared

/// Day 5: Hydrothermal Venture
/// https://adventofcode.com/2021/day/5
/// You come across a field of hydrothermal vents on the ocean floor!
module Day05 =

    type Line = { P1: Point2d; P2: Point2d } with
        member __.Dx = __.P2.X - __.P1.X
        member __.Dy = __.P2.Y - __.P1.Y
        member __.UnitDirection = { Direction2d.X = sign __.Dx; Y = sign __.Dy }
        member __.Length = max (abs __.Dx) (abs __.Dy)

    let parse = function // E.g. 797,773 -> 168,144
        | Regex "(\d+),(\d+) -> (\d+),(\d+)" [Int x1; Int y1; Int x2; Int y2 ] ->
            { P1 = { X = x1; Y = y1 }; P2 = { X = x2; Y = y2 } }
        | _ -> failwith "Invalid input"
        
    let parseInput() = getFile (2021, 5) |> readLinesAs parse

    let getPointsOnLine (line: Line) =
        line.P1.GetPointsInDirection line.UnitDirection
        |> Seq.take (line.Length + 1)

    let getOverlappingPointCount (pred: Line -> bool) =
        parseInput()
        |> Seq.filter pred
        |> Seq.collect getPointsOnLine
        |> Seq.countBy id
        |> countIf (fun (_, c) -> c >= 2)

    // Consider only horizontal and vertical lines. At how many points do at least two lines overlap?
    let Part1() = getOverlappingPointCount (fun x -> x.UnitDirection.X = 0 || x.UnitDirection.Y = 0)
        
    // Consider all of the lines. At how many points do at least two lines overlap?
    let Part2() = getOverlappingPointCount (fun _ -> true)