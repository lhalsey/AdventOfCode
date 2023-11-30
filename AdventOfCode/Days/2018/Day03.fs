namespace AdventOfCode.Days.Y2018

open AdventOfCode.Shared.Utility
open AdventOfCode.Shared

/// Day 3: No Matter How You Slice It
/// https://adventofcode.com/2018/day/3
/// The Elves managed to locate the chimney-squeeze prototype fabric for Santa's suit (thanks to someone who helpfully wrote its box IDs on the wall of the warehouse in the middle of the night).
module Day03 =

    type Rect = { Id: int; Start: Point2d; End: Point2d } with
        member __.AllPoints =
            List.allPairs [__.Start.Y .. __.End.Y] [__.Start.X .. __.End.X]
            |> List.map (fun (y, x) -> { X = x; Y = y })

    let parse = function // E.g. #1 @ 49,222: 19x20
        | Regex "#(\d+) @ (\d+),(\d+): (\d+)x(\d+)" [Int id; Int x; Int y; Int w; Int h] ->
            { Id = id; Start = { X = x; Y = y }; End = { X = x + w - 1; Y = y + h - 1 }}
        | x -> failwithf "Invalid input: %s" x
        
    let parseInput() = getFile (2018, 3) |> readLinesAs parse

    let getCellCount (rects: Rect seq) =       
        rects
        |> Seq.collect (fun x -> x.AllPoints)
        |> Seq.countBy id

    // If the Elves all proceed with their own plans, none of them will have enough fabric.
    // How many square inches of fabric are within two or more claims?
    let Part1() = parseInput() |> getCellCount |> countIf (fun (_, v) -> v > 1)

    // What is the ID of the only claim that doesn't overlap?
    let Part2() =
        let rects = parseInput()
        let cellCount = rects |> getCellCount |> readOnlyDict

        rects
        |> Seq.find (fun x -> x.AllPoints |> Seq.forall (fun p -> cellCount[p] = 1))
        |> fun x -> x.Id

