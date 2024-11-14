namespace AdventOfCode.Days.Y2023

open AdventOfCode.Shared.Utility

/// Day 6: Wait For It
/// https://adventofcode.com/2023/day/6
/// The ferry quickly brings you across Island Island.
module Day06 =

    type Race = { Milliseconds: int64; Millimeters: int64 }

    let getNums (s: string) = s |> split ' ' |> Array.skip 1 |> Array.map int 

    let parse (lines: string array) =
        Array.zip (getNums lines[0]) (getNums lines[1])
        |> Array.map (fun (t, d) -> { Milliseconds = t; Millimeters = d })

    let parseInput() = getFile (2023, 6) |> readAllLines |> parse

    let getDistance (race: Race) (holdMs: int64) = (race.Milliseconds - holdMs) * holdMs

    let getWaysToBeat (race: Race) =
        [0L..race.Milliseconds]
        |> List.map (getDistance race)
        |> countIf (fun x -> x > race.Millimeters)

    let rec binarySearch pred lower upper =
        let mid = (lower + upper) / 2L

        if lower = mid then mid 
        else if pred mid then binarySearch pred mid upper
        else binarySearch pred lower mid

    // Determine the number of ways you could beat the record in each race.
    // What do you get if you multiply these numbers together?
    let Part1() =
        parseInput()
        |> Array.map getWaysToBeat
        |> Array.reduce (*)

    // How many ways can you beat the record in this one much longer race?
    let Part2() =
        let race =
            (("", ""), (parseInput()))
            ||> Array.fold (fun (a1, a2) x -> ($"{a1}{x.Milliseconds}", $"{a2}{x.Millimeters}" ) )
            |> fun (ms, mm) -> { Milliseconds = int64 ms; Millimeters = int64 mm }

        let lowerBoundPred (x: int64) =
            let dist = getDistance race x 
            let prev = getDistance race (x - 1L)
            dist > prev && dist < race.Millimeters

        let upperBoundPred (x: int64) =
            let dist = getDistance race x 
            let prev = getDistance race (x - 1L)
            dist > prev || dist > race.Millimeters

        // Do a binary search to find first annd last winning times
        let lower = binarySearch lowerBoundPred 0L race.Milliseconds
        let upper = binarySearch upperBoundPred 0L race.Milliseconds

        upper - lower