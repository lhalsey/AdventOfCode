namespace AdventOfCode.Days.Y2020

open AdventOfCode.Shared.Utility

/// Day 5: Binary Boarding
/// https://adventofcode.com/2020/day/5
/// You board your plane only to discover a new problem: you dropped your boarding pass!
module Day05 =

    let parseInput() = getFile (2020, 5) |> readLines //As (Seq.map parse)

    // Observe that the binary partitioning to narrow the range in row or column is simply
    // equivalent to parsing the zones as binary digits
    let getSeatId (zones: string) =
        let getBit = function 'B' | 'R' -> 1 | _ -> 0

        zones |> Seq.fold (fun acc x -> acc * 2 + getBit x) 0


    // As a sanity check, look through your list of boarding passes. What is the highest seat ID
    // on a boarding pass?
    let Part1() = parseInput() |> Seq.map getSeatId |> Seq.max

    // It's a completely full flight, so your seat should be the only missing boarding pass in your list
    // What is the ID of your seat?
    let Part2() =
        parseInput()
        |> Seq.map getSeatId
        |> Seq.sort      // Sort and find non-consecutive pair of seat ids
        |> Seq.pairwise  // Could use Gauss to calculate expected sums and take the difference
        |> Seq.find (fun (x, y) -> x <> y - 1) // But it's mot significantly quicker
        |> fun (x, _) -> x + 1