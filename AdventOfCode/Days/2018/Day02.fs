namespace AdventOfCode.Days.Y2018

open AdventOfCode.Shared.Utility

/// Day 2: Inventory Management System
/// https://adventofcode.com/2018/day/2
/// You stop falling through time, catch your breath, and check the screen on the device.
module Day02 =

    let parseInput() = getFile (2018, 2) |> readLines

    let getSequenceLengths (elems: 'a seq) =
        elems
        |> Seq.groupBy id
        |> Seq.map (fun (_, x) -> Seq.length x)
        |> toHashSet

    let getCount (c2, c3) elems =
        let hs = getSequenceLengths elems

        let c2 = if hs.Contains 2 then c2 + 1 else c2
        let c3 = if hs.Contains 3 then c3 + 1 else c3

        (c2, c3)

    let tryFindMatchExcludingIndex (ids: string seq) (n: int) =
        ids
        |> Seq.map (fun x -> $"{x[..n - 1]}{x[n + 1..]}")
        |> tryFindDuplicate

    // Counting the number that have an ID containing exactly two of any letter and then separately counting those
    // with exactly three of any letter. You can multiply those two counts together to get a rudimentary checksum.
    // What is the checksum for your list of box IDs?
    let Part1() = parseInput() |> Seq.fold getCount (0, 0) ||> (*)

    // What letters are common between the two correct box IDs?
    let Part2() =
        let input = parseInput()

        Seq.initInfinite id |> Seq.pick (tryFindMatchExcludingIndex input)