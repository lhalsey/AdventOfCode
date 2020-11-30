namespace AdventOfCode.Days.Y2016

open AdventOfCode.Shared.Utility
open AdventOfCode.Shared

/// Day 1: No Time for a Taxicab
/// https://adventofcode.com/2016/day/1
/// Santa's sleigh uses a very high-precision clock to guide its movements, and the clock's oscillator is regulated by stars.
module Day01 =

    // E.g. "L1, R3, R1, L5, L2 ..."
    let parse (s: string) =
        s.Split (", ")
        |> Array.map (fun x -> Direction2d.GetTurn x.[0], int x.[1..])

    let parseInput() = getFile (2016, 1) |> readAllText |> parse

    let getLocations() =
        let input = parseInput() |> Seq.toList

        let rec getLocationsR input loc dir =
            seq {
                match input with
                | [] -> ()
                | (turn, dist)::t ->
                    let dir = turn dir

                    let (locs, loc) = // Travel one block at a time in new direction
                        (loc, seq { 1 .. dist })
                        ||> Seq.mapFold (fun acc _ -> (acc + dir, acc + dir))

                    yield! locs
                    yield! getLocationsR t loc dir
                }

        getLocationsR input Point2d.Origin Direction2d.North


    // How many blocks away is Easter Bunny HQ?
    let Part1() = getLocations() |> Seq.last |> fun x -> x.ManhattanDistance

    // How many blocks away is the first location you visit twice?
    let Part2() = getLocations() |> findDuplicate |> fun x -> x.ManhattanDistance