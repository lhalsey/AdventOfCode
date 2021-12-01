namespace AdventOfCode.Days.Y2018

open AdventOfCode.Shared.Utility
open AdventOfCode.Shared

/// Day 20: A Regular Map
/// https://adventofcode.com/2018/day/20
/// While you were learning about instruction pointers, the Elves made considerable progress.
module Day20 =

    // E.g. "^ENWWW(NEEE|SSE(EE|N))$"
    let parseInput() = getFile (2018, 20) |> readAllText

    let getRoomDistances() =

        let rec traverseR (dirs: char list) (loc: Point2d) (locs: Point2d list) (dist: int) (map: Map<Point2d, int>) =
            match dirs, locs with
            | '^'::t, _ -> traverseR t loc locs dist map // Start - skip token
            | '$'::[], _ -> map |> Seq.map (fun x -> x.Value) // End - return min distance to each room
            | dir::t, _ when "NEWS".Contains dir -> // Direction - move and check if we have been here before
                let loc = loc + Direction2d.GetDirection dir // If so, then reset distance if it's less than current
                let dist =
                    match map.TryFind loc with
                    | Some v -> min (dist + 1) v
                    | None -> dist + 1

                traverseR t loc locs dist (map.Add(loc, dist))
            | '|'::t, l::locs -> // Branch - Revert to previous location & distance when group started
                let dist = map.[l]
                traverseR t l (l::locs) dist map
            | '('::t, locs -> traverseR t loc (loc::locs) dist map // Start of branch group - store current location
            | ')'::t, _::locs -> traverseR t loc locs dist map // End of branch group - discard location from stack
            | x, _ -> failwithf "Invalid input: %A" x

        let route = parseInput() |> Seq.toList
        traverseR route Point2d.Origin [] 0 Map.empty
        

    // What is the largest number of doors you would be required to pass through to reach a room?
    let Part1() = getRoomDistances() |> Seq.max

    // How many rooms have a shortest path from your current location that pass through at least 1000 doors?
    let Part2() = getRoomDistances() |> countIf (fun x -> x >= 1_000)