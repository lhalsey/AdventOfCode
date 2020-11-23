namespace AdventOfCode.Days.Y2019

open AdventOfCode.Shared.Utility
open AdventOfCode.Shared

open System

/// Day 3: Crossed Wires
/// https://adventofcode.com/2019/day/3
/// The gravity assist was successful, and you're well on your way to the Venus refuelling station.
module Day03 =

    // E.g. "R990" -> (East, 990)
    let parseInstruction (instr: string) = Direction2d.GetDirection instr.[0], int instr.[1..]

    // TODO: Consider comparing wire sections rather than points on wire for performance improvement
    let expandMovement (dir, dist) = dir |> Seq.replicate dist
         
    let getWireLocations (instructions: string) =
        instructions
        |> split ','
        |> Seq.map parseInstruction    // Extract direction & distance
        |> Seq.collect expandMovement  // Expand to unit movements
        |> Seq.scan (+) Point2d.Origin // Trace path unit by unit
        |> Seq.skip 1                  // Don't include central port

    let parseInput() = getFile (2019, 3) |> readLinesAs getWireLocations

    // Opening the front panel reveals a jumble of wires.
    // Specifically, two wires are connected to a central port and extend outward on a grid.
    // You trace the path each wire takes as it leaves the central port, one wire per line
    // of text (your puzzle input).
    // What is the Manhattan distance from the central port to the closest intersection?
    let Part1() =

        let getDistanceMap (points: Point2d seq) =
            points
            |> Seq.groupBy (fun x -> x.ManhattanDistance)

        let wireMaps =
            parseInput()
            |> Seq.map getDistanceMap
            |> Seq.toList

        let wireMap2 = wireMaps.[1] |> readOnlyDict

        let hasCommonElement (dist: int, points: Point2d seq) =
            let s1 = set points

            match wireMap2.TryGetValue dist with
            | (true, s2) -> s2 |> Seq.exists s1.Contains
            | _ -> false
   
        wireMaps.[0] // Faster to sort by distance and terminate when we have found match
        |> Seq.sortBy fst // than to compare all values and select minimum
        |> Seq.find hasCommonElement
        |> fst


    // What is the fewest combined steps the wires must take to reach an intersection?
    let Part2() =

        let getWireMap locations =
            locations
            |> Seq.mapi (fun steps loc -> loc, steps + 1) // Track steps from central port
            |> Seq.distinctBy fst // Take lowest steps if revisited
            |> readOnlyDict
           
        let wirePaths = parseInput() |> Seq.toList

        let wireMap2 = getWireMap wirePaths.[1]

        let rec getMinIntersection (points: Point2d list, steps: int, min: int) =
            match points with
            | [] -> min                 // Traversed whole path
            | _ when steps > min -> min // Gone too far to find new min
            | current::tail ->          // Check if current point is intersection & min

                let min = 
                    match wireMap2.TryGetValue current with
                    | (true, d) when d + steps < min -> d + steps
                    | _ -> min

                getMinIntersection(tail, steps + 1, min)
  
        getMinIntersection (Seq.toList wirePaths.[0], 1, Int32.MaxValue)      