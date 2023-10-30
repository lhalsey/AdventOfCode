namespace AdventOfCode.Days.Y2022

open AdventOfCode.Shared.Utility
open AdventOfCode.Shared

/// Day 9: Rope Bridge
/// https://adventofcode.com/2022/day/9
/// This rope bridge creaks as you walk along it. You aren't sure how old it is,
/// or whether it can even support your weight.
module Day09 =

    let parse (s: string) = // E.g. D 10
        let (dir, steps) = s |> splitIntoPair " "
        Seq.replicate (int steps) (Direction2d.GetDirection dir[0])

    let parseInput() = getFile (2022, 9) |> readLinesAs parse |> Seq.concat

    // If the head is ever two steps directly up, down, left, or right from the tail,
    // the tail must also move one step in that direction so it remains close enough.
    // Otherwise, if the head and tail aren't touching and aren't in the same row or column,
    // the tail always moves one step diagonally to keep up.
    let getPosition knot1 knot2 =
        let offset = knot1 - knot2
        if abs offset.X <= 1 && abs offset.Y <= 1 then knot2 else knot2 + offset.Unit

    let move (knots: Point2d list, visited: Point2d Set) (dir: Direction2d)  =
        // Move the head according to the next instruction and then move each knot in order
        // threading the previous knot through to determine the new position
        let knots =
            match knots with
            | h::t -> (h + dir, t) ||> List.scan getPosition
            | _ -> failwith "Rope needs at least one knot"

        let visited = visited.Add knots[knots.Length - 1]

        (knots, visited)

    let getVisited numKnots =
        let input = parseInput()
        let knots = List.replicate numKnots Point2d.Origin
        let state = (knots, set [ Point2d.Origin ])

        let (_, visited) = (state, input) ||> Seq.fold move

        visited.Count

    // Simulate your complete hypothetical series of motions.
    // How many positions does the tail of the rope visit at least once?
    let Part1() = getVisited 2
      
    // Simulate your complete series of motions on a larger rope with ten knots.
    // How many positions does the tail of the rope visit at least once?
    let Part2() = getVisited 10