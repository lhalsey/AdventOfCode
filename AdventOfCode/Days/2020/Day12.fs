namespace AdventOfCode.Days.Y2020

open AdventOfCode.Shared.Utility
open AdventOfCode.Shared

/// Day 12: Rain Risk
/// https://adventofcode.com/2020/day/12
/// Your ferry made decent progress toward the island, but the storm came in faster than anyone expected
module Day12 =

    type Instruction =
        North of int | South of int | East of int | West of int | Left of int | Right of int | Forward of int

    type DirectionMode = Ship | Waypoint

    // E.g. "L90"
    let parse (s: string) =
        match s.[0], int s.[1..] with
        | 'N', x -> North x
        | 'S', x -> South x
        | 'E', x -> East x
        | 'W', x -> West x
        | 'L', x -> Left x
        | 'R', x -> Right x
        | 'F', x -> Forward x
        | _ -> failwithf "Invalid input: %s" s
        
    let parseInput() = getFile (2020, 12) |> readLinesAs parse

    // For Part 1 we use 'Ship' mode where N, S, E & W result in the ship moving in
    // that direction regardless of which direction it is facing. The left, right &
    // forward instructions affect the ship itself.
    // For Part 2, we use 'WayPoint' mode where N, S, E & W result in the waypoint
    // moving. Left & Right rotate the waypoint around the ship and Forward means move
    // forward to the waypoint a number of times equal to the value.
    let getDistance dir directionMode =
        let executeInstruction (shipLoc: Point2d, dir: Direction2d) (instr: Instruction) =
            let turn turnDir times =
                [ 1..times ] |> List.fold (fun acc _ -> turnDir acc) dir

            let move instrDir dist =
                match directionMode with
                | Ship -> shipLoc + instrDir * dist, dir
                | Waypoint -> shipLoc, dir + instrDir * dist

            match instr with
            | North dist -> move Direction2d.North dist
            | South dist -> move Direction2d.South dist
            | East dist -> move Direction2d.East dist
            | West dist -> move Direction2d.West dist
            | Left angle -> shipLoc, turn Direction2d.TurnLeft (angle / 90)
            | Right angle -> shipLoc, turn Direction2d.TurnRight (angle / 90)
            | Forward dist -> shipLoc + dir * dist, dir

        ((Point2d.Origin, dir), parseInput())
        ||> Seq.fold executeInstruction
        |> fun (loc, _) -> loc.ManhattanDistance

    // Figure out where the navigation instructions lead. What is the Manhattan distance
    // between that location and the ship's starting position?
    let Part1() = getDistance Direction2d.East DirectionMode.Ship

    // Figure out where the navigation instructions actually lead. What is the Manhattan distance
    // between that location and the ship's starting position?
    let Part2() = getDistance { X = 10; Y = -1 } DirectionMode.Waypoint