namespace AdventOfCode.Days.Y2015

open AdventOfCode.Shared.Utility
open AdventOfCode.Shared

/// Day 3: Perfectly Spherical Houses in a Vacuum
/// https://adventofcode.com/2015/day/3
/// Santa is delivering presents to an infinite two-dimensional grid of houses.
module Day03 =
    let parse = function
        | '^' -> Direction2d.North
        | '>' -> Direction2d.East
        | 'v' -> Direction2d.South
        | '<' -> Direction2d.West
        | x -> failwithf "Unexpected input: %c" x

    // E.g. ">^^v^<>v<<<v<v^>>v^^^ ..."
    let parseInput() = getFile (2015, 3) |> readAllText |> Seq.map parse

    // How many houses receive at least one present?
    let Part1() =
        let directions = parseInput()
        let santa = Point2d.Origin

        (santa, directions)
        ||> Seq.scan (fun loc dir -> loc + dir) 
        |> Seq.distinct
        |> Seq.length

    // The next year, to speed up the process, Santa creates a robot version of himself,
    // Robo-Santa, to deliver presents with him.
    // This year, how many houses receive at least one present?
    let Part2() =
        let directions = parseInput()
        let (santa, robot) = (Point2d.Origin, Point2d.Origin)

        ((santa, robot), directions) // Swap santa and robot after each direction
        ||> Seq.scan (fun (loc1, loc2) dir -> (loc2, loc1 + dir)) 
        |> Seq.distinctBy snd
        |> Seq.length