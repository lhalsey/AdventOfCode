namespace AdventOfCode.Days.Y2021

open AdventOfCode.Shared.Utility

/// Day 2: Dive!
/// https://adventofcode.com/2021/day/2
/// Now, you need to figure out how to pilot this thing.
module Day02 =

    type Command = Forward of int | Down of int | Up of int

    type Location = { Horizontal: int; Depth: int; Aim: int }

    let parse = function
        | Regex "forward (\d+)" [Int d] -> Forward d
        | Regex "down (\d+)" [Int d] -> Down d
        | Regex "up (\d+)" [Int d] -> Up d
        | x -> failwithf "Invalid input: %s" x

    let move loc = function
        | Forward d -> { loc with Horizontal = loc.Horizontal + d }
        | Down d -> { loc with Depth = loc.Depth + d }
        | Up d -> { loc with Depth = loc.Depth - d }

    let moveWithAim loc = function
        | Forward d -> { loc with Horizontal = loc.Horizontal + d; Depth = loc.Depth + (loc.Aim * d) }
        | Down d -> { loc with Aim = loc.Aim + d }
        | Up d -> { loc with Aim = loc.Aim - d }

    let parseInput() = getFile (2021, 2) |> readLinesAs parse

    let getLocation moveF =
        let commands = parseInput()
        let start = { Horizontal = 0; Depth = 0; Aim = 0 }
        let location = (start, commands) ||> Seq.fold moveF

        location.Horizontal * location.Depth

    // Calculate the horizontal position and depth you would have after following the planned course.
    // What do you get if you multiply your final horizontal position by your final depth?
    let Part1() = getLocation move
        
    // Using this new interpretation of the commands, calculate the horizontal position and depth you would have
    // after following the planned course.
    // What do you get if you multiply your final horizontal position by your final depth?
    let Part2() = getLocation moveWithAim