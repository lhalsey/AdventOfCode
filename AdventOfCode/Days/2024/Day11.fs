namespace AdventOfCode.Days.Y2024

open AdventOfCode.Shared.Utility
open Checked

/// Day 11: Plutonian Pebbles
/// https://adventofcode.com/2024/day/11
/// The ancient civilization on Pluto was known for its ability to manipulate spacetime, and while
/// The Historians explore their infinite corridors, you've noticed a strange set of physics-defying stones.
module Day11 =

    let parseInput() = getFile (2024, 11) |> readAllText |> split ' '

    let split (stone: string) =

        let getStone (s: string) =
            let s' = trimStart '0' s
            if s' = "" then "0" else s'

        seq {
            yield getStone stone[..stone.Length/2 - 1]
            yield getStone stone[stone.Length/2 ..]
        }

    let transform (stone: string) =
        seq {
            match stone with
            | "0" -> yield "1"
            | s when s.Length % 2 = 0 -> yield! split s
            | Int64 i -> yield i * 2024L |> string
            | x -> failwithf "Invalid input: %A" x
        }

    let splitStones (times: int) =
        let rec splitStoneR (stone: string, t: int) =
            match t with
            | 0 -> 1L // No more splits, single stone
            | _ -> let stones = transform stone
                   stones |> Seq.sumBy (fun s -> splitStonesMemo (s, t - 1))
        and splitStonesMemo = memoise splitStoneR

        parseInput()
        |> Array.map (fun s -> splitStonesMemo (s, times))
        |> Array.fold (+) 0L

    // Consider the arrangement of stones in front of you. How many stones will you have after blinking 25 times?
    let Part1() = splitStones 25

    // How many stones would you have after blinking a total of 75 times?
    let Part2() = splitStones 75