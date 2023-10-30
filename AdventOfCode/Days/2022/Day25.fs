namespace AdventOfCode.Days.Y2022

open AdventOfCode.Shared.Utility
open AdventOfCode.Shared.Constants
open Checked

/// Day 25: Full of Hot Air
/// https://adventofcode.com/2022/day/25
/// As the expedition finally reaches the extraction point, several large hot air balloons drift down to meet you.
module Day25 =

    let getValue = function
        | '-' -> -1L
        | '=' -> -2L
        | CInt64 x -> x
        | x -> failwithf "Invalid input: %c" x

    let parse (s: string) = // E.g. 21==112=11=100-
        (0L, s)
        ||> Seq.fold (fun acc x -> (acc * 5L) + (getValue x))

    let parseInput() = getFile (2022, 25) |> readLinesAs parse

    let getSnafu (x: int64) =
        let getChar = function
            | 4L -> (-1L, "-")
            | 3L -> (-2L, "=")
            | x -> (x, string x)

        let rec getSnafuR (x: int64) (s: string) =
            match x with
            | 0L -> s
            | _ -> let rem = x % 5L
                   let (v, c) = getChar rem
                   getSnafuR ((x - v) / 5L) $"{c}{s}"

        getSnafuR x ""


    // The Elves are starting to get cold. What SNAFU number do you supply to Bob's console?
    let Part1() = parseInput() |> Seq.sum |> getSnafu

    let Part2() = 0 // GotAllTheStars