namespace AdventOfCode.Days.Y2017

open AdventOfCode.Shared.Utility

/// Day 1: Inverse Captcha
/// https://adventofcode.com/2017/day/1
/// The night before Christmas, one of Santa's Elves calls you in a panic.
module Day01 =

    let parseInput() =
        getFile (2017, 1)
        |> readAllText
        |> Seq.map (System.Char.GetNumericValue >> int)
        |> Seq.toList

    let getSum offset input =

        input @ input
        |> List.skip offset
        |> Seq.zip input
        |> Seq.sumBy (fun (x, y) -> if x = y then x else 0)

    // What is the solution to your captcha?
    let Part1() = parseInput() |> getSum 1
        
    // What is the solution to your new captcha?
    let Part2() = 
        let input = parseInput()
        getSum (input.Length / 2) input