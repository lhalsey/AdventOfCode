namespace AdventOfCode.Days.Y2024

open AdventOfCode.Shared.Utility
open System.Text.RegularExpressions

/// Day 3: Mull It Over
/// https://adventofcode.com/2024/day/3
/// "Our computers are having issues, so I have no idea if we have any Chief Historians in stock!
/// You're welcome to check the warehouse, though," 
module Day03 =

    let parseInput() = getFile (2024, 3) |> readAllText

    // Scan the corrupted memory for uncorrupted mul instructions.
    // What do you get if you add up all of the results of the multiplications?
    let Part1() =
        let input = parseInput()

        let instructions = Regex.Matches(input, "mul\((\d+),(\d+)\)+")

        instructions |> Seq.sumBy (fun x -> (int x.Groups[1].Value) * (int x.Groups[2].Value))

    // Handle the new instructions; what do you get if you add up all of the results of just the enabled multiplications?
    let Part2() =
        let input = parseInput()

        let instructions = Regex.Matches(input, "mul\((\d+),(\d+)\)+|do\(\)|don't\(\)")

        let processInstruction (isEnabled, sum) (x: Match) =
            match x.Groups[0].Value[..2] with
            | "mul" -> (isEnabled, sum + isEnabled * (int x.Groups[1].Value) * (int x.Groups[2].Value))
            | "do(" -> (1, sum)
            | "don" -> (0, sum)
            | _ -> failwithf "Invalid input %A" x

        ((1, 0), instructions)
        ||> Seq.fold processInstruction
        |> snd