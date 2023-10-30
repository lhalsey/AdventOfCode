namespace AdventOfCode.Days.Y2017

open AdventOfCode.Shared.Utility

/// Day 5: A Maze of Twisty Trampolines, All Alike
/// https://adventofcode.com/2017/day/5
/// An urgent interrupt arrives from the CPU: it's trapped in a maze of jump instructions, and it would like assistance from any programs with spare cycles to help find the exit.
module Day05 =

    let parseInput() = getFile (2017, 5) |> readLinesAs int |> Seq.toArray

    let getSteps (offsetF: int -> int) (input: int[]) =
        let rec getStepsR (input: int[]) index steps =
            if index < 0 || index >= input.Length then steps else
                let move = input[index]
                input[index] <- offsetF move
                getStepsR input (index + move) (steps + 1)

        getStepsR input 0  0

    let Part1() = parseInput() |> getSteps (fun x -> x + 1)

    let Part2() = parseInput() |> getSteps (fun x -> if x >= 3 then x - 1 else x + 1)