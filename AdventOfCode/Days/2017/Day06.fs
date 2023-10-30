namespace AdventOfCode.Days.Y2017

open AdventOfCode.Shared.Utility

/// Day 6: Memory Reallocation
/// https://adventofcode.com/2017/day/6
/// A debugger program here is having an issue: it is trying to repair a memory reallocation routine, but it keeps getting stuck in an infinite loop.
module Day06 =

    let parseInput() = getFile (2017, 6) |> readAllText |> split '\t' |> Array.map int

    let redistribute (banks: int[]) =
        let rec redistributeR (banks: int[]) index remainingBlocks =
            if remainingBlocks = 0 then banks else
                let index = if index < banks.Length - 1 then index + 1 else 0
                banks[index] <- banks[index] + 1
                redistributeR banks index (remainingBlocks - 1)

        let banks = Array.copy banks
        let most = Array.max banks
        let index = banks |> Array.findIndex (fun x -> x = most)
        banks[index] <- 0
        redistributeR banks index most

    let distributionToString (banks: int[]) = banks |> Seq.map string |> String.concat ","

    let getCyclesUntilDuplicate (banks: int[]) =
        banks
        |> Seq.unfold (fun x -> Some(x, redistribute x))
        |> countUntilDuplicateBy distributionToString

    // Given the initial block counts in your puzzle input, how many redistribution cycles
    // must be completed before a configuration is produced that has been seen before?
    let Part1() = parseInput() |> getCyclesUntilDuplicate
        
    // How many cycles are in the infinite loop that arises from the configuration in your puzzle input?
    let Part2() =
        let firstCycle =
            parseInput()
            |> Seq.unfold (fun x -> Some(x, redistribute x))
            |> findDuplicateBy distributionToString

        getCyclesUntilDuplicate firstCycle