namespace AdventOfCode.Days.Y2022

open AdventOfCode.Shared.Utility
open System

/// Day 5: Supply Stacks
/// https://adventofcode.com/2022/day/5
/// The expedition can depart as soon as the final supplies have been unloaded from the ships.
module Day05 =

    type Input = 
        | Move of int * int * int
        | Row of int option []

    let parse = function
        | Regex "move (\d+) from (\d+) to (\d+)" [Int a; Int b; Int c] -> Move (a, b - 1, c - 1) |> Some
        | _ -> None

    let parseInput() = getFile (2022, 5) |> readLinesAs parse |> Seq.choose id

    let getTopCrates fMove =
        let moves = parseInput()

        let stacks = [| "ZVTBJGR"; "LVRJ"; "FQS"; "GQVFLNHZ"; "WMSCJTQR"; "FHCTWS"; "JNFVCZD"; "QFRWDZGL"; "PVWBJ" |]

        let move (stacks: string[]) (instruction: Input) = 
            match instruction with
            | Move (num, source, dest) -> 
                let crates = stacks[source][..num - 1] |> fMove
                let rest = stacks[source][num ..]
                stacks[source] <- rest
                stacks[dest] <- $"{crates}{stacks[dest]}"
                stacks
            | _ -> stacks

        let stacks =
            (stacks, moves)
            ||> Seq.fold (fun acc x -> move acc x)

        stacks |> Array.map Seq.head |> String

    // Crates are moved one at a time
    // After the rearrangement procedure completes, what crate ends up on top of each stack?
    let Part1() = getTopCrates reverse

    // Multiple crates are moved
    // After the rearrangement procedure completes, what crate ends up on top of each stack?
    let Part2() = getTopCrates id