namespace AdventOfCode.Days.Y2023

open AdventOfCode.Shared.Utility
open FSharp.Collections.ParallelSeq

/// Day 12: Hot Springs
/// https://adventofcode.com/2023/day/12
/// You finally reach the hot springs! You can see steam rising from secluded areas attached
/// to the primary, ornate building.
module Day12 =

    type Record = { Springs: char list; Counts: int list }

    let parse (s: string) =  // E.g. ???.### 1,1,3
        let tokens = s |> split ' '
        let counts = tokens[1] |> split ',' |> Array.map int |> Array.toList

        { Springs = Seq.toList tokens[0]; Counts = counts }

    let parseInput() = getFile (2023, 12) |> readLinesAs parse

    let getArrangements (record: Record) =
        let rec getArrangementsR (springs: char list) (counts: int list) (count: int) =
            match springs, counts with
            | [], [] -> if count = 0 then 1 else 0
            | [], [c] -> if count = c then 1 else 0
            | [], _ -> 0
            | '.'::t, cs when count = 0 -> getArrangementsR t cs 0
            | '.'::t, c::cs -> if count = c then getArrangementsR t cs 0 else 0
            | '#'::_, [] -> 0
            | '#'::t, c::cs -> if count < c then getArrangementsR t (c::cs) (count + 1) else 0
            | '?'::t, cs ->
                let assumeSpring = getArrangementsR ('#'::t) cs count
                let assumeNotSpring = getArrangementsR ('.'::t) cs count
                assumeSpring + assumeNotSpring
            | x -> failwithf "Invalid input: %A" x

        getArrangementsR record.Springs record.Counts 0

    let unfold (record: Record) =
        let springs = record.Springs @ '?'::record.Springs @ '?'::record.Springs @ '?'::record.Springs @ '?'::record.Springs
        let counts = List.replicate 5 record.Counts |> List.concat

        { Springs = springs; Counts = counts }

    // For each row, count all of the different arrangements of operational and broken springs
    // that meet the given criteria. What is the sum of those counts?
    let Part1() = parseInput() |> Seq.sumBy getArrangements
        
    // Unfold your condition records; what is the new sum of possible arrangement counts?
    let Part2() =
         parseInput()
         |> PSeq.map unfold
         |> PSeq.sumBy getArrangements