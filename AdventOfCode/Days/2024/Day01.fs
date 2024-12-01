namespace AdventOfCode.Days.Y2024

open AdventOfCode.Shared.Utility
open System

/// Day 1: Historian Hysteria
/// https://adventofcode.com/2024/day/1
/// The Chief Historian is always present for the big Christmas sleigh launch, but nobody has seen him in months!
module Day01 =

    let parseRow (s: string) = s |> split ' ' |> fun x -> Int32.Parse x[0], Int32.Parse x[1]

    let parseInput() = getFile (2024, 1) |> readLinesAs parseRow |> Seq.toList

    let Part1() =

        let (l1, l2) = parseInput() |> List.unzip

        List.zip (List.sort l1) (List.sort l2)
        |> List.sumBy (fun (x, y) -> abs (x - y))

    let Part2() =

        let (l1, l2) = parseInput() |> List.unzip

        let l2FreqMap = l2 |> List.countBy id |> readOnlyDict

        let getValue x =
            match tryFind x l2FreqMap with
            | Some count -> x * count
            | None -> 0

        l1 |> List.sumBy getValue