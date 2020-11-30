namespace AdventOfCode.Days.Y2015

open System.Collections.Generic
open AdventOfCode.Shared.Utility
open FSharp.Collections.ParallelSeq

/// Day 13: Knights of the Dinner Table
/// https://adventofcode.com/2015/day/13
/// In years past, the holiday feast with your family hasn't gone so well.
module Day13 =

    type Mode = IncludeMe | ExcludeMe

    type Guest = string
    type HappinessMap = IReadOnlyDictionary<Guest * Guest, int>

    // E.g. "Alice would lose 57 happiness units by sitting next to Bob."
    let parse (s: string) =
        let tokens = s |> split ' '

        let pair = (tokens.[0], tokens.[10].Trim('.'))
        let units = int tokens.[3]
        let amount = if tokens.[2] = "gain" then units else -units

        (pair, amount)

    let getTotalHappiness mode (happinessMap: HappinessMap) =

        let extra = match mode with ExcludeMe -> [] | IncludeMe -> [ "Me" ]

        let guests =
            happinessMap.Keys
            |> Seq.map fst
            |> Seq.distinct
            |> Seq.toList
            |> List.append extra

        let getHappiness pair = // If pair not found then one of them is me
            match happinessMap.TryGetValue pair with (true, v) -> v | _ -> 0

        let evaluate guests =
            guests
            |> List.take 1 // Add first guest again to complete the circle
            |> List.append guests
            |> List.pairwise
            |> List.sumBy (fun (p1, p2) -> getHappiness(p1, p2) + getHappiness(p2, p1))

        // Fix first guest (as the guests form a circle) to reduce permutations
        let getPermutations (guests: Guest list) =
            let firstGuest = guests |> List.take 1

            guests
            |> List.tail
            |> permutations
            |> Seq.map (List.append firstGuest)

        guests
        |> getPermutations
        |> PSeq.map evaluate
        |> PSeq.max

    let parseInput() = getFile (2015, 13) |> readLinesAs parse |> readOnlyDict

    // What is the total change in happiness for the optimal seating arrangement
    // of the actual guest list?
    let Part1() = parseInput() |> getTotalHappiness ExcludeMe

    // What is the total change in happiness for the optimal seating arrangement
    // that actually includes yourself?
    let Part2() = parseInput() |> getTotalHappiness IncludeMe