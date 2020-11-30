namespace AdventOfCode.Days.Y2015

open AdventOfCode.Shared.Utility
open FSharp.Collections.ParallelSeq

/// Day 9: All in a Single Night
/// https://adventofcode.com/2015/day/9
/// Every year, Santa manages to deliver all of his presents in a single night.
module Day09 =

    // E.g. "Faerun to Norrath = 129"
    let parse (s: string) = 
        let tokens = s |> split ' '
        let (source, dest, dist) = tokens.[0], tokens.[2], int tokens.[4]
        seq { yield (source, dest), dist; yield (dest, source), dist }

    let parseInput() = getFile (2015, 9) |> readLines |> Seq.collect parse |> readOnlyDict

    // Calculate distance for all possible routes
    let getDistances() = 
        let distanceMap = parseInput()

        let getTotalDistance = Seq.pairwise >> Seq.sumBy (fun x -> distanceMap.[x])

        let locations = distanceMap.Keys |> Seq.map fst |> Seq.distinct |> Seq.toList

        locations
        |> permutations
        |> PSeq.map getTotalDistance

    // What is the distance of the shortest route?
    let Part1() = getDistances() |> Seq.min

    // What is the distance of the longest route?
    let Part2() = getDistances() |> Seq.max