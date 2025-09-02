namespace AdventOfCode.Days.Y2024

open AdventOfCode.Shared.Utility
open System.Collections.Generic
open AdventOfCode.Shared

/// Day 8: Resonant Collinearity
/// https://adventofcode.com/2024/day/8
/// You find yourselves on the roof of a top-secret Easter Bunny installation.
module Day08 =

    let parseInput() = getFile (2024, 8) |> parseGrid id

    let getAntinodes1 (points: IReadOnlySet<Point2d>) (p1: Point2d, p2: Point2d) =
        let delta = p1 - p2

        seq [ p1 + delta; p2 - delta ] |> Seq.filter points.Contains

    let getAntinodes2 (points: IReadOnlySet<Point2d>) (p1: Point2d, p2: Point2d) =
        let delta = p1 - p2
        
        let pos = Seq.initInfinite (fun x -> p1 + delta * x) |> Seq.takeWhile points.Contains
        let neg = Seq.initInfinite (fun x -> p1 - delta * x) |> Seq.takeWhile points.Contains

        Seq.append pos neg
        

    let getUniqueAntinodePoints getAntinodes =
        let input = parseInput()
        let points = input |> Seq.map fst |> toReadOnlyHashSet

        let antennaGroups =
            input
            |> Seq.groupBy snd
            |> Seq.filter (fun (x, _) -> x <> '.')
            |> Seq.map (snd >> Seq.map fst >> Seq.toList)

        let getAntinodesByGroup (antennas: Point2d list) =
            uniquePairs antennas
            |> Seq.collect (getAntinodes points)

        antennaGroups
        |> Seq.collect getAntinodesByGroup
        |> countDistinct
        

    // Calculate the impact of the signal. How many unique locations within the bounds of the map contain an antinode?
    let Part1() = getUniqueAntinodePoints getAntinodes1

    // Calculate the impact of the signal using this updated model.
    // How many unique locations within the bounds of the map contain an antinode?
    let Part2() = getUniqueAntinodePoints getAntinodes2