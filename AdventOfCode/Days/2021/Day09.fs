namespace AdventOfCode.Days.Y2021

open AdventOfCode.Shared
open AdventOfCode.Shared.Utility
open FSharp.Collections.ParallelSeq
open System.Collections.Generic

/// Day 9: Smoke Basin
/// https://adventofcode.com/2021/day/9
/// These caves seem to be lava tubes.
module Day09 =

    let [<Literal>] MaxHeight = 9

    let parseInput() = getFile (2021, 9) |> parseGrid (System.Char.GetNumericValue >> int) |> readOnlyDict

    let getLowPoints (heightMap: IReadOnlyDictionary<Point2d, int>) =
        let isLowest (point: KeyValuePair<Point2d, int>) = 
            point.Key.GetAdjacent()
            |> Seq.choose (fun x -> tryFind x heightMap)
            |> Seq.forall (fun x -> point.Value < x)

        heightMap |> PSeq.filter isLowest

    let getBasinSize (heightMap: IReadOnlyDictionary<Point2d, int>) (loc: Point2d) =

        let isInBasin (point: Point2d) =
            match tryFind point heightMap with
            | None -> false
            | Some h -> h < MaxHeight

        let getChildren (point: Point2d) =
            point.GetAdjacent()
            |> Seq.filter isInBasin
            |> Seq.toList
            
        [ loc ]
        |> breadthFirstSearch getChildren
        |> Seq.length

    // Find all of the low points on your heightmap.
    // What is the sum of the risk levels of all low points on your heightmap?
    let Part1() =
        parseInput()
        |> getLowPoints
        |> Seq.sumBy (fun x -> x.Value + 1)

    // What do you get if you multiply together the sizes of the three largest basins?
    let Part2() =
        let heightMap = parseInput()
        
        getLowPoints heightMap
        |> PSeq.map (fun x -> getBasinSize heightMap x.Key)
        |> Seq.sortDescending
        |> Seq.take 3
        |> Seq.reduce (*)