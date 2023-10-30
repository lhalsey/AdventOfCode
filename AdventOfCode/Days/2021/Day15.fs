namespace AdventOfCode.Days.Y2021

open AdventOfCode.Shared.Utility
open AdventOfCode.Shared
open System.Collections.Generic

/// Day 15: Chiton
/// https://adventofcode.com/2021/day/15
/// You've almost reached the exit of the cave, but the walls are getting closer together
module Day15 =

    let parseInput() = getFile (2021, 15) |> parseGrid (System.Char.GetNumericValue >> int) |> readOnlyDict

    let findLowestRiskPath (sizeMultiplier: int) (grid: IReadOnlyDictionary<Point2d, int>) =
        let origMaxX = grid |> Seq.map (fun x -> x.Key.X) |> Seq.max
        let size = origMaxX + 1
        let maxX = size * sizeMultiplier - 1

        let Target = { X = maxX; Y = maxX } // Cavern is square
        
        // Minimum estimate is that we add one value of risk per step
        let distanceToTarget (state: Point2d) = state.ManhattanDistanceTo Target

        let getRisk (point: Point2d) =
            let value = grid.[{ X = point.X % size; Y = point.Y % size }]
            let risk = value + (point.X / size) + (point.Y / size)
            if risk > 9 then (point, (risk % 10) + 1) else (point, risk)

        let getChildren (state: Point2d) =
            state.GetAdjacent()
            |> Seq.filter (fun x -> x.X >= 0 && x.X <= maxX && x.Y >= 0 && x.Y <= maxX)
            |> Seq.map getRisk

        aStar Point2d.Origin ((=) Target) getChildren distanceToTarget
        |> Seq.head

    // What is the lowest total risk of any path from the top left to the bottom right?
    let Part1() = parseInput() |> findLowestRiskPath 1

    // Using the full map, what is the lowest total risk of any path from the top left to the bottom right?
    let Part2() = parseInput() |> findLowestRiskPath 5