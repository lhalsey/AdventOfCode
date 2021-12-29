namespace AdventOfCode.Days.Y2021

open AdventOfCode.Shared.Utility
open AdventOfCode.Shared

/// Day 15: Chiton
/// https://adventofcode.com/2021/day/15
/// You've almost reached the exit of the cave, but the walls are getting closer together
module Day15 =

    let parseInput() = getFile (2021, 15) |> parseGrid (System.Char.GetNumericValue >> int) |> Map

    let findLowestRiskPath (grid: Map<Point2d, int>) =
        let maxX = grid |> Seq.map (fun x -> x.Key.X) |> Seq.max

        let Target = { X = maxX; Y = maxX } // Cavern is square

        // Minimum estimate is that we add one value of risk per step
        let distanceToTarget (state: Point2d) = state.ManhattanDistanceTo Target

        let getChildren (state: Point2d) =
            state.GetAdjacent()
            |> Seq.choose (fun p -> grid.TryFind p |> Option.map (fun risk -> p, risk))

        aStar Point2d.Origin Target getChildren distanceToTarget
        |> Seq.head

    let expandGrid (multiplier: int) (grid: Map<Point2d, int>) =
        let maxX = grid |> Seq.map (fun x -> x.Key.X) |> Seq.max
        let size = maxX + 1

        let newMaxX = size * multiplier - 1

        let tiles = List.allPairs [0..newMaxX] [0..newMaxX]

        let getRisk (x, y) =
            let origValue = grid.[ { X = x % size; Y = y % size }]
            let newValue = origValue + (x / size) + (y / size)
            if newValue > 9 then (newValue % 10) + 1 else newValue

        tiles
        |> List.map (fun (x, y) -> { X = x; Y = y}, getRisk (x, y))
        |> Map

    // What is the lowest total risk of any path from the top left to the bottom right?
    let Part1() = parseInput() |> findLowestRiskPath

    // Using the full map, what is the lowest total risk of any path from the top left to the bottom right?
    let Part2() = parseInput() |> expandGrid 5 |> findLowestRiskPath