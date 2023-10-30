namespace AdventOfCode.Days.Y2022

open AdventOfCode.Shared.Utility
open AdventOfCode.Shared

/// Day 12: Hill Climbing Algorithm
/// https://adventofcode.com/2022/day/12
/// You try contacting the Elves using your handheld device, but the river you're following
/// must be too low to get a decent signal.
module Day12 =

    type Square = Start | End | Elevation of int

    let parse = function 'S' -> Start | 'E' -> End | x -> Elevation ((int x - int 'a') + 1)

    let parseInput() = getFile (2022, 12) |> parseGrid parse |> Map

    // We traverse in reverse order as it is more efficient to go from one end point to one of many
    // starting points than the other way around
    let getMinimumSteps (startCriteria: Square -> bool) =
        let map = parseInput()

        let start = map |> Seq.find (fun x -> x.Value = End) |> fun x -> x.Key

        let getHeight = function Start -> 1 | End -> 26 | Elevation x -> x

        let getMoves (point: Point2d) =
            let height = getHeight map[point]

            point.GetAdjacent()
            |> Seq.filter (fun x -> map.ContainsKey x && getHeight map[x] >= (height - 1))
            |> Seq.map (fun x -> x, 1) // Cost is 1 step

        // Minimum estimate is that we decrease height by one on each remaining step
        let heightDiff (state: Point2d) = getHeight map[state] - 1

        let isGoal (point: Point2d) = startCriteria map[point]

        aStar start isGoal getMoves heightDiff |> Seq.head


    // What is the fewest steps required to move from your current position to the location
    // that should get the best signal?
    let Part1() = getMinimumSteps (fun x -> x = Start)
     
    // What is the fewest steps required to move starting from any square with elevation a to the location
    // that should get the best signal?
    let Part2() = getMinimumSteps (fun x -> x = Start || x = Elevation 1)