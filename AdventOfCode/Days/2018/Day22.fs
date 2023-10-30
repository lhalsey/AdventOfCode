namespace AdventOfCode.Days.Y2018

open AdventOfCode.Shared.Utility
open AdventOfCode.Shared

/// Day 22: Mode Maze
/// https://adventofcode.com/2018/day/22
/// This is it, your final stop: the year -483.
module Day22 =

    let [<Literal>] Depth = 11_739
    let [<Literal>] TargetX = 11
    let [<Literal>] TargetY = 718
    let [<Literal>] TargetOffset = 50 // Assume we don't go more than this past the target

    let Target = { X = TargetX; Y = TargetY }

    type RegionType = Rocky | Wet | Narrow

    type Tool = Torch | ClimbingGear | Neither

    type State = { Location: Point2d; Tool: Tool }

    let getType =
        let rec getErosionLevel (x: int, y:int) =

            let getGeologicIndex (x: int, y:int) =
                match x, y with
                | 0, 0 -> 0
                | TargetX, TargetY -> 0
                | x, 0 -> x * 16_807
                | 0, y -> y * 48_271
                | x, y -> getErosionLevelMemo(x - 1, y) * getErosionLevelMemo(x, y - 1)

            (getGeologicIndex(x, y) + Depth) % 20_183

        and getErosionLevelMemo = memoise getErosionLevel

        fun (x, y) ->
            match getErosionLevelMemo(x, y) % 3 with
            | 0 -> Rocky
            | 1 -> Wet
            | 2 -> Narrow
            | x -> failwithf "Invalid input: %A" x

    let getRisk = function Rocky -> 0 | Wet -> 1 | Narrow -> 2

    let getRequiredItem (r1: RegionType) (r2: RegionType) (tool: Tool) =
        match r1, r2, tool with
        | r1, r2, t when r1 = r2 -> t   // Moving between like regions just keep current tool
        | Rocky, Wet, _ -> ClimbingGear // Otherwise need to use tool that satisfies both regions
        | Wet, Rocky, _ -> ClimbingGear
        | Rocky, Narrow, _ -> Torch
        | Narrow, Rocky, _ -> Torch
        | Wet, Narrow, _ -> Neither
        | Narrow, Wet, _ -> Neither
        | x -> failwithf "Invalid state: %A" x

    let getShortestPath() =

        let inRange (loc: Point2d) =
            loc.X >= 0 && loc.Y >= 0 && loc.X <= TargetX + TargetOffset && loc.Y <= TargetY + TargetOffset

        let getNewState (state: State) (newLoc: Point2d) =
            let currentType = getType (state.Location.X, state.Location.Y)
            let nextType = getType (newLoc.X, newLoc.Y)
            let requiredItem = getRequiredItem currentType nextType state.Tool

            // If we need to change tool then it costs an extra 7 minutes
            let cost = if state.Tool = requiredItem then 1 else 8

            { Location = newLoc; Tool = requiredItem }, cost

        let getChildren (state: State) =
            state.Location.GetAdjacent()
            |> Seq.filter inRange
            |> Seq.map (getNewState state)

        let start = { Location = Point2d.Origin; Tool = Torch }
        let goal = { Location = Target; Tool = Torch }

        // Minimum estimate is that we can move one cell per minute
        let distanceToTarget (state: State) = state.Location.ManhattanDistanceTo Target
        // let dijkstra (state: State) = 0

        aStar start ((=) goal) getChildren distanceToTarget
        |> Seq.head


    // What is the total risk level for the smallest rectangle that includes 0,0 and the target's coordinates?
    let Part1() =
        ([0..TargetX], [0..TargetY])
        ||> List.allPairs 
        |> List.sumBy (getType >> getRisk)

    // What is the fewest number of minutes you can take to reach the target?
    let Part2() = getShortestPath()