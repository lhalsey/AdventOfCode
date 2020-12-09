namespace AdventOfCode.Days.Y2018

open AdventOfCode.Shared.Utility
open AdventOfCode.Shared

/// Day 17: Reservoir Research
/// https://adventofcode.com/2018/day/17
/// You arrive in the year 18.
module Day17 =

    let [<Literal>] Clay = '#'
    let [<Literal>] Spring = '+'
    let [<Literal>] FallingWater = '|'
    let [<Literal>] SettledWater = '~'

    // x=732, y=919..935
    let parse = function
        | Regex "x=(\d+), y=(\d+)..(\d+)" [Int x; Int y1; Int y2] ->
            [y1 .. y2] |> List.map (fun y -> { X = x; Y = y }, Clay)
        | Regex "y=(\d+), x=(\d+)..(\d+)" [Int y; Int x1; Int x2] ->
            [x1 .. x2] |> List.map (fun x -> { X = x; Y = y }, Clay)
        | x -> failwithf "Invalid input: %s" x

    let parseInput() =
        getFile (2018, 17)
        |> readLinesAs parse
        |> List.concat
        |> Map
        |> Map.add { X = 500; Y = 0 } Spring


    let getWater (grid: Map<Point2d, char>) =

        let maxY = grid |> Seq.map (fun x -> x.Key.Y) |> Seq.max

        let start = { X = 500; Y = 1 }

        let rec traverse (loc: Point2d) (grid: Map<Point2d, char>) (visited: Point2d Set) (last: Point2d) =
            //if loc.Y > maxY then grid // Gone as far down as we can
            //else
            let below = loc + Direction2d.South
            let left = loc + Direction2d.West
            let right = loc + Direction2d.East

            let canMoveTo (pt: Point2d) =
                match grid.TryFind pt with
                | Some Clay
                | Some SettledWater -> false
                | _ -> true

            let grid = grid.Add (loc, FallingWater)
            let visited = visited.Add loc

            if loc.Y >= maxY then
                if loc = last then grid else traverse start grid Set.empty loc
            else if canMoveTo below then traverse below grid visited last
            else if canMoveTo left && visited.Contains left |> not then traverse left grid visited last
            else if canMoveTo right && visited.Contains right |> not then traverse right grid visited last
            //else if loc = last then grid
            else traverse start (grid.Add (loc, SettledWater)) Set.empty last


        let result = traverse start grid Set.empty start

        let image = ImageProvider.getImage id result

        result
        |> Map.toSeq
        |> countIf (fun (k, v) -> v = FallingWater || v = SettledWater)


    let Part1() = parseInput() |> getWater

    let Part2() =
        0