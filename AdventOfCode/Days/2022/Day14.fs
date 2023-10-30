namespace AdventOfCode.Days.Y2022

open AdventOfCode.Shared.Utility
open AdventOfCode.Shared

/// Day 14: Regolith Reservoir
/// https://adventofcode.com/2022/day/14
/// The distress signal leads you to a giant waterfall!
module Day14 =

    let SandOrigin = { X = 500; Y = 0}

    let parse (s: string) = // 503,4 -> 502,4 -> 502,9 -> 494,9
        s
        |> splitOn " -> "
        |> Array.map (splitIntoPairAs "," int)
        |> Array.map (fun (x, y) -> { Point2d.X = x; Y = y })

    let parseInput() = getFile (2022, 14) |> readLinesAs parse

    let getPath (points: Point2d[]) =
        points
        |> Array.pairwise
        |> Seq.collect (fun (p1, p2) -> p1.GetPointsBetween p2)

    let getMap hasFloor =
        let input = parseInput()
        let rocks = input |> Seq.collect getPath

        let left = rocks |> Seq.map (fun x -> x.X) |> Seq.min
        let right = rocks |> Seq.map (fun x -> x.X) |> Seq.max
        let bottom = rocks |> Seq.map (fun x -> x.Y) |> Seq.max

        let getFloor() =
            let p1 = { X = left - 10_000; Y = bottom + 2 }
            let p2 = { X = right + 10_000; Y = bottom + 2 }

            p1.GetPointsBetween p2

        let addAbyssMarkers map =
            map
            |> Map.add { X = left - 1; Y = bottom } 'X'
            |> Map.add { X = right + 1; Y = bottom } 'X'

        let rocks = if hasFloor then Seq.append rocks (getFloor()) else rocks
        
        let map = 
            rocks
            |> Seq.map (fun p -> p, '#')
            |> Map
            |> Map.add SandOrigin '+'
            
        if hasFloor then map else addAbyssMarkers map

    let dropSand (map: Map<Point2d, char>) =
        let rec moveSand (point: Point2d) =
            let options = [ point + Direction2d.South; point + Direction2d.SouthWest; point + Direction2d.SouthEast ]
            let cells = options |> List.map (fun p -> p, map.TryFind p)
            let free = cells |> List.tryFind (fun (_, c) -> c = Some 'X' || c.IsNone)

            match free with
            | Some (p, None) -> moveSand p       // Empty cell so move
            | Some (_, Some _) -> None           // Found the abyss!
            | None -> Some (map.Add(point, 'o')) // Come to rest

        if map.TryFind SandOrigin = Some 'o' then None else moveSand SandOrigin |> Option.map (fun x -> map, x)

    let simulate (hasFloor: bool) =
        let map = getMap hasFloor
        let steps = map |> Seq.unfold dropSand
        //let result = steps |> Seq.last
        //let image = ImageProvider.getImage id result

        steps |> Seq.length

    let Part1() = simulate false
       
    let Part2() = simulate true