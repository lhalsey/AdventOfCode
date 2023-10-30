namespace AdventOfCode.Days.Y2022

open AdventOfCode.Shared.Utility
open AdventOfCode.Shared

/// Day 22: Monkey Map
/// https://adventofcode.com/2022/day/22
/// The monkeys take you on a surprisingly easy trail through the jungle.
module Day22 =

    type Tile = Open | Wall | Empty

    let parse = function
        | '.' -> Open
        | '#' -> Wall
        | _ -> Empty

    let parseInput() =
        let fileName = getFile (2022, 22)
        let map = fileName |>  parseGrid parse |> Map
        let directions = fileName |> readLines |> Seq.last

        (map, directions)

    let rec parseDirections (s: string) =
        seq {
            match s with
            | "" -> ()
            | Regex "^R(\d+)(.*)" [Int distance; tail] -> yield (Direction2d.TurnRight, distance); yield! parseDirections tail
            | Regex "^L(\d+)(.*)" [Int distance; tail] -> yield (Direction2d.TurnLeft, distance); yield! parseDirections tail
            | _ -> failwithf "Invalid input: %s" s
        }

    let getTilesOnY (map: Map<Point2d, Tile>) yVal =
         map
         |> Seq.filter (fun x -> x.Key.Y = yVal && (x.Value = Open || x.Value = Wall))
         |> Seq.map (fun x -> x.Key.X)

    let getTilesOnX (map: Map<Point2d, Tile>) xVal =
         map
         |> Seq.filter (fun x -> x.Key.X = xVal && (x.Value = Open|| x.Value = Wall))
         |> Seq.map (fun x -> x.Key.Y)

    let getLeftMost (map: Map<Point2d, Tile>) y = getTilesOnY map y |> Seq.min |> fun x -> { X = x; Y = y }

    let getRightMost (map: Map<Point2d, Tile>) y = getTilesOnY map y |> Seq.max |> fun x -> { X = x; Y = y }

    let getUpMost (map: Map<Point2d, Tile>) x = getTilesOnX map x |> Seq.min |> fun y -> { X = x; Y = y }

    let getDownMost (map: Map<Point2d, Tile>) x = getTilesOnX map x |> Seq.max |> fun y -> { X = x; Y = y }

    let move (map: Map<Point2d, Tile>)
             (pos: Point2d, dir: Direction2d)
             (turn: Direction2d -> Direction2d, distance: int) =
        
        let dir = turn dir

        let wrap pos =
            if dir = Direction2d.East then getLeftMost map pos.Y
            else if dir = Direction2d.West then getRightMost map pos.Y
            else if dir = Direction2d.North then getDownMost map pos.X
            else getUpMost map pos.X

        let rec moveR (pos: Point2d) (distLeft: int) =
            let newPos = pos + dir
            match distLeft, map.TryFind newPos with
            | 0, _ -> pos, dir
            | _, Some Wall -> pos, dir
            | _, Some Open -> moveR newPos (distLeft - 1)
            | _, _ -> 
                let newPos = wrap pos
                if map[newPos] = Wall then pos, dir else moveR newPos (distLeft - 1)

        moveR pos distance

    let getDirValue dir =
         if dir = Direction2d.East then 0
         else if dir = Direction2d.West then 2
         else if dir = Direction2d.North then 3
         else 1

    let Part1() =
        let (map, directions) = parseInput()

        let dirs = parseDirections $"R{directions}" |> Seq.toList

        let topLeftOpen = getLeftMost map 0

        let state = topLeftOpen, Direction2d.North

        let (finalPos, finalDir) =
            (state, dirs)
            ||> List.fold (fun acc x -> move map acc x)

        (finalPos.Y + 1) * 1_000 + (finalPos.X + 1) * 4 + (getDirValue finalDir)


    let Part2() =
        0