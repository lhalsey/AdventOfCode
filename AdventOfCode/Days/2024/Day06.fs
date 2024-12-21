namespace AdventOfCode.Days.Y2024

open AdventOfCode.Shared.Utility
open AdventOfCode.Shared
open FSharpx.Collections
open System.Collections.Generic
open System.Collections.ObjectModel

/// Day 6: Title TBC
/// https://adventofcode.com/2024/day/6
/// Description TBC
module Day06 =

    type CellType = Obstacle | Guard | Empty

    let parse = function '#' -> Obstacle | '^' -> Guard | '.' -> Empty | c -> failwithf "Invalid cell %c" c

    let parseInput() = getFile (2024, 6) |> parseGrid parse |> dict

    let getCellCount (start: Point2d) (grid: IReadOnlyDictionary<Point2d, CellType>)  = 
        let rec getCellCountR (pos: Point2d) (dir: Direction2d) (visited: Set<(Point2d * Direction2d)>) =
            
            let nextCell = grid |> tryFind (pos + dir)                

            match nextCell with
            | None -> visited |> Seq.distinctBy fst |> Seq.length                         // Out of bounds
            | Some Empty -> getCellCountR (pos + dir) dir (visited |> Set.add (pos + dir, dir)) // Continue into empty cell
            | Some Obstacle -> getCellCountR pos (Direction2d.TurnRight dir) visited       // Turn right at obstacle
            | x -> failwithf "Invalid input: %A" x

        getCellCountR start Direction2d.North (Set [start, (Direction2d.North)])

    let Part1() =
        let grid = parseInput()

        // Get guard position then mark cell as empty
        let guard = grid |> Seq.find (fun x -> x.Value = Guard) |> fun x -> x.Key
        //grid[guard] <- Empty
        grid.Remove guard |> ignore
        grid.Add (guard, Empty)

        let grid2 = new ReadOnlyDictionary<Point2d, CellType>(grid)

        getCellCount guard grid2

    let Part2() =
        0