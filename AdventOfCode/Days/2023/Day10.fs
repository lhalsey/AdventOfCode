namespace AdventOfCode.Days.Y2023

open AdventOfCode.Shared.Utility
open AdventOfCode.Shared
open System.Collections.Generic

/// Day 10: Pipe Maze
/// https://adventofcode.com/2023/day/10
/// You use the hang glider to ride the hot air from Desert Island all the way up to the floating metal island.
module Day10 =

    type Problem = { CellMap: IReadOnlyDictionary<Point2d, char>; Start: Point2d }

    let parseInput() = getFile (2023, 10) |> parseGrid id

    let getProblem() =
        let cells = parseInput()
        let start = cells |> Seq.find (fun (_, y) -> y = 'S') |> fst

        { CellMap = readOnlyDict cells; Start = start }

    let getSteps (problem: Problem) (dir: Direction2d) =       

        let move (pos: Point2d, dir: Direction2d) =
            let cell = problem.CellMap[pos]

            let newDir =
                match dir, cell with
                | dir, 'S' -> dir // Start is '-' in our puzzle
                | dir, '|' -> dir
                | dir, '-' -> dir
                | dir, 'L' when dir = Direction2d.West -> Direction2d.North
                | dir, 'L' when dir = Direction2d.South -> Direction2d.East
                | dir, 'J' when dir = Direction2d.East -> Direction2d.North
                | dir, 'J' when dir = Direction2d.South -> Direction2d.West
                | dir, 'F' when dir = Direction2d.West -> Direction2d.South
                | dir, 'F' when dir = Direction2d.North -> Direction2d.East
                | dir, '7' when dir = Direction2d.East -> Direction2d.South
                | dir, '7' when dir = Direction2d.North -> Direction2d.West
                | x -> failwithf "Invalid input: %A" x

            pos + newDir, newDir

        (problem.Start, dir) |> Seq.unfold (fun x -> Some (x, move x))
            

    let Part1() =
        let problem = getProblem()

        // Travel in opposite directions and count steps until they meet - this is the furthest point
        let eastRoute = getSteps problem Direction2d.East |> Seq.map fst
        let westRoute = getSteps problem Direction2d.West |> Seq.map fst

        Seq.zip eastRoute westRoute
        |> Seq.findIndex (fun (x, y) -> x = y && x <> problem.Start)

    let Part2() = // 78 x
        let problem = getProblem()

        let getInnerCell (pos: Point2d, dir) =
            let cell = pos + (Direction2d.TurnRight dir)

            let getChildren (p: Point2d) =
                p.GetAllAdjacent()
                |> Seq.filter (fun x -> tryFind x problem.CellMap = Some '.')
                |> Seq.toList

            let inners =
                if tryFind cell problem.CellMap = Some '.'
                then breadthFirstSearch getChildren [cell]
                else Seq.empty

            inners

        let inner =
            getSteps problem Direction2d.East
            |> Seq.skip 1
            |> takeUntil (fun (pos, _) -> pos = problem.Start)
            |> Seq.collect getInnerCell
            |> Seq.distinct

        Seq.length inner
