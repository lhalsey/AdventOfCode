namespace AdventOfCode.Days.Y2021

open AdventOfCode.Shared
open AdventOfCode.Shared.Utility
open System.Collections.Generic

/// Day 25: Sea Cucumber
/// https://adventofcode.com/2021/day/25
/// This is it: the bottom of the ocean trench, the last place the sleigh keys could be.
module Day25 =

    type CellType = Down | Across | Empty

    type State = { Across: IReadOnlySet<Point2d>; Down: IReadOnlySet<Point2d>; Width: int; Height: int }
    
    let parse = function 'v' -> Down | '>' -> Across | _ -> Empty

    let parseInput() =
        let file = getFile (2021, 25)
        let input = file |> readAllLines
        let grid = file |> parseGrid parse

        let across = grid |> Seq.filter (fun (_, x) -> x = Across) |> Seq.map fst |> toReadOnlyHashSet
        let down = grid |> Seq.filter (fun (_, x) -> x = Down) |> Seq.map fst |> toReadOnlyHashSet

        { Across = across; Down = down; Width = input.[0].Length; Height = input.Length }

    let step (state: State) =
        let moveAcross (p: Point2d) =
            let p2 = { p with X = (p.X + 1) % state.Width }
            if state.Down.Contains p2 || state.Across.Contains p2 then p else p2

        let moveDown (across: IReadOnlySet<Point2d>) (p: Point2d) =
            let p2 = { p with Y = (p.Y + 1) % state.Height }
            if state.Down.Contains p2 || across.Contains p2 then p else p2

        let across = state.Across |> Seq.map moveAcross |> toReadOnlyHashSet
        let down = state.Down |> Seq.map (moveDown across) |> toReadOnlyHashSet

        { state with Across = across; Down = down }

    let areEquivalent (s1: State, s2: State) = s1.Across.SetEquals s2.Across && s1.Down.SetEquals s2.Down
    
    // Find somewhere safe to land your submarine. What is the first step on which no sea cucumbers move?
    let Part1() =
        let state = parseInput()

        state
        |> Seq.unfold (fun x -> Some(x, step x))
        |> Seq.pairwise
        |> Seq.findIndex areEquivalent
        |> (+) 1


    let Part2() = 1