namespace AdventOfCode.Days.Y2018

open AdventOfCode.Shared.Utility
open AdventOfCode.Shared
open System.Collections.Generic

/// Day 15: Beverage Bandits
/// https://adventofcode.com/2018/day/15
/// Having perfected their hot chocolate, the Elves have a new problem: the Goblins that live
/// in these caves will do anything to steal it.
module Day15 =

    type Unit = Elf of int | Goblin of int
    type Cell = Open | Wall | Unit of Unit

    let parse = function
        | '.' -> Open
        | '#' -> Wall   
        | 'E' -> Unit (Elf 200)
        | 'G' -> Unit (Goblin 200)
        | x -> failwithf "Invalid cell type: %c" x

    let parseInput() = getFile (2018, 15) |> parseGrid parse

    let isUnit = function Unit _ -> true | _ -> false

    let getTargetsInRange (unit: KeyValuePair<Point2d, Cell>) (map: Map<Point2d, Cell>) =
        let isTarget (cell: Cell) =
            match unit.Value, cell with
            | Unit (Goblin _), Unit (Elf _) -> true
            | Unit (Elf _), Unit (Goblin _) -> true
            | _ -> false

        unit.Key.GetAdjacent()
        |> Seq.filter (fun x -> isTarget map.[x])
        |> Seq.toList

    let takeTurn (unit: KeyValuePair<Point2d, Cell>) (map: Map<Point2d, Cell>) =
        let targets = getTargetsInRange unit map

        match targets with
        | [] -> 1 // Need to move
        | _ -> 2 // Already have target(s)
        

    let Part1() =
        let map = parseInput() |> Map

        let units =
            map
            |> Seq.filter (fun x -> isUnit x.Value)
            |> Seq.toList

        let unit = units.Head
        let t1 = takeTurn unit map

        1

    let Part2() =
        0