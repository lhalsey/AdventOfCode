namespace AdventOfCode.Days.Y2020

open AdventOfCode.Shared.Utility
open AdventOfCode.Shared
open System.Collections.Generic
open FSharp.Collections.ParallelSeq

/// Day 11: Seating System
/// https://adventofcode.com/2020/day/11
/// Your plane lands with plenty of time to spare.
module Day11 =

    type Cell = Floor | Empty | Occupied

    type SeatMap = IReadOnlyDictionary<Point2d, Cell>

    let parse = function '.' -> Floor | 'L' -> Empty | '#' -> Occupied | x -> failwithf "Invalid: %c" x

    let parseInput() = getFile (2020, 11) |> parseGrid parse |> readOnlyDict

    let getOccupiedAdjacent (seatMap: SeatMap) (point: Point2d) = 
        point.GetAllAdjacent()
        |> Seq.filter (fun x -> seatMap |> tryFind x = Some Occupied)

    let getOccupiedInSight (seatMap: SeatMap) (point: Point2d) =
        let isOccupied (cell: Point2d) (dir: Direction2d) = 
            cell.GetPointsInDirection dir
            |> Seq.pick (fun x ->
                match seatMap |> tryFind x with Some Occupied -> Some true | Some Floor -> None | _ -> Some false)

        Direction2d.AllDirections
        |> Seq.filter (fun dir -> isOccupied (point + dir) dir)
       
    let getOccupiedSeats getOccupied occupiedSeatThreshold =

        let getNext (seatMap: SeatMap) =
            let nextCell (point: Point2d) = function
                | Floor -> Floor
                | Empty -> if getOccupied seatMap point |> Seq.isEmpty then Occupied else Empty
                | Occupied -> if getOccupied seatMap point |> hasAtLeast occupiedSeatThreshold then Empty else Occupied

            seatMap
            |> PSeq.map (fun x -> x.Key, (nextCell x.Key x.Value))
            |> readOnlyDict

        // Keep updating seat map until it becomes stable
        parseInput()
        |> Seq.unfold (fun x -> Some(x, getNext x))
        |> Seq.map (fun x -> x.Values |> countIf ((=) Occupied))
        |> Seq.pairwise
        |> Seq.find (fun (x, y) -> x = y)
        |> fst
 

    // Simulate your seating area by applying the seating rules repeatedly until no seats change state.
    // How many seats end up occupied?
    let Part1() = getOccupiedSeats getOccupiedAdjacent 4
        
    // Given the new visibility method and the rule change for occupied seats becoming empty,
    // once equilibrium is reached, how many seats end up occupied?
    let Part2() = getOccupiedSeats getOccupiedInSight 5