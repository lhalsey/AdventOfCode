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

    let parseInput() = getFile (2020, 11) |> parseGrid parse |> Seq.toList

    let getNeighboursAdjacent (seats: IReadOnlySet<Point2d>) (point: Point2d) = 
        let neighbours =
            point.GetAllAdjacent()
            |> Seq.filter seats.Contains
            |> Seq.toList

        point, neighbours

    // TODO: Fix magic numbers
    let getNeighboursInSight (seats: IReadOnlySet<Point2d>) (point: Point2d) =
        let firstSeat (dir: Direction2d) = 
            point.GetPointsInDirection dir
            |> Seq.skip 1
            |> Seq.takeWhile (fun x -> x.X >= 0 && x.X < 100 && x.Y >= 0 && x.Y < 100)
            |> Seq.tryFind seats.Contains

        let neighbours =
            Direction2d.AllDirections
            |> Seq.choose firstSeat
            |> Seq.toList

        point, neighbours
       
    let getOccupiedSeats getNeighbours occupiedSeatThreshold =
        let input = parseInput()

        let seats =
            input
            |> List.filter (fun (_, c) -> c = Occupied || c = Empty)
            |> List.map fst
            |> toReadOnlyHashSet

        let neighbourMap =
            seats
            |> PSeq.map (getNeighbours seats)
            |> readOnlyDict

        let getNext (occupied: IReadOnlySet<Point2d>) =
            let isOccupied (point: Point2d) =
                let occupiedNeighbours = neighbourMap.[point] |> Seq.filter occupied.Contains

                match occupied.Contains point with
                | false -> occupiedNeighbours |> hasAtMost 0
                | true -> occupiedNeighbours |> hasAtMost occupiedSeatThreshold

            seats
            |> PSeq.filter isOccupied
            |> toReadOnlyHashSet

        let occupied =
            input
            |> List.filter (fun (_, c) -> c = Occupied)
            |> Seq.map fst
            |> toReadOnlyHashSet

        // Keep updating set of occupied seats until it becomes stable
        occupied
        |> Seq.unfold (fun x -> Some(x, getNext x))
        |> Seq.map (fun x -> x.Count)
        |> Seq.pairwise
        |> Seq.find (fun (x, y) -> x = y)
        |> fst
 

    // Simulate your seating area by applying the seating rules repeatedly until no seats change state.
    // How many seats end up occupied?
    let Part1() = getOccupiedSeats getNeighboursAdjacent 3
        
    // Given the new visibility method and the rule change for occupied seats becoming empty,
    // once equilibrium is reached, how many seats end up occupied?
    let Part2() = getOccupiedSeats getNeighboursInSight 4