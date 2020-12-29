namespace AdventOfCode.Days.Y2020

open System.Collections.Generic
open AdventOfCode.Shared.Utility

/// Day 24: Lobby Layout
/// https://adventofcode.com/2020/day/24
/// Your raft makes it to the tropical island; it turns out that the small crab was an excellent navigator.
module Day24 =

    // TOOD: Use trapezoidal coordinates, e.g. make NW = N and SE = South
    // Can then reuse logic from previous days
    // Or just double East and West amounts
    type HexDirection = SouthEast | SouthWest | NorthEast | NorthWest | East | West

    type HexPoint= { X: decimal; Y: decimal } with
        static member (+) (a, b) = { X = a.X + b.X; Y = a.Y + b.Y } 
        static member Origin = { X = 0m; Y = 0m }

    let Sin60 = 0.866m // ~ Sqrt 3 / 2

    // E.g. "sesenwnenenewseeswwswswwnenewsewsw"
    let parse (s: string) = 
        let rec parseR = function
            | [] -> []
            | 's'::'e'::t -> SouthEast::parseR t 
            | 's'::'w'::t -> SouthWest::parseR t 
            | 'n'::'e'::t -> NorthEast::parseR t
            | 'n'::'w'::t -> NorthWest::parseR t
            | 'e'::t -> East::parseR t
            | 'w'::t -> West::parseR t
            | x -> failwithf "Invalid input: %A" x

        parseR (Seq.toList s)

    let parseInput() = getFile (2020, 24) |> readLinesAs parse |> Seq.toList

    let getOffset = function
        | SouthEast -> { X = 0.5m; Y = Sin60 }
        | SouthWest -> { X = -0.5m; Y = Sin60 }
        | NorthEast -> { X = 0.5m; Y = -Sin60 }
        | NorthWest -> { X = -0.5m; Y = -Sin60 }
        | East -> { X = 1m; Y = 0m }
        | West -> { X = -1m; Y = 0m }

    let move (dirs: HexDirection list) =
        (HexPoint.Origin, dirs)
        ||> List.fold (fun acc x -> acc + getOffset x)

    let getBlackTiles() =
        let input = parseInput()
        let points = input |> List.map move

        points
        |> List.countBy id
        |> List.filter (fun (_, v) -> v % 2 = 1)
        |> List.map fst

    let getNeighboursF =
        let offsets = 
            [ SouthWest; SouthEast; NorthEast; NorthWest; East; West ]
            |> List.map getOffset

        fun (point: HexPoint) -> offsets |> List.map ((+) point)

    let evolve getNeighbours (blacks: IReadOnlySet<HexPoint>) =
        let getBlack (tile, count) =
            match blacks.Contains tile, count with
            | true, 1
            | true, 2
            | false, 2 -> Some tile
            | _ -> None

        blacks
        |> Seq.collect getNeighbours
        |> Seq.countBy id
        |> Seq.choose getBlack
        |> toReadOnlyHashSet


    // Go through the renovation crew's list and determine which tiles they need to flip.
    // After all of the instructions have been followed, how many tiles are left with the black side up?
    let Part1() = getBlackTiles() |> List.length
        
    // How many tiles will be black after 100 days?
    let Part2() =
        let blacks = getBlackTiles() |> toReadOnlyHashSet

        let getNeighbours = memoise getNeighboursF

        blacks
        |> Seq.unfold (fun x -> Some(x, evolve getNeighbours x))
        |> Seq.item 100
        |> fun x -> x.Count