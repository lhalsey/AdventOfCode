namespace AdventOfCode.Days.Y2019

open System

open AdventOfCode.Shared
open AdventOfCode.Shared.Utility
open System.Collections.Generic
open FSharp.Collections.ParallelSeq

/// Day 18: Many-Worlds Interpretation
/// https://adventofcode.com/2019/day/18
/// As you approach Neptune, a planetary security system detects you and activates
/// a giant tractor beam on Triton!
module Day18 =

    type KeyType = int // Use bit mask for representing keys for efficiency

    type Cell = Wall | Empty | Entrance of int | Key of KeyType | Door of KeyType

    type Path = { Start: KeyType; Key: KeyType; Distance: int; Doors: KeyType }

    type CellMap = IReadOnlyDictionary<Point2d, Cell>

    type Part = Part1 | Part2

    [<CustomEquality; NoComparison>] // Just want to use location for equality in BFS
    type State = { Location: Point2d; Distance: int; Doors: KeyType } with
        override __.GetHashCode() = hash __.Location
        override __.Equals(other) = 
            match other with
            | :? State as s -> s.Location = __.Location
            | _ -> false

    type KeyState = { CurrentKeys: KeyType; NeededKeys: KeyType }

    let getKeyValue (c: char) = pown 2 (int c - int 'a')

    let getCell = function
        | '#' -> Wall
        | '.' -> Empty
        | '@' -> Entrance 0
        | c when Char.IsLower c -> Key (getKeyValue c)
        | c when Char.IsUpper c -> Door (Char.ToLower c |> getKeyValue)
        | x -> failwithf "Unexpected cell type: %c" x

    let parseInput() = getFile (2019, 18) |> readLines

    let updateMap (cells: (Point2d * Cell) seq) =

        let (entrance, _) =
            cells
            |> Seq.find (fun (_, c) -> match c with Entrance _ -> true | _ -> false)

        let newWalls =
            entrance.GetAdjacent()
            |> Seq.append [entrance]
            |> Seq.map (fun p -> (p, Wall))

        let newEntrances =
            entrance.GetDiagonalAdjacent()
            |> Seq.mapi (fun i p -> (p, Entrance i))

        cells
        |> Seq.append newWalls
        |> Seq.append newEntrances
        |> Seq.distinctBy fst

    // Convert input to map of locations and (visitable) cell types
    let getCellMap part =

        // For part 2 we need to convert the entrance to four entrances and add walls
        let update =  match part with Part1 -> id | Part2 -> updateMap

        parseInput()
        |> Seq.mapi (fun row rowStr -> rowStr |> Seq.mapi (fun col c -> { X = col; Y = row }, getCell c))
        |> Seq.concat
        |> update
        |> Seq.filter (fun (_, c) -> c <> Wall)
        |> readOnlyDict

    // Explore adjacent cells (that are on the map) keeping track of any doors we pass through
    let getAdjacent (cells: CellMap) (state: State)  =

        let doors =
            match cells.[state.Location] with
            | Door d -> d + state.Doors
            | _ -> state.Doors

        state.Location.GetAdjacent()
        |> Seq.filter (fun x -> cells.ContainsKey x)
        |> Seq.toList
        |> List.map (fun x -> { Location = x; Distance = state.Distance + 1; Doors = doors })

    // Use BFS to find shortest paths from start cell to keys
    let getPaths (cells: CellMap) (startPoint: Point2d, startValue: int)  =
        let root = { Location = startPoint; Distance = 0; Doors = 0 }

        let getKey (state: State) =
            match cells.[state.Location] with
            | Key k -> Some { Start = startValue; Key = k; Distance = state.Distance; Doors = state.Doors }
            | _ -> None

        [ root ]
        |> breadthFirstSearch (getAdjacent cells)
        |> Seq.choose getKey
        |> Seq.toList

    // For all entrances and keys, find shortest paths to each key
    let getPathMap (cells: CellMap) =
        let getKeyOrEntranceValue (cell: KeyValuePair<Point2d, Cell>) =
            match cell.Value with
            | Entrance i -> Some (cell.Key, (getKeyValue ('z' + char (1 + i))))
            | Key k -> Some (cell.Key, k)
            | _ -> None

        cells
        |> Seq.choose getKeyOrEntranceValue
        |> PSeq.map (fun (x, y) -> (y, getPaths cells (x, y)))
        |> readOnlyDict

    let getKeys (cells: CellMap) =
        cells.Values
        |> Seq.choose (function Key k -> Some k | _ -> None)
        |> Seq.toList

    let getEntrances (cells: CellMap) =
        cells.Values
        |> Seq.choose (function Entrance i -> Some i | _ -> None)
        |> Seq.map (fun i -> getKeyValue ('z' + char (1 + i)))
        |> Seq.toList

    let solve (cells: CellMap) =

        // TODO: Tidy this up - not easy to follow after performance optimisations
        let pathMap = getPathMap cells
        let keys = getKeys cells
        let entrances = getEntrances cells
        let keysAndEntrances = keys @ entrances
            
        // Start from entrance needing all keys and try recursively collecting all the keys
        // that we can reach (not blocked by door). We use memoisation to avoid exploring
        // sub-problems we have already found an optimal solution for
        let rec solveR (keyState: KeyState) =

            let isValid (path: Path) =
                keyState.NeededKeys &&& path.Key > 0 &&
                keyState.NeededKeys &&& path.Doors = 0 

            let getDistance (path: Path) =

                let currentKeys = keyState.CurrentKeys - path.Start + path.Key

                let neededKeys = keyState.NeededKeys - path.Key

                path.Distance + solveMemoise { CurrentKeys = currentKeys; NeededKeys = neededKeys }

            if keyState.NeededKeys = 0 then 0 else // All keys collected, zero distance remaining
            keysAndEntrances
            |> List.filter (fun k -> keyState.CurrentKeys &&& k > 0)
            |> List.collect (fun k -> pathMap.[k])
            |> List.filter isValid  // Which keys can we reach
            |> List.map getDistance // And what is the minimum distance we need to travel
            |> List.min
        and solveMemoise = memoise solveR

        solveMemoise { CurrentKeys = List.sum entrances; NeededKeys = List.sum keys }


    // How many steps is the shortest path that collects all of the keys?
    let Part1() = solve (getCellMap Part1)

    // After updating your map and using the remote-controlled robots, what is the
    // fewest steps necessary to collect all of the keys?
    let Part2() = solve (getCellMap Part2)