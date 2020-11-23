namespace AdventOfCode.Days.Y2019

open System
open AdventOfCode.Shared
open AdventOfCode.Shared.Utility

/// Day 20: Donut Maze
/// https://adventofcode.com/2019/day/20
/// You notice a strange pattern on the surface of Pluto and land nearby to get a closer look.
module Day20 =

    type Levels = SingleLevel | MultiLevel

    type Portal = { Inner: Point2d; Outer: Point2d }

    [<CustomEquality; NoComparison>] // Just want to use location & level for equality
    type State = { Location: Point2d; Level: int; Distance: int } with
       override __.GetHashCode() = hash (__.Location, __.Level)
       override __.Equals(other) = 
           match other with
           | :? State as s -> s.Location = __.Location && s.Level = __.Level
           | _ -> false

    let parseInput() = getFile (2019, 20) |> readLines

    let [<Literal>] Entrance = '@'
    let [<Literal>] Exit = '$'
    let [<Literal>] Passage = '.'

    // Condense two character portal identifier into single character for simplicity
    let getPortal = function
        | ('A', 'A') -> Entrance
        | ('Z', 'Z') -> Exit
        | (a, b) when Char.IsLetter a && Char.IsLetter b -> (min a b, max a b) |> hash |> char
        | (_, b) -> b

    let getCell charGroup =
        match Array.toList charGroup with
        | a::b::'.'::[] when Char.IsLetter a && Char.IsLetter b -> getPortal (a, b)
        | '.'::b::a::[] when Char.IsLetter a && Char.IsLetter b -> getPortal (a, b)
        | a::b::' '::[] when Char.IsLetter a && Char.IsLetter b -> ' '
        | ' '::b::a::[] when Char.IsLetter a && Char.IsLetter b -> ' '
        | _::'#'::_::[] -> ' '
        | _::b::_::[] -> b
        | x -> failwithf "Invalid input: %A" x

    let getMap() =
        let input = parseInput()

        // Pretty involved logic just to map portals to single chars!
        // TODO: Tidy this up
        let parseRow (row: char seq) =
            row
            |> Seq.windowed 3 
            |> Seq.map getCell

        // Apply portal transforms horizontally and then vertically
        let cells = 
            input
            |> Seq.map parseRow
            |> Seq.transpose
            |> Seq.map parseRow
            |> Seq.transpose

        let portalValues = ['A'..'Z'] @ ['a'..'z']

        // Map hashed portal codes to more readable values (mainly for visualisation)
        let portalMap =
            cells
            |> Seq.concat
            |> Seq.countBy id
            |> Seq.filter (fun (_, c) -> c = 2)
            |> Seq.map fst
            |> Seq.zip <| portalValues
            |> readOnlyDict

        let update cell = match portalMap.TryGetValue cell with (true, p) -> p | _ -> cell

        let getCells row = Seq.mapi (fun col c -> ({ X = col; Y = row }, update c ))

        cells
        |> Seq.mapi getCells
        |> Seq.concat
        |> Seq.filter (fun (_, c) -> c <> ' ')
        |> readOnlyDict

    let solve levelType =
        let map = getMap()
        //let image = ImageProvider.getImage id map

        let width = map |> Seq.map (fun x -> x.Key.X) |> Seq.max
        let height = map |> Seq.map (fun x -> x.Key.Y) |> Seq.max
        let mid = { X = width / 2; Y = height / 2 }

        let getPortal (points: Point2d seq) =
            match Seq.toList points with
            | p1::p2::[] when p1.ManhattanDistanceTo mid < p2.ManhattanDistanceTo mid ->
                { Inner = p1; Outer = p2 }
            | p1::p2::[] -> { Inner = p2; Outer = p1 }
            | x -> failwithf "Should only have two of each portal: %A" x
  
        let portalMap =
            map
            |> Seq.groupBy (fun x -> x.Value)
            |> Seq.filter (fun (_, v) -> Seq.length v = 2)
            |> Seq.map (fun (k, v) -> (k, v |> Seq.map (fun x -> x.Key) |> getPortal))
            |> readOnlyDict

        let entrance = map |> Seq.find (fun x -> x.Value = Entrance)

        let state = { Location = entrance.Key; Level = 0; Distance = 0 }

        let getNewState (state: State) loc = // Move to empty cell or use portal
            match map.[loc] with
            | Passage
            | Entrance 
            | Exit -> { Location = loc; Level = state.Level; Distance = state.Distance + 1 }
            | portal ->
                    let (loc, level) =
                        match portalMap.[portal] with
                        | { Inner = i; Outer = o } when loc = i -> (o, state.Level - 1)
                        | { Inner = i; Outer = o } when loc = o -> (i, state.Level + 1)
                        | _ -> failwithf "Can't find portal %c in portal map" portal

                    let level =
                        match levelType with
                        | SingleLevel -> state.Level
                        | MultiLevel -> level

                    { Location = loc; Distance = state.Distance; Level = level } // No step cost for portal

        let getChildren state = 
            state.Location.GetAdjacent()
            |> Seq.filter map.ContainsKey
            |> Seq.map (getNewState state)
            |> Seq.filter (fun x -> x.Level <= 0)
            |> Seq.toList

        let exit =
            [ state ]
            |> breadthFirstSearch getChildren
            |> Seq.find (fun x -> x.Level = 0 && map.[x.Location] = Exit)

        exit.Distance - 2 // Don't include entrance or exit as steps


    // In your maze, how many steps does it take to get from the open tile marked AA to the
    // open tile marked ZZ?
    let Part1() = solve SingleLevel
        
    // In your maze, when accounting for recursion, how many steps does it take to get from
    // the open tile marked AA to the open tile marked ZZ, both at the outermost layer?
    let Part2() = solve MultiLevel