namespace AdventOfCode.Days.Y2019

open AdventOfCode.Shared.Utility
open System.Collections.Generic
open FSharp.Collections.ParallelSeq

/// Day 24: Planet of Discord
/// https://adventofcode.com/2019/day/24
/// You land on Eris, your last stop before reaching Santa.
module Day24 =

    type Mode = SingleLevel | MultiLevel

    type CellType = int

    let [<Literal>] Empty = 0
    let [<Literal>] Bug = 1

    let [<Literal>] GridSize = 5
    let [<Literal>] NumCells = 25
    let [<Literal>] CentreIndex = 12

    let [<Literal>] OuterLevel = 0
    let [<Literal>] CurrentLevel = 1
    let [<Literal>] InnerLevel = 2

    let parseCell = function '#' -> Some Bug | '.' -> Some Empty | _ -> None

    let parseInput() =
        getFile (2019, 24)
        |> readAllText
        |> Seq.choose parseCell
        |> Seq.toArray

    type Level = { Cells: CellType[] } with
        member __.Bugs = __.Cells |> Array.sum
        member __.BioDiversity = // Tiles with bugs have value 2^n where n is positional index
            ((1, 0), __.Cells)
            ||> Seq.fold (fun (pow, sum) cell -> (pow * 2, sum + cell * pow))
            |> snd
        static member Empty = { Cells = Array.zeroCreate NumCells }

    type Generation = { Levels: Level[] } with
        member __.Bugs = __.Levels |> Seq.sumBy (fun x -> x.Bugs)
        member __.BioDiversity = __.Levels |> Seq.sumBy (fun x -> x.BioDiversity)

    // Need to use partial active pattern as corner points have two edges
    let (| LeftEdge |_|) = function (_, 0) -> Some LeftEdge | _ -> None
    let (| RightEdge |_|) = function (_, 4) -> Some RightEdge | _ -> None
    let (| TopEdge |_|) = function (0, _) -> Some TopEdge | _ -> None
    let (| BottomEdge |_|) = function (4, _) -> Some BottomEdge | _ -> None

    let (| Mid | RightOfMid | LeftOfMid | BelowMid | AboveMid | Other |) = function
        | (2, 2) -> Mid | (2, 3) -> RightOfMid | (2, 1) -> LeftOfMid
        | (3, 2) -> BelowMid | (1, 2) -> AboveMid | _ -> Other
    
    let getAdjacentCellMap mode =

        let adjacentF index = // Single level
            let (row, col) = (index / GridSize, index % GridSize)

            let outer = Seq.map (fun x -> (OuterLevel, x))

            let topEdge = [ 0 .. 4 ]
            let leftEdge = [ 0 .. 5 .. 20 ]
            let bottomEdge = [ 20 .. 24 ]
            let rightEdge = [ 4 .. 5 .. 24 ]

            match ((row, col), mode) with
            | Mid, MultiLevel -> [] // Ignore mid as infinite!
            | _ ->
                seq {
                    match ((row, col), mode) with // Left
                    | LeftEdge, SingleLevel  -> ()
                    | LeftEdge, MultiLevel   -> yield (InnerLevel, CentreIndex - 1) 
                    | RightOfMid, MultiLevel -> yield! outer rightEdge
                    | _                      -> yield (CurrentLevel, index - 1)

                    match ((row, col), mode) with // Right
                    | RightEdge, SingleLevel -> ()
                    | RightEdge, MultiLevel  -> yield (InnerLevel, CentreIndex + 1)
                    | LeftOfMid, MultiLevel  -> yield! outer leftEdge
                    | _                      -> yield (CurrentLevel, index + 1)

                    match ((row, col), mode) with // Up
                    | TopEdge, SingleLevel -> ()
                    | TopEdge, MultiLevel  -> yield (InnerLevel, CentreIndex - GridSize)
                    | BelowMid, MultiLevel -> yield! outer bottomEdge
                    | _                    -> yield (CurrentLevel, index - GridSize)

                    match ((row, col), mode) with // Down
                    | BottomEdge, SingleLevel -> ()
                    | BottomEdge, MultiLevel  -> yield (InnerLevel, CentreIndex + GridSize) 
                    | AboveMid, MultiLevel    -> yield! outer topEdge
                    | _                       -> yield (CurrentLevel, index + GridSize)
                } |> Seq.toList

        List.init NumCells (fun x -> x, adjacentF x) |> readOnlyDict

    let addOuterLevels (levels: Level[]) =
        let (firstLevel, lastLevel) = (Array.head levels, Array.last levels)

        // Pad at least one outer layer either side as Windowed reduces 3 layers to 1
        // And add additional empty layers if bugs are about to expand to next layer
        seq { 
            yield Level.Empty
            if firstLevel.Bugs > 0 then yield Level.Empty
            yield! levels
            if lastLevel.Bugs > 0 then yield Level.Empty
            yield Level.Empty
        } |> Seq.toArray

    let simulate (mode: Mode) =
        let level0 = { Cells = parseInput() }
        let state = { Levels = [| level0 |] }
        let adjacentCellMap = getAdjacentCellMap mode

        let evolveLevel (levels: Level[]) =

            let getEvolvedCell (index: int) (cell: CellType) =

                let adjacentBugs = 
                    adjacentCellMap.[index]
                    |> List.sumBy (fun (lvl, i) -> levels.[lvl].Cells.[i])

                // A bug dies (becoming an empty space) unless there is exactly one bug adjacent to it.
                // An empty space becomes infested with a bug if exactly one or two bugs are adjacent to it.
                // Otherwise, a bug or empty space remains the same
                match (cell, adjacentBugs) with
                | (Bug, 1) | (Empty, 1) | (Empty, 2) -> Bug
                | _ -> Empty

            let currentLevel = levels.[CurrentLevel]

            { currentLevel with Cells = currentLevel.Cells |> Array.mapi getEvolvedCell }

        let evolveGeneration (generation: Generation) = 

            let levels =
                generation.Levels
                |> addOuterLevels  // Add empty levels for expansion
                |> Array.windowed 3 // Inner, Current, Outer levels
                |> PSeq.map evolveLevel // Parallelizing gives ~100% speed up
                |> PSeq.toArray

            { generation with Levels = levels }

        state
        |> Seq.unfold (fun gen -> Some(gen, evolveGeneration gen))


    // What is the biodiversity rating for the first layout that appears twice?
    let Part1() =
        let statesSeen = HashSet<int>()

        simulate SingleLevel
        |> Seq.map (fun x -> x.BioDiversity)
        |> Seq.find (statesSeen.Add >> not)

    // This 5x5 grid is only one level in an infinite number of recursion levels.
    // Starting with your scan, how many bugs are present after 200 minutes?
    let Part2() =
        simulate MultiLevel
        |> Seq.item 200
        |> fun x -> x.Bugs


//     |     |         |     |     
//  0  |  1  |    2    |  3  |  4  
//     |     |         |     |     
//-----+-----+---------+-----+-----
//     |     |         |     |     
//  5  |  6  |    7    |  8  |  9 
//     |     |         |     |     
//-----+-----+---------+-----+-----
//     |     |A|B|C|D|E|     |     
//     |     |-+-+-+-+-|     |     
//     |     |F|G|H|I|J|     |     
//     |     |-+-+-+-+-|     |     
// 10  | 11  |K|L|?|N|O|  13 |  14 
//     |     |-+-+-+-+-|     |     
//     |     |P|Q|R|S|T|     |     
//     |     |-+-+-+-+-|     |     
//     |     |U|V|W|X|Y|     |     
//-----+-----+---------+-----+-----
//     |     |         |     |     
// 15  | 16  |    17   |  18 |  19 
//     |     |         |     |     
//-----+-----+---------+-----+-----
//     |     |         |     |     
// 20  | 21  |    22   |  23 |  24 
//     |     |         |     |     