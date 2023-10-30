namespace AdventOfCode.Days.Y2022

open AdventOfCode.Shared.Utility
open AdventOfCode.Shared
open System.Collections.Generic
open FSharp.Collections.ParallelSeq

/// Day 23: Unstable Diffusion
/// https://adventofcode.com/2022/day/23
/// You enter a large crater of gray dirt where the grove is supposed to be.
module Day23 =

    let parse = function '#' -> true | _ -> false

    let parseInput() =
        getFile (2022, 23)
        |> parseGrid parse
        |> Seq.filter snd
        |> Seq.map fst
        |> toHashSet

    let tryMove (elves: HashSet<Point2d>, dirs: Direction2d list) (elf: Point2d) =
        if dirs |> List.exists (fun x -> elves.Contains (elf + x)) then None else Some (elf + dirs[0])

    let getMove (elves: HashSet<Point2d>, dirLists: Direction2d list list) (elf: Point2d) =
        let shouldMove = elf.GetAllAdjacent() |> Seq.exists (elves.Contains)

        if shouldMove then 
            dirLists
            |> List.tryPick (fun dirs -> tryMove (elves, dirs) elf)
            |> Option.map (fun p -> (elf, p))
        else None

    let getMoves (elves: HashSet<Point2d>, dirLists: Direction2d list list) =
        let moves = elves |> PSeq.choose (fun x -> getMove (elves, dirLists) x) |> Seq.toList

        let duplicateTargets =
            moves
            |> List.map snd
            |> List.countBy id
            |> List.filter (fun (_, c) -> c > 1)
            |> List.map fst
            |> toHashSet

        let validMoves = moves |> List.filter (fun (e, p) -> duplicateTargets.Contains p |> not)

        let newElves = elves |> toHashSet
        validMoves |> List.iter (fun (e, p) -> newElves.Remove e |> ignore; newElves.Add p |> ignore)

        let newDirLists = dirLists.Tail @ [dirLists.Head]

        if validMoves.IsEmpty then None else Some((elves, dirLists), (newElves, newDirLists))

    let simulate() =
        let elves = parseInput()

        let tryNorth = [ Direction2d.North; Direction2d.NorthWest; Direction2d.NorthEast ]
        let trySouth = [ Direction2d.South; Direction2d.SouthEast; Direction2d.SouthWest ]
        let tryWest = [ Direction2d.West; Direction2d.NorthWest; Direction2d.SouthWest ]
        let tryEast = [ Direction2d.East; Direction2d.NorthEast; Direction2d.SouthEast ]
        let dirLists = [ tryNorth; trySouth; tryWest; tryEast ]

        (elves, dirLists) |> Seq.unfold getMoves

    // Simulate the Elves' process and find the smallest rectangle that contains the Elves after 10 rounds.
    // How many empty ground tiles does that rectangle contain?
    let Part1() =
        let (elves, _) = simulate() |> Seq.item 10

        elves
        |> Seq.map (fun x -> x, '#')
        |> readOnlyDict
        |> ImageProvider.getImage id
        |> countIf ((=) ' ')

    // Figure out where the Elves need to go. What is the number of the first round where no Elf moves?
    let Part2() = simulate() |> Seq.length |> (+) 1