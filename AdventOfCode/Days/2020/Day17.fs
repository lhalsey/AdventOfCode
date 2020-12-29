namespace AdventOfCode.Days.Y2020

open AdventOfCode.Shared.Utility
open System.Collections.Generic

/// Day 17: Conway Cubes
/// https://adventofcode.com/2020/day/17
/// As your flight slowly drifts through the sky, the Elves at the Mythical Information Burea
/// at the North Pole contact you.
module Day17 =

    let [<Literal>] Inactive = 0
    let [<Literal>] Active = 1

    type NDimPoint = int list
    type NDimSet = IReadOnlySet<NDimPoint>

    // Cartesian product of lists - every combination of one element from each list
    let cartesianM (elems: 'a list list) =
        let elems = List.rev elems
        let seed = elems.Head |> List.map List.singleton
    
        let combineList acc l = List.allPairs l acc |> List.map (fun (x, y) -> x::y)
    
        (seed, elems.Tail) ||> List.fold combineList

    let getNeighboursF (dimensions: int) =
        let offsets =
            cartesianM (List.replicate dimensions [-1 .. 1])
            |> List.filter (List.exists ((<>) 0)) // Ignore cell itself

        fun point -> offsets |> Seq.map (List.map2 (+) point)

    let evolve getNeighbours (activeCells: NDimSet)  =
        let getActive ((cell: NDimPoint), (count: int)) =
            match activeCells.Contains cell, count with
            | true, 2
            | true, 3
            | false, 3 -> Some cell
            | _ -> None

        // World is sparsely populated so only evaluate for active cells and their neighbours
        // TODO: Note that Z and W planes are symmetrical so perhaps we can only calculate positive values
        // and then multiply the count by 4 while accounting for double counted cells on X and Y axes
        activeCells
        |> Seq.collect getNeighbours
        |> Seq.countBy id
        |> Seq.choose getActive
        |> toReadOnlyHashSet

    let parse = function '.' -> Inactive | '#' -> Active | x -> failwithf "Invalid input: %c" x

    let parseInput() = getFile (2020, 17) |> readLinesAs (Seq.map parse)

    let simulateWithDimensions dimensions =
        let Generations = 6

        let getNeighbours = getNeighboursF dimensions // Just calculate offsets once

        // Start with X and Y plane and add other dimensions
        let createNDimPoint x y = x::y::List.replicate (dimensions - 2) 0

        let getPoint rowI = Seq.mapi (fun colI c -> if c = Active then Some (createNDimPoint colI rowI) else None)

        let activeCells =
            parseInput()
            |> Seq.mapi getPoint
            |> Seq.concat
            |> Seq.choose id
            |> toReadOnlyHashSet

        activeCells
            |> Seq.unfold (fun x -> Some(x, evolve getNeighbours x))
            |> Seq.item Generations
            |> fun active -> active.Count


    // Starting with your given initial configuration, simulate six cycles.
    // How many cubes are left in the active state after the sixth cycle?
    let Part1() = simulateWithDimensions 3

    // Starting with your given initial configuration, simulate six cycles in a 4-dimensional space.
    // How many cubes are left in the active state after the sixth cycle?
    let Part2() = simulateWithDimensions 4