namespace AdventOfCode.Days.Y2022

open AdventOfCode.Shared.Utility
open System

/// Day 3: Rucksack Reorganization
/// https://adventofcode.com/2022/day/3
/// One Elf has the important job of loading all of the rucksacks with supplies for the jungle journey.
module Day03 =

    let parseInput() = getFile (2022, 3) |> readLines

    let getItemInTwoCompartments (items: string) =
        let mid = items.Length / 2
        let compartment1 = items[..mid - 1] |> toHashSet
        items[mid..] |> Seq.find compartment1.Contains

    let getCommonItem (itemLists: string seq) =
        itemLists
        |> Seq.map set
        |> Set.intersectMany 
        |> Set.minElement

    let getPriority (c: char) = if Char.IsLower c then (int c - int 'a') + 1 else (int c - int 'A') + 27

    // Find the item type that appears in both compartments of each rucksack.
    // What is the sum of the priorities of those item types?
    let Part1() = parseInput() |> Seq.sumBy (getItemInTwoCompartments >> getPriority)

    // Find the item type that corresponds to the badges of each three-Elf group.
    // What is the sum of the priorities of those item types?
    let Part2() = parseInput() |> Seq.chunkBySize 3 |> Seq.sumBy (getCommonItem >> getPriority)