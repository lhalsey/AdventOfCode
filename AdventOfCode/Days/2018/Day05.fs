namespace AdventOfCode.Days.Y2018

open AdventOfCode.Shared.Utility
open System

/// Day 5: Alchemical Reduction
/// https://adventofcode.com/2018/day/5
/// You've managed to sneak in to the prototype suit manufacturing lab.
module Day05 =

    let parseInput() = getFile (2018, 5) |> readAllText

    let react = function
        | x, y::ys when (int x ^^^ int y) = 32 -> ys // Reaction, e.g. Aa or bB, so remove element
        | x, ys -> x::ys // No reaction, so add element

    let getReactedLength (s: string) =
        (s, [])
        ||> Seq.foldBack (fun x acc -> react (x, acc))
        |> List.length

    // How many units remain after fully reacting the polymer you scanned?
    let Part1() = parseInput() |> getReactedLength

    // What is the length of the shortest polymer you can produce by removing all units of exactly one type and
    // fully reacting the result?
    let Part2() =
        let input = parseInput()

        let removeUnits (c: char) = input.Replace(c.ToString(), "", StringComparison.OrdinalIgnoreCase)

        input 
        |> Seq.distinctBy Char.ToUpper
        |> Seq.map (removeUnits >> getReactedLength)
        |> Seq.min