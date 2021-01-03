namespace AdventOfCode.Days.Y2020

open System.Collections.Generic
open AdventOfCode.Shared.Utility
open FSharpx.Collections

/// Day 10: Adapter Array
/// https://adventofcode.com/2020/day/10
/// Patched into the aircraft's data port, you discover weather forecasts of a massive tropical storm.
module Day10 =

    let [<Literal>] MaxJoltageDiff = 3
    let [<Literal>] OutletJoltage = 0

    let parseInput() =
        getFile (2020, 10)
        |> readLinesAs int
        |> Seq.toList
        |> List.sort

    let input = parseInput()


    // Find a chain that uses all of your adapters to connect the charging outlet to your device's built-in
    // adapter and count the joltage differences between the charging outlet, the adapters, and your device.
    // What is the number of 1-jolt differences multiplied by the number of 3-jolt differences?
    let Part1() =    
        //let input = parseInput()
        let deviceJoltage = input |> List.max |> (+) 3

        OutletJoltage :: input @ [ deviceJoltage ]
        |> List.pairwise
        |> List.countBy (fun (a1, a2) -> a2 - a1)
        |> Map
        |> fun diffs -> diffs.[1] * diffs.[3]


    // What is the total number of distinct ways you can arrange the adapters to connect the charging
    // outlet to your device?
    let Part2() =
        // This is slightly more performant and concise than previous attempt, but not as good as
        // the lazy map implementation in Haskell
        // https://github.com/mstksg/advent-of-code-2020/blob/master/reflections.md#day-9
        let getCombos (elems: IReadOnlySet<int>) =
            let max = Seq.max elems
            
            let rec getCombosR = function
                | i when i = max -> 1L
                | i when elems.Contains i |> not -> 0L
                | i -> [1 .. MaxJoltageDiff] |> List.sumBy (fun x -> getCombosMemo (x + i))
            and getCombosMemo = memoise getCombosR

            getCombosMemo OutletJoltage

        parseInput()
        |> List.cons OutletJoltage
        |> toReadOnlyHashSet
        |> getCombos