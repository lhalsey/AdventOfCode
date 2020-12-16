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

        //OutletJoltage :: input @ [ deviceJoltage ]
        //|> List.pairwise
        //|> List.fold (fun (s1, s2) (a1, a2) ->
        //    match a2 - a1 with
        //    | 1 -> (s1 + 1, s2)
        //    | 3 -> (s1, s2 + 1)
        //    | _ -> (s1, s2)) (0, 0)
        //||> (*)

    //let Part2() =
    //    let deviceJoltage = input |> List.max |> (+) 3

    //    let m =
    //        OutletJoltage :: input @ [ deviceJoltage ]
    //        |> List.pairwise
    //        |> List.countBy (fun (a1, a2) -> a2 - a1)
    //        |> Map

    //    let multipliers = [1, 1; 3, 4 ]

    //    let m2 =
    //        multipliers
    //        |> List.map (fun (x, y) -> pown m.[x] y)

    //    m2 |> List.reduce (*)

    //let Part2() =
    //    let input = input |> List.cons 0
    //    let max = List.max input
    //    let a = Array.zeroCreate (max + MaxJoltageDiff)
    //    input |> List.iter (fun x -> a.[x] <- 1L)

    //    [ max - 1 .. -1 .. 0]
    //    |> List.iter (fun x -> if a.[x] = 1L then a.[x] <- a.[x + 1] + a.[x + 2] + a.[x + 3])

    //    a.[0]



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
                | i -> [1 .. MaxJoltageDiff] |> List.sumBy ((+) i >> getCombosMemo)
            and getCombosMemo = memoise getCombosR

            getCombosMemo OutletJoltage

        parseInput()
        |> List.cons OutletJoltage
        |> toReadOnlyHashSet
        |> getCombos

        //let getCombos elems =
        //    let maxJoltageDiff = 3
        //    let outletJoltage = 0
        //    let max = Array.last elems // Device joltage is max + 3 so last element must be max
            
        //    let (|Valid|OutOfRange|) (index: int) =
        //        if index >= 0 && index < elems.Length then Valid elems.[index] else OutOfRange
            
        //    // Use current element index and last element added as efficient key for memoisation
        //    let rec getCombosR (index, last) = 
        //        match index with 
        //        | OutOfRange when last = max -> 1L // No more elements so count if last element valid
        //        | Valid value when value - last <= maxJoltageDiff ->
        //            let incl = getCombosMemo (index + 1, value)
        //            let excl = getCombosMemo (index + 1, last)
        //            incl + excl // Try including and excluding current element
        //        | _ -> 0L
        //    and getCombosMemo = memoise getCombosR

        //    getCombosMemo (0, outletJoltage)