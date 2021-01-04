namespace AdventOfCode.Days.Y2020

open AdventOfCode.Shared.Utility
open MathNet.Numerics

/// Day 13: Shuttle Search
/// https://adventofcode.com/2020/day/13
/// Your ferry can make it safely to a nearby port, but it won't get much further.
module Day13 =

    // E.g. "13,x,x,x,x,x,x,37,x,x,x,x,x,461,x,x,x,x ..."
    let parseInput() =
        let lines = getFile (2020, 13) |> readLines |> Seq.toList
        let buses =
            lines.[1]
            |> split ','
            |> Array.indexed
            |> Array.filter (fun (_, x) -> x <> "x")
            |> Array.map (fun (i, x) -> int64 i, int64 x)
            |> Array.toList

        (int64 lines.[0], buses)

    // https://en.wikipedia.org/wiki/Chinese_remainder_theorem
    let chineseRemainder (nums: (int64 * int64) list) =
        let nums =
            nums
            |> List.map (fun (x, y) -> Euclid.Modulus(-x, y), y)
            |> List.sortByDescending snd

        let findM res offset a n =
            let rec findMR res =
                if res % a = n then res else findMR (res + offset)
                
            findMR res

        let findRes elems =
            let rec findResR elems res offset =
                match elems with
                | [] -> res
                | (a, n)::t ->
                    let res = findM res offset n a
                    findResR t res (offset * n)
                       
            match elems with
            | (a, n)::t -> findResR t a n
            | _ -> -1L
        
        findRes nums


    // What is the ID of the earliest bus you can take to the airport multiplied by the
    // number of minutes you'll need to wait for that bus?
    let Part1() =
        let (target, buses) = parseInput()

        buses
        |> List.map (fun (_, id) -> id, id - target % id)
        |> List.minBy snd
        |> fun (id, time) -> id * time

    // What is the earliest timestamp such that all of the listed bus IDs depart at offsets matching
    // their positions in the list?
    let Part2() =
        let (_, buses) = parseInput()

        chineseRemainder buses