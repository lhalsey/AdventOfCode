namespace AdventOfCode.Days.Y2021

open AdventOfCode.Shared.Utility

/// Day 14: Extended Polymerization
/// https://adventofcode.com/2021/day/14
/// The incredible pressures at this depth are starting to put a strain on your submarine.
module Day14 =

    let parseInput() =
        let lines = getFile (2021, 14) |> readLines
        let template = lines |> Seq.head
        let rules =
            lines
            |> Seq.skip 2
            |> Seq.map (splitIntoPair  " -> ")
            |> Seq.map (fun (x, y) -> (x.[0], x.[1]), y.[0])
            |> readOnlyDict

        (template, rules)

    let mergeCounts (counts: ('a * int64) list list) =
        counts
        |> List.concat
        |> List.groupBy fst
        |> List.map (fun (x, y) -> x, y |> List.sumBy snd)

    let expand (steps: int) =
        let (template, rules) = parseInput()

        let rec expandR (pair: char * char, n: int) =
            match pair, n with 
            | (_, e2), 0 -> [e2, 1L] // Fully expanded, return last char of pair (so as not to double count)
            | (e1, e2), _ ->
                let elem = rules.[pair]
                let pair1 = e1, elem
                let pair2 = elem, e2
                let count1 = expandMemoise (pair1, n - 1)
                let count2 = expandMemoise (pair2, n - 1)

                mergeCounts [count1; count2]

        and expandMemoise = memoise expandR

        let counts =
            template
            |> Seq.pairwise
            |> Seq.map (fun x -> expandMemoise (x, steps))
            |> Seq.append [[(template.[0], 1L)]] // Prepend first letter as excluded in expansion
            |> Seq.toList
            |> mergeCounts

        let max = counts |> List.map snd |> List.max
        let min = counts |> List.map snd |> List.min
                   
        max - min

    // Apply 10 steps of pair insertion to the polymer template and find the most and least common elements
    // in the result.
    // What do you get if you take the quantity of the most common element and subtract the quantity of the
    // least common element?
    let Part1() = expand 10
        
    // Apply 40 steps of pair insertion to the polymer template and find the most and least common elements
    // in the result.
    // What do you get if you take the quantity of the most common element and subtract the quantity of the
    // least common element?
    let Part2() = expand 40