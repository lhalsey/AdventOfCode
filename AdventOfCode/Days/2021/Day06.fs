namespace AdventOfCode.Days.Y2021

open AdventOfCode.Shared.Utility

/// Day 6: Lanternfish
/// https://adventofcode.com/2021/day/6
/// The sea floor is getting steeper. Maybe the sleigh keys got carried this way?
module Day06 =

    let parseInput() = getFile (2021, 6) |> readCsv |> List.map int

    let update (fishCount: (int * int64) list) =
        let updateGroup = function
            | (0, c) -> [(8, c); (6, c)] // Fish reproduces at 0 days and resets to 6 days, new fish is 8 days
            | (x, c) -> [(x - 1, c)]     // All other fish decrement by 1 day

        fishCount // Update all age groups and merge
        |> List.collect updateGroup
        |> List.groupBy fst
        |> List.map (fun (x, cs) -> (x, cs |> List.sumBy snd))
        
    let getFishCountAfterDays days =
        let input = // Group fish by age to reduce calculations
            parseInput()
            |> List.countBy id
            |> List.map (fun (x, c) -> (x, int64 c)) // Numbers get big!

        let last = // Evolve for x days and take final population
            input
            |> Seq.unfold (fun x -> Some(x, update x))
            |> Seq.item days
        
        last |> List.sumBy snd

    // Find a way to simulate lanternfish. How many lanternfish would there be after 80 days?
    let Part1() = getFishCountAfterDays 80

    // How many lanternfish would there be after 256 days?
    let Part2() = getFishCountAfterDays 256