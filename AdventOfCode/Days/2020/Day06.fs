namespace AdventOfCode.Days.Y2020

open AdventOfCode.Shared.Utility
open FSharp.Collections.ParallelSeq

/// Day 6: Custom Custom
/// https://adventofcode.com/2020/day/6
/// As your flight approaches the regional airport where you'll switch to a much larger plane,
/// customs declaration forms are distributed to the passengers.
module Day06 =

    let parseInput() = getFile (2020, 6) |> readAllText |> splitOn "\n\n" 

    // This is ~3x slower than using distinct/countBy but is elegant and consistent for both parts
    let getYesAnswers combine group =
        group
        |> split '\n'
        |> Array.map set
        |> combine
        |> Set.count

    // For each group, count the number of questions to which anyone answered "yes".
    // What is the sum of those counts?
    let Part1() = parseInput() |> PSeq.sumBy (getYesAnswers Set.unionMany)

    // For each group, count the number of questions to which everyone answered "yes".
    //What is the sum of those counts?
    let Part2() = parseInput() |> PSeq.sumBy (getYesAnswers Set.intersectMany)