namespace AdventOfCode.Days.Y2015

open AdventOfCode.Shared.Utility

/// Day 24: It Hangs in the Balance
/// https://adventofcode.com/2015/day/24
/// It's Christmas Eve, and Santa is loading up the sleigh for this year's deliveries.
module Day24 =

    let parseInput() = getFile (2015, 24) |> readLinesAs int |> Seq.toList

    let getQuantumEntanglement = Seq.map int64 >> Seq.reduce (*)

    let getSmallestGroup groups = 
        let packages = parseInput()
        let groupWeight = List.sum packages / groups

        // MoreLinq Subsets returns sequences in ascending order of size as we need
        let smallestGroupSize =
            packages
            |> subsets
            |> Seq.find (fun x -> Seq.sum x = groupWeight)
            |> Seq.length

        // As the packages are already sorted in ascending order the first group
        // we find also has the lowest QE value, but we check them all just to be thorough!
        let smallestGroups =
            packages
            |> subsetsWithSize smallestGroupSize
            |> Seq.filter (fun x -> Seq.sum x = groupWeight)
            
        smallestGroups
        |> Seq.map getQuantumEntanglement
        |> Seq.min


    // The packages need to be split into three groups of exactly the same weight, and every package
    // has to fit.
    // What is the quantum entanglement of the first group of packages in the ideal configuration?
    let Part1() = getSmallestGroup 3

    // Balance the sleigh again, but this time, separate the packages into four groups instead of three.
    // Now, what is the quantum entanglement of the first group of packages in the ideal configuration?
    let Part2() = getSmallestGroup 4