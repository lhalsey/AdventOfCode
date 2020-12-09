namespace AdventOfCode.Days.Y2020

open AdventOfCode.Shared.Utility
open FSharp.Collections.ParallelSeq

/// Day 3: Toboggan Trajectory
/// https://adventofcode.com/2020/day/3
/// With the toboggan login problems resolved, you set off toward the airport.
module Day03 =

    let [<Literal>] Tree = '#'

    let parseInput() = getFile (2020, 3) |> readAllLines

    // Toboggan down the slope from top left to the bottom using provided angle
    // The same pattern repeats to the right many times so use mod width for X
    let countTrees (input: string []) (dx, dy) =
        let width = input.[0].Length

        let traverse (trees, column) (row: string) =
            let trees = if row.[column % width] = Tree then trees + 1 else trees
            (trees, column + dx)

        ((0, 0), takeEvery dy input)
        ||> Seq.fold traverse
        |> fst

    let getTreesProduct slopes =
        let input = parseInput()
        
        slopes
        |> PSeq.map (countTrees input >> int64)
        |> PSeq.reduce (Checked.(*))

    // Starting at the top-left corner of your map and following a slope of right 3 and down 1,
    // how many trees would you encounter?
    let Part1() = getTreesProduct [ (3, 1) ]

    // What do you get if you multiply together the number of trees encountered on each of the listed slopes?
    let Part2() = getTreesProduct  [ (1, 1); (3, 1); (5, 1); (7, 1); (1, 2) ]