namespace AdventOfCode.Days.Y2021

open AdventOfCode.Shared.Utility

/// Day 7: The Treachery of Whales
/// https://adventofcode.com/2021/day/7
/// A giant whale has decided your submarine is its next meal, and it's much faster than you are.
/// There's nowhere to run!
module Day07 =

    let parseInput() = getFile (2021, 7) |> readCsv |> List.map int

    let getAbsDistance pos x = abs (pos - x)

    let getTriangle (x: int) = (x * (x + 1)) / 2

    let getLowestFuel (costF: int -> int -> int) =
        let input = parseInput()
        let (lowest, highest) = (List.min input, List.max input)

        let getFuel pos = input |> List.sumBy (costF pos)

        [lowest .. highest] |> List.map getFuel |> List.min

    // Determine the horizontal position that the crabs can align to using the least fuel possible.
    // How much fuel must they spend to align to that position?
    let Part1() = getLowestFuel getAbsDistance
        
    // Determine the horizontal position that the crabs can align to using the least fuel possible so they can
    // make you an escape route! How much fuel must they spend to align to that position?
    let Part2() = getLowestFuel  (fun pos -> (getAbsDistance pos >> getTriangle))