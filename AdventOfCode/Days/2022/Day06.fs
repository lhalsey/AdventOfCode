namespace AdventOfCode.Days.Y2022

open AdventOfCode.Shared.Utility

/// Day 6: Tuning Trouble
/// https://adventofcode.com/2022/day/6
/// The preparations are finally complete; you and the Elves leave camp on foot and
/// begin to make your way toward the star fruit grove.
module Day06 =

    let parseInput() = getFile (2022, 6) |> readAllText

    let getMarkerIndex (markerLength: int) (s: string) =
        s
        |> Seq.windowed markerLength
        |> Seq.findIndex areAllDistinct
        |> (+) markerLength

    // How many characters need to be processed before the first start-of-packet marker is detected?
    let Part1() = parseInput() |> getMarkerIndex 4

    // How many characters need to be processed before the first start-of-message marker is detected?
    let Part2() = parseInput() |> getMarkerIndex 14