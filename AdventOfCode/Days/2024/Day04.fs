namespace AdventOfCode.Days.Y2024

open AdventOfCode.Shared.Utility
open AdventOfCode.Shared
open System.Collections.Generic

/// Day 4: Ceres Search
/// https://adventofcode.com/2024/day/4
/// "Looks like the Chief's not here. Next!" One of The Historians pulls out a device and pushes the only button on it.
module Day04 =

    let parseInput() = getFile (2024, 4) |> parseGrid id |> readOnlyDict

    let isFound (grid: IReadOnlyDictionary<Point2d, char>) (word: string) (point: Point2d)  (dir: Direction2d) =
        (point, [1 .. word.Length - 1])
        ||> Seq.scan (fun acc _ -> acc + dir)
        |> Seq.map (fun x -> tryFind x grid)
        |> Seq.zip word
        |> Seq.forall (fun (x, y) -> Some x = y)

    let Part1() =
        let grid = parseInput()

        let getCountForCell (point: Point2d) =
            Direction2d.AllDirections
            |> countIf (isFound grid "XMAS" point)

        grid
        |> Seq.filter (fun x -> x.Value = 'X')
        |> Seq.sumBy (fun x -> getCountForCell x.Key)
        

    let Part2() =
        let grid = parseInput()

        let valid = [ "AMSMS"; "ASSMM"; "AMMSS"; "ASMSM" ]

        let isX (point: Point2d) =
            let directions = [ Direction2d.Zero; Direction2d.NorthWest; Direction2d.NorthEast; Direction2d.SouthWest; Direction2d.SouthEast ]
            let cells = directions |> List.choose (fun dir -> tryFind (point + dir) grid) |> List.toArray |> fun x -> new string(x)

            valid |> List.contains cells

        grid.Keys |> countIf isX