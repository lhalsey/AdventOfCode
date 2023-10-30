namespace AdventOfCode.Days.Y2022

open AdventOfCode.Shared.Utility
open AdventOfCode.Shared

/// Day 15: Beacon Exclusion Zone
/// https://adventofcode.com/2022/day/15
/// You feel the ground rumble again as the distress signal leads you to a large network of subterranean tunnels.
module Day15 =

    type Reading = { Sensor: Point2d; Beacon: Point2d } with
        member __.Distance = __.Sensor.ManhattanDistanceTo __.Beacon

    let parse = function // Sensor at x=20, y=1: closest beacon is at x=15, y=3
        | Regex "Sensor at x=([+-]*\d+), y=([+-]*\d+): closest beacon is at x=([+-]*\d+), y=([+-]*\d+)"
            [Int sx; Int sy; Int bx; Int by] -> { Sensor = { X = sx; Y = sy }; Beacon = { X = bx; Y = by } }
        | x -> failwithf "Invalid pattern: %s" x

    let parseInput() = getFile (2022, 15) |> readLinesAs parse

    let Part1() =
        let readings = parseInput() |> Seq.toList

        let minX = readings |> List.map (fun x -> x.Sensor.X - x.Distance) |> List.min
        let maxX = readings |> List.map (fun x -> x.Sensor.X + x.Distance) |> List.max

        let testRow = 2_000_000
        let rowPoints = { X = minX; Y = testRow }.GetPointsBetween { X = maxX; Y = testRow }

        let isCovered (p: Point2d) =
            let inRange (reading: Reading) =
                reading.Sensor.ManhattanDistanceTo p <= reading.Distance && reading.Beacon <> p

            readings |> List.exists inRange

        let coverage = rowPoints |> Seq.map isCovered

        coverage |> countIf id

    let Part2() =
        failwith "Too slow!"

        let readings = parseInput() |> Seq.toList

        let isCovered (p: Point2d) =
            let inRange (reading: Reading) =
                reading.Sensor.ManhattanDistanceTo p <= reading.Distance || reading.Beacon = p

            readings |> List.exists inRange

        let upperRange = 4_000_000
        let allPoints = List.allPairs [0..upperRange] [0..upperRange] |> List.map (fun (x, y) -> { X = x; Y = y })
        let uncovered = allPoints |> List.find (isCovered >> not)

        uncovered.X * upperRange + uncovered.Y