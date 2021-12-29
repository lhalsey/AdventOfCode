namespace AdventOfCode.Days.Y2021

open AdventOfCode.Shared
open AdventOfCode.Shared.Utility

/// Day 19: Beacon Scanner
/// https://adventofcode.com/2021/day/19
/// As your probe drifted down through this area, it released an assortment of beacons and scanners into the water.
module Day19 =

    let parseLines (lines: string list) = 
        let rec parseLinesR lines beacons =
            seq {
                match lines with
                | (Regex "--- scanner (\d+) ---" _)::xs -> yield! parseLinesR xs []
                | (Regex "([+-]*\d+),([+-]*\d+),([+-]*\d+)" [Int x; Int y; Int z])::xs ->
                    let beacon = { X = x; Y = y; Z = z }
                    yield! parseLinesR xs (beacon::beacons)
                | [] -> yield beacons
                | _::xs -> yield beacons         
                           yield! parseLinesR xs []
                }
        parseLinesR lines [] |> Seq.toList |> List.map (List.sortBy (fun x -> x.ManhattanDistance))

    let parseInput() = getFile (2021, 19) |> readLines |> Seq.toList |> parseLines

    let normalise (pivot: Point3d) (beacons: Point3d list) =
        beacons
        |> List.map (fun b -> b.ManhattanDistanceTo pivot)

    let Part1() =
        let scanners = parseInput()

        let (s1, s2) = scanners.[1], scanners.[4]

        let overlap ((n1: int list), (n2: int list)) =
            let i = Set.intersect (set n1) (set n2)
            let ns = i |> Set.toList |> List.sort
            let i1 = ns |> List.map (fun x -> n1 |> List.findIndex (fun y -> y = x))
            let i2 = ns |> List.map (fun x -> n2 |> List.findIndex (fun y -> y = x))

            (i1, i2) 
            ||> List.zip
            |> List.map (fun (x, y) -> s1.[x], s2.[y])


        let s_0_1 =
            List.allPairs s1 s2
            |> List.map (fun (p1, p2) -> (p1, normalise p1 s1), (p2, normalise p2 s2))
            |> List.map (fun ((p1, n1), (p2, n2)) -> overlap (n1, n2))
            |> List.sortByDescending (fun x -> x.Length)
            |> List.filter (fun x -> x.Length >= 12)

        let getOffset ((p0_1: Point3d), (p0_2: Point3d)) ((p1_1: Point3d), (p1_2: Point3d)) =
            let x1 = p0_1.X + p0_2.X, p1_1.X + p1_2.X
            let x2 = p0_1.X - p0_2.X, p1_1.X - p1_2.X
            let y1 = p0_1.Y + p0_2.Y, p1_1.Y + p1_2.Y
            let y2 = p0_1.Y - p0_2.Y, p1_1.Y - p1_2.Y
            let z1 = p0_1.Z + p0_2.Z, p1_1.Z + p1_2.Z
            let z2 = p0_1.Z - p0_2.Z, p1_1.Z - p1_2.Z

            let d1 = if p0_1.X - p0_2.X = p1_1.X - p1_2.X then p0_1.X - p0_2.X else p0_1.X + p0_2.X
            let d2 = if p0_1.Y - p0_2.Y = p1_1.Y - p1_2.Y then p0_1.Y - p0_2.Y else p0_1.Y + p0_2.Y
            let d3 = if p0_1.Z - p0_2.Z = p1_1.Z - p1_2.Z then p0_1.Z - p0_2.Z else p0_1.Z + p0_2.Z
            { X = d1; Y = d2; Z = d3 }

        let ds = 
            s_0_1
            |> List.map (fun x -> getOffset x.[0] x.[1])

        0

    let Part2() =
        0