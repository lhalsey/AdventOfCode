namespace AdventOfCode.Days.Y2021

open AdventOfCode.Shared
open AdventOfCode.Shared.Utility

/// Day 22: Reactor Reboot
/// https://adventofcode.com/2021/day/22
/// Operating at these extreme ocean depths has overloaded the submarine's reactor; it needs to be rebooted.
module Day22 =

    type Cuboid = { P1: Point3d; P2: Point3d; IsOn: bool } with
        member __.IsInRange (range: int) = 
            [ __.P1.X; __.P1.Y; __.P1.Z; __.P2.X; __.P2.Y; __.P2.Z ]
            |> List.map abs
            |> List.max
            |> fun x -> x <= range

        member __.Intersect (c2: Cuboid) =
            let p1 =
                { X = max __.P1.X c2.P1.X
                  Y = max __.P1.Y c2.P1.Y
                  Z = max __.P1.Z c2.P1.Z }
            let p2 =
                { X = min __.P2.X c2.P2.X
                  Y = min __.P2.Y c2.P2.Y
                  Z = min __.P2.Z c2.P2.Z }
            if p1.X <= p2.X && p1.Y <= p2.Y && p1.Z <= p2.Z
            then Some { P1 = p1; P2 = p2; IsOn = not c2.IsOn }
            else None

        member __.Volume =
            int64 (__.P2.X + 1 - __.P1.X) *
            int64 (__.P2.Y + 1 - __.P1.Y) *
            int64 (__.P2.Z + 1 - __.P1.Z)

        member __.Value = if __.IsOn then __.Volume else -__.Volume

    let parse = function
        | Regex "on x=([+-]*\d+)..([+-]*\d+),y=([+-]*\d+)..([+-]*\d+),z=([+-]*\d+)..([+-]*\d+)"
            [Int x1; Int x2; Int y1; Int y2; Int z1; Int z2 ] ->
                { P1 = { X = x1; Y = y1; Z = z1; }; P2 = { X = x2; Y = y2; Z = z2; }; IsOn = true }
        | Regex "off x=([+-]*\d+)..([+-]*\d+),y=([+-]*\d+)..([+-]*\d+),z=([+-]*\d+)..([+-]*\d+)"
            [Int x1; Int x2; Int y1; Int y2; Int z1; Int z2 ] ->
                { P1 = { X = x1; Y = y1; Z = z1; }; P2 = { X = x2; Y = y2; Z = z2; }; IsOn = false }
        | x -> failwithf "Invalid input: %s" x

    let parseInput() = getFile (2021, 22) |> readLinesAs parse |> Seq.toList

    let getOnCubes (cuboids: Cuboid list) =

        // If current cuboid overlaps with any other cuboids then we need to add
        // negating sub-cuboids to compensate
        let addCuboids (cuboids: Cuboid list) (cuboid: Cuboid) =
            let intersections = cuboids |> List.choose (fun x -> cuboid.Intersect x)
            let additions = if cuboid.IsOn then cuboid::intersections else intersections
            cuboids @ additions

        ([], cuboids)
        ||> List.fold addCuboids
        |> List.sumBy (fun x -> x.Value)

    // Execute the reboot steps. Afterward, considering only cubes in the region x=-50..50,y=-50..50,z=-50..50,
    // how many cubes are on?
    let Part1() =
       parseInput()
       |> List.filter (fun cuboid -> cuboid.IsInRange 50)
       |> getOnCubes

    // Starting again with all cubes off, execute all reboot steps. Afterward, considering all cubes,
    // how many cubes are on?
    let Part2() = parseInput() |> getOnCubes