namespace AdventOfCode.Days.Y2021

open AdventOfCode.Shared
open AdventOfCode.Shared.Utility

/// Day 22: Reactor Reboot
/// https://adventofcode.com/2021/day/22
/// Operating at these extreme ocean depths has overloaded the submarine's reactor; it needs to be rebooted.
module Day22 =

    type Cuboid = { P1: Point3d; P2: Point3d; IsOn: int } with
        member __.IsInRange (range: int) = 
            [ __.P1.X; __.P1.Y; __.P1.Z; __.P2.X; __.P2.Y; __.P2.Z ]
            |> List.map abs
            |> List.max
            |> fun x -> x <= range

        member __.Intersect (c2: Cuboid) =
            let range (x1, x2) (x3, x4) =
                let s = x3 |> max x1 |> min x2
                let e = x4 |> max x1 |> min x2
                e - s

            let dx = range (c2.P1.X, c2.P2.X) (__.P1.X, __.P2.X)
            let dy = range (c2.P1.Y, c2.P2.Y) (__.P1.Y, __.P2.Y)
            let dz = range (c2.P1.Z, c2.P2.Z) (__.P1.Z, __.P2.Z)

            if c2.P1.X > __.P2.X || c2.P2.X < __.P1.X ||
               c2.P1.Y > __.P2.Y || c2.P2.Y < __.P1.Y ||
               c2.P1.Z > __.P2.Z || c2.P2.Z < __.P1.Z
            then None
            else Some { X = dx; Y = dy; Z = dz }

    let parse = function
        | Regex "on x=([+-]*\d+)..([+-]*\d+),y=([+-]*\d+)..([+-]*\d+),z=([+-]*\d+)..([+-]*\d+)"
            [Int x1; Int x2; Int y1; Int y2; Int z1; Int z2 ] ->
                { P1 = { X = x1; Y = y1; Z = z1; }; P2 = { X = x2; Y = y2; Z = z2; }; IsOn = 1 }
        | Regex "off x=([+-]*\d+)..([+-]*\d+),y=([+-]*\d+)..([+-]*\d+),z=([+-]*\d+)..([+-]*\d+)"
            [Int x1; Int x2; Int y1; Int y2; Int z1; Int z2 ] ->
                { P1 = { X = x1; Y = y1; Z = z1; }; P2 = { X = x2; Y = y2; Z = z2; }; IsOn = 0 }
        | x -> failwithf "Invalid input: %s" x

    let parseInput() = getFile (2021, 22) |> readLinesAs parse |> Seq.toList

    let setCubes (onCubes: Point3d Set) (cuboid: Cuboid) =
        let points =
            [cuboid.P1.X .. cuboid.P2.X]
            |> List.allPairs [cuboid.P1.Y .. cuboid.P2.Y]
            |> List.allPairs [cuboid.P1.Z .. cuboid.P2.Z]
            |> List.map (fun (x, (y, z)) -> { X = x; Y = y; Z = z })

        let f = if cuboid.IsOn = 0 then Set.remove else Set.add

        (onCubes, points)
        ||> List.fold (fun acc x -> f x acc)

    let isInRange (p: Point3d) (range: int) = abs p.X <= range || abs p.Y <= range || abs p.Z <= range

    // Execute the reboot steps. Afterward, considering only cubes in the region x=-50..50,y=-50..50,z=-50..50,
    // how many cubes are on?
    let Part1() =
        let cuboids =
            parseInput()
            |> List.filter (fun cuboid -> cuboid.IsInRange 50)

        let res = 
            (Set.empty, cuboids)
            ||> List.fold (fun acc x -> setCubes acc x)

        res.Count

    // Starting again with all cubes off, execute all reboot steps. Afterward, considering all cubes,
    // how many cubes are on?
    let Part2() =
        let cuboids =
            parseInput()

        let pairs = 
            cuboids
            |> List.pairwise
            |> List.map (fun (x, y) -> x.Intersect y)

        0