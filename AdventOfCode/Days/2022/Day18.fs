namespace AdventOfCode.Days.Y2022

open AdventOfCode.Shared.Utility
open AdventOfCode.Shared

/// Day 18: Boiling Boulders
/// https://adventofcode.com/2022/day/18
/// You and the elephants finally reach fresh air. You've emerged near the base of a large volcano
/// that seems to be actively erupting!
module Day18 =

    let parse (s: string) =  
        let tokens = s |> split ',' |> Array.map int
        { X = tokens[0]; Y = tokens[1]; Z = tokens[2] }

    let parseInput() = getFile (2022, 18) |> readLinesAs parse |> Seq.toList

    // What is the surface area of your scanned lava droplet?
    let Part1() =
        let cubes = parseInput()
        let cubeSet = cubes |> toReadOnlyHashSet

        let getUnconnectedSides (cube: Point3d) =
            Direction3d.AllDirections
            |> countIf (fun x -> cubeSet.Contains (cube + x) |> not)

        cubes |> List.sumBy getUnconnectedSides

    // What is the exterior surface area of your scanned lava droplet?
    let Part2() =
        let cubes = parseInput()
        let cubeSet = cubes |> toReadOnlyHashSet
        let westmostCube = cubes |> List.minBy (fun x -> x.X)

        let state = (westmostCube, Direction3d.West) // Choose an outside cube face

        let getChildren (cube: Point3d, side: Direction3d) =
             
            let tryMove (dir: Direction3d) =
                if (cubeSet.Contains (cube + dir + side)) then (cube + dir + side, dir.Opposite) // Turn back
                else if (cubeSet.Contains (cube + dir) |> not) then (cube, dir) // Go straight
                else (cube + dir, side) // Turn forward

            Direction3d.AllDirections
            |> Seq.filter (fun x -> x <> side && x <> side.Opposite)
            |> Seq.map tryMove
            |> Seq.toList

        [ state ] // Flood fill
        |> breadthFirstSearch getChildren
        |> Seq.length