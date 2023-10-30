namespace AdventOfCode.Days.Y2017

open AdventOfCode.Shared
open System

/// Day 3: Spiral Memory
/// https://adventofcode.com/2017/day/3
/// You come across an experimental new kind of memory stored on an infinite two-dimensional grid.
module Day03 =

    let Target = 277_678

    let getSteps (n: int) =

        // Determine which layer the square is in
        let c = n |> Math.Sqrt |> Math.Ceiling |> int
        let layer = c / 2
        let squaresOnOneSide = layer * 2 + 1

        // Max value in that layer is in the bottom right and is a square number
        let maxInLayer = squaresOnOneSide * squaresOnOneSide

        // Determine which side of the square we are on
        let distToMax = maxInLayer - n;
        let side = distToMax / (squaresOnOneSide - 1);

        // The square in the middle of this side can go straight to the port (in layer moves)
        let sideMid = maxInLayer - (side * (squaresOnOneSide - 1)) - layer;

        // How far are we from the mid and subsequently the port
        abs (n - sideMid) + layer;

    let buildSpiral() =
        let rec buildSpiralR (location: Point2d) (dir: Direction2d) (sumMap: Map<Point2d, int>) =
            seq {
                let location = location + dir
                let sum = location.GetAllAdjacent() |> Seq.sumBy (sumMap.TryFind >> Option.defaultValue 0)
                yield sum

                let left = Direction2d.TurnLeft dir
                let dir = if sumMap.ContainsKey (location + left) then dir else left
                yield! buildSpiralR location dir (sumMap.Add (location, sum))
            }

        buildSpiralR Point2d.Origin Direction2d.East (Map [ Point2d.Origin, 1 ])

    // How many steps are required to carry the data from the square identified in your puzzle input
    // all the way to the access port?
    let Part1() = getSteps Target

    // What is the first value written that is larger than your puzzle input?
    let Part2() = buildSpiral() |> Seq.find (fun x -> x > Target)