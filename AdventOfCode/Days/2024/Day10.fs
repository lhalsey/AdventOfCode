namespace AdventOfCode.Days.Y2024

open AdventOfCode.Shared.Utility
open System
open AdventOfCode.Shared

/// Day 10: Hoof It
/// https://adventofcode.com/2024/day/10
/// You all arrive at a Lava Production Facility on a floating island in the sky.
module Day10 =

    let parseInput() = getFile (2024, 10) |> parseGrid (Char.GetNumericValue >> int) |> readOnlyDict

    let getScore aggregateF =
        let heightMap = parseInput()

        let trailHeads = heightMap |> Seq.filter (fun x -> x.Value = 0)

        let getNumTrails (point: Point2d) = 
            let rec getNumTrailsR (pt: Point2d) (height: int) =
                seq {
                    match tryFind pt heightMap with
                    | Some 9 when height = 8 -> yield pt // Reached trail end
                    | Some nh when nh = height + 1 -> // Gradual slope, keep going
                        yield! pt.GetAdjacent() |> Seq.collect (fun pt -> getNumTrailsR pt nh)
                    | _ -> () // No viable steps
                }

            getNumTrailsR point -1 |> aggregateF

        trailHeads |> Seq.sumBy (fun x -> getNumTrails x.Key) 

    // What is the sum of the scores of all trailheads on your topographic map?
    let Part1() = getScore countDistinct
        
    // What is the sum of the ratings of all trailheads?
    let Part2() = getScore Seq.length