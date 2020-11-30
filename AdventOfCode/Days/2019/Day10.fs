namespace AdventOfCode.Days.Y2019

open AdventOfCode.Shared
open AdventOfCode.Shared.Utility
open FSharp.Collections.ParallelSeq

/// Day 10: Monitoring Station
/// https://adventofcode.com/2019/day/10
/// You fly into the asteroid belt and reach the Ceres monitoring station.
module Day10 =

    // E.g. "#.#....#.#......#.....#......####."
    let getInput() =
        getFile (2019, 10)
        |> parseGrid id
        |> Seq.filter (fun (_, c) -> c = '#')
        |> Seq.map fst
        |> Seq.toList

    let getDetectableAsteroidCount (object: Point2d, targets) =
        targets                              
        |> List.distinctBy object.AngleTo // Calculate angle between object and targets
        |> List.length                    // The first object obscures the rest
 
    let getDistIndexes (object: Point2d) (angle, group) =
        group
        |> List.sortBy object.ManhattanDistanceTo // Don't need Euclidian as all have same angle
        |> List.mapi (fun index asteroid -> (index, angle), asteroid)
        
    // Sort by index of distance from asteroid per angle group
    // E.g. (0, 0º), (0, 5º), (1, 0º), ...
    let getAsteroidsByRotation (object: Point2d) targets =
        targets 
        |> List.filter ((<>) object)
        |> List.groupBy object.AngleTo
        |> List.collect (getDistIndexes object)
        |> List.sortBy fst
  
    // The best location for a new monitoring station on this map is the location that
    // can detect more asteroids than any other location.
    // Find the best location for a new monitoring station.
    // How many other asteroids can be detected from that location?
    let Part1() =
        let asteroids = getInput()
        
        partitionSingle asteroids
        |> PSeq.map getDetectableAsteroidCount
        |> PSeq.max

    // The Elves are placing bets on which will be the 200th asteroid to be vaporized.
    // Win the bet by determining which asteroid that will be;
    // what do you get if you multiply its X coordinate by 100 and then add its Y coordinate?
    // (For example, 8,2 becomes 802.)
    let Part2() =
        let Target = 200 - 1 // Zero-based
        let asteroids = getInput()

        let (station, _) =
            partitionSingle asteroids
            |> PSeq.maxBy getDetectableAsteroidCount

        getAsteroidsByRotation station asteroids
        |> Seq.item Target
        |> fun (_, asteroid) -> asteroid.X * 100 + asteroid.Y