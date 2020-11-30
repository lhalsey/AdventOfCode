namespace AdventOfCode.Days.Y2015

open AdventOfCode.Shared.Utility

/// Day 14: Reindeer Olympics
/// https://adventofcode.com/2015/day/14
/// This year is the Reindeer Olympics!
module Day14 =

    // TODO: Optimise algo based on fact that number is coprime with cycle length of each reindeer
    let TargetTime = 2503 

    type Reindeer = { Name: string; Speed: int; FlyingDuration: int; RestDuration: int }

    // E.g. "Vixen can fly 8 km/s for 8 seconds, but then must rest for 53 seconds."
    let parse (s: string) =
        let tokens = s |> split ' '

        { Name = tokens.[0]
          Speed = int tokens.[3]
          FlyingDuration = int tokens.[6]
          RestDuration = int tokens.[13] }

    let parseInput() = getFile (2015, 14) |> readLinesAs parse

    // Infinite sequence of speeds per second
    let rec getSpeeds (reindeer: Reindeer) =
        seq { yield! Seq.replicate reindeer.FlyingDuration reindeer.Speed
              yield! Seq.replicate reindeer.RestDuration 0
              yield! getSpeeds reindeer }

    let getDistances = getSpeeds >> Seq.scan (+) 0

    // Given the descriptions of each reindeer (in your puzzle input), after exactly 2503 seconds,
    // what distance has the winning reindeer traveled?
    let Part1() =
        let reindeers = parseInput()

        reindeers
        |> Seq.map (getDistances >> Seq.item TargetTime)
        |> Seq.max

    // at the end of each second, he awards one point to the reindeer currently in the lead.
    // (If there are multiple reindeer tied for the lead, they each get one point.)
    // Again given the descriptions of each reindeer (in your puzzle input), after exactly 2503 seconds,
    // how many points does the winning reindeer have?
    let Part2() =
        let reindeers = parseInput()

        // Get indexes of each reindeer which is leading at this point in time
        let getLeaders (distances: int seq) =
            let leadingDistance = distances |> Seq.max

            distances
            |> Seq.indexed
            |> Seq.filter (fun (_, x) -> x = leadingDistance)
            |> Seq.map fst
        
        // Derive distances for each reindeer for each second and transpose to get a 
        // time slice which we can use to determine which reindeers are leading at that second
        let scores =
            reindeers
            |> Seq.map (getDistances >> Seq.skip 1 >> Seq.take TargetTime)
            |> Seq.transpose
            |> Seq.collect getLeaders
            |> Seq.countBy id

        scores |> Seq.map snd |> Seq.max