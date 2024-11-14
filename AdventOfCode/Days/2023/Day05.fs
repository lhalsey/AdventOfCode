namespace AdventOfCode.Days.Y2023

open AdventOfCode.Shared.Utility
open FSharp.Collections.ParallelSeq

/// Day 5: If You Give A Seed A Fertilizer
/// https://adventofcode.com/2023/day/5
/// You take the boat and find the gardener right where you were told he would be: managing a giant
/// "garden" that looks more to you like a farm.
module Day05 =

    type Mapping = { SourceStart: int64; DestinationStart: int64; Length: int64 }

    type EntityMap = { Source: string; Destination: string; Mappings: Mapping list }

    type Problem = { Seeds: int64 array; EntityMaps: EntityMap list }

    let getNums = split ' ' >> Array.map int64

    let buildProblem (problem: Problem) (line: string) =
        match line with
        | Regex "seeds: (.+)" [seeds] -> { problem with Seeds = getNums seeds }
        | Regex "(\w+)-to-(\w+) map:" [ src; dst ] ->
            let entityMap = { Source = src; Destination = dst; Mappings = [] }
            { problem with EntityMaps = entityMap::problem.EntityMaps }
        | Regex "(\d+) (\d+) (\d+)" [Int64 dstStart; Int64 srcStart; Int64 length] ->
            let mapping = { SourceStart = srcStart; DestinationStart = dstStart; Length = length }
            match problem.EntityMaps with
            | h::t -> { problem with EntityMaps = ({ h with Mappings = mapping::h.Mappings })::t }
            | _ -> failwith "Missing entity map"
        | _ -> problem // Whitespace

    let parseInput() = getFile (2023, 5) |> readLines

    let getProblem() =
        let input = parseInput()

        let problem =
            ({ Seeds = [||]; EntityMaps = [] }, input)
            ||> Seq.fold buildProblem

        let entityMaps =
            problem.EntityMaps |> List.map (fun x -> { x with Mappings = x.Mappings |> List.sortBy _.SourceStart })

        { problem with EntityMaps = entityMaps }

    let getLocation (problem: Problem) (seed: int64) =
        let rec getLocationR mapKey value =
            match mapKey with
            | "location" -> value
            | _ ->
                let entityMap = problem.EntityMaps |> List.find (fun x -> x.Source = mapKey)

                let mapping =
                    entityMap.Mappings
                    |> List.tryFind (fun x -> value >= x.SourceStart && value < x.SourceStart + x.Length)

                let newValue =
                    match mapping with
                    | Some m -> m.DestinationStart + (value - m.SourceStart)
                    | _ -> value

                getLocationR entityMap.Destination newValue

        getLocationR "seed" seed
        

    // What is the lowest location number that corresponds to any of the initial seed numbers?
    let Part1() =
        let problem = getProblem()
        
        problem.Seeds
        |> PSeq.map (getLocation problem)
        |> PSeq.min

    // Consider all of the initial seed numbers listed in the ranges on the first line of the almanac.
    // What is the lowest location number that corresponds to any of the initial seed numbers?
    let Part2() =
        let problem = getProblem()

        let seeds =
            problem.Seeds
            |> Seq.chunkBySize 2 
            |> Seq.collect (fun x -> [| x[0] .. x[0] + x[1] - 1L |])

        let seedCount = Seq.length seeds // 2,104,769,314
        
        seeds
        |> Seq.take 100_000_000
        |> PSeq.map (getLocation problem)
        |> PSeq.min