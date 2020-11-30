namespace AdventOfCode.Days.Y2015

open System.Collections.Generic
open AdventOfCode.Shared.Utility
open AdventOfCode.Shared
open FSharp.Collections.ParallelSeq

/// Day 18: Like a GIF For Your Yard
/// https://adventofcode.com/2015/day/18
/// After the million lights incident, the fire code has gotten stricter: now, at most ten thousand lights are allowed.
module Day18 =

    type Mode = WorkingLights | FaultyLights

    let [<Literal>] Off = 0
    let [<Literal>] On = 1
    let [<Literal>] MaxX = 99
    let [<Literal>] MaxY = 99
    let [<Literal>] NumSteps = 100

    let parseInput() = getFile (2015, 18) |> parseGrid (function '#' -> On | _ -> Off)

    // TODO: Consider using one dimensional array for performance improvement
    let getSequence mode (grid: IReadOnlyDictionary<Point2d, int>) =

        // A light which is on stays on when 2 or 3 neighbors are on, and turns off otherwise
        // A light which is off turns on if exactly 3 neighbors are on, and stays off otherwise
        // And with faulty lights then the corner lights are always on
        let getValue (grid: IReadOnlyDictionary<Point2d, int>) (kvp: KeyValuePair<Point2d, int>) =
            
            let adjacentLit = 
                kvp.Key.GetAllAdjacent()
                |> Seq.sumBy (fun p -> grid |> tryFind p |> Option.defaultValue Off)

            let newValue =
                match mode, kvp.Key, kvp.Value, adjacentLit with
                | FaultyLights, { X = 0; Y = 0 }, _, _ 
                | FaultyLights, { X = 0; Y = MaxY }, _, _ 
                | FaultyLights, { X = MaxX; Y = 0 }, _, _ 
                | FaultyLights, { X = MaxX; Y = MaxY }, _, _ 
                | _, _, On, 2
                | _, _, _, 3 -> On
                | _ -> Off

            (kvp.Key, newValue)

        let getNext (grid: IReadOnlyDictionary<Point2d, int>) =
            grid
            |> PSeq.map (getValue grid)
            |> readOnlyDict

        grid |> Seq.unfold (fun x -> Some (x, getNext x))

    let getLights mode =
        parseInput()
        |> readOnlyDict
        |> getSequence mode
        |> Seq.item NumSteps
        |> Seq.sumBy (fun x -> x.Value)

    // In your grid of 100x100 lights, given your initial configuration,
    // how many lights are on after 100 steps?
    let Part1() = getLights WorkingLights
        

    // In your grid of 100x100 lights, given your initial configuration,
    // but with the four corners always in the on state, 
    // how many lights are on after 100 steps?
    let Part2() = getLights FaultyLights