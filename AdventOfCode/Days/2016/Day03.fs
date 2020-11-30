namespace AdventOfCode.Days.Y2016

open AdventOfCode.Shared.Utility
open System

/// Day 3: Squares With Three Sides
/// https://adventofcode.com/2016/day/3
/// Now that you can think clearly, you move deeper into the labyrinth of hallways and office furniture that makes up this part of Easter Bunny HQ.
module Day03 =

    // E.g. "  785  516  744"
    let parse (s: string) = s.Split(" ", StringSplitOptions.RemoveEmptyEntries) |> Array.map int
        
    let parseInput() = getFile (2016, 3) |> readLinesAs parse

    let isTriangle (sides: int seq) =
        let sorted = sides |> Seq.sort |> Seq.toList
        sorted.[0] + sorted.[1] > sorted.[2]


    // In your puzzle input, how many of the listed triangles are possible?
    let Part1() = parseInput() |> countIf isTriangle

    // In your puzzle input, and instead reading by columns, how many of the
    // listed triangles are possible?
    let Part2() =
        parseInput()
        |> Seq.transpose
        |> Seq.concat
        |> Seq.chunkBySize 3
        |> countIf isTriangle