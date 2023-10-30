namespace AdventOfCode.Days.Y2022

open AdventOfCode.Shared.Utility

/// Day 10: Cathode-Ray Tube
/// https://adventofcode.com/2022/day/10
/// You avoid the ropes, plunge into the river, and swim to shore.
module Day10 =

    let [<Literal>] CRTWidth = 40

    type Instruction = NoOp | AddX of int

    let parse = function
        | Regex "noop" [] -> NoOp
        | Regex "addx ([+-]*\d+)" [Int value] -> AddX value
        | x -> failwithf "Invalid input %s" x

    let getIncrementPerCycle = function
        | NoOp -> [ 0 ]
        | AddX value -> [ 0; value ]

    let parseInput() = getFile (2022, 10) |> readLinesAs parse

    let getCycleValues() =
        let increments = parseInput() |> Seq.collect getIncrementPerCycle

        (1, increments)
        ||> Seq.scan (fun acc v -> acc + v)
        |> Seq.indexed

    // Find the signal strength during the 20th, 60th, 100th, 140th, 180th, and 220th cycles.
    // What is the sum of these six signal strengths?
    let Part1() =
        getCycleValues()
        |> Seq.skip 19
        |> takeEvery CRTWidth
        |> Seq.sumBy (fun (cycle, value) -> (cycle + 1) * value)

    // Render the image given by your program. What eight capital letters appear on your CRT?
    let Part2() =
        getCycleValues()
        |> Seq.map (fun (cycle, value) -> if abs ((cycle % CRTWidth) - value) <= 1 then '#' else '.')
        |> Seq.chunkBySize CRTWidth
        |> Seq.filter (fun x -> x.Length = CRTWidth)
        |> Seq.map charsToStr
        |> String.concat "\n"