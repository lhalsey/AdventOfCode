namespace AdventOfCode.Days.Y2019

open System
open AdventOfCode.Shared.Utility
open AdventOfCode.Days.Y2019.Shared.IntCodeInterpreter
open FSharp.Collections.ParallelSeq

/// Day 19: Tractor Beam
/// https://adventofcode.com/2019/day/19
/// Unsure of the state of Santa's ship, you borrowed the tractor beam technology from Triton.
module Day19 =

    type Feedback = Stationary | BeingPulled

    let getInterpreter() = getFile (2019, 19) |> Interpreter.Create

    let getValueAt =
        let state = getInterpreter() |> run

        let getValue = function
            | 0L::[] -> Stationary
            | 1L::[] -> BeingPulled
            | x -> failwithf "Expected single output, but got %A" x

        fun (x, y) -> // Return function so we only create interpreter once
            state
            |> provideInput (int64 x)
            |> provideInput (int64 y)
            |> readOutput
            |> fst
            |> getValue

    // Find first X, Y coordinate that is pulled by beam starting from X coordinate for previous row.
    let getFirstPulledCoordinate =

        let getCol startColumn row =
            seq { startColumn .. Int32.MaxValue }
            |> Seq.find (fun column -> getValueAt (column, row) = BeingPulled)

        // Determine gradient so we can (cautiously) estimate where beam starts
        let sampleSize = 100
        let sampleColumn = getCol 0 sampleSize - 1
        let getEstimatedStartColumn row = (row * sampleColumn) / sampleSize

        fun row ->
            let startColumn = getEstimatedStartColumn row
            let col = getCol startColumn row

            (col, row)

    let canFitSquareOfSize size (column, row) =

        let squareTop = row - (size - 1)
        let squareRight = column + (size - 1)

        match getValueAt (squareRight, squareTop) with
        | BeingPulled -> Some (column * 10_000 + squareTop)
        | Stationary -> None

    // How many points are affected by the tractor beam in the 50x50 area closest to the emitter?
    let Part1() =
        let range = seq { 0..49 }

        Seq.allPairs range range
        |> PSeq.filter (fun (x, y) -> getValueAt (x, y) = BeingPulled)
        |> PSeq.length

    // Find the 100x100 square closest to the emitter that fits entirely within the tractor beam;
    // within that square, find the point closest to the emitter.
    // What value do you get if you take that point's X coordinate, multiply it by 10000, then add
    // the point's Y coordinate?
    let Part2() = // TODO: Consider using binary chop to further improve performance
        let SquareSize = 100

        seq { SquareSize .. Int32.MaxValue }
        |> PSeq.pick (getFirstPulledCoordinate >> canFitSquareOfSize SquareSize)
