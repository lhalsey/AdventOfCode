namespace AdventOfCode.Days.Y2019

open System
open MathNet.Numerics.LinearAlgebra

open AdventOfCode.Shared.Utility

/// Day 16: Flawed Frequency Transmission
/// https://adventofcode.com/2019/day/16
/// You're 3/4ths of the way through the gas giants.
module Day16 =
   
    let [<Literal>] OffsetLength = 7
    let [<Literal>] NumDigits = 8
    let [<Literal>] NumPhases = 100
    let [<Literal>] NumRepeats = 10_000
  
    let getInput() =
        getFile (2019, 16) 
        |> readAllText
        |> Seq.map (Char.GetNumericValue >> int)
        |> Seq.toArray

    let getNumber numDigits (nums: int seq) =
        nums
        |> Seq.take numDigits
        |> Seq.reduce (fun acc x -> 10 * acc + x)

    let getMatrix size = 
        
        let phases = [ 0.0; 1.0; 0.0; -1.0 ]

        // The base pattern is 0, 1, 0, -1.
        // Then, repeat each value in the pattern a number of times equal to the position
        // in the output list being considered
        // E.g. 3rd row = 0, 0, 0, 1, 1, 1, 0, 0, 0, -1, -1, -1
        let phaseValue row col = phases.[((col + 1) / (row + 1)) % phases.Length]

        SparseMatrix.init size size phaseValue // Use sparse as large regions after first few rows


    // After 100 phases of FFT, what are the first eight digits in the final output list?
    let Part1() =
        let input = getInput() |> Array.map float 

        let matrix = getMatrix input.Length

        let nextPhase (v: Vector<float>) =
            let v = matrix * v // Sum each value multiplied by corresponding value in phase list
            v.PointwiseAbs().Modulus 10.0 // Take last digit, e.g. -17 -> 7

        input
        |> DenseVector.ofArray
        |> Seq.unfold (fun state -> Some(state, nextPhase state))
        |> Seq.item NumPhases
        |> Seq.map int
        |> getNumber NumDigits

    // After repeating your input signal 10,000 times and running 100 phases of FFT,
    // what is the eight-digit message embedded in the final output list?
    let Part2() =
        let input = getInput()
        let offset = input |> getNumber OffsetLength

        // Only take last digits needed
        let nums =
            input
            |> Array.replicate NumRepeats
            |> Array.concat
            |> Array.skip offset

        // 2nd half of phase list forms a triangle of 1's so we can just calculate the cumulative sum
        let nextPhase (num: int[]) =
            (num, 0)
            ||> Array.scanBack (fun x acc -> (acc + x) % 10)
            |> Array.take num.Length // Skip 0 seed

        (nums, seq { 1..NumPhases })
        ||> Seq.fold (fun acc _ -> nextPhase acc)
        |> getNumber NumDigits