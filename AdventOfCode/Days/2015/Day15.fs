namespace AdventOfCode.Days.Y2015

open MathNet.Numerics.LinearAlgebra
open AdventOfCode.Shared.Utility
open FSharp.Collections.ParallelSeq

/// Day 15: Science for Hungry People
/// https://adventofcode.com/2015/day/15
/// Today, you set out on the task of perfecting your milk-dunking cookie recipe.
module Day15 =

    // E.g. "Frosting: capacity 4, durability -2, flavor 0, texture 0, calories 5"
    // Just extract values so we can put into matrix (reverse as we want calories first)
    let parse = splitAny " ," >> Seq.choose tryParseAsFloat >> Seq.rev

    let parseInput() = getFile (2015, 15) |> readLinesAs parse |> DenseMatrix.ofRowSeq

     // The total score of a cookie can be found by adding up each of the properties
     // (negative totals become 0) and then multiplying together everything except calories.
    let getScoreAndCalories (ingredients: Matrix<_>) (amounts: Vector<_>) =
        let scores = (amounts * ingredients).PointwiseMaximum(0.0) |> Seq.toList

        let calories = scores |> List.head
        let score = scores |> List.tail |> List.reduce (*)

        (int score, int calories)

    let getMaxScore (targetCalories: int option) =
        let NumTeaspoons = 100
        let ingredients = parseInput()

        let caloriesFilter =
            match targetCalories with
            | None -> fun _ -> true
            | Some target -> fun cal -> cal = target

        // Try all combinations of Frosting, Candy, Butterscotch & Sugar
        let combos =
            seq { for a in 0 .. NumTeaspoons do 
                  for b in 0 .. NumTeaspoons - a do
                  for c in 0 .. NumTeaspoons - (a + b) ->
                    let d = NumTeaspoons - (a + b + c)
                    [ a; b; c; d ] |> List.map float |> DenseVector.ofList
                }

        combos
        |> PSeq.map (getScoreAndCalories ingredients)
        |> PSeq.filter (fun (_, calories) -> caloriesFilter calories)
        |> PSeq.map (fun (score, _) -> score)
        |> PSeq.max


    // Given the ingredients in your kitchen and their properties, what is the total score
    // of the highest-scoring cookie you can make?
    let Part1() = getMaxScore None

    // Given the ingredients in your kitchen and their properties, what is the total score
    // of the highest-scoring cookie you can make with a calorie total of 500?
    let Part2() = getMaxScore (Some 500)