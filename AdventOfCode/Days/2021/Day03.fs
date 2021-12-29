namespace AdventOfCode.Days.Y2021

open AdventOfCode.Shared.Utility
open System

/// Day 3: Binary Diagnostic
/// https://adventofcode.com/2021/day/3
/// The submarine has been making some odd creaking noises, so you ask it to produce a diagnostic report just in case.
module Day03 =

    let parseInput() = getFile (2021, 3) |> readLines

    let mostCommon = Seq.countBy id >> Seq.maxBy (fun (x, c) -> (c, x)) >> fst // Largest value breaks tie

    let leastCommon = Seq.countBy id >> Seq.minBy (fun (x, c) -> (c, x)) >> fst // Smallest value breaks tie

    let binaryToDecimal x = Convert.ToInt32(x, 2)

    let getValue pred = 
        parseInput()
        |> Seq.transpose
        |> Seq.map pred
        |> charsToStr

    let getRating pred (rows: string list) =
        let rec getRatingR (rows: string list) pred index =
            match rows with
            | [x] -> x
            | xs -> let target = xs |> List.map (fun x -> x.[index]) |> pred
                    let matches = xs |> List.filter (fun x -> x.[index] = target)
                    getRatingR matches pred (index + 1)

        getRatingR rows pred 0

    // Use the binary numbers in your diagnostic report to calculate the gamma rate and epsilon rate,
    // then multiply them together. What is the power consumption of the submarine?
    let Part1() =
        let gamma = getValue mostCommon
        let epsilon = getValue leastCommon

        (binaryToDecimal gamma) * (binaryToDecimal epsilon)

    // Use the binary numbers in your diagnostic report to calculate the oxygen generator rating and CO2
    // scrubber rating, then multiply them together. What is the life support rating of the submarine? 
    let Part2() =
        let input = parseInput() |> Seq.toList
        let ogr = input |> getRating mostCommon
        let csr = input |> getRating leastCommon

        (binaryToDecimal ogr) * (binaryToDecimal csr)