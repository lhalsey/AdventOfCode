namespace AdventOfCode.Days.Y2021

open AdventOfCode.Shared.Utility

/// Day 10: Syntax Scoring
/// https://adventofcode.com/2021/day/10
/// You ask the submarine to determine the best route out of the deep-sea cave ...
module Day10 =

    let [<Literal>] ScoreMultiplier = 5L

    type LineType = Valid | Corrupt of char | Incomplete of char list

    let (openElements, closeElements) = "([{<", ")]}>"

    let elementPairs = (openElements, closeElements) ||> Seq.zip |> Map

    let (|Open|_ |) (x: char) = if openElements.Contains x then Some x else None

    let (|Close|_ |) (x: char) = if closeElements.Contains x then Some x else None

    let parse (input: string) =
        let rec parseR (chars: char list) (stack: char list) =
            match chars, stack with
            | [], [] -> Valid
            | (Open o)::xs, _ -> parseR xs (elementPairs.[o]::stack) // Open element; push close element onto stack
            | (Close c)::xs, y::ys when c = y -> parseR xs ys // Matching close element; pop off stack
            | (Close c)::_, _ -> Corrupt c // Close element does not match last open element
            | [], _ -> Incomplete stack // Unmatched close elements remain on stack
            | _ -> failwith "Invalid state"

        parseR (Seq.toList input) []

    let parseInput() = getFile (2021, 10) |> readLinesAs parse

    let getCorruptScore =
        function ')' -> 3 | ']' -> 57 | '}' -> 1197 | '>' -> 25137 | x -> failwithf "Invalid char: %c" x

    let getIncompleteScore =
        function ')' -> 1L | ']' -> 2L | '}' -> 3L | '>' -> 4L | x -> failwithf "Invalid char: %c" x

    let getTotalIncompleteScore (chars: char list) =
        chars |> List.fold (fun score c -> score * ScoreMultiplier + getIncompleteScore c) 0L

    // Find the first illegal character in each corrupted line of the navigation subsystem.
    // What is the total syntax error score for those errors?
    let Part1() =
        parseInput()
        |> Seq.sumBy (function Corrupt x -> getCorruptScore x | _ -> 0)

    // Find the completion string for each incomplete line, score the completion strings, and sort the scores.
    // What is the middle score?
    let Part2() =
        parseInput()
        |> Seq.choose (function Incomplete stack -> getTotalIncompleteScore stack |> Some | _ -> None)
        |> Seq.map double
        |> median
        |> int64