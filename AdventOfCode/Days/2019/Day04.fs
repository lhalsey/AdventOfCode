namespace AdventOfCode.Days.Y2019

open FSharp.Collections.ParallelSeq

/// Day 4: Secure Container
/// https://adventofcode.com/2019/day/4
/// You arrive at the Venus fuel depot only to discover it's protected by a password.
module Day04 =

    // E.g. 367479 -> 367777
    let getNextNonDecreasing (num: string) =

        let increaseIndex = 
            num
            |> Seq.pairwise
            |> Seq.tryFindIndex (fun (x, y) -> x > y)

        match increaseIndex with
        | Some i -> num.[0..i] + new string(num.[i], num.Length - i - 1)
        | None -> num // Valid so return as is

    let rec getNonDecreasingSeq n =
        seq {
            let next = getNextNonDecreasing (string n) |> int
            yield next
            yield! getNonDecreasingSeq (next + 1)
        }

    // We can get a more than 10x speed increase by skipping over ranges of numbers
    // which are not non-decreasing
    let getValid pred =
        let (Start, End) = (367_479, 893_698) // Puzzle input

        getNonDecreasingSeq Start
        |> PSeq.takeWhile (fun x -> x <= End)
        |> PSeq.filter (string >> pred)
        |> PSeq.length

    // Because we have filtered numbers to be non-decreasing we know that any repeated
    // numbers will be adjacent
    let hasPair = Seq.countBy id >> Seq.exists (fun (_, c) -> c >= 2)

    let hasStrictPair = Seq.countBy id >> Seq.exists (fun (_, c) -> c = 2)

    // How many different passwords within the range given in your puzzle input meet these criteria?
    // It is a six-digit number.
    // The value is within the range given in your puzzle input.
    // Two adjacent digits are the same (like 22 in 122345).
    // Going from left to right, the digits never decrease;
    // they only ever increase or stay the same (like 111123 or 135679).
    let Part1() = getValid hasPair
        

    // An Elf just remembered one more important detail:
    // The two adjacent matching digits are not part of a larger group of matching digits.
    // How many different passwords within the range given in your puzzle input meet all of the criteria?
    let Part2() = getValid hasStrictPair