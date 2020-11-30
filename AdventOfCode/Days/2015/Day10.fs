namespace AdventOfCode.Days.Y2015

open System
open AdventOfCode.Shared.Utility

/// Day 10: Elves Look, Elves Say
/// https://adventofcode.com/2015/day/10
/// Today, the Elves are playing a game called look-and-say.
module Day10 =

    let Input = "1321131112"

    let getNextSequence (x: int seq) =
        seq {
            for kvp in runLengthEncode x do
                yield kvp.Value
                yield kvp.Key
            }

    let getLengthAfter iterations =
        // It's faster to keep values as int sequence rather than string
        // assuming we don't have a double digit sequence length
        let input = Input |> Seq.map (Char.GetNumericValue >> int)

        (input, seq { 1 .. iterations })
        ||> Seq.fold (fun acc _ -> getNextSequence acc)
        |> Seq.length


    // Today, the Elves are playing a game called look-and-say.
    // Starting with the digits in your puzzle input, apply this process 40 times.
    // What is the length of the result?
    let Part1() = getLengthAfter 40

    // Now, starting again with the digits in your puzzle input, apply this process 50 times.
    // What is the length of the new result?
    let Part2() = getLengthAfter 50