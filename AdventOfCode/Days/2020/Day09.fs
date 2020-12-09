namespace AdventOfCode.Days.Y2020

open AdventOfCode.Shared.Utility

/// Day 9: Encoding Error
/// https://adventofcode.com/2020/day/9
/// With your neighbor happily enjoying their video game, you turn your attention to an open data
// port on the little screen in the seat in front of you.
module Day09 =

    let [<Literal>] Preamble = 25

    let parseInput() = getFile (2020, 9) |> readLinesAs int64 |> Seq.toArray

    let tryFindTwoNumbersSummingTo target (nums: int64[]) = 
        let seen = nums |> toReadOnlyHashSet
        nums |> Array.tryFind (fun x -> seen.Contains (target - x))

    let findContiguousSum (target: int64) (nums: int64[]) =
        let rec findContiguousSumR i1 i2 = function
            | s when s = target -> nums.[i1 .. i2 - 1]
            | s when s > target -> findContiguousSumR (i1 + 1) i2 (s - nums.[i1]) // Reduce subsequence from start
            | s -> findContiguousSumR i1 (i2 + 1) (s + nums.[i2]) // Extend subsequence from end

        findContiguousSumR 0 0 0L

    // The first step of attacking the weakness in the XMAS data is to find the first number in the
    // list (after the preamble) which is not the sum of two of the 25 numbers before it.
    // What is the first number that does not have this property?
    let Part1() =
        parseInput() 
        |> Array.windowed (Preamble + 1)
        |> Array.find (fun x -> tryFindTwoNumbersSummingTo x.[Preamble] x.[0..Preamble-1] |> Option.isNone)
        |> Array.last

    // You must find a contiguous set of at least two numbers in your list which sum to the invalid number
    // from step 1. To find the encryption weakness, add together the smallest and largest number in this
    // contiguous range.
    let Part2() =
        let target = 756_008_079L
        let input = parseInput()

        let result = findContiguousSum target input
        Array.min result + Array.max result
        