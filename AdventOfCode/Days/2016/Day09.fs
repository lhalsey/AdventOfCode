namespace AdventOfCode.Days.Y2016

open AdventOfCode.Shared.Utility

/// Day 9: Explosives in Cyberspace
/// https://adventofcode.com/2016/day/9
/// Wandering around a secure area, you come across a datalink port to a new part of the network.
module Day09 =

    type Mode = V1 | V2

    let parseInput() = getFile (2016, 9) |> readAllText

    // E.g. "A(2x2)BCD(2x2)EFG"
    let rec getDecompressedLength mode (s: string) : int64 =
        match s.IndexOf '(' with
        | -1 -> int64 s.Length // No markers, just return text length
        | startIndex ->
            let endIndex = s.IndexOf ')'
            let (length, reps) = s.[startIndex + 1 .. endIndex - 1] |> splitIntoPairAs "x" int
            let repeated = s.[endIndex + 1 .. endIndex + length]

            let repeatedLen =
                match mode with
                | V1 -> int64 reps * int64 repeated.Length
                | V2 -> int64 reps * (getDecompressedLength mode repeated)

            let tail = s.[endIndex + 1 + length ..]
            int64 startIndex + repeatedLen + (getDecompressedLength mode tail)


    // What is the decompressed length of the file (your puzzle input)? Don't count whitespace.
    let Part1() = parseInput() |> getDecompressedLength V1

    // In version two, the only difference is that markers within decompressed data are decompressed.
    // What is the decompressed length of the file using this improved format?
    let Part2() = parseInput() |> getDecompressedLength V2