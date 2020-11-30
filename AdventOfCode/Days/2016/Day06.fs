namespace AdventOfCode.Days.Y2016

open AdventOfCode.Shared.Utility

/// Day 6: Signals and Noise
/// https://adventofcode.com/2016/day/6
/// Something is jamming your communications with Santa.
module Day06 =
    
    let [<Literal>] Input = "ojvtpuvg"

    let parseInput() = getFile (2016, 6) |> readLines

    let decode f =
        parseInput()
        |> Seq.transpose
        |> Seq.map f
        |> charsToStr

    let mostCommon = Seq.countBy id >> Seq.maxBy snd >> fst

    let leastCommon = Seq.countBy id >> Seq.minBy snd >> fst

    // All you need to do is figure out which character is most frequent for each position.
    // Given the recording in your puzzle input, what is the error-corrected version of the
    // message being sent?
    let Part1() = decode mostCommon

    // Choose the least common letter to reconstruct the original message.
    // Given the recording in your puzzle input and this new decoding methodology, what is the
    // original message that Santa is trying to send?
    let Part2() = decode leastCommon