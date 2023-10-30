namespace AdventOfCode.Days.Y2017

open System
open AdventOfCode.Shared.Utility

/// Day 4: High-Entropy Passphrases
/// https://adventofcode.com/2017/day/4
/// A new system policy has been put in place that requires all accounts to use a passphrase instead of simply a password.
module Day04 =

    let parseInput() = getFile (2017, 4) |> readLines

    let sortWord (s: string) = s |> Seq.sort |> Seq.toArray |> String

    let isValidBy (f: string -> string) (s: string)  = s |> split ' ' |> areAllDistinctBy f

    // The system's full passphrase list is available as your puzzle input. How many passphrases are valid?
    let Part1() = parseInput() |> countIf (isValidBy id)

    // Under this new system policy, how many passphrases are valid?
    let Part2() = parseInput() |> countIf (isValidBy sortWord)