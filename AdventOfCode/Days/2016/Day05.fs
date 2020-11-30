namespace AdventOfCode.Days.Y2016

open AdventOfCode.Shared.Utility

/// Day 5: How About a Nice Game of Chess?
/// https://adventofcode.com/2016/day/5
/// You are faced with a security door designed by Easter Bunny engineers that seem to have acquired most of their security knowledge by watching hackingmovies.
module Day05 =

    let parseInput() = getFile (2016, 5) |> readLines

    // The sixth character in the valid hash is the next character of the password.
    // Given the actual Door ID, what is the password?
    let Part1() =
        0

    // Instead of simply filling in the password from left to right, the hash now also
    // indicates the position within the password to fill.
    // Given the actual Door ID and this new method, what is the password?
    let Part2() =
        0