namespace AdventOfCode.Days.Y2015

open System.Text.RegularExpressions
open AdventOfCode.Shared.Utility

/// Day 8: Matchsticks
/// https://adventofcode.com/2015/day/8
/// Space on the sleigh is limited this year, and so Santa will be bringing his list as a digital copy.
module Day08 =

    let [<Literal>] NumOuterQuotes = 2

    // E.g. "sjdivfriyaaqa\xd2v\"k\"mpcu\"yyu\"en" ...
    let parseInput() = getFile (2015, 8) |> readLines

    let getDiffUnescaped (s: string) =
        s.Length - Regex.Unescape(s).Length + NumOuterQuotes

    // Regex.Escape doesn't do what we want so just count the \ and " quote characters!
    let getDiffEscaped (s: string) =
        let length = s |> countIf (fun x -> x = '\\' || x = '\"')
            
        length + NumOuterQuotes

    // Disregarding the whitespace in the file, what is the number of characters of code
    // for string literals minus the number of characters in memory for the values of the
    // strings in total for the entire file?
    let Part1() = parseInput() |> Seq.sumBy getDiffUnescaped

    // Your task is to find the total number of characters to represent the newly encoded
    // strings minus the number of characters of code in each original string literal.
    let Part2() = parseInput() |> Seq.sumBy getDiffEscaped