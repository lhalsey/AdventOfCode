namespace AdventOfCode.Days.Y2015

open AdventOfCode.Shared.Utility

/// Day 1: Not Quite Lisp
/// https://adventofcode.com/2015/day/1
/// Santa was hoping for a white Christmas, but his weather machine's snow function is powered
/// by stars, and he's fresh out!
module Day01 =

    // An opening parenthesis, (, means he should go up one floor, and a closing parenthesis, ),
    // means he should go down one floor.
    let parse = function '(' -> 1 | ')' -> -1 | x -> failwithf "Unexpected input: %c" x
    
    // E.g. "(((())))()((((((((())..."
    let parseInput() = getFile (2015, 1) |> readAllText |> Seq.map parse

    // To what floor do the instructions take Santa?
    let Part1() = parseInput() |> Seq.sum

    // What is the position of the character that causes Santa to first enter the basement?
    let Part2() =
        parseInput()
        |> Seq.scan (+) 0
        |> Seq.findIndex (fun level -> level < 0)