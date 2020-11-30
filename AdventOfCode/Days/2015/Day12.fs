namespace AdventOfCode.Days.Y2015

open AdventOfCode.Shared.Utility

/// Day 12: JSAbacusFramework.io
/// https://adventofcode.com/2015/day/12
/// Santa's Accounting-Elves need help balancing the books after a recent order.
module Day12 =

    // TODO: Consider using JSON type provider
    let parseInput() = getFile (2015, 12) |> readAllText

    let getSum (input: string) =
        input
        |> splitAny "[]{}:,"
        |> Array.sumBy (tryParseAsInt >> Option.defaultValue 0)

    // Ignore any object (and all of its children) which has any property with the value "red".
    // Do this only for objects ({...}), not arrays ([...]).
    let removeRedObjects (input: string) = 
        let rec removeRedObjectsR chars stack curr =
            match chars, stack with
            | [], _ -> curr // Processed all input chars
            | '{'::t, _ -> removeRedObjectsR t (curr::stack) "" // New stack frame
            | '}'::t, x::xs -> // Retrieve previous stack frame and append current if valid
                let curr = if curr.Contains ":\"red\"" then x else x + curr
                removeRedObjectsR t xs curr
            | h::t, _ -> removeRedObjectsR t stack $"{curr}{h}" // Any other character append to current

        removeRedObjectsR (input |> Seq.toList) [] ""
        

    // What is the sum of all numbers in the document?
    let Part1() = parseInput() |> getSum

    // Ignore any object (and all of its children) which has any property with the value "red"
    let Part2() = parseInput() |> removeRedObjects |> getSum