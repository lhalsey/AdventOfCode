namespace AdventOfCode.Days.Y2020

open AdventOfCode.Shared.Utility
open FSharp.Collections.ParallelSeq

/// Day 18: Operation Order
/// https://adventofcode.com/2020/day/18
/// As you look out the window and notice a heavily-forested continent slowly appear over the horizon,
/// you are interrupted by the child sitting next to you.
module Day18 =

    // TODO: Investigate FParsec - https://github.com/kimvais/AdventOfCode2020/blob/master/Day18.fs
    type Token = Number of int64 | Add | Multiply | OpenBracket | CloseBracket

    let parseToken = function
        | "+" -> Add
        | "*" -> Multiply
        | "(" -> OpenBracket
        | ")" -> CloseBracket
        | Int64 n -> Number n
        | x -> failwithf "Invalid input: %s" x

    // E.g. "((8 + 6 + 4 + 9 * 2 + 9) * 8 + 2 * (7 + 2 + 4 * 2 + 4) * 2 * 5) * 3"
    let parseLine (line: string) = 
        $"({line})" // Put parentheses around expression so top level is evaluated as well
        |> replace "(" "( " // Pad parentheses to make tokenizing easier
        |> replace ")" " )"
        |> split ' '
        |> Array.toList
        |> List.map parseToken

    let parseInput() = getFile (2020, 18) |> readLinesAs parseLine

    let rec reduceAdditions = function
        | [] -> []
        | Number n1::Add::Number n2::t -> reduceAdditions ((Number(n1 + n2))::t)
        | h::t -> h::(reduceAdditions t) // We reverse list, but should only have multiplications left

    let rec evaluateFrame = function
        | Number n1::[] -> n1
        | Number n1::Add::t -> n1 + evaluateFrame t
        | Number n1::Multiply::t -> n1 * evaluateFrame t
        | x -> failwithf "Invalid input: %A" x

    let evaluate evalF (tokens: Token list) =
        let evaluateToken acc token =
            match token, acc with
            | OpenBracket, _ -> []::acc // Create new stack frame
            | CloseBracket, a1::a2::aa -> (Number (evalF a1)::a2)::aa // Evaluate current stack frame
            | _, a::aa -> (token::a)::aa // Add operation or number to current frame
            | x -> failwithf "Invalid input: %A" x

        ([[]], tokens)
        ||> List.fold evaluateToken
        |> function [[Number n]] -> n | _ -> failwith "Failed to evaluate"

    // Multiplication and addition have same precedence. Parentheses can override this order.
    // Evaluate the expression on each line of the homework; what is the sum of the resulting values?
    let Part1() = parseInput() |> PSeq.sumBy (evaluate evaluateFrame)

    // Now we have new precedence levels - addition is evaluated before multiplication.
    // What do you get if you add up the results of evaluating the homework problems using these new rules?
    let Part2() = parseInput() |> PSeq.sumBy (evaluate (reduceAdditions >> evaluateFrame))