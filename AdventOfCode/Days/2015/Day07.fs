namespace AdventOfCode.Days.Y2015

open AdventOfCode.Shared.Utility

/// Day 7: Some Assembly Required
/// https://adventofcode.com/2015/day/7
/// This year, Santa brought little Bobby Tables a set of wires and bitwise logic gates!
module Day07 =

    type Signal =
        | Input of string
        | Not of string
        | Or of string * string
        | And of string * string
        | LShift of string * string
        | RShift of string * string

    let parseSignal (input: string) =
        match input |> split ' ' |> Seq.toList with
        | x::[] -> Input x
        | "NOT"::x::[] -> Not x
        | x::"OR"::y::[] -> Or (x, y)
        | x::"AND"::y::[] -> And (x, y)
        | x::"LSHIFT"::y::[] -> LShift (x, y)
        | x::"RSHIFT"::y::[] -> RShift (x, y)
        | x -> failwithf "Unexpected input: %A" x

    // E.g. "dy RSHIFT 3 -> ea"
    let parse (s: string) = 
        let tokens = s.Split " -> "
        (tokens.[1], parseSignal tokens.[0])

    let parseInput() = getFile (2015, 7) |> readLinesAs parse 

    let getSignal input signal =

        let signalMap = input |> readOnlyDict

        let rec getSignalR signal =
            match signalMap.TryGetValue signal with
            | false, _ -> int signal // Constant
            | _, Input x -> getSignalMemo x
            | _, Not x -> ~~~ (getSignalMemo x)
            | _, Or (x, y) -> (getSignalMemo x) ||| (getSignalMemo y)
            | _, And (x, y) -> (getSignalMemo x) &&& (getSignalMemo y)
            | _, LShift (x, y) -> (getSignalMemo x) <<< (getSignalMemo y)
            | _, RShift (x, y) -> (getSignalMemo x) >>> (getSignalMemo y)
        and getSignalMemo = memoise getSignalR

        getSignalMemo signal

    // In little Bobby's kit's instructions booklet (provided as your puzzle input),
    // what signal is ultimately provided to wire a?
    let Part1() =
        let input = parseInput()

        getSignal input "a"

    // Now, take the signal you got on wire a, override wire b to that signal, and reset
    // the other wires (including wire a). What new signal is ultimately provided to wire a?
    let Part2() =
        let input = parseInput()
        let signalA = getSignal input "a" |> string

        let input = [ ("b", Input signalA) ] |> Seq.append input

        getSignal input "a"