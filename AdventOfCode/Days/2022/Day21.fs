namespace AdventOfCode.Days.Y2022

open AdventOfCode.Shared.Utility
open Checked
open System

/// Day 21: Monkey Math
/// https://adventofcode.com/2022/day/21
/// The monkeys are back! You're worried they're going to try to steal your stuff again, but it seems like
/// they're just holding their ground and making various monkey noises at you.
module Day21 =

    let parseInput() = getFile (2022, 21) |> readLinesAs (splitIntoPair ": ") |> Map

    let getRootValue (key: string) (map: Map<string, string>) =
        let rec getValue = function
            | Regex "(\d+)" [Int64 i] -> i
            | Regex "(\w+) \+ (\w+)" [x; y] -> getValue map[x] + getValue map[y]
            | Regex "(\w+) \* (\w+)" [x; y] -> getValue map[x] * getValue map[y]
            | Regex "(\w+) \- (\w+)" [x; y] -> getValue map[x] - getValue map[y]
            | Regex "(\w+) \/ (\w+)" [x; y] -> getValue map[x] / getValue map[y]
            | x -> failwithf "Invalid input: %s" x

        getValue (map[key])

    // However, your actual situation involves considerably more monkeys.
    // What number will the monkey named root yell?
    let Part1() = parseInput() |> getRootValue "root"

    // What number do you yell to pass root's equality test?
    let Part2() =

        failwith "Too slow!"

        let map = parseInput()
        let (m1, m2) = map["root"] |> splitIntoPair " + "

        let isMatch (i: int64) =
            let map = map.Add("humn", string i)
            let v1 = getRootValue m1 map
            let v2 = getRootValue m2 map
            v1 = v2

        seq { 0L .. Int64.MaxValue } |> Seq.find isMatch