namespace AdventOfCode.Days.Y2020

open System
open System.Collections.Generic
open AdventOfCode.Shared.Utility

/// Day 19: Monster Messages
/// https://adventofcode.com/2020/day/19
/// You land in an airport surrounded by dense forest.
module Day19 =

    type Rule = Leaf of char | And of int list | Or of int list * int list

    // E.g. "50: 15 65 | 60 74"
    let parseRule (s: string) =
        let tokens = s |> split ':'

        let rules = tokens.[1].Trim() |> split '|' |> Array.toList

        let getSubRules (s: string) =
            s.Trim() |> split ' ' |> Array.map (string >> int) |> Array.toList

        let subRules =
            match rules with
            | ["\"a\""] -> Leaf 'a'
            | ["\"b\""] -> Leaf 'b'
            | [r1] -> And (getSubRules r1)
            | [r1; r2] -> Or (getSubRules r1, getSubRules r2)
            | x -> failwithf "Invalid input: %A" x

        int tokens.[0], subRules

    let parseInput() =
        let input = getFile (2020, 19) |> readLines |> Seq.toList

        let blankIndex = input |> List.findIndex (String.IsNullOrEmpty)
        let rules = input.[0..blankIndex - 1] |> List.map parseRule
        let data = input.[blankIndex + 1..]

        rules, data

    let isMatch (ruleMap: IReadOnlyDictionary<int, Rule>) (s: string) =
        let rec isMatchR (chars: char list) (rule: Rule) =
            seq {
                match rule, chars with
                | Leaf _, [] -> () // Not enough letters to match rule
                | Leaf c, h::t -> if h = c then yield t // Continue if first letter matches
                | And [], _ -> yield chars // Rules all matched, return remaining letters
                | And (r1::rs), _ -> // Try to match all rules
                    for m in isMatchR chars ruleMap.[r1] do
                        yield! isMatchR m (And(rs))
                | Or (r1, r2), _ -> // Try to match either rule
                    yield! isMatchR chars (And(r1))
                    yield! isMatchR chars (And(r2)) }

        // Check if there is one path where all letters are matched (ends up empty)
        isMatchR (Seq.toList s) ruleMap.[0] |> Seq.exists List.isEmpty

    let getMatches addOverrides =
        let (rules, data) = parseInput() 
        let ruleMap = rules |> List.map addOverrides |> readOnlyDict

        data |> parallelCountIf (isMatch ruleMap)


    // How many messages completely match rule 0?
    let Part1() = getMatches id

    // After updating rules 8 and 11, how many messages completely match rule 0?
    let Part2() =
        let addOverride (index, rule) =
            match index with
            | 8 -> 8, Or([42], [42; 8])
            | 11 -> 11, Or([42; 31], [42; 11; 31])
            | _ -> index, rule

        getMatches addOverride