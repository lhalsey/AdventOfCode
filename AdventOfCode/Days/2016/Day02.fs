namespace AdventOfCode.Days.Y2016

open System
open AdventOfCode.Shared.Utility
open AdventOfCode.Shared

/// Day 2: Bathroom Security
/// https://adventofcode.com/2016/day/2
/// You arrive at Easter Bunny Headquarters under cover of darkness.
module Day02 =

    // E.g. "DULUDRDD ..."
    let parse (s: string) = s |> Seq.map Direction2d.GetDirection
        
    let parseInput() = getFile (2016, 2) |> readLinesAs parse

    let Keypad1 =
         [ "123"
           "456"
           "789" ]

    let  Keypad2 =
        [ "  1  "
          " 234 "
          "56789"
          " ABC "
          "  D  " ]

    // Map each key to a 2d point
    let getKeyMap (keypad: string list) =
        keypad
        |> Seq.mapi (fun row str -> str |> Seq.mapi (fun col c -> ({ X = col; Y = row }, c)))
        |> Seq.concat
        |> Seq.filter (fun (_, c) -> c <> ' ')
        |> readOnlyDict

    let getCode (keypad: string list) = 
        let input = parseInput()
        let keyMap = getKeyMap keypad

        // Always start at 5 key
        let start = keyMap |> Seq.find (fun x -> x.Value = '5') |> fun x -> x.Key

        let tryMove point dir = // Move to next key unless it takes us off the keypad
            if keyMap.ContainsKey (point + dir) then point + dir else point

        // For each instruction line, apply moves and return end location
        let applyInstructionLine (loc: Point2d) (input: Direction2d seq) =
            (loc, input) ||> Seq.fold tryMove

        (start, input)
        ||> Seq.scan applyInstructionLine
        |> Seq.skip 1 // Ignore start location
        |> Seq.map (fun x -> keyMap.[x])
        |> Seq.toArray
        |> String


    let Part1() = getCode Keypad1

    let Part2() = getCode Keypad2