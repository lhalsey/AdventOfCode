namespace AdventOfCode.Days.Y2021

open AdventOfCode.Shared.Utility

/// Day 8: Seven Segment Search
/// https://adventofcode.com/2021/day/8
/// You barely reach the safety of the cave when the whale smashes into the cave mouth, collapsing it
module Day08 =

    type Row = { Input: string[]; Output: string[] }

    // E.g. "dg debg edgfc afbgcd efdbgc gdc bfdceag bfdec febcad gfaec | dcg bdaegfc egbd dcgfe"
    let parse x = 
        let (input, output) = x |> splitIntoPair "|"

        let splitAndSort (s: string) = s |> split ' ' |> Array.map (Seq.sort >> charsToStr)

        { Input = splitAndSort input ; Output = splitAndSort output }

    let segmentMap =
        [ 
            0, "abcefg"
            1, "cf"
            2, "acdeg"
            3, "acdfg"
            4, "bcdf"
            5, "abdfg"
            6, "abdefg"
            7, "acf"
            8, "abcdefg"
            9, "abcdfg"
        ] |> Map

    let compare (digit1: string) (digit2: string) = 
        (set digit1) - (set digit2)

    let parseInput() = getFile (2021, 8) |> readLinesAs parse |> Seq.toArray

    let getNumKnownDigits (row: Row) =
        let knownDigitMap = [ (2, 1); (3, 7); (4, 4); (7, 8) ] |> Map

        row.Output
        |> countIf (fun x -> knownDigitMap.ContainsKey x.Length)

    let getOutputValue (row: Row) =
        let knownDigitMap = [ (2, 1); (3, 7); (4, 4); (7, 8) ] |> Map

        let tryGetDigit (s: string) = 
            match knownDigitMap.TryFind s.Length with
            | Some x -> Some (s, x) 
            | _ -> None

        let knownInputDigits =
            row.Input
            |> Array.choose tryGetDigit
            |> Map

        let optionsMap = [ (5, [2; 3; 5]); (6, [0; 6; 9]) ] |> Map

        let isMatch (o: string) (s: string) (i: int) =
            let k1 = segmentMap.[i]
            let k2 = knownInputDigits |> Seq.find (fun x -> x.Value = i)

            let d1 = compare o k1
            let d2 = compare s k2.Key

            d1.Count = d2.Count

        let tryDigit (o: string) (s: string) =
            knownDigitMap.Values
            |> Seq.forall (isMatch o s)
            
        let deriveDigit (s: string) =
            let options = optionsMap.[s.Length]

            options
            |> List.find (fun o -> tryDigit segmentMap.[o] s)

        let getDigit (s: string) = 
            match knownInputDigits.TryFind s with
            | Some x -> x
            | None -> deriveDigit s

        row.Output
        |> Array.map getDigit
        |> Array.fold (fun acc x -> acc * 10 + x) 0
        

    // In the output values, how many times do digits 1, 4, 7, or 8 appear?
    let Part1() = parseInput() |> Array.sumBy getNumKnownDigits

    // For each entry, determine all of the wire/segment connections and decode the four-digit output values.
    // What do you get if you add up all of the output values?
    let Part2() = parseInput() |> Array.sumBy getOutputValue