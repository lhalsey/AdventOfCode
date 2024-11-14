namespace AdventOfCode.Days.Y2023

open AdventOfCode.Shared.Utility
open System

/// Day 1: Trebuchet?!
/// https://adventofcode.com/2023/day/1
/// Something is wrong with global snow production, and you've been selected to take a look.
module Day01 =

    let parseInput() = getFile (2023, 1) |> readLines

    let digits = [ "zero"; "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine" ]

    let digitMap = digits |> List.indexed |> List.map (fun (x, y) -> (y, x)) |> dict

    let getValue (s: string) =
        let nums =
            s
            |>
            Seq.filter Char.IsDigit
            |> Seq.map (Char.GetNumericValue >> int)
            |> Seq.toArray

        nums[0] * 10 + nums[nums.Length - 1]

    let getDigitBy (s: string) (f: string -> string) =
        let rec getDigitByR (rest: string) =
            if Char.IsDigit rest[0] then
                Char.GetNumericValue rest[0] |> int
            else
                match digits |> List.tryFind (fun x -> rest.StartsWith (f x)) with
                | Some d -> digitMap[d]
                | None -> getDigitByR rest[1..]

        getDigitByR (f s)

    //let getWordDigitBy (s: string) (f: string -> string) =
    //    digits
    //    |> List.tryFind (fun x -> s.StartsWith (f x))
    //    |> Option.map (fun x -> digitMap[x])

    //let getDigitBy (s: string) (f: string -> string) =
    //    f s
    //    |> Seq.mapi (fun i _ -> s[i..])
    //    |> Seq.pick (fun x -> tryParseAsInt x[..0] |> Option.orElse (getWordDigitBy x f))

    let getValue2 (s: string) = (getDigitBy s id) * 10 + (getDigitBy s reverse)


    // What is the sum of all of the calibration values?
    let Part1() = parseInput() |> Seq.sumBy getValue

    // It looks like some of the digits are actually spelled out with letters.
    // What is the sum of all of the calibration values?
    let Part2() = parseInput() |> Seq.sumBy getValue2