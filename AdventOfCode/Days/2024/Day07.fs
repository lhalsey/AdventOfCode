namespace AdventOfCode.Days.Y2024

open AdventOfCode.Shared.Utility

/// Day 7: Bridge Repair
/// https://adventofcode.com/2024/day/7
/// The Historians take you to a familiar rope bridge over a river in the middle of a jungle.
module Day07 =

    type Equation = { TestValue: int64; Numbers: int64 list }

    let parse (s: string) =
        let tokens = s |> split ':'
        let numbers = tokens[1] |> split ' ' |> Array.map int64 |> Array.toList

        { TestValue = int64 tokens[0]; Numbers = numbers }

    let parseInput() = getFile (2024, 7) |> readLinesAs parse |> Seq.toList

    let combine (x: int64) (y: int64) = (string x + string y) |> int64

    let canCalibrate operations (e: Equation)  =
        let rec canCalibrateR (numbers: int64 list) (total: int64 option) =
            let tryOp op h t =
                match op with
                | "+" -> canCalibrateR t (Some ((Option.defaultValue 0L total) + h))
                | "*" -> canCalibrateR t (Some ((Option.defaultValue 1L total) * h))
                | "||" -> false
                | x -> failwithf "Invalid input %A" x

            let tryOp2 op h1 h2 t =
                match op with
                | "+" -> canCalibrateR (h2::t) (Some ((Option.defaultValue 0L total) + h1))
                | "*" -> canCalibrateR (h2::t) (Some ((Option.defaultValue 1L total) * h1))
                | "||" -> canCalibrateR ((combine h1 h2)::t) total
                | x -> failwithf "Invalid input %A" x

            match numbers with
            | [] -> total = Some e.TestValue
            | h1::h2::t -> operations |> List.exists (fun x -> tryOp2 x h1 h2 t)
            | h::t -> operations |> List.exists (fun x -> tryOp x h t)

        canCalibrateR e.Numbers None

    let Part1() =
        parseInput()
        |> List.filter (canCalibrate ["+"; "*"])
        |> List.sumBy (fun x -> x.TestValue)

    let Part2() =
        let input = parseInput()

        let valid =
            input
            |> List.filter (canCalibrate ["+"; "*"; "||"])

        valid |> List.sumBy (fun x -> x.TestValue)