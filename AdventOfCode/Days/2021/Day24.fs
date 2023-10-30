namespace AdventOfCode.Days.Y2021

open AdventOfCode.Shared.Utility

/// Day 24: Arithmetic Logic Unit
/// https://adventofcode.com/2021/day/24
/// Magic smoke starts leaking from the submarine's arithmetic logic unit (ALU).
module Day24 =

    type Block = { A: int; B: int; C: int }

    let getValue (s: string) =
        let tokens = split ' ' s
        tokens.[2] |> int

    let getBlock (lines: string[]) =
        { A = getValue lines.[4]
          B = getValue lines.[5]
          C = getValue lines.[15] }

    let parse (lines: string seq) =
        lines
        |> Seq.chunkBySize 18 // TODO: Break on inp
        |> Seq.map (Seq.toArray >> getBlock)
        |> Seq.toList

    let parseInput() = getFile (2021, 24) |> readLines |> parse

    let rec check (number: int64) (z: int) (blocks: Block list) =
        seq {
            match blocks with
            | [] -> if z = 0 then yield number
            | h::t when h.B < 0 -> 
                yield!
                    seq [1..9]
                    |> Seq.filter (fun x -> z % 26 + h.B = x)
                    |> Seq.collect (fun x -> check (number * 10L + int64 x) (z / 26) t)
            | h::t ->
                yield!
                    seq [1..9]
                    |> Seq.collect (fun x -> check (number * 10L + int64 x) (z * 26 + x + h.C) t)
        }

    let Part1() = parseInput() |> check 0L 0 |> Seq.max

    let Part2() = parseInput() |> check 0L 0 |> Seq.min