namespace AdventOfCode.Days.Y2015

open AdventOfCode.Shared.Utility

/// Day 6: Probably a Fire Hazard
/// https://adventofcode.com/2015/day/6
/// Because your neighbors keep defeating you in the holiday house decorating contest year after
// year, you've decided to deploy one million lights in a 1000x1000 grid.
module Day06 =

    type InstructionType = Toggle | TurnOn | TurnOff
    type Instruction = InstructionType * (int * int * int * int)

    // E.g. "turn off 367,664 through 595,872"
    let parse (s: string) =
        let tokens = s |> split ' ' |> Seq.toList

        let getInts x y = 
            $"{x},{y}"
            |> split ','
            |> Array.map int
            |> fun x -> (x.[0], x.[1], x.[2], x.[3])
        
        match tokens with
        | "turn"::"on"::x::_::y::[] -> TurnOn, getInts x y
        | "turn"::"off"::x::_::y::[] -> TurnOff, getInts x y
        | "toggle"::x::_::y::[] -> Toggle, getInts x y
        | _ -> failwithf "Unexpected input: %A" tokens

    let parseInput() = getFile (2015, 6) |> readLinesAs parse |> Seq.toList

    let update (x1, y1, x2, y2) f (arr: int[,]) =
        arr.[x1 .. x2, y1 .. y2] <- arr.[x1 .. x2, y1 .. y2] |> Array2D.map f

    let apply arr = function
        | TurnOn, pts -> arr |> update pts (fun _ -> 1)
        | TurnOff, pts -> arr |> update pts (fun _ -> 0)
        | Toggle, pts -> arr |> update pts (fun x -> 1 - x)

    let applyWithBrightness arr = function
        | TurnOn, pts -> arr |> update pts ((+) 1)
        | TurnOff, pts -> arr |> update pts (fun x -> x - 1 |> max 0)
        | Toggle, pts -> arr |> update pts ((+) 2)

    let setLights f =
        let lights = Array2D.create 1_000 1_000 0
        
        parseInput() |> Seq.iter (f lights)
        
        lights |> Seq.cast<int> |> Seq.sum

    // After following the instructions, how many lights are lit?
    let Part1() = setLights apply

    // What is the total brightness of all lights combined after following Santa's instructions?
    let Part2() = setLights applyWithBrightness