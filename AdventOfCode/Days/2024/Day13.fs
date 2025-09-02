namespace AdventOfCode.Days.Y2024

open AdventOfCode.Shared.Utility
//open AdventOfCode.Shared

/// Day 13: Claw Contraption
/// https://adventofcode.com/2024/day/13
/// Next up: the lobby of a resort on a tropical island. The Historians take a moment to admire
/// the hexagonal floor tiles before spreading out.
module Day13 =

    type Vector2d = { X: int64; Y: int64 } with
        static member (+) (a, b) = { X = a.X + b.X; Y = a.Y + b.Y }
        static member (-) (a, b) = { X = a.X - b.X; Y = a.Y - b.Y }
        static member (*) (a, b) = { X = a.X * b; Y = a.Y * b }


    type Machine = { A: Vector2d; B: Vector2d; Prize: Vector2d }

    // Sample input:
    // Button A: X+26, Y+91
    // Button B: X+82, Y+37
    // Prize: X=5592, Y=8072
    let getPoint = function
        | Regex "Button .: X\+(\d+), Y\+(\d+)" [Int x; Int y] -> { X = x; Y = y }
        | Regex "Prize: X=(\d+), Y=(\d+)" [Int x; Int y] -> { X = x; Y = y }
        | x -> failwithf "Invalid input: %s" x
            
    let parse (lines: string seq) =
        let points = lines |> Seq.map getPoint |> Seq.toList

        { A = points[0]; B = points[1]; Prize = points[2] }

    let parseInput() = getFile (2024, 13) |> readLines |> splitBy "" |> Seq.map parse |> Seq.toList

    let rec findBitonicPoint getValue lower upper =
        if upper < lower then
            None
        else
            let mid = (lower + upper) / 2L
            let m = getValue mid
            let mLower = getValue (mid - 1L)
            let mUpper = getValue (mid + 1L)

            if m < mLower && m < mUpper then Some mid
            else if m > mUpper then findBitonicPoint getValue (mid + 1L) upper
            else findBitonicPoint getValue lower (mid - 1L)

    let getTokens (offset: Vector2d) (machine: Machine)  =
        let getDist (a: int64) =
            let p = machine.A * a         // Point pressing A gets us to
            let delta = (machine.Prize + offset) - p // Remaining distance to prize
            let b = double delta.X / double machine.B.X // Number of B presses if possible to reach prize
            let diff = (double machine.B.X * b - double delta.X) + (double machine.B.Y * b - double delta.Y)
            abs diff |> int64

        let getAmount a =
            let p = machine.A * a         // Point pressing A gets us to
            let delta = (machine.Prize + offset) - p // Remaining distance to prize
            let b = delta.X / machine.B.X // Number of B presses if possible to reach prize
            if machine.B * b = delta then Some (a * 3L + b) else None // A costs 3, B costs 1


        let aPresses = findBitonicPoint getDist 0L (100L + offset.X)

        aPresses |> Option.bind getAmount

    let getTokensWithOffset offset =
        parseInput()
        |> List.choose (getTokens { X = offset; Y = offset })
        |> List.sum

    // Figure out how to win as many prizes as possible.
    // What is the fewest tokens you would have to spend to win all possible prizes?
    let Part1() = getTokensWithOffset 0L

    // Using the corrected prize coordinates, figure out how to win as many prizes as possible.
    // What is the fewest tokens you would have to spend to win all possible prizes?
    let Part2() = getTokensWithOffset 10_000_000_000_000L