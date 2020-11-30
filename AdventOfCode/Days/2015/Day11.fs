namespace AdventOfCode.Days.Y2015

open System
open AdventOfCode.Shared.Utility

/// Day 11: Corporate Policy
/// https://adventofcode.com/2015/day/11
/// Santa's previous password expired, and he needs help choosing a new one.
module Day11 =

    let Input = "hxbxwxba"

    // Passwords must include one increasing straight of at least three letters,
    // like abc, bcd, cde, and so on, up to xyz. They cannot skip letters;
    // abd doesn't count.
    let hasIncreasingStraightOfThree =
        Array.map int
        >> Array.windowed 3
        >> Array.exists (fun x -> x.[0] = x.[1] - 1 && x.[1] = x.[2] - 1)

    // Passwords must contain at least two different, non-overlapping pairs of letters,
    // like aa, bb, or zz.
    let hasTwoDifferentPairs =
        Array.pairwise
        >> Array.filter (fun (x, y) -> x = y)
        >> Array.distinct
        >> hasAtLeast 2

    let getPasswords() =
        let isValid (s: char[]) = hasIncreasingStraightOfThree s && hasTwoDifferentPairs s
            
        // It's about 2x faster to use char array rather than immutable string
        // And ~50% faster to skip invalid letters when incrementing rather than filter out later
        let increment (s: char[]) =
            let rec incrementR n =
                match s.[n] with
                | 'z' -> s.[n] <- 'a'; incrementR (n - 1)
                | 'h' -> s.[n] <- 'j'; s
                | 'n' -> s.[n] <- 'p'; s
                | 'k' -> s.[n] <- 'm'; s
                | _ -> s.[n] <- s.[n] + char 1; s

            incrementR (s.Length - 1)

        Input
        |> Seq.toArray
        |> Seq.unfold (fun x -> Some(x, increment x))
        |> Seq.skip 1
        |> Seq.filter isValid
        |> Seq.map String

    // Given Santa's current password (your puzzle input), what should his next password be?
    let Part1() = getPasswords() |> Seq.head

    // Santa's password expired again. What's the next one?
    let Part2() = getPasswords() |> Seq.item 1