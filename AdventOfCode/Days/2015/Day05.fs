namespace AdventOfCode.Days.Y2015

open AdventOfCode.Shared.Utility

/// Day 5: Doesn't He Have Intern-Elves For This?
/// https://adventofcode.com/2015/day/5
/// Santa needs help figuring out which strings in his text file are naughty or nice.
module Day05 =

    let parseInput() = getFile (2015, 5) |> readLines

    let hasInvalidWords (s: string) = ["ab"; "cd"; "pq"; "xy"] |> List.exists s.Contains

    let hasPair (s: string) = s |> Seq.pairwise |> Seq.exists (fun (x, y) -> x = y)

    let hasRepeatedPair (s: string) = 
        s
        |> Seq.pairwise   // Find repeated pair of letters
        |> Seq.indexed    // that do not overlap
        |> Seq.groupBy snd
        |> Seq.exists (fun (_, y) -> ((Seq.last >> fst) y) > ((Seq.head >> fst) y) + 1)

    let hasRepeatWithGap gap (s: string) =
        (s, Seq.skip (gap + 1) s)
        ||> Seq.exists2 (=)

    let hasVowels n (s: string) =
        s
        |> Seq.filter ("aeiou".Contains)
        |> hasAtLeast n

    let getMatches conditions =
        parseInput()
        |> Seq.filter (fun x -> conditions |> List.forall (fun f -> f x))
        |> Seq.length
        

    // A nice string contains at least three vowels, contains at least one letter that
    // appears twice in a row and does not contain the strings ab, cd, pq, or xy
    // How many strings are nice?
    let Part1() = getMatches [ hasVowels 3; hasPair; hasInvalidWords >> not ]

    // Now a nice string contains a pair of any two letters that appears at least twice in
    // the string without overlapping and it contains at least one letter which repeats with
    // exactly one letter between them
    // How many strings are nice under these new rules?
    let Part2() = getMatches [ hasRepeatedPair; hasRepeatWithGap 1 ]