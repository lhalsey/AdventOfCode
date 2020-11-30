namespace AdventOfCode.Days.Y2015

open AdventOfCode.Shared.Utility

/// Day 17: No Such Thing as Too Much
/// https://adventofcode.com/2015/day/17
/// The elves bought too much eggnog again - 150 liters this time.
module Day17 =

    let parseInput() = getFile (2015, 17) |> readLinesAs int |> Seq.toList

    let getBucketCombos target buckets =
        let rec getBucketCombosR unused sum bucketsUsed =
            seq {
                match unused, sum with
                | _, s when s = target -> yield bucketsUsed
                | h::t, s when s < target ->
                    yield! getBucketCombosR t s bucketsUsed             // Don't use bucket
                    yield! getBucketCombosR t (s + h) (bucketsUsed + 1) // Use bucket
                | _ -> () // Sum exceeded target so no further exploring here
            }

        // Sort buckets by largest first so we can shortcut branches sooner
        let buckets = buckets |> List.sortByDescending id
        getBucketCombosR buckets 0 0


    // Filling all containers entirely, how many different combinations of containers
    // can exactly fit all 150 liters of eggnog?
    let Part1() =
        parseInput()
        |> getBucketCombos 150
        |> Seq.length

    // Find the minimum number of containers that can exactly fit all 150 liters of eggnog.
    // How many different ways can you fill that number of containers and still hold
    // exactly 150 litres?
    let Part2() =
        parseInput()
        |> getBucketCombos 150
        |> Seq.countBy id
        |> Seq.minBy fst
        |> snd