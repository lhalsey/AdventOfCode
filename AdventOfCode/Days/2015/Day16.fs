namespace AdventOfCode.Days.Y2015

open System.Text.RegularExpressions
open AdventOfCode.Shared.Utility

/// Day 16: Aunt Sue
/// https://adventofcode.com/2015/day/16
/// Your Aunt Sue has given you a wonderful gift, and you'd like to send her a thank you card.
module Day16 =

    type Mode = UseExact | UseRanges

    // E.g. Sue 25: goldfish: 3, akitas: 2, trees: 2
    let parse (s: string) =
        let getFact (m: Match) =
            let tokens = m.Value.Split ": "
            (tokens.[0], int tokens.[1])
            
        let facts = Regex.Matches(s, "\w+: \d+") |> Seq.map getFact |> Seq.toList
        let id = s |> split ' ' |> fun x -> x.[1].Trim ':' |> int

        (id, facts)

    // The cats and trees readings indicates that there are greater than that many,
    // while the pomeranians and goldfish readings indicate that there are fewer than that many
    // Note we switch the operator direction as we will apply as "(<) expected actual" which
    // is equivalent to "expected < actual"
    let getOverrides() =
        [ "cats", (<)
          "trees", (<)
          "pomeranians", (>)
          "goldfish", (>) ]
        |> readOnlyDict

    let getCriteria mode =
        let overrides = match mode with UseExact -> readOnlyDict [] | _ -> getOverrides()

        // Exact match unless using ranges
        let getPred (item, qty) =
            let pred = overrides |> tryFind item |> Option.defaultValue (=)
            (item, pred qty)

        [ "children", 3
          "cats", 7
          "samoyeds", 2
          "pomeranians", 3
          "akitas", 0
          "vizslas", 0
          "goldfish", 5
          "trees", 3
          "cars", 2
          "perfumes", 1 ]
          |> List.map getPred
          |> readOnlyDict

    let parseInput() = getFile (2015, 16) |> readLinesAs parse

    let findAunt mode =
        let aunts = parseInput()
        let criteria = getCriteria mode

        let isAunt = List.forall (fun (item, qty) -> criteria.[item] qty)

        aunts
        |> Seq.find (fun (_, facts) -> isAunt facts)
        |> fst


    // What is the number of the Sue that got you the gift?
    let Part1() = findAunt UseExact

    // The cats and trees readings indicates that there are greater than that many while
    // the pomeranians and goldfish readings indicate that there are fewer than that many.
    // What is the number of the real Aunt Sue?
    let Part2() = findAunt UseRanges