namespace AdventOfCode.Days.Y2023

open AdventOfCode.Shared.Utility

/// Day 4: Scratchcards
/// https://adventofcode.com/2023/day/4
/// The gondola takes you up. Strangely, though, the ground doesn't seem to be coming with you;
/// you're not climbing a mountain.
module Day04 =

    type Card = { Id: int; Wins: int Set; Ours: int Set }

    let getNums (s: string) =
        s |> trim |> split ' ' |> Array.map int |> set

    let parse = function
        | Regex "Card[ ]+(\d+): ([0-9 ]+)\|([0-9 ]+)" [Int id; wins; ours] ->
            { Id = id; Wins = getNums wins; Ours = getNums ours }
        | x -> failwithf "Invalid input: %s" x

    let parseInput() = getFile (2023, 4) |> readLinesAs parse

    let getWins (card: Card) = Set.intersect card.Ours card.Wins |> Set.count

    let getScore (card: Card) =
        match getWins card with
        | 0 -> 0
        | x -> pown 2 (x - 1)

    let countCards (cards: Card seq) =
        let getCardCount card (cardMap: Map<int, int>)  =
            let score = getWins card
            let copies = List.sumBy (fun x -> cardMap[x]) [card.Id + 1 .. card.Id + score]
            cardMap.Add (card.Id, 1 + copies) // Card itself and count for each card below per win

        (cards, Map.empty)
        ||> Seq.foldBack getCardCount // Reverse order as we rely on cards below
        |> fun x -> x.Values
        |> Seq.sum

        
    // How many points are they worth in total?
    let Part1() = parseInput() |> Seq.sumBy getScore

    // Process all of the original and copied scratchcards until no more scratchcards are won.
    // Including the original set of scratchcards, how many total scratchcards do you end up with?
    let Part2() = parseInput() |> countCards 