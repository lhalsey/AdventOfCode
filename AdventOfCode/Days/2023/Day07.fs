namespace AdventOfCode.Days.Y2023

open AdventOfCode.Shared.Utility

/// Day 7: Camel Cards
/// https://adventofcode.com/2023/day/7
/// Your all-expenses-paid trip turns out to be a one-way, five-minute ride in an airship.
module Day07 =

    type Row = { Hand: string; Bid: int }

    type Mode = Standard | UseJokerRule

    type Rank = HighCard | OnePair | TwoPair  | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind 

    let parse = split ' ' >> fun x -> { Hand = x[0]; Bid = int x[1] } // E.g. QTTQK 749

    let parseInput() = getFile (2023, 7) |> readLinesAs parse

    let replaceJokers (hand: string) =
        let getCardToReplace() =
            hand
            |> Seq.filter (fun x -> x <> 'J')
            |> Seq.countBy id
            |> Seq.maxBy snd
            |> fst

        if hand = "JJJJJ" then hand else hand.Replace('J', (getCardToReplace()))

    let getHandType hand mode =
        let hand = 
            match mode with
            | UseJokerRule -> replaceJokers hand
            | Standard -> hand

        let cardCounts =
            hand
            |> Seq.countBy id
            |> Seq.map snd
            |> Seq.sortDescending
            |> Seq.toList

        match cardCounts with
        | 5::_ -> FiveOfAKind
        | 4::_ -> FourOfAKind
        | 3::2::_ -> FullHouse
        | 3::_ -> ThreeOfAKind
        | 2::2::_ -> TwoPair
        | 2::_ -> OnePair
        | _ -> HighCard

    let getHandValue hand mode =
        let getCardValue = function
            | CInt x -> x
            | 'T' -> 10
            | 'J' when mode = Standard -> 11
            | 'J' when mode = UseJokerRule -> 1
            | 'Q' -> 12
            | 'K' -> 13
            | 'A' -> 14 // Ace is high
            | x -> failwithf "Invalid input: %A" x

        hand |> Seq.map getCardValue |> Seq.toList

    let getHandScore (mode: Mode) (row: Row) =        
        getHandType row.Hand mode, getHandValue row.Hand mode

    let getTotalWinnings mode = 
        parseInput()
        |> Seq.sortBy (getHandScore mode)
        |> Seq.indexed
        |> Seq.sumBy (fun (i, x) -> (i + 1) * x.Bid)

    // Find the rank of every hand in your set. What are the total winnings?
    let Part1() = getTotalWinnings Standard
        
    // Using the new joker rule, find the rank of every hand in your set. What are the new total winnings?
    let Part2() = getTotalWinnings UseJokerRule