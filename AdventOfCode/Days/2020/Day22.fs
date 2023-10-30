namespace AdventOfCode.Days.Y2020

open AdventOfCode.Shared.Utility
open System
open System.Collections.Generic

/// Day 22: Crab Combat
/// https://adventofcode.com/2020/day/22
/// It only takes a few hours of sailing the ocean on a raft for boredom to sink in.
module Day22 =

    type Mode = Normal | Recursive

    type Winner = Player1 | Player2

    let parsePlayer (s: string) =
        s |> splitOn Environment.NewLine |> Array.tail |> Array.map int |> Array.toList

    let parseInput() =
        let players = getFile (2020, 22) |> readAllText |> splitOn $"{Environment.NewLine}{Environment.NewLine}"
        parsePlayer players.[0], parsePlayer players.[1]

    let playGame mode = 
        let (player1, player2) = parseInput()

        // Would prefer to use immutable Set rather than HashSet but this is ~3x faster
        let rec play (seen: HashSet<int>) = function
            | [], p2 -> Player2, p2
            | p1, [] -> Player1, p1
            | p1, p2 when seen.Add (hash p1) |> not || seen.Add (hash p2) |> not -> Player1, p1
            | h1::t1, h2::t2 ->
                let winner = 
                    if mode = Recursive && t1.Length >= h1 && t2.Length >= h2 then
                        let cards = List.take h1 t1, List.take h2 t2
                        play (HashSet()) cards |> fst           // Determine winner by recursive combat
                    else if h1 > h2 then Player1 else Player2 // Or highest card wins

                match winner with
                | Player1 -> play seen (t1 @ [h1; h2], t2)
                | Player2 -> play seen (t1, t2 @ [h2; h1])

        let (_, cards) = play (HashSet()) (player1, player2) 

        // The bottom card in their deck is worth the value of the card multiplied by 1, the
        // second-from-the-bottom card is worth the value of the card multiplied by 2, and so on.
        cards
        |> List.rev
        |> List.mapi (fun i x -> (i + 1) * x)
        |> List.sum


    // Play the small crab in a game of Combat using the two decks you just dealt.
    // What is the winning player's score?
    let Part1() = playGame Normal

    // Defend your honor as Raft Captain by playing the small crab in a game of Recursive Combat using the
    // same two decks as before. What is the winning player's score?
    let Part2() = playGame Recursive