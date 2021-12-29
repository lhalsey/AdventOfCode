namespace AdventOfCode.Days.Y2021

open AdventOfCode.Shared.Utility
open System

/// Day 4: Giant Squid
/// https://adventofcode.com/2021/day/4
/// You're already almost 1.5km (almost a mile) below the surface of the ocean, already so deep that you
/// can't see any sunlight.
module Day04 =
    
    let [<Literal>] BoardSize = 5
    let [<Literal>] BoardNumbers = 25

    type Board = { Numbers: Map<int, int>; Matches: int list }

    let parseInput() = getFile (2021, 4) |> readLines |> Seq.filter (String.IsNullOrWhiteSpace >> not)

    let getProblem() =
        let input = parseInput()
        let (head, tail) = (Seq.head input, Seq.tail input)

        let numbers = head |> split ',' |> Seq.map int |> Seq.toList
        
        let boards = // A board is a map of (number, board location), matched numbers and win status
            tail
            |> Seq.collect (split ' ' >> Seq.map int)
            |> Seq.chunkBySize BoardNumbers
            |> Seq.map (Seq.mapi (fun i x -> (x, i)) >> Map)
            |> Seq.map (fun x -> { Numbers = x; Matches = []})
            |> Seq.toList

        (numbers, boards)

    let updateBoard (number: int) (board: Board) =
        match tryFind number board.Numbers with
        | Some x -> { board with Matches = x::board.Matches }
        | None -> board
                    
    let hasWon (board: Board) =
        let rows = board.Matches |> List.countBy (fun x -> x / BoardSize)
        let cols = board.Matches |> List.countBy (fun x -> x % BoardSize)

        (rows @ cols) |> List.exists (fun (_, c) -> c = BoardSize)

    // Update each board and return winners and non-winners
    let callNumber (boards: Board list) (number: int) =
        boards
        |> List.map (updateBoard number)
        |> List.partition hasWon

    let getWinners() =
        let rec getWinnersR numbers boards =
            seq {
                match numbers with
                | num::nums ->
                    match callNumber boards num with
                    | [], [] -> ()
                    | winners, nonwinners ->
                        yield! winners |> Seq.map (fun x -> (x, num))
                        yield! getWinnersR nums nonwinners

                | _ -> failwith "No winner"
                }

        let (numbers, boards) = getProblem()
        getWinnersR numbers boards

    let getScore (board: Board, lastNumber: int) =
        let score =
            board.Numbers
            |> Seq.filter (fun x -> board.Matches |> List.contains x.Value |> not)
            |> Seq.sumBy (fun x -> x.Key)

        score * lastNumber

    // To guarantee victory against the giant squid, figure out which board will win first.
    // What will your final score be if you choose that board?
    let Part1() = getWinners() |> Seq.head |> getScore
        
    // Figure out which board will win last. Once it wins, what would its final score be?
    let Part2() = getWinners() |> Seq.last |> getScore