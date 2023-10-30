namespace AdventOfCode.Days.Y2022

open AdventOfCode.Shared.Utility

/// Day 2: Rock Paper Scissors
/// https://adventofcode.com/2022/day/2
/// The Elves begin to set up camp on the beach. To decide whose tent gets to be closest to the snack storage,
/// a giant Rock Paper Scissors tournament is already in progress.
module Day02 =

    type Item = Rock | Paper | Scissors

    type Result = Win | Draw | Loss

    let parseInput() = getFile (2022, 2) |> readLines

    let getItem = function 'A' | 'X' -> Rock | 'B' | 'Y' -> Paper | 'C' | 'Z' -> Scissors | x -> failwithf "Invalid input %c" x

    let getResult = function 'X' -> Loss | 'Y' -> Draw | 'Z' -> Win | x -> failwithf "Invalid input %c" x

    let getItemScore = function Rock -> 1 | Paper -> 2 | Scissors -> 3

    let getRoundScore = function Win -> 6 | Draw -> 3 | Loss -> 0

    let getRoundResult = function
        | Rock, Paper -> Win
        | Rock, Scissors -> Loss
        | Paper, Rock -> Loss
        | Paper, Scissors -> Win
        | Scissors, Rock -> Win
        | Scissors, Paper -> Loss
        | _ -> Draw

    let getRoundItem = function
        | Rock, Win -> Paper
        | Rock, Loss -> Scissors
        | Paper, Win -> Scissors
        | Paper, Loss -> Rock
        | Scissors, Win -> Rock
        | Scissors, Loss -> Paper
        | x, Draw -> x

    let getScoreByItem (round: string) =
        let oppItem, ourItem = getItem round[0], getItem round[2]
        let result = getRoundResult (oppItem, ourItem)
        getItemScore ourItem + getRoundScore result

    let getScoreByResult (round: string) =
        let oppItem, result = getItem round[0], getResult round[2]
        let ourItem = getRoundItem (oppItem, result)
        getItemScore ourItem + getRoundScore result

    // What would your total score be if everything goes exactly according to your strategy guide?
    let Part1() = parseInput() |> Seq.sumBy getScoreByItem

    // Following the Elf's instructions for the second column, what would your total score be if everything
    // goes exactly according to your strategy guide?
    let Part2() = parseInput() |> Seq.sumBy getScoreByResult