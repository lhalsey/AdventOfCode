namespace AdventOfCode.Days.Y2016

open AdventOfCode.Shared.Utility

/// Day 10: Balance Bots
/// https://adventofcode.com/2016/day/10
/// You come upon a factory in which many robots are zooming around handing small microchips to each other.
module Day10 =

    type Bot = int
    type Token = int

    type Instruction =
        | Input of Token * Bot
        | Delivery of Bot * Bot * Bot

    let parse = function
        | Regex "value (\d+) goes to bot (\d+)" [token; bot] -> Input (int token, int bot)
        | Regex "bot (\d+) gives low to bot (\d+) and high to bot (\d+)" [bot; low; high]
            -> Delivery (int bot, int low, int high)
        | x -> failwithf "Invalid input: %s" x

    let parseInput() = getFile (2016, 10) |> readLinesAs parse

    // Based on your instructions, what is the number of the bot that is responsible for comparing
    // value-61 microchips with value-17 microchips?
    let Part1() =
        let input = parseInput() |> Seq.toList

        1

    // What do you get if you multiply together the values of one chip in each of outputs 0, 1, and 2?
    let Part2() =
        0