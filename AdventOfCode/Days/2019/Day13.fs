namespace AdventOfCode.Days.Y2019

open AdventOfCode.Shared.Utility
open AdventOfCode.Shared.IntCodeInterpreter

/// Day 13: Care Package
/// https://adventofcode.com/2019/day/13
/// As you ponder the solitude of space and the ever-increasing three-hour roundtrip for
/// messages between you and Earth, you notice that the Space Mail Indicator Light is blinking.
module Day13 =

    let [<Literal>] Block = 2
    let [<Literal>] Bat = 3
    let [<Literal>] Ball = 4
    let [<Literal>] PacketSize = 3

    type ArcadeBot = { Blocks: int; BatX: int; BallX: int; Score: int }

    // We don't actually need to track each tile type unless we want to visualise the game
    // Instead just track number of blocks for part 1 and bat, ball & score for part 2
    let update (bot: ArcadeBot) (packet: int list) =
        match packet with
        | _::_::Block::[]  -> { bot with Blocks = bot.Blocks + 1 }
        | x::_::Bat::[]    -> { bot with BatX = x }
        | x::_::Ball::[]   -> { bot with BallX = x }
        | -1::0::score::[] -> { bot with Score = score }
        | _ -> bot

    let processOutput output bot =
        output
        |> List.map int
        |> List.chunkBySize PacketSize // X, Y, Tile id
        |> List.fold update bot

    let rec playGame bot = function
        | Terminated _ -> bot
        | Output (output, state) ->
            let bot = processOutput output bot
            playGame bot state
        | Input f -> // Simply move bat toward ball on horizontal axis
            let joystick = compare bot.BallX bot.BatX // -1 = Left, 0 = Neutral, 1 = Right
            let state = f (int64 joystick)
            playGame bot state

    let getInterpreter() = getFile(2019, 13) |> Interpreter.Create

    let runWith memoryValues =
        let programState = 
            getInterpreter()
            |> setMemory memoryValues
            |> run

        let arcadeBot = { Blocks = 0; BatX = 0; BallX = 0; Score = 0 }

        playGame arcadeBot programState 

    // Start the game.
    // How many block tiles are on the screen when the game exits?
    let Part1() = runWith [] |> fun x -> x.Blocks

    // Memory address 0 represents the number of quarters that have been inserted;
    // set it to 2 to play for free.
    // Beat the game by breaking all the blocks.
    // What is your score after the last block is broken?
    let Part2() = runWith [ 0L, 2L ] |> fun x -> x.Score