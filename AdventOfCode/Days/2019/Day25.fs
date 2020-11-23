namespace AdventOfCode.Days.Y2019

open System
open AdventOfCode.Shared.Utility
open AdventOfCode.Shared.IntCodeInterpreter

/// Day 25: Cryostasis
/// https://adventofcode.com/2019/day/25
/// As you approach Santa's ship, your sensors report two important details:
module Day25 =

    type Mode = Interactive | AI | Playback

    type GameState = { Input: string list; Password: int64 option }

    type IOHandler =
        { GetInput: GameState -> string * GameState
          HandleOutput: GameState * string -> GameState
          InitialState: GameState }

    let getInterpreter() = getFile (2019, 25) |> Interpreter.Create

    let tryGetCode (text: string) =
        if text.Contains "airlock" then
            text
            |> split ' '
            |> Array.tryPick (fun x -> match Int64.TryParse x with (true, v) -> Some v | _ -> None)
        else None

    let play (io: IOHandler) programState =
        let rec playR (ps, gs) =

            let charsToStr = List.map char >> List.toArray >> String

            let provideInputStream input state =
                (state, input) 
                ||> Seq.fold (fun acc x -> acc |> provideInput x)

            match ps with
            | Output (output, ps) ->
                let text = charsToStr output
                let gs = { gs with Password = tryGetCode text }
                let gs = io.HandleOutput (gs, text)
                playR (ps, gs)
            | Input f ->
                let (input, gs) = io.GetInput gs
                let inputStream = input |> Seq.map int64
                let ps = provideInputStream inputStream ps
                playR (ps, gs)
            | Terminated _ ->
                io.HandleOutput (gs, "--- TERMINATED ----")

        playR (programState, io.InitialState)

    let getSolution() =
        let commandsToPressureSensor =
            [ "south"; "south"; "take hypercube"; "north"; "north"; "north"; "take tambourine";
              "east"; "take astrolabe"; "south"; "take shell"; "north"; "east"; "north";
              "take klein bottle"; "north"; "take easter egg"; "south"; "south"; "west"; "west";
              "south"; "west"; "take dark matter"; "west"; "north"; "west"; "take coin"; "south"; ]

        let allItems =
            [ "coin"; "klein bottle"; "shell"; "easter egg"; "astrolabe"; "tambourine"; "dark matter" ]

        let dropAll = allItems |> List.map (fun x -> $"drop {x}")

        let neededItems = [ "dark matter"; "tambourine"; "coin" ]

        let takeNeeded = neededItems |> List.map (fun x -> $"take {x}")

        commandsToPressureSensor @ dropAll @ takeNeeded @ ["south"]
        |> List.map (fun x -> $"{x}\n")

    let getValues (lines: string[]) (prefix: string)  =
        lines
        |> Array.skipWhile (fun x -> x <> prefix)
        |> Array.skipWhile (fun x -> x = prefix) // Don't just skip 1 as may be empty
        |> Array.takeWhile (fun x -> x.StartsWith "- ")
        |> Array.map (fun x -> x.[2..])

    let parseOutput ((gs: GameState), (output: string)) =
        let lines = output |> split '\n'
        let dirs = getValues lines "Doors here lead:"
        let items = getValues lines "Items here:"

        gs

    let getIOFunctions mode =

        let gameState = { Input = []; Password = None }

        match mode with
        | Interactive -> 
            { GetInput = fun gs -> (Console.ReadLine() + "\n", gs)
              HandleOutput = fun (gs, o) -> Console.WriteLine o; gs
              InitialState = gameState }
        | Playback ->
            { GetInput = fun gs ->
                match gs.Input with
                | h::t -> (Console.WriteLine $">> {h}"); (h, { gs with Input = t })
                | _ -> failwith "No more input"
              HandleOutput = fun (gs, o) -> Console.WriteLine o; gs 
              InitialState = { gameState with Input = getSolution() } }
        | AI -> // TODO: Finish logic
            { GetInput = fun gs -> (Console.ReadLine() + "\n", gs)
              HandleOutput = parseOutput
              InitialState = gameState }
            
            
    let run() =
        let programState = getInterpreter() |> run
        let io = getIOFunctions Playback
        let result = play io programState
        
        Option.get result.Password
        

    // Look around the ship and see if you can find the password for the main airlock.
    let Part1() = run()

    let Part2() = 1L // Align Warp Drive!