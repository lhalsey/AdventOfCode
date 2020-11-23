namespace AdventOfCode.Days.Y2019

open AdventOfCode.Shared.Utility
open AdventOfCode.Shared
open AdventOfCode.Shared.IntCodeInterpreter

/// Day 15: Oxygen System
/// https://adventofcode.com/2019/day/15
/// Out here in deep space, many things can go wrong.
module Day15 =

    type RepairBot =
        { Steps: int
          Location: Point2d
          Direction: Direction2d } with

          member __.Move() = { __ with Steps = __.Steps + 1; Location = __.Location + __.Direction }

    type State = { Bot: RepairBot; ProgramState: ProgramState; FoundOxygen: bool }

    type Feedback = Wall | Valid | ValidWithOxygen

    let getFeedback = function 0L -> Wall | 1L -> Valid | 2L -> ValidWithOxygen | _ -> failwith "Invalid"

    let getInterpreter() = getFile(2019, 15) |> Interpreter.Create

    let processOutput output (state: State) = 
        match getFeedback output with
        | Wall -> None
        | Valid -> Some { state with Bot = state.Bot.Move() }
        | ValidWithOxygen -> Some { state with Bot = state.Bot.Move(); FoundOxygen = true }

    let getOutput (state: State) =
        match state.ProgramState with
        | Output (output::[], programState) ->
            { state with ProgramState = programState } |> processOutput output
        | x -> failwithf "Expected single item output, but got %A" x

    let getValidMoves state f =

        let isNewDirection dir = state.Bot.Direction + dir <> Direction2d.Zero
        
        let directions =
            [ (Direction2d.North, 1L)
              (Direction2d.South, 2L)
              (Direction2d.West,  3L)
              (Direction2d.East,  4L) ]

        // Try all directions except going back the way we just came
        directions
        |> List.filter (fun (dir, _) -> isNewDirection dir)
        |> List.map (fun (dir, dirId) ->
            { state with Bot = { state.Bot with Direction = dir }; ProgramState = f dirId })
        |> List.choose getOutput

    let rec getAdjacentCells (state: State) =
        match state.ProgramState with
        | Terminated _ -> []
        | Input f -> getValidMoves state f
        | _ -> failwithf "Expected input or termination, but got %A" state

    let runWith programState =
        let repairBot =
            { Steps = 0
              Location = Point2d.Origin
              Direction = Direction2d.Zero }

        [ { Bot = repairBot; ProgramState = programState; FoundOxygen = false } ]
        |> breadthFirstSearch getAdjacentCells

    let findOxygen() =
        let state = getInterpreter() |> run
        runWith state |> Seq.find (fun state -> state.FoundOxygen)

    // What is the fewest number of movement commands required to move the repair droid
    // from its starting position to the location of the oxygen system?
    let Part1() = findOxygen().Bot.Steps

    // Use the repair droid to get a complete map of the area. How many minutes will it take
    // to fill with oxygen?
    let Part2() =
        // Start from oxygen tank and see what the furthest point we can reach is
        findOxygen().ProgramState
        |> runWith
        |> Seq.map (fun state -> state.Bot.Steps)
        |> Seq.max