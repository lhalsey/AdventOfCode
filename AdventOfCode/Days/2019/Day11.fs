namespace AdventOfCode.Days.Y2019

open AdventOfCode.Shared.Utility
open AdventOfCode.Days.Y2019.Shared.IntCodeInterpreter
open AdventOfCode.Shared

/// Day 11: Space Police
/// https://adventofcode.com/2019/day/11
/// On the way to Jupiter, you're pulled over by the Space Police.
module Day11 =

    let [<Literal>] Black = 0L
    let [<Literal>] White = 1L

    let getDirection = function
        | 0L -> Direction2d.TurnLeft
        | 1L -> Direction2d.TurnRight
        | x -> failwithf "Invalid direction value: %i" x

    type PaintBot = { PaintMap: Map<Point2d, int64>; Location: Point2d; Direction: Direction2d } with

        member __.Current = // Colour of current panel, black if we have never visited
            match __.PaintMap.TryFind __.Location with Some v -> v | _ -> Black

        member __.Update col dir = // Paint current panel, turn & move forward one panel
            let map = __.PaintMap.Add (__.Location, col)
            let direction = __.Direction |> getDirection dir
            let loc = __.Location + direction

            { PaintMap = map; Location = loc; Direction = direction }


    let rec getPaintMap state (paintBot: PaintBot) =
        match state with
        | Terminated _ -> paintBot.PaintMap // Finished painting, return map
        | Input f -> // Supply colour of current panel and resume execution
            let state' = f paintBot.Current 
            getPaintMap state' paintBot
        | Output (col::dir::[], state') -> // Read 2 outputs(colour & direction), update paintBot and resume
            let paintBot = paintBot.Update col dir
            getPaintMap state' paintBot
        | _ -> failwithf "Invalid state: %A" state
            
    let getInterpreter() = getFile(2019, 11) |> Interpreter.Create

    let runWithInput input =
        let programState = 
            getInterpreter()
            |> run
            |> provideInput (int64 input) // Colour of initial cell

        let paintBot = { PaintMap = Map.empty; Location = Point2d.Origin; Direction = Direction2d.North }

        getPaintMap programState paintBot


    // Build a new emergency hull painting robot and run the Intcode program on it.
    // How many panels does it paint at least once?
    let Part1() = runWithInput Black |> Map.count

    // After starting the robot on a single white panel instead, what registration
    // identifier does it paint on your hull?
    let Part2() = runWithInput White |> ImageProvider.getImage (function White -> '█' | _ -> ' ')