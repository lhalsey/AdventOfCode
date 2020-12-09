namespace AdventOfCode.Days.Y2019

open System

open AdventOfCode.Shared
open AdventOfCode.Shared.Utility
open AdventOfCode.Days.Y2019.Shared.IntCodeInterpreter

/// Day 17: Set and Forget
/// https://adventofcode.com/2019/day/17
/// An early warning system detects an incoming solar flare and automatically activates the ship's
/// electromagnetic shield.
module Day17 =

    type VacBot = { Location: Point2d; Direction: Direction2d }

    let getInterpreter() = getFile(2019, 17) |> Interpreter.Create
    
    let readOutput = function
        | Output (output, state') -> (output, state')
        | state -> failwithf "Expected output, but got %A" state

    let getMap memoryValues =
        
        let (output, state) =
            getInterpreter()
            |> setMemory memoryValues
            |> run
            |> readOutput

        let map =
            output
            |> List.map char
            |> List.toArray
            |> String

        (map, state)

    let parseMap map =
        let cells =
            map
            |> split '\n'
            |> Array.mapi (fun row rowStr -> rowStr |> Seq.mapi (fun col c -> (c, { X = col; Y = row })))
            |> Seq.concat
            |> Seq.groupBy fst
            |> Seq.map (fun (k, v) -> (k, v |> Seq.map snd))
            |> readOnlyDict

        let start = cells.['^'] |> Seq.head
        let scaffold = set cells.['#']

        (start, scaffold)

    let getIntersectionSum() =
        let (map, _) = getMap []

        let (_, scaffold) = parseMap map

        // Point is intersection if surrounded by scaffold in all directions
        let isIntersection (point: Point2d) = 
            [ Direction2d.North; Direction2d.East; Direction2d.South; Direction2d.West ]
            |> List.forall (fun dir -> point + dir |> scaffold.Contains)

        scaffold
        |> Seq.filter isIntersection
        |> Seq.sumBy (fun p -> p.X * p.Y)

    // Traverse the scaffold to the end and return a sequence of (direction, distance) pairs
    let getDirections (scaffold: Point2d Set) (bot: VacBot) =
        let rec getDirectionsR bot (dir, dist) = 
            seq {
                let straight = { bot with Location = bot.Location + bot.Direction }

                let turnLeft = Direction2d.TurnLeft bot.Direction
                let left = { bot with Location = bot.Location + turnLeft; Direction = turnLeft }

                let turnRight = Direction2d.TurnRight bot.Direction
                let right = { bot with Location = bot.Location + turnRight; Direction = turnRight }

                // Go straight if we can and increase distance count
                if scaffold.Contains straight.Location then
                    yield! getDirectionsR straight (dir, dist + 1)

                // Otherwise yield previous move, turn left and reset distance count
                else if scaffold.Contains left.Location then
                    if dist > 1 then yield (dir, dist)
                    yield! getDirectionsR left ('L', 1)

                // Otherwise yield previous move, turn right and reset distance count
                else if scaffold.Contains right.Location then
                    if dist > 1 then yield (dir, dist)
                    yield! getDirectionsR right ('R', 1)

                // Otherwise we have found the end so yield final move
                else yield (dir, dist)
                }

        getDirectionsR bot ('?', 1)


    // Derive three functions (A, B & C) which represent a series of movements (e.g. 10,L,8,R,6)
    // with 20 char max per function. It must be possible to form a main function from these movement
    // functions (e.g. A,B,C,B,A,C) to reach the end of the scaffold.
    let getFunctions directions =

        let MaxFunctions = 3 // A, B, C
        let MaxDirections = 5 // E.g. 5 x "10R," = 20 chars

        // Recursively try different combinations of using existing functions, extending functions
        // and creating new functions until we can match all the movements
        let rec getFunctionsR
            (dirs: (char * int) list)
            (functions: (char * int) list list)
            (indexes: int list) =
            seq {
                match dirs, functions with
                | [], fns -> yield (List.rev fns, List.rev indexes) // Success!
                | h::t, [] -> yield! getFunctionsR t [[h]] (0::indexes) // No functions, start a new one
                | h::t, f::fns -> 
                    for i in [0..functions.Length - 1] do // Use existing function if we can
                        let fn = functions.[i]
                        
                        let matchLength = // Match as many directions as we can
                            fn
                            |> Seq.zip dirs
                            |> Seq.takeWhile (fun (x, y) -> x = y)
                            |> Seq.length

                        if matchLength = fn.Length then // Next steps all match current function
                            let index = functions.Length - 1 - i
                            yield! getFunctionsR (dirs |> List.skip matchLength) functions (index::indexes)

                    if f.Length < MaxDirections then // Add to end of existing list if not too long 
                        yield! getFunctionsR t ((f @ [h])::fns) indexes

                    if functions.Length < MaxFunctions then // Start new function list
                        yield! getFunctionsR t ([h]::functions) (functions.Length::indexes)
            }

        getFunctionsR directions [] []


    // Traverse scaffold and then determine what input to pass to interpreter
    let getInput map =
        
        let (start, scaffold) = parseMap map
        let vacBot = { Location = start; Direction = Direction2d.North }
        let directions = getDirections scaffold vacBot |> Seq.toList

        // We need to condense the route we have just traversed into 3 movement functions
        let (functions, indexes) = getFunctions directions |> Seq.head
        
        let routine =
            indexes
            |> List.map (fun x -> "ABC".[x] |> string)
            |> String.concat ","
        
        let getFunction f =
            f
            |> List.map (fun (dir, dist) -> $"{dir},{dist}")
            |> String.concat ","
        
        let fns = functions |> List.map getFunction 
        
        routine::fns @ ["n\n"] // No to video feed
        |> String.concat "\n"
        |> Seq.map int64
        |> Seq.toList

    // Enter main routine followed by functions A, B & C all converted to int64 sequences
    let runWith memoryValues =
            
        let rec run (state, output) input =
            match state, input with
            | Input f, h::t -> run (f h, output) t
            | Output (o, s), _ -> run (s, o::output) input
            | _ -> output

        let (map, state) = getMap memoryValues
        let input = getInput map

        // Final output value is amount of dust collected
        run (state, []) input |> List.head  |> Seq.last


    // What is the sum of the alignment parameters for the scaffold intersections?
    let Part1() = getIntersectionSum()

    // Force the vacuum robot to wake up by changing the value in your ASCII program
    // at address 0 from 1 to 2.
    // After visiting every part of the scaffold at least once, how much dust does the
    // vacuum robot report it has collected?
    let Part2() = runWith [(0L, 2L)]