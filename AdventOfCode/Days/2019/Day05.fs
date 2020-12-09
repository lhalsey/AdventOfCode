namespace AdventOfCode.Days.Y2019

open AdventOfCode.Shared.Utility
open AdventOfCode.Days.Y2019.Shared.IntCodeInterpreter

/// Day 5: Sunny with a Chance of Asteroids
/// https://adventofcode.com/2019/day/5
/// You're starting to sweat as the ship makes its way toward Mercury.
module Day05 =

    let getInterpreter() = getFile (2019, 5) |> Interpreter.Create

    let runWithInput input = 
        getInterpreter()
        |> run
        |> provideInput input
        |> readFinalOutput

    // After providing 1 to the only input instruction and passing
    // all the tests, what diagnostic code does the program produce?
    let Part1() = runWithInput 1L

    // What is the diagnostic code for system ID 5?
    let Part2() = runWithInput 5L