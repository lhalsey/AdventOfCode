namespace AdventOfCode.Days.Y2019

open AdventOfCode.Shared.Utility
open AdventOfCode.Days.Y2019.Shared.IntCodeInterpreter

/// Day 9: Sensor Boost
/// https://adventofcode.com/2019/day/9
/// You've just said goodbye to the rebooted rover and left Mars when you receive
/// a faint distress signal coming from the asteroid belt.
module Day09 =

    let getInterpreter() = getFile(2019, 9) |> Interpreter.Create

    let runWithInput input = 
        getInterpreter()
        |> run
        |> provideInput input
        |> readFinalOutput

    // Once your Intcode computer is fully functional, the BOOST program should
    // report no malfunctioning opcodes when run in test mode; it should only output
    // a single value, the BOOST keycode. What BOOST keycode does it produce?
    let Part1() = runWithInput 1L

    // The program runs in sensor boost mode by providing the input instruction
    // the value 2. Run the BOOST program in sensor boost mode. What are the
    // coordinates of the distress signal?
    let Part2() = runWithInput 2L