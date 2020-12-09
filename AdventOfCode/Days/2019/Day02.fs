namespace AdventOfCode.Days.Y2019

open AdventOfCode.Shared.Utility
open AdventOfCode.Days.Y2019.Shared.IntCodeInterpreter

/// Day 2: 1202 Program Alarm
/// https://adventofcode.com/2019/day/2
/// On the way to your gravity assist around the Moon, your ship computer beeps angrily
/// about a 1202 program alarm.
module Day02 =

    let run (noun, verb) interpreter = 
        interpreter
        |> setMemory [1L, noun; 2L, verb]
        |> run
        |> function
            | Terminated i -> i |> getMemory 0L
            | _ -> failwith "Failed to terminate"

    let getInterpreter() = getFile (2019, 2) |> Interpreter.Create 

    // The first step is to restore the gravity assist program (your puzzle input)
    // to the "1202 program alarm" state it had just before the last computer caught fire.
    // To do this, before running the program, replace position 1 with the value 12
    // and replace position 2 with the value 2.
    // What value is left at position 0 after the program halts?
    let Part1() = getInterpreter() |> run (12L, 2L)

    // The inputs should still be provided to the program by replacing the values at addresses 1 and 2,
    // just like before. In this program, the value placed in address 1 is called the noun, and the
    // value placed in address 2 is called the verb.
    // Each of the two input values will be between 0 and 99, inclusive.
    // Find the input noun and verb that cause the program to produce the output 19690720.
    // What is 100 * noun + verb? (For example, if noun=12 and verb=2, the answer would be 1202.)
    let Part2() =
        let Target = 19_690_720L
        let interpreter = getInterpreter()
        
        let range = seq { 0L..99L }

        Seq.allPairs range range // Running in parallel is marginally slower
        |> Seq.find (fun (noun, verb) -> interpreter |> run (noun, verb) = Target)
        |> fun (noun, verb) -> 100L * noun + verb