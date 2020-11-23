namespace AdventOfCode.Days.Y2019

open System
open AdventOfCode.Shared.Utility
open AdventOfCode.Shared.IntCodeInterpreter

/// Day 21: Springdroid Adventure
/// https://adventofcode.com/2019/day/21
/// You lift off from Pluto and start flying in the direction of Santa.
module Day21 =

    let getInterpreter() = getFile (2019, 21) |> Interpreter.Create
    
    let runWithInput input =
        let (_, state) =
            getInterpreter()
            |> run
            |> readOutput // "Input instructions:\n"

        let (output, _) =
            (state, input)
            ||> Seq.fold (fun acc x -> acc |> provideInput (int64 x))
            |> readOutput

        let result = output |> List.map char |> List.toArray |> String

        List.last output


    // Program the springdroid with logic that allows it to survey the hull without falling into space.
    // What amount of hull damage does it report?
    // Worked out by hand
    // TODO: Automate this
    // Jump if imminent hole or I can land safely after hole
    // !A || (D && !C)
    let Part1() = runWithInput "NOT C J\nAND D J\nNOT A T\nOR T J\nWALK\n"

    // Jump if hole within 3 and 4 is safe and not then forced to make suicidal jump
    // !(A || B || C) && D && (E || H)
    let Part2() = runWithInput "OR A T\nAND B T\nAND C T\nNOT T J\nAND D J\nOR E T\nOR H T\nAND T J\nRUN\n"