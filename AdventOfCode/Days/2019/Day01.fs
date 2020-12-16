namespace AdventOfCode.Days.Y2019

open AdventOfCode.Shared.Utility

/// Day 1: The Tyranny of the Rocket Equation
/// https://adventofcode.com/2019/day/1
/// Santa has become stranded at the edge of the Solar System while delivering presents
/// to other planets!
module Day01 =

    // To find the fuel required for a module, take its mass,
    // divide by three, round down, and subtract 2
    let moduleFuel mass = mass / 3 - 2

    // For each module mass, calculate its fuel and add it to the total.
    // Then, treat the fuel amount you just calculated as the input mass
    // and repeat the process, continuing until a fuel requirement is zero or negative
    let rec totalFuel mass =
        match moduleFuel mass with
        | fuel when fuel <= 0 -> 0
        | fuel -> fuel + totalFuel fuel

    let parseInput() = getFile (2019, 1) |> readLinesAs int

    // What is the sum of the fuel requirements for all of the modules on your spacecraft?
    let Part1() = parseInput() |> Seq.sumBy moduleFuel

    // What is the sum of the fuel requirements for all of the modules on your spacecraft when
    // also taking into account the mass of the added fuel?
    let Part2() = parseInput() |> Seq.sumBy totalFuel