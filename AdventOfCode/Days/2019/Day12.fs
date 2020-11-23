namespace AdventOfCode.Days.Y2019

open AdventOfCode.Shared.Utility
open System.Numerics
open MathNet.Numerics
open FSharp.Collections.ParallelSeq

/// Day 12: The N-Body Problem
/// https://adventofcode.com/2019/day/12
/// The space near Jupiter is not a very safe place; you need to be careful of a big distracting
/// red spot, extreme radiation, and a whole lot of moons swirling around.
module Day12 =

    type Moon = { Position: Vector3; Velocity: Vector3 }
  
    // E.g. "<x=-9, y=-1, z=-1>"
    let parse = function
        | Regex @"<x=(-?[\d]+), y=(-?[\d]+), z=(-?[\d]+)>" [x; y; z]
            -> { Position = Vector3(float32 x, float32 y, float32 z); Velocity = Vector3.Zero }
        | x -> failwithf "Invalid input: %s" x

    let parseInput() = getFile(2019, 12) |> readLinesAs parse |> Seq.toList

    let absSum (v3: Vector3) = abs v3.X + abs v3.Y + abs v3.Z

    let potentialEnergy (moon: Moon) = absSum moon.Position

    let kineticEnergy (moon: Moon) = absSum moon.Velocity

    let getTotalEnergy (moon: Moon) = (potentialEnergy moon * kineticEnergy moon) |> int

    // To apply gravity, consider every pair of moons. On each axis (x, y, and z),
    // the velocity of each moon changes by exactly +1 or -1 to pull the moons together.
    let getGravity (m1: Moon) (m2: Moon) =
        let x = compare m2.Position.X m1.Position.X
        let y = compare m2.Position.Y m1.Position.Y
        let z = compare m2.Position.Z m1.Position.Z

        Vector3(float32 x, float32 y, float32 z)

    let updateMoon (moon: Moon, moons: Moon list) =

        let velocityChange = moons |> List.sumBy (getGravity moon)
        let velocity = moon.Velocity + velocityChange

        { Position = moon.Position + velocity; Velocity = velocity }

    let updateMoons moons =
        partitionSingle moons [] // Compare each moon against all other moons
        |> Seq.toList
        |> List.map updateMoon

    let sumTotalEnergy moons steps =
        moons
        |> Seq.unfold (fun state -> Some(state, updateMoons state))
        |> Seq.item steps
        |> Seq.sumBy getTotalEnergy

    let getCycleLength (moons: Moon list) dimensionF =

        let pred = List.forall (fun moon -> (dimensionF moon.Velocity) = 0.0f)

        moons
        |> Seq.unfold (fun state -> Some(state, updateMoons state))
        |> Seq.skip 1 // Skip start state as it also has zero velocity
        |> Seq.findIndex pred
        |> fun x -> int64 x + 1L // Add one as we skip start state

    // What is the total energy in the system after simulating the moons given
    // in your scan for 1000 steps?
    let Part1() =
        let moons = parseInput()

        sumTotalEnergy moons 1_000


    // How many steps does it take to reach the first state that exactly matches a previous state?
    let Part2() =
        let moons = parseInput()

        // Recognise that the X, Y and Z values are independent of each other
        // We need to find a cycle in each and then find when these cycles coincide via the LCM
        [ (fun (m: Vector3) -> m.X); (fun m -> m.Y); (fun m -> m.Z) ]
        |> PSeq.map (getCycleLength moons)
        |> Seq.toArray
        |> Euclid.LeastCommonMultiple
        |> (*) 2L // Velocity first reaches zero at halfway point so double