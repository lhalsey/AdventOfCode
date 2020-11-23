namespace AdventOfCode.Days.Y2019

open AdventOfCode.Shared.Utility
open FSharp.Collections.ParallelSeq
open System.Collections.Generic

/// Day 6: Universal Orbit Map
/// https://adventofcode.com/2019/day/6
/// You've landed at the Universal Orbit Map facility on Mercury.
module Day06 =

    let [<Literal>] You = "YOU"
    let [<Literal>] Santa = "SAN"

    // E.g. "ZM3)Q4Q" -> Q4Q is in orbit around ZM3
    let parse (orbit: string) =
        orbit
        |> split ')'
        |> fun x -> x.[1], x.[0]

    let getOrbit (orbitMap: IReadOnlyDictionary<_,_>) obj =
        match orbitMap.TryGetValue obj with
        | true, o -> Some (obj, o)
        | _ -> None
        
    let getOrbits orbitMap obj = obj |> Seq.unfold (getOrbit orbitMap)

    let getInput() = getFile (2019, 6) |> readLinesAs parse

    //         G - H       J - K - L
    //        /           /
    // COM - B - C - D - E - F
    //                \
    //                 I
    // D directly orbits C and indirectly orbits B and COM, a total of 3 orbits.
    // L directly orbits K and indirectly orbits J, E, D, C, B, and COM, a total of 7 orbits.
    // What is the total number of direct and indirect orbits in your map data?
    let Part1() =
        let orbits = getInput()
        let orbitMap = orbits |> readOnlyDict

        orbits
        |> PSeq.sumBy (fun (obj, _) -> getOrbits orbitMap obj |> Seq.length)

    // What is the minimum number of orbital transfers required to move from the object
    // YOU are orbiting to the object SAN is orbiting?
    // (Between the objects they are orbiting - not between YOU and SAN.)
    let Part2() =

        let orbitMap = getInput() |> readOnlyDict

        let getOrbitTransferCount obj =
            obj
            |> getOrbits orbitMap
            |> Seq.skip 1
            |> Seq.mapi (fun transfers obj -> obj, transfers)

        let santaOrbits = getOrbitTransferCount Santa |> readOnlyDict
        let yourOrbits = getOrbitTransferCount You

        let (firstCommonObj, transfers) =
            yourOrbits
            |> Seq.find (fun (obj, _) -> santaOrbits.ContainsKey obj)

        santaOrbits.[firstCommonObj] + transfers