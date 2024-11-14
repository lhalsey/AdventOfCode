namespace AdventOfCode.Days.Y2023

open AdventOfCode.Shared.Utility
open System.Collections.Generic

/// Day 8: Haunted Wasteland
/// https://adventofcode.com/2023/day/8
/// You're still riding a camel across Desert Island when you spot a sandstorm quickly approaching.
module Day08 =

    type Problem = { Directions: char seq; NodeDict: IReadOnlyDictionary<string, string * string> }

    let parse = function // E.g. AAA = (BBB, CCC)
        | Regex "(\w+) = \((\w+), (\w+)\)" [ label; l ; r ] -> (label, (l, r))
        | x -> failwithf "Invalid input: %s" x

    let parseInput() = getFile (2023, 8) |> readLines

    let getProblem() =
        let input = parseInput()

        let directions = Seq.head input
        let nodes = input |> Seq.skip 2 |> Seq.map parse

        { Directions = repeatInfinite directions; NodeDict = readOnlyDict nodes }

    let getSteps problem startNode =

        let move currNode dir = 
            let (left, right) = problem.NodeDict[currNode]

            if dir = 'L' then left else right

        (startNode, problem.Directions)
        ||> Seq.scan move
        |> Seq.findIndex (fun x -> x.EndsWith "Z")


    // Starting at AAA, follow the left/right instructions. How many steps are required to reach ZZZ?
    let Part1() = getSteps (getProblem()) "AAA"

    // Simultaneously start on every node that ends with A.
    // How many steps does it take before you're only on nodes that end with Z?
    let Part2() =
        let problem = getProblem()

        let startNodes =
            problem.NodeDict.Keys 
            |> Seq.filter (fun x -> x.EndsWith "A")
            |> Seq.toArray

        let cycles = startNodes |> Array.map ((getSteps problem) >> int64)

        MathNet.Numerics.Euclid.LeastCommonMultiple cycles