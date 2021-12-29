namespace AdventOfCode.Days.Y2021

open AdventOfCode.Shared.Utility
open AdventOfCode.Shared

/// Day 11: Dumbo Octopus
/// https://adventofcode.com/2021/day/11
/// You enter a large cavern full of rare bioluminescent dumbo octopuses!
module Day11 =

    let [<Literal>] FlashEnergyLevelThreshold = 9

    // This code was 3 times faster when I used a dictionary, but all the mutability made me sad!
    let parseInput() = getFile (2021, 11) |> parseGrid (System.Char.GetNumericValue >> int) |> Map

    let updateMap = Seq.fold (fun (map: Map<_, _>) x -> map.Add x)

    let incrementAll (energyMap: Map<Point2d, int>) =
        energyMap
        |> Seq.map (fun x -> x.Key, x.Value + 1)
        |> Map

    let incrementAdjacent (energyMap: Map<Point2d, int>) (point: Point2d) =
        let getAdjacentUnflashed (point: Point2d) =
            energyMap.TryFind point
            |> Option.filter (fun energy -> energy > 0) // Zero energy has already flashed
            |> Option.map (fun energy -> point, energy + 1)

        point.GetAllAdjacent()
        |> Seq.choose getAdjacentUnflashed
        |> Seq.append (Seq.singleton (point, 0)) // Reset flashed octopus to zero energy level
        |> updateMap energyMap

    let getNextStep (energyMap: Map<Point2d, int>) = // Increment all and cascade flashes
        let rec getNextStepR (energyMap: Map<Point2d, int>) =
            let flashers =
                energyMap
                |> Seq.filter (fun x -> x.Value > FlashEnergyLevelThreshold)
                |> Seq.map (fun x -> x.Key)
                |> Seq.toList

            match flashers with // Apply flash effects and recurse until no more flashers
            | [] -> energyMap
            | _ -> (energyMap, flashers)
                   ||> List.fold incrementAdjacent
                   |> getNextStepR

        energyMap |> incrementAll |> getNextStepR

    let getFlashes (map: Map<_, int>) = map |> countIf (fun x -> x.Value = 0)
        
    // Given the starting energy levels of the dumbo octopuses in your cavern, simulate 100 steps.
    // How many total flashes are there after 100 steps?
    let Part1() =
        parseInput()
        |> Seq.unfold (fun x -> Some(getFlashes x, getNextStep x))
        |> Seq.skip 1
        |> Seq.take 100
        |> Seq.sum

    // What is the first step during which all octopuses flash?
    let Part2() =
        let energyMap = parseInput()

        energyMap
        |> Seq.unfold (fun x -> Some(getFlashes x, getNextStep x))
        |> Seq.findIndex ((=) energyMap.Count)