namespace AdventOfCode.Days.Y2019

open System

open AdventOfCode.Shared.Utility

/// Day 14: Space Stoichiometry
/// https://adventofcode.com/2019/day/14
/// As you approach the rings of Saturn, your ship's low fuel indicator turns on.
module Day14 =

    let [<Literal>] Ore = "ORE"
    let [<Literal>] Fuel = "FUEL"

    type Name = string
    type Quantity = int64
    type Chemical = { Name: Name; Quantity: Quantity }

    let getChemical = function
        | qty::chem::_ -> { Name = chem; Quantity = int64 qty}
        | x            -> failwithf "Invalid input: %A" x

    // E.g. "1 RNQHX, 1 LFKRJ, 1 JNGM => 8 DSRGV"
    let getReaction (s: string) =
        let chemicals =
            s
            |> splitAny "=> ,"
            |> Array.toList
            |> List.chunkBySize 2 
            |> List.map getChemical
            |> List.rev

        let outputChemical = List.head chemicals
        let inputChemicals = List.tail chemicals
        outputChemical.Name, (outputChemical.Quantity, inputChemicals)

    let getReactionMap() = getFile (2019, 14) |> readLinesAs getReaction |> Map

    let calculateOre fuelQty =
        let inventory = Map [Fuel, -fuelQty; Ore, 0L]
        let reactionMap = getReactionMap()

        let getRequirements inv =
            inv
            |> Map.toList
            |> List.tryFind (fun (chem, quantity) -> quantity < 0L && chem <> Ore)

        let update (chem, qty) (inv: Map<Name, Quantity>) =
            let currQty = inv.TryFind chem |> Option.defaultValue 0L
            inv.Add (chem, currQty + qty)

        let create inv (reqChem, reqQty) =
            let (outputQty, chems) = reactionMap.[reqChem]
            let units = (decimal -reqQty) / (decimal outputQty) |> Math.Ceiling |> int64

            let inv =
                (inv, chems) // Decrement required chemicals quantity and increment created
                ||> List.fold (fun acc c -> acc |> update (c.Name, -c.Quantity * units))
                |> update (reqChem, units * outputQty)

            (-inv.[Ore], inv) // Return ore used & updated inventory

        // Get requirements and create first chemical if any
        let updateInventory inv = getRequirements inv |> Option.map (create inv)

        // Keep replacing requirements with constituent chemicals until we are left with just ore
        inventory
        |> Seq.unfold updateInventory
        |> Seq.last


    let rec binarySearch pred lower upper =
        let mid = (lower + upper) / 2L

        if lower = mid then mid 
        else if pred mid then binarySearch pred mid upper
        else binarySearch pred lower mid


    // Given the list of reactions in your puzzle input, what is the minimum amount of ORE
    // required to produce exactly 1 FUEL?
    let Part1() = calculateOre 1L

    // Given 1 trillion ORE, what is the maximum amount of FUEL you can produce?
    let Part2() =
        let Target = 1_000_000_000_000L
        let lower = Target / (calculateOre 1L)
        let upper = lower * 2L
        let f x = calculateOre x < Target

        binarySearch f lower upper