namespace AdventOfCode.Days.Y2015

/// Day 20: Infinite Elves and Infinite Houses
/// https://adventofcode.com/2015/day/20
/// To keep the Elves busy, Santa has them deliver some presents by hand, door-to-door.
module Day20 =

    let TargetPresents = 33_100_000

    let deliver upper elfMultiplier maxHouses =
        let presents = Array.zeroCreate (upper + 1)

        let rec deliverR elf =
            let numPresents = elf * elfMultiplier

            // If find target can return early as no previous houses will receive any more presents
            match presents.[elf] + numPresents with
            | p when p >= TargetPresents -> elf 
            | _ ->
                // Deliver to all houses that are multiple of elf number
                // Note that using "for house in [elf .. elf .. upperBound] do" was ~10x slower due
                // to F# not having special handling for quick iteration in this case!
                // https://stackoverflow.com/questions/59096348/write-a-for-loop-that-increments-the-index-twice
                let rec deliverTo house delivered =
                    presents.[house] <- presents.[house] + numPresents
                    let next = house + elf

                    match next, maxHouses with
                    | n, _ when n > upper -> () // No point delivering beyond upper bound
                    | _, Some max when delivered >= max -> () // Our work here is done!
                    | _ -> deliverTo next (delivered + 1) // Keep on truckin'!

                deliverTo elf 0

                deliverR (elf + 1)

        deliverR 1

    let getLowestHouseNumber elfMultiplier maxHouses =
        
        let upper = // Powers of 2 have 2n - 1 presents
            1
            |> Seq.unfold (fun x -> Some(x, x + x))
            |> Seq.find (fun x -> (x * 2 - 1) * elfMultiplier >= TargetPresents)

        deliver upper elfMultiplier maxHouses


    // What is the lowest house number of the house to get at least as many presents
    // as the number in your puzzle input?
    let Part1() = getLowestHouseNumber 10 None

    // Elves stop after delivering 50 presents and deliver 11 times their number.
    // With these changes, what is the new lowest house number of the house to get at
    // least as many presents as the number in your puzzle input?
    let Part2() = getLowestHouseNumber 11 (Some 50)