namespace AdventOfCode.Days.Y2020

/// Day 15: Rambunctious Recitation
/// https://adventofcode.com/2020/day/15
/// You catch the airport shuttle and try to book a new flight to your vacation island.
module Day15 =

    let Input = [ 20; 0; 1; 11; 6; 3 ]

    // This is essentially Van Eck Sequence and I don't think we have any choice but to brute force
    let getNthNumber n =
        // Don't love the mutability, but ...
        // Map takes over 1 minute, Dictionary is ~2 seconds and array is ~500ms
        let timeMap = Array.zeroCreate n

        // Use one-based timestamps so zero indicates value not spoken so far
        Input |> List.iteri (fun i x -> timeMap.[x] <- i + 1)
        
        let rec getNthNumberR (last: int) (t: int) =
            if t = n then last else
            let next = match timeMap.[last] with 0 -> 0 | x -> t - x
            timeMap.[last] <- t
            getNthNumberR next (t + 1)

        getNthNumberR 0 (Input.Length + 1) // After starting numbers, first number is zero
 
    
    // Given your starting numbers, what will be the 2020th number spoken?
    let Part1() = getNthNumber 2_020

    // Given your starting numbers, what will be the 30000000th number spoken?
    let Part2() = getNthNumber 30_000_000