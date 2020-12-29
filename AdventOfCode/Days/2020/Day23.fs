namespace AdventOfCode.Days.Y2020

/// Day 23: Crab Cups
/// https://adventofcode.com/2020/day/23
/// The small crab challenges you to a game!
module Day23 =

    let parseInput() =
        "459672813"
        |> Seq.toList
        |> List.map (string >> int)

    let play (moves: int) (input: int list)  =
        let minCup = List.min input
        let maxCup = List.max input
        let nextCup = [| 0 .. maxCup |]

        // Create array in which each element indicates which cup is next
        // This allows us to efficiently insert the picked up cups
        input @ [input.Head]
        |> List.pairwise
        |> List.iter (fun (x, y) -> nextCup.[x] <- y)

        // The crab selects a destination cup: the cup with a label equal to the current cup's
        // label minus one. If this would select one of the cups that was just picked up, the
        // crab will keep subtracting one until it finds a cup that wasn't just picked up.
        // If at any point in this process the value goes below the lowest value on any cup's label,
        // it wraps around to the highest value on any cup's label instead.
        let rec playR curr n =
            let cup1 = nextCup.[curr]
            let cup2 = nextCup.[cup1]
            let cup3 = nextCup.[cup2]

            let dest =
                curr
                |> Seq.unfold (fun x -> if x = minCup then Some(maxCup, maxCup) else Some(x - 1, x - 1))
                |> Seq.find (fun x -> x <> cup1 && x <> cup2 && x <> cup3)

            nextCup.[curr] <- nextCup.[cup3] // Current cup points to cup after last picked up cup
            nextCup.[cup3] <- nextCup.[dest] // Last picked up cup points to cup after destination
            nextCup.[dest] <- cup1 // Destination cup points to first picked up cup

            match n with
            | m when m = moves -> 1 |> Seq.unfold (fun x -> Some(nextCup.[x], nextCup.[x]))
            | _ -> playR nextCup.[curr] (n + 1)

        playR input.Head 1


    // Using your labeling, simulate 100 moves. What are the labels on the cups after cup 1?
    let Part1() =
        let Moves = 100

        parseInput()
        |> play Moves
        |> Seq.takeWhile ((<>) 1)
        |> Seq.reduce (fun acc x -> acc * 10 + x)

    // Your labeling is still correct for the first few cups; after that, the remaining cups are just
    // numbered in an increasing fashion starting from the number after the highest number in your list
    // and proceeding one by one until one million is reached. The crab is going to do ten million moves!
    // Determine which two cups will end up immediately clockwise of cup 1. What do you get if you multiply
    // their labels together?
    let Part2() =
        let Moves = 10_000_000
        let Max = 1_000_000

        let input = parseInput()
        let n = (List.max input) + 1
        let input = input @ [ n .. Max]

        input
        |> play Moves
        |> Seq.take 2
        |> Seq.map int64
        |> Seq.reduce (*)