namespace AdventOfCode.Days.Y2018

open AdventOfCode.Shared.Utility

/// Day 12: Subterranean Sustainability
/// https://adventofcode.com/2018/day/12
/// The year 518 is significantly more underground than your history books implied.
module Day12 =

    let [<Literal>] FiftyBillion = 50_000_000_000L

    let parseInput() =
        let lines = getFile (2018, 12) |> readAllLines

        // E.g. "initial state: #....##.#."
        let initial = lines.[0] |> splitOn "initial state: " |> fun x -> x.[1]

        // E.g ".#.## => #"
        let rules = lines.[2..] |> Array.map (splitIntoPair " => ") |> readOnlyDict

        initial, rules

    let getRow n =
        let (initial, rules) = parseInput()

        // Pad string with empty cells, find the character mapping for each window of
        // 5 chars and keep track of start index so we can calculate pot locations later
        let getNext ((row: string), (startIndex: int)) =
            let row =
                "...." + row + "...."
                |> Seq.windowed 5
                |> Seq.map (fun x -> rules.[charsToStr x])
                |> String.concat ""

            let offset = row.Length - row.TrimStart('.').Length

            row.Trim('.'), (startIndex + 2) - offset

        (initial, 0)
        |> Seq.unfold (fun x -> Some(x, getNext x))
        |> Seq.item n

    let getRowValue ((row: string), (startIndex: int)) =
        row
        |> Seq.mapi (fun i x -> if x = '#' then (i - startIndex) else 0)
        |> Seq.sum
        

    // After 20 generations, what is the sum of the numbers of all pots which contain a plant?
    let Part1() = getRow 20 |> getRowValue

    // After fifty billion (50000000000) generations, what is the sum of the numbers of all pots
    // which contain a plant?
    // By plotting the first 1000 values as a graph we can see that after 100 the numbers simply
    // increase by 15 each row so we can calculate in constant time
    let Part2() =
        let row100 = getRow 100 |> getRowValue |> int64
        row100 + (FiftyBillion - 100L) * 15L