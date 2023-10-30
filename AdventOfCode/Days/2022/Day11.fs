namespace AdventOfCode.Days.Y2022

open AdventOfCode.Shared.Utility

/// Day 11: Monkey in the Middle
/// https://adventofcode.com/2022/day/11
/// As you finally start making your way upriver, you realize your pack is much lighter than you remember.
module Day11 =

    type Monkey = {
        Id: int
        Items: int64 list
        Inspections: int
        Operation: int64 -> int64
        TestDivisor: int64
        TrueMonkey: int
        FalseMonkey: int }

    let getId = function
        | Regex "Monkey (\d+):" [Int i] -> i
        | x -> failwithf "Invalid id %s" x

    let getItems = function
        | Regex "Starting items: (.*)" [x] -> x |> splitOn "," |> Array.map int64 |> Array.toList
        | x -> failwithf "Invalid items: %s" x

    let getOperation = function
        | Regex "Operation: new = old \* old" [] -> fun x -> Checked.(*) x x
        | Regex "Operation: new = old \* (\d+)" [Int64 i] -> fun x -> Checked.(*) x i
        | Regex "Operation: new = old \+ (\d+)" [Int64 i] -> fun x -> Checked.(+) x i
        | x -> failwithf "Invalid operation %s" x

    let getTest = function
        | Regex "Test: divisible by (\d+)" [Int i] -> i
        | x -> failwithf "Invalid test %s" x

    let getTrue = function
        | Regex "If true: throw to monkey (\d+)" [Int i] -> i
        | x -> failwithf "Invalid true %s" x

    let getFalse = function
        | Regex "If false: throw to monkey (\d+)" [Int i] -> i
        | x -> failwithf "Invalid false %s" x

    let parse (rows: string[]) =
        { 
            Id = getId rows[0]
            Items = getItems rows[1]
            Inspections = 0
            Operation = getOperation rows[2]
            TestDivisor = getTest rows[3]
            TrueMonkey = getTrue rows[4]
            FalseMonkey = getFalse rows[5]
        }

    let parseInput() =
        getFile (2022, 11)
        |> readLines
        |> Seq.map trim
        |> splitBy ""
        |> Seq.map (Seq.toArray >> parse)
        |> Seq.map (fun x -> x.Id, x)
        |> Map

    let simulateRound (superDivisor: int64) (worryF: int64 -> int64) (state: Map<int, Monkey>) =
        let rec simulateRoundR (currMonkeyId: int) (state: Map<int, Monkey>) =
            let monkey = state[currMonkeyId]

            match monkey.Items with
            | [] -> if currMonkeyId = state.Count - 1 then state else simulateRoundR (currMonkeyId + 1) state
            | h::t -> 
                let worry = h |> monkey.Operation |> worryF
                let target = if worry % monkey.TestDivisor = 0 then state[monkey.TrueMonkey] else state[monkey.FalseMonkey]
                
                state
                |> Map.add currMonkeyId { monkey with Items = t; Inspections = monkey.Inspections + 1 }
                |> Map.add target.Id { target with Items = target.Items @ [worry % superDivisor] }
                |> simulateRoundR currMonkeyId

        simulateRoundR 0 state

    let simulateRounds numRounds worryF =
        let state = parseInput()

        // We can mod by the super divisor to keep the numbers small while getting the same divisible results
        let superDivisor = state.Values |> Seq.map (fun x -> x.TestDivisor) |> Seq.reduce (*)

        (state, seq [1..numRounds])
        ||> Seq.fold (fun acc _ -> simulateRound superDivisor worryF acc)
        |> Seq.map (fun x -> x.Value.Inspections)
        |> partialSortDesc 2
        |> Seq.map int64
        |> Seq.reduce (*)

    // Figure out which monkeys to chase by counting how many items they inspect over 20 rounds.
    // What is the level of monkey business after 20 rounds of stuff-slinging simian shenanigans?
    let Part1() = simulateRounds 20 (fun x -> x / 3L)
       
    // Starting again from the initial state in your puzzle input,
    // what is the level of monkey business after 10000 rounds?
    let Part2() = simulateRounds 10_000 id