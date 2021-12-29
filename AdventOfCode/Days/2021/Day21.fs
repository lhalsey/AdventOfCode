namespace AdventOfCode.Days.Y2021

open AdventOfCode.Shared.Utility

/// Day 21: Dirac Dice
/// https://adventofcode.com/2021/day/21
/// There's not much to do as you slowly descend to the bottom of the ocean.
module Day21 =

    let [<Literal>] BoardSpaces = 10
    let [<Literal>] DiceRollsPerTurn = 3
    let [<Literal>] DiracDiceSides = 3

    type Player = { Space: int; Score: int }
    type State = { DiceRolls: int; Players: Player list}

    let move space roll = ((space + roll - 1) % BoardSpaces) + 1

    let roll diceRolls count =
        (diceRolls, 0)
        |> Seq.unfold (fun (d, sum) -> Some ((d, sum), (d + 1, sum + (d % 100 + 1))))
        |> Seq.item count

    let play p1Space p2Space target =
        let rec playR (state: State) =
            let (diceRolls, diceScore) = roll state.DiceRolls DiceRollsPerTurn

            match state.Players with
            | p1::ps -> let space = move p1.Space diceScore
                        let player = { Space = space; Score = p1.Score + space }
                        let state = { DiceRolls = diceRolls; Players = ps @ [ player ]}
                        if player.Score >= target
                        then state.Players.Head.Score * state.DiceRolls
                        else playR state
            | _ -> failwith "Need at least two players"

        playR { DiceRolls = 0; Players = [ { Space = p1Space; Score = 0 }; { Space = p2Space; Score = 0 }]}

    let combine f l1 l2 = List.allPairs l1 l2 |> List.map (fun (x, y) -> f x y)
         
    let rollCount = // All combinations of dice rolls with count of sum
        [1..DiracDiceSides]
        |> List.replicate DiceRollsPerTurn
        |> List.reduce (combine (+))
        |> List.countBy id
        |> List.map (fun (r, c) -> r, int64 c)

    let play2 p1Space p2Space target =
        let rec play2R (p1Space: int, p1Score: int, p2Space: int, p2Score: int, isP1ToPlay: bool) =

            let getOutcome roll =
                match isP1ToPlay with
                | true -> let space = move p1Space roll
                          (space, p1Score + space, p2Space, p2Score, false)
                | false -> let space = move p2Space roll
                           (p1Space, p1Score, space, p2Score + space, true)

            match p1Score, p2Score with
            | p1, _ when p1 >= target -> 1L
            | _, p2 when p2 >= target -> 0L
            | _, _ -> rollCount |> List.sumBy (fun (roll, count) -> count * play2Memo (getOutcome roll))

        and play2Memo = memoise play2R // Let's memoise as there are in the order of a quadrillion possible outcomes!

        play2Memo (p1Space, 0, p2Space, 0, true)
                   
    // Play a practice game using the deterministic 100-sided die. The moment either player wins, what do you get
    // if you multiply the score of the losing player by the number of times the die was rolled during the game?
    let Part1() = play 10 2 1_000

    // Using your given starting positions, determine every possible outcome.
    // Find the player that wins in more universes; in how many universes does that player win?
    let Part2() = play2 10 2 21