namespace AdventOfCode.Days.Y2015

open System
open System.Collections.Generic

/// Day 22: Wizard Simulator 20XX
/// https://adventofcode.com/2015/day/22
/// Little Henry Case decides that defeating bosses with swords and stuff is boring.
module Day22 =

    // Could model spell effects differently but this way is very efficient for caching state
    type State =
        { BossHP: int
          BossDamage: int
          DifficultyDamage: int
          PlayerHP: int
          Mana: int
          ManaSpent: int
          Shield: int
          Poison: int
          Recharge: int }

    type Spell = 
        { Name: string
          Cost: int
          CanCast: State -> bool
          Cast: State -> State }

    let isShieldActive (state: State) = state.Shield > 0
    let initShield (state: State) = { state with Shield = 6 }
    let isPoisonActive (state: State) = state.Poison > 0
    let initPoison (state: State) = { state with Poison = 6 }
    let isRechargeActive (state: State) = state.Recharge > 0
    let initRecharge (state: State) = { state with Recharge = 5 }

    let healPlayer (hp: int) (state: State) = { state with PlayerHP = state.PlayerHP + hp }

    let (| BossDead | PlayerDead | BothAlive |) = function
        | { BossHP = bossHP } when bossHP <= 0 -> BossDead
        | { PlayerHP = playerHP } when playerHP <= 0 -> PlayerDead
        | _ -> BothAlive

    let hitPlayer (hp: int) (state: State) =
        match state with
        | BossDead -> state // Dead boss can't attack
        | _ -> { state with PlayerHP = state.PlayerHP - hp }

    let hitBoss (hp: int) (state: State) =
        match state with
        | PlayerDead -> state // Dead player can't attack
        | _ -> { state with BossHP = state.BossHP - hp }

    let bossAttack (state: State) =
        let shield = if isShieldActive state then 7 else 0
        state |> hitPlayer (max 1 (state.BossDamage - shield))

    let castSpell (spell: Spell) (state: State) =
        match state with
        | BossDead -> state // Save your mana if boss is already dead!
        | _ -> { state with Mana = state.Mana - spell.Cost; ManaSpent = state.ManaSpent + spell.Cost }
               |> spell.Cast

    let applyEffects (state: State) =

        let inline applyIf pred f state = if pred state then f state else state

        state
        |> applyIf isShieldActive (fun s -> { s with Shield = s.Shield - 1 })
        |> applyIf isPoisonActive (fun s -> { s with Poison = s.Poison - 1 } |> hitBoss 3)
        |> applyIf isRechargeActive (fun s -> { s with Mana = s.Mana + 101; Recharge = s.Recharge - 1 })

    let getSpells() =
        let always = fun _ -> true

        [ { Name = "Magic Missile"; Cost = 53; CanCast = always; Cast = hitBoss 4 }
          { Name = "Drain"; Cost = 73; CanCast = always; Cast = hitBoss 2 >> healPlayer 2 }
          { Name = "Shield"; Cost = 113; CanCast = isShieldActive >> not; Cast = initShield }
          { Name = "Poison"; Cost = 173; CanCast = isPoisonActive >> not; Cast = initPoison }
          { Name = "Recharge"; Cost = 229; CanCast = isRechargeActive >> not; Cast = initRecharge } ]

    let playGame state =
        // TODO: Figure out how to make immutable
        let visited = HashSet<State>()
        let mutable minVictorySpend = Int32.MaxValue
        let spells = getSpells()

        let playRound state spell = 
            state
            |> hitPlayer state.DifficultyDamage // Hero's turn
            |> castSpell spell
            |> applyEffects // Boss's turn
            |> bossAttack
            |> applyEffects

        let rec playGameR state =
            seq {
                match state with
                | PlayerDead -> ()
                | BossDead -> // If min spend then update
                    if state.ManaSpent < minVictorySpend then
                        minVictorySpend <- state.ManaSpent
                        yield state.ManaSpent
                | _ ->
                    let newStates = // Determine what spells are available and play one round using each
                        spells      // Prune states we've visited or that are more expensive than min
                        |> List.filter (fun spell -> spell.CanCast state && state.Mana >= spell.Cost)
                        |> List.map (playRound state)
                        |> List.filter (fun x -> x.ManaSpent <= minVictorySpend && visited.Add x)

                    for newState in newStates do yield! playGameR newState
                }

        playGameR state

    let getMinSpend difficultyDamage =
        let state = 
            { BossHP = 55
              BossDamage = 8
              DifficultyDamage = difficultyDamage
              PlayerHP = 50
              Mana = 500
              ManaSpent = 0
              Shield = 0
              Poison = 0
              Recharge = 0 }

        state |> playGame |> Seq.min


    // You start with 50 hit points and 500 mana points. What is the least amount of mana
    // you can spend and still win the fight? 
    let Part1() = getMinSpend 0

    // At the start of each player turn (before any other effects apply), you lose 1 hit point.
    // If this brings you to or below 0 hit points, you lose.
    // With the same starting stats for you and the boss, what is the least amount of mana
    //you can spend and still win the fight?
    let Part2() = getMinSpend 1