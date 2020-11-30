namespace AdventOfCode.Days.Y2015

open AdventOfCode.Shared.Utility

/// Day 21: RPG Simulator 20XX
/// https://adventofcode.com/2015/day/21
/// Little Henry Case got a new video game for Christmas.
module Day21 =

    type Item = { Type: string; Name: string; Cost: int; Damage: int; Armor: int } with
        static member (+) (a, b) =
            { Type = "Combined"
              Name = $"{a.Name} & {b.Name}"
              Cost = a.Cost + b.Cost
              Damage = a.Damage + b.Damage
              Armor = a.Armor + b.Armor }

    type Player = { HitPoints: int; Damage: int; Armor: int } with
        // Damage dealt by an attacker each turn is equal to the attacker's damage score
        // minus the defender's armor score. An attacker always does at least 1 damage.
        member __.IsAttackedBy (attacker: Player) =
            { __ with HitPoints = __.HitPoints - max 1 (attacker.Damage - __.Armor) }

    type Victor = Hero | Boss

    // E.g. "Ring,Damage +1,25,1,0"
    let parse (s: string) =
        let tokens = s |> split ','

        { Type = tokens.[0]
          Name = tokens.[1]
          Cost = int tokens.[2]
          Damage = int tokens.[3]
          Armor = int tokens.[4] }

    let parseInput() = getFile (2015, 21) |> readLinesAs parse

    // It's easier to treat item combinations as a single item with aggregated
    // cost, damage & armor. This method returns all combined items which can
    // be made by selecting a single item from each list.
    let combineItems (items1: (Item option) seq) (items2: (Item option) seq) =
        Seq.allPairs items1 items2
        |> Seq.filter (fun (i1, i2) -> i1 <> i2)
        |> Seq.map (function
            | Some i1, Some i2 -> Some (i1 + i2)
            | Some i1, None -> Some i1
            | None, Some i2 -> Some i2
            | _ -> None)

    let getItemCombos() =
        let itemMap =  parseInput() |> Seq.groupBy (fun x -> x.Type) |> readOnlyDict

        // Must have one weapon, zero or one armor, zero to two rings
        let weapons = itemMap.["Weapon"] |> Seq.map Some
        let armors = itemMap.["Armor"] |> Seq.map Some |> Seq.append [ None ]
        let rings = itemMap.["Ring"] |> Seq.map Some |> Seq.append [ None ]

        let weaponAndArmor = combineItems weapons armors
        let ringCombos = combineItems rings rings

        combineItems weaponAndArmor ringCombos |> Seq.choose id

    let getVictor (itemCombo: Item) =
        let boss = { HitPoints = 104; Damage = 8; Armor = 1 }
        let hero = { HitPoints = 100; Damage = itemCombo.Damage; Armor = itemCombo.Armor }

        // Fight until the death - hero attacks first
        let rec getVictorR (boss: Player) (hero: Player) =
            match boss.IsAttackedBy hero, hero.IsAttackedBy boss with
            | { HitPoints = h }, _ when h <= 0 -> Hero
            | _, { HitPoints = h } when h <= 0 -> Boss
            | boss', player' -> getVictorR boss' player'

        getVictorR boss hero

    let getCost criteria = 
        getItemCombos()
        |> Seq.sortBy (fun x -> x.Cost)
        |> criteria
        |> fun x -> x.Cost


    // What is the least amount of gold you can spend and still win the fight?
    //let Part1() = getCost (Seq.find (fun x -> getVictor x = Hero))
    let Part1() = getCost (Seq.find (fun x -> getVictor x = Hero))

    // What is the most amount of gold you can spend and still lose the fight?
    let Part2() = getCost (Seq.findBack (fun x -> getVictor x = Boss))