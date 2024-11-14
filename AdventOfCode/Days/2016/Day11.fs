namespace AdventOfCode.Days.Y2016

open AdventOfCode.Shared.Utility

/// Day 11: Radioisotope Thermoelectric Generators
/// https://adventofcode.com/2016/day/11
/// You come upon a column of four floors that have been entirely sealed off from the rest of the
/// building except for a small dedicated lobby.
module Day11 =

    type Mode = V1 | V2
    type Floor = int
    type Element = Promethium | Cobalt | Curium | Ruthenium | Plutonium | Elerium | Dilithium
    type Item =  Microchip of Element | Generator of Element
    type FloorMap = Map<Floor, Item Set>  
    
    let [<Literal>] TopFloor = 3

    let getElement = function Microchip e -> e | Generator e -> e

    [<CustomEquality; NoComparison>]
    type State = { MyFloor: Floor; FloorMap: FloorMap; Steps: int } with
        member __.CurrentItems = __.FloorMap.[__.MyFloor]

        override __.GetHashCode() =
            let getPair (floorItems: (Floor * Item) seq) =
                match Seq.toList floorItems with
                | (f1, Generator _)::(f2, Microchip _)::[] -> f1, f2
                | (f1, Microchip _)::(f2, Generator _)::[] -> f2, f1
                | x -> failwithf "Invalid element pair: %A" x

            let elementFloors =
                __.FloorMap
                |> Seq.collect (fun x -> x.Value |> Set.map (fun y -> x.Key, y))
                |> Seq.groupBy (fun (_, item) -> getElement item)
                |> Seq.map (fun (_, floorItems) -> getPair floorItems)
                |> Seq.countBy id
                |> Seq.sortBy fst
                |> Seq.toList

            hash (__.MyFloor, elementFloors)

        override __.Equals(b) =
            match b with
            | :? State as s -> __.GetHashCode() = s.GetHashCode()
            | _ -> false


    let parseInput (mode: Mode) =
        let floorMap =
            [ 0, set [ Generator Promethium; Microchip Promethium ]
              1, set [ Generator Cobalt; Generator Curium; Generator Ruthenium; Generator Plutonium]
              2, set [ Microchip Cobalt; Microchip Curium; Microchip Ruthenium; Microchip Plutonium ]
              3, Set.empty ]
             |> Map

        let extraObjects =
            set [ Generator Elerium; Microchip Elerium; Generator Dilithium; Microchip Dilithium ]

        match mode with
        | V1 -> floorMap
        | V2 -> floorMap.Add (0, Set.union floorMap.[0] extraObjects)

    let isComplete (state: State) =
        let lowerFloorsEmpty = [ 0 .. TopFloor - 1 ] |> List.forall (fun f -> Set.isEmpty state.FloorMap.[f])

        state.MyFloor = TopFloor && lowerFloorsEmpty

    let isValidCombo (items: Item Set) =
        let (generators, microchips) =
            items
            |> Set.partition (fun x -> match x with Generator _ -> true | _ -> false)

        let matches =
            microchips
            |> Set.forall (fun x -> match x with Microchip e -> generators.Contains (Generator e) | _ -> false)

        generators.IsEmpty || matches
   
    let getAvailableFloors (floor: Floor) =
        [ if floor < TopFloor then yield floor + 1
          if floor > 0 then yield floor - 1 ]

    let getValidState (state: State) (floor: Floor) (items: Item Set) =
        let currItems = state.CurrentItems - items
        let newItems = state.FloorMap.[floor] + items
        let map = state.FloorMap.Add(state.MyFloor, currItems).Add(floor, newItems)

        if isValidCombo currItems && isValidCombo newItems
        then Some { MyFloor = floor; FloorMap = map; Steps = state.Steps + 1 }
        else None

    // The lift's capacity rating means it can carry at most yourself and two RTGs or
    // microchips in any combination.
    let getAvailableMoves (state: State) =
        let availableFloors = getAvailableFloors state.MyFloor
        let singleItems = state.CurrentItems |> Set.toList
        let itemPairs = uniquePairs singleItems |> Seq.toList |> List.map (fun (x, y) -> set [x; y])
        let availableItems = (singleItems |> List.map Set.singleton) @ itemPairs

        let moves = 
            List.allPairs availableFloors availableItems
            |> List.choose (fun (floor, items) -> getValidState state floor items)

        moves

    let getMinSteps (mode: Mode) = 
        let input = parseInput mode
        let state = { MyFloor = 0; FloorMap = input; Steps = 0 }
        
        [ state ]
        |> breadthFirstSearch getAvailableMoves
        |> Seq.find isComplete
        |> fun x -> x.Steps


    // In your situation, what is the minimum number of steps required to bring all of the objects to
    // the fourth floor?
    let Part1() = getMinSteps V1

    // What is the minimum number of steps required to bring all of the objects, including these four
    // new ones, to the fourth floor?
    let Part2() = getMinSteps V2