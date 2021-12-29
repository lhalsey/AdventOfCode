namespace AdventOfCode.Days.Y2021

open AdventOfCode.Shared.Utility
open System

/// Day 23: Amphipod
/// https://adventofcode.com/2021/day/23
/// A group of amphipods notice your fancy submarine and flag you down.
module Day23 =

    let [<Literal>] MinHallIndex = 0
    let [<Literal>] MaxHallIndex = 10 // TODO: (Rooms + 1) * 2

    let aMap = [ ('A', 1); ('B', 10); ('C', 100); ('D', 1_000) ] |> Map

    type Mode = V1 | V2

    type Amphipod = char

    type State = { Hallway: Map<int, Amphipod>; Rooms: Map<Amphipod, Amphipod list> }

    let parse (s: string) = s |> Seq.filter Char.IsLetter |> Seq.toList

    let parseInput() =
        getFile (2021, 23)
        |> readLinesAs parse
        |> Seq.toList
        |> List.filter (fun x -> x.IsEmpty |> not)
        |> List.transpose

    let getRooms (amphipods: char list list) =
        amphipods
        |> List.mapi (fun i x -> (char(int 'A' + i), x))
        |> Map

    let getRoomIndex (roomType: Amphipod) = (int roomType - int 'A') * 2 + 2

    let getCost (amphipod: Amphipod) (fromIndex: int) (toIndex: int) (occupants: int) (roomSize: int) =
        let distance = abs (fromIndex - toIndex) + (roomSize - occupants + 1)
        let multipler = aMap.[amphipod]

        distance * multipler

    let rec doEntries (roomSize: int) (state: State, cost: int)  =

        let tryEnter (fromIndex: int, amphipod: Amphipod) =
            let amphipods = state.Rooms.[amphipod]
            let toIndex = getRoomIndex amphipod

            let isRoomReady() = amphipods |> List.forall (fun a -> a = amphipod)

            let isReachable() = 
                let step = sign (toIndex - fromIndex)

                [fromIndex+step..step..toIndex]
                |> List.exists (fun i -> state.Hallway.ContainsKey i)
                |> not

            let getState() = 
                let cost = getCost amphipod fromIndex toIndex (amphipods.Length + 1) roomSize

                let state = 
                    { Hallway = state.Hallway.Remove fromIndex
                      Rooms = state.Rooms.Add (amphipod, amphipod::amphipods) }

                (state, cost)

            if isRoomReady() && isReachable() then Some (getState()) else None

        let newState = state.Hallway |> Seq.tryPick (fun x -> tryEnter (x.Key, x.Value))

        match newState with
        | None -> state, cost
        | Some (s, c) -> doEntries roomSize (s, (cost + c)) 

    let getExits (roomSize: int) (state: State) =
        let tryExit (roomType: Amphipod, amphipods: Amphipod list) =
            let isDone = amphipods |> List.forall (fun a -> a = roomType)

            let getOptions (amphipod: Amphipod) (remaining: Amphipod list) =
                let fromIndex = getRoomIndex roomType

                let getValidSpots (range: int list) =
                    range
                    |> List.takeWhile (fun i -> state.Hallway.ContainsKey i |> not)
                    |> List.filter (fun i -> [2; 4; 6; 8] |> List.contains i |> not)

                let getState (toIndex: int) =
                    let cost = getCost amphipod fromIndex toIndex amphipods.Length roomSize

                    let state = 
                        { Hallway = state.Hallway.Add (toIndex, amphipod)
                          Rooms = state.Rooms.Add (roomType, remaining) }

                    (state, cost)

                let rightMoves = getValidSpots [fromIndex+1..MaxHallIndex]
                let leftMoves = getValidSpots [fromIndex-1..-1..MinHallIndex]

                rightMoves @ leftMoves |> List.map getState

            match isDone, amphipods with
            | true, _ -> []
            | _, [] -> []
            | _, x::xs -> getOptions x xs

        state.Rooms |> Seq.collect (fun x -> tryExit (x.Key, x.Value))


    let getChildren (roomSize: int) (state: State)  =
        let exits = getExits roomSize state |> Seq.toList

        let children = exits |> List.map (doEntries roomSize)

        children |> List.toSeq

    let getEstimate (roomSize: int) (state: State) = 
        let hallWayEstimate = 
            state.Hallway
            |> Seq.sumBy (fun x -> getCost x.Value x.Key (getRoomIndex x.Value) 2 roomSize)

        let getRoomEstimate roomType amphipod =
            let fromIndex = getRoomIndex roomType
            let toIndex = getRoomIndex amphipod
            if fromIndex = toIndex then 0 else (getCost amphipod fromIndex toIndex 2 roomSize)

        let roomsEstimate =
            state.Rooms
            |> Seq.sumBy (fun x -> x.Value |> List.sumBy (fun y -> getRoomEstimate x.Key y))

        let estimate = hallWayEstimate + roomsEstimate
        estimate

    let solve mode =
        let input = parseInput()

        let extras = ["DD"; "CB"; "BA"; "AC"]

        let input =
            match mode with
            | V1 -> input
            | V2 -> input |> List.zip extras |> List.map (fun (x, y) -> y |> List.insertManyAt 1 x)
        
        let rooms = input |> getRooms
        
        let numRooms = rooms.Count
        let roomSize = rooms |> Seq.head |> fun x -> x.Value.Length

        let targetRooms =
            [0..numRooms-1]
            |> List.map (fun i -> List.replicate roomSize (char(i + int 'A')))
            |> getRooms

        let start = { Hallway = Map.empty; Rooms = rooms }
        let target = { Hallway = Map.empty; Rooms = targetRooms }
        
        aStar start target (getChildren roomSize) (getEstimate roomSize)
        |> Seq.head
       
    // What is the least energy required to organize the amphipods?
    let Part1() = solve V1

    // Using the initial configuration from the full diagram, what is the least energy required to
    // organize the amphipods?
    let Part2() = solve V2