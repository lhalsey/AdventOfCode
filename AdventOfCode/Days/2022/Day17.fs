namespace AdventOfCode.Days.Y2022

open AdventOfCode.Shared.Utility
open AdventOfCode.Shared

/// Day 17: Pyroclastic Flow
/// https://adventofcode.com/2022/day/17
/// Your handheld device has located an alternative exit from the cave for you and the elephants.
module Day17 =

    type State = { Placed: Point2d Set; Top: int;  RockIndex: int; JetIndex: int }

    type Rock = { Points: Point2d list; Width: int }

    let parse = function '>' -> Direction2d.East | '<' -> Direction2d.West | x -> failwithf "Invalid input: %c" x

    let parseInput() = getFile (2022, 17) |> readAllText |> Seq.map parse

    let getRocks() =
        let horizontal = [ { X = 0; Y = 0 }; { X = 1; Y = 0 }; { X = 2; Y = 0 }; { X = 3; Y = 0 } ]
        let plus = [ { X = 1; Y = 0 }; { X = 0; Y = 1}; { X = 1; Y = 1 }; { X = 2; Y = 1 }; { X = 1; Y = 2 } ]
        let l = [ { X = 0; Y = 0 }; { X = 1; Y = 0 }; { X = 2; Y = 0 }; { X = 2; Y = 1 }; { X = 2; Y = 2 }]
        let vertical = [ { X = 0; Y = 0 }; { X = 0; Y = 1 }; { X = 0; Y = 2 }; { X = 0; Y = 3 } ]
        let square = [ { X = 0; Y = 0 }; { X = 1; Y = 0 }; { X = 0; Y = 1 }; { X = 1; Y = 1 } ]

        let getWidth (points: Point2d list) = points |> List.map (fun x -> x.X) |> List.max |> (+) 0

        [ horizontal; plus; l; vertical; square ]
        |> List.map (fun x -> { Points = x; Width = getWidth x })

    let dropRocks (numRocks: int) =
        let jets = parseInput() |> Seq.toList
        let rocks = getRocks()
        
        let rec dropRock (state: State) =
            let rec dropRockR (state: State) (pos: Point2d) =

                let jet = jets[state.JetIndex % jets.Length]
                let rock = rocks[state.RockIndex % rocks.Length]

                let tryMove (pos: Point2d) (dir: Direction2d) =
                    let newPos = pos + dir
                    let newPoints = rock.Points |> List.map ((+) newPos)
                    let hasNoOverlap = newPoints |> List.exists state.Placed.Contains |> not
                    let canMove = newPos.X >= 0 && newPos.X + rock.Width <= 6 && newPos.Y > 0 && hasNoOverlap
                    if canMove then newPos else pos

                let posAfterJet = tryMove pos jet
                let posAfterFall = tryMove posAfterJet Direction2d.North
                let isSettled = posAfterFall.Y = posAfterJet.Y

                match isSettled with
                | true ->
                    let newPoints = rock.Points |> List.map ((+) posAfterFall) |> set
                    let placed = state.Placed |> Set.union newPoints
                    let topOfNew = newPoints |> Set.map (fun x -> x.Y) |> Set.maxElement
                    let top = max state.Top topOfNew
                    { Placed = placed; RockIndex = state.RockIndex + 1; JetIndex = state.JetIndex + 1; Top = top }
                | false ->
                    dropRockR ({ state with JetIndex = state.JetIndex + 1 }) posAfterFall

            let pos = { X = 2; Y = state.Top + 4 }
            dropRockR state pos

        let state = { Placed = Set.empty; Top = 0; RockIndex = 0; JetIndex = 0 }

        //let state =
        //    (state, [1L..numRocks])
        //    ||> List.fold (fun acc _ -> dropRock acc)

        let state = 
            state
            |> Seq.unfold (fun x -> Some(x, dropRock x))
            |> Seq.item numRocks

        //let map = state.Placed |> Set.map (fun x -> x, '#') |> Map
        //let image = ImageProvider.getImage id map

        abs state.Top


    let Part1() = dropRocks 2022

    let Part2() = dropRocks 1_000 //_000_000_000L