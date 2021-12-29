namespace AdventOfCode.Days.Y2021

open AdventOfCode.Shared.Utility

/// Day 12: Passage Pathing
/// https://adventofcode.com/2021/day/12
/// With your submarine's subterranean subsystems subsisting suboptimally, the only way you're getting out
/// of this cave anytime soon is by finding a path yourself.
module Day12 =
    type CaveType = Start | End | SmallCave of string | BigCave of string

    type State = { Current: CaveType; SmallCavesVisited: string Set; SmallCaveRevisits: int }

    let parseCave = function
        | "start" -> Start
        | "end" -> End
        | LowerCase x -> SmallCave x
        | x -> BigCave x

    let parse (s: string) =
        let (x, y) = s |> splitIntoPair "-"
        let (cave1, cave2) = parseCave x, parseCave y
        [ cave1, cave2; cave2, cave1 ]

    let parseInput() =
        getFile (2021, 12)
        |> readLinesAs parse
        |> List.concat
        |> List.filter (fun (x, y) -> x <> End && y <> Start) // Can only leave Start cave and enter End cave
        |> List.groupBy fst
        |> List.map (fun (x, y) -> x, y |> List.map snd)
        |> Map

    let getNumPaths (maxSmallCaveRevisits: int) =
        let pathMap = parseInput()

        let rec getNumPathsR = function
            | { Current = End } -> 1
            | state -> 
                let canRevisitSmallCave = state.SmallCaveRevisits < maxSmallCaveRevisits
                
                let tryVisitSmallCave (cave: string) = 
                    match state.SmallCavesVisited.Contains cave, canRevisitSmallCave with
                    | true, false -> None // Been to small cave before, no revisits left
                    | true, true ->       // Been to small cave before, but can revisit once
                        Some { state with Current = (SmallCave cave); SmallCaveRevisits = state.SmallCaveRevisits + 1 }
                    | false, _ ->         // Not been to small cave before
                        Some { state with Current = (SmallCave cave); SmallCavesVisited = state.SmallCavesVisited.Add cave }

                let getNewState = function
                    | SmallCave c -> tryVisitSmallCave c
                    | c -> Some { state with Current = c }

                pathMap.TryFind state.Current
                |> Option.defaultValue []
                |> List.choose getNewState
                |> List.sumBy getNumPathsMemo

        and getNumPathsMemo = memoise getNumPathsR

        getNumPathsMemo { Current = Start; SmallCavesVisited = Set.empty; SmallCaveRevisits = 0 }

    // How many paths through this cave system are there that visit small caves at most once?
    let Part1() = getNumPaths 0

    // Given these new rules, how many paths through this cave system are there?
    let Part2() = getNumPaths 1