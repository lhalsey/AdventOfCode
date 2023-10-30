namespace AdventOfCode.Days.Y2022

open AdventOfCode.Shared.Utility

/// Day 16: Proboscidea Volcanium
/// https://adventofcode.com/2022/day/16
/// The sensors have led you to the origin of the distress signal: yet another handheld device,
/// just like the one the Elves gave you.
module Day16 =

    type Valve = { Id: string; Rate: int; Tunnels: string[] }

    type State = { MinutesRemaining: int; CurrentValve: Valve; TotalPressure: int; OpenValves: string Set;}

    let parse = function // Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
        | Regex "Valve (.+) has flow rate=(\d+); .* to valves* (.+)" [id; Int rate; tunnels] -> 
            { Id = id; Rate = rate; Tunnels = tunnels |> splitOn ", " }
        | x -> failwithf "Invalid input: %s" x

    let parseInput() = getFile (2022, 16) |> readLinesAs parse

    let solve (valveMap: Map<string, Valve>) (state: State) =
        let nonZeroValveCount = valveMap |> countIf (fun x -> x.Value.Rate > 0)

        let rec solveR (state: State) =
            let getOptions (state: State) =
                seq {
                    let moves =
                        state.CurrentValve.Tunnels
                        |> Array.map (fun v -> { state with CurrentValve = valveMap[v]
                                                            MinutesRemaining = state.MinutesRemaining - 1 })

                    yield! moves

                    let cv = state.CurrentValve
                    if (state.OpenValves.Contains cv.Id |> not) && cv.Rate > 0 then
                           yield { state with MinutesRemaining = state.MinutesRemaining - 1 
                                              TotalPressure = state.TotalPressure + (cv.Rate * (state.MinutesRemaining - 1)) 
                                              OpenValves = state.OpenValves.Add cv.Id }
                }

            match state with
            | { MinutesRemaining = 0 } -> state.TotalPressure // Time up
            | { OpenValves = ov } when ov.Count = nonZeroValveCount -> state.TotalPressure // Opened all non-zero valves
            | _ ->
                getOptions state
                |> Seq.map solveMemo
                |> Seq.max

        and solveMemo = memoise solveR

        solveR state


    let Part1() =
        failwith "Too slow!"

        let valveMap = parseInput() |> Seq.map (fun x -> x.Id, x) |> Map
        let start = valveMap["AA"]

        let state = { MinutesRemaining = 30; CurrentValve = start; TotalPressure = 0; OpenValves = Set.empty }

        solve valveMap state

    let Part2() =
        0