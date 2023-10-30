namespace AdventOfCode.Days.Y2022

open AdventOfCode.Shared.Utility

/// Day 19: Not Enough Minerals
/// https://adventofcode.com/2022/day/19
/// Your scans show that the lava did indeed form obsidian!
module Day19 =

    type Resource = string
    type Cost = (Resource * int) list
    type BluePrint = { Id: int;  CostMap: Map<Resource, Cost>}

    let parseCost = function // E.g. Each geode robot costs 4 ore and 9 obsidian.
        | Regex " Each (.+) robot costs (\d+) (.+) and (\d+) (.+)" [ robot; Int qty1; res1; Int qty2; res2 ] ->
            robot, [(res1, qty1); (res2, qty2)]
        | Regex " Each (.+) robot costs (\d+) (.+)" [ robot; Int qty; res ] -> robot, [(res, qty)]
        | x -> failwithf "Invalid cost: %s" x

    let parse = function // E.g. Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 4 ore.
        | Regex "Blueprint (\d+):(.+)" [ Int id; xs ] -> 
            let costs = xs |> split '.' |> Array.map parseCost |> Map
            { Id = id; CostMap = costs }
        | x -> failwithf "Invalid blueprint: %s" x

    let parseInput() = getFile (2022, 19) |> readLinesAs parse |> Seq.toList

    let getGeodes (blueprint: BluePrint) (minutes: int) =
        let rec getGeodesR (resources: Map<Resource, int>) (robots: Map<Resource, int>) (minutes: int) =
            // Spend resources on new robots
            let priorities = ["geode"; "obsidian"; "clay"; "ore"]

            let rec spend ((resources: Map<Resource, int>), (newRobots: Map<Resource, int>)) resource =
                let cost = blueprint.CostMap[resource]
                let canAfford = cost |> List.forall (fun (r, q) -> (resources.TryFind r |> Option.defaultValue 0) >= q)

                match canAfford with
                | false -> resources, newRobots
                | true ->
                    let resources = mapCombine (Map cost) resources (-)
                    let newRobots = newRobots.Add(resource, 1)

                    spend (resources, newRobots) resource

            let resources, newRobots =
                ((resources, Map.empty), priorities)
                ||> List.fold spend

            // Collect resources from robots
            let resources = mapCombine robots resources (+)

            // Build new robots
            let robots = mapCombine newRobots robots (+)

            match minutes with
            | 0 -> resources.TryFind "geode" |> Option.defaultValue 0
            | _ -> getGeodesR resources robots (minutes - 1)

        let robots = Map ["ore", 1]
        getGeodesR Map.empty robots minutes

    let Part1() =
        let blueprints = parseInput()

        let g1 = getGeodes blueprints[0] 24

        let results =
            blueprints
            |> List.map (fun x -> x, getGeodes x 24)

        results |> List.sumBy (fun (x, y) -> x.Id * y)

    let Part2() =
        0