namespace AdventOfCode.Days.Y2024

open AdventOfCode.Shared.Utility
open System.Collections.Generic

/// Day 5: Print Queue
/// https://adventofcode.com/2024/day/5
/// Satisfied with their search on Ceres, the squadron of scholars suggests subsequently scanning the
/// stationery stacks of sub-basement 17.
module Day05 =

    let parse (s: string) = s |> split ',' |> Array.toList 

    let parseInput() = getFile (2024, 5) |> readLinesAs parse |> Seq.toList

    let getProblem() =
        let input = parseInput()

        let (order, production) =
            input
            |> List.filter (fun x -> x.Length > 0)
            |> List.partition (fun x -> x[0].Contains '|')

        let orderMap =
            order
            |> List.map (fun x -> x[0] |> split '|'|> fun x -> x[0], x[1])
            |> groupBy snd (fun k v -> k, v |> Seq.map fst |> Seq.toList)
            |> readOnlyDict

        (orderMap, production)

    let isValid (orderMap: IReadOnlyDictionary<string, string list>) (pages: string list) =
        
        let rec getPostDependencies (page: string) =
            match tryFind page orderMap with
            | Some x -> x
            | None -> []

        let rec hasNoPostDependencies (pages: string list) =
            match pages with
            | [] -> true
            | h::t -> let deps = getPostDependencies h |> Set
                      let pre = Set t
                      let isOk = deps |> Set.intersect pre = Set.empty
                      isOk && (hasNoPostDependencies t)

        pages |> hasNoPostDependencies

    let getValid (orderMap: IReadOnlyDictionary<string, string list>) (pages: string list) =
        
        let rec getPostDependencies (page: string) =
            match tryFind page orderMap with
            | Some x -> x
            | None -> []

        let hasNoPostDependencies (pages: string list) =
            let rec hasNoPostDependenciesR (index: int) (ps: string list) =
                match index with
                | i when i >= pages.Length -> ps
                | i -> let elem = ps[i]
                       let deps = getPostDependencies elem |> toHashSet
                       let after = ps[i+1 ..]
                       let before = ps[..i]
                       let (invalid, valid) = after |> List.partition (deps.Contains)

                       match invalid with
                       | [] -> hasNoPostDependenciesR (i + 1) ps
                       | _ -> hasNoPostDependenciesR 0 (invalid @ before @ valid)
                | _ -> failwith "uh oh"

            hasNoPostDependenciesR 0 pages

        pages |> hasNoPostDependencies

    let getMidValues (elems: string list list) = elems |> List.map (fun x -> x[x.Length / 2]) |> List.sumBy int


    let Part1() =
        let (orderMap, production) = getProblem()

        production
        |> List.filter (isValid orderMap)
        |> getMidValues

    let Part2() =
        let (orderMap, production) = getProblem()

        production
        |> List.filter (isValid orderMap >> not)
        |> List.map (getValid orderMap)
        |> getMidValues