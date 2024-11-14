namespace AdventOfCode.Days.Y2023

open AdventOfCode.Shared.Utility
open AdventOfCode.Shared
open System.Collections.Generic

/// Day 3: Gear Ratios
/// https://adventofcode.com/2023/day/3
/// You and the Elf eventually reach a gondola lift station; he says the gondola lift will take you up
/// to the water source, but this is as far as he can bring you.
module Day03 =

    type Cell = Number of int | Symbol of string | Empty

    let parse = function
        | Int x -> Number x
        | "." -> Empty
        | x -> Symbol x

    let parseInput() = getFile (2023, 3) |> parseGrid (string >> parse)

    let getPartNumbers (cellMap: IReadOnlyDictionary<Point2d, Cell>) (cells: Point2d list) = 
        let rec getPartNumbersR (cells: Point2d list) (currentNum: int) (hasAdjacent: bool) (gears: Point2d Set) =
            seq {
                let hasAdjacentSymbol (p: Point2d) =
                    p.GetAllAdjacent()
                    |> Seq.exists(fun x -> tryFind x cellMap |> function Some (Symbol _) -> true | _ -> false)

                let getAdjacentGears (p: Point2d) =
                    p.GetAllAdjacent()
                    |> Seq.filter(fun x -> tryFind x cellMap |> function Some (Symbol "*") -> true | _ -> false)
                    |> Seq.toList

                if cells.IsEmpty then
                    if currentNum > 0 then yield (currentNum, gears)
                else
                    let h = cells.Head
                    match cellMap[h], getAdjacentGears h with
                    | Number x, y::ys ->
                        yield! getPartNumbersR cells.Tail (currentNum * 10 + x) true (Set.union (set (y::ys)) gears)
                    | Number x, _ when hasAdjacentSymbol h ->
                        yield! getPartNumbersR cells.Tail (currentNum * 10 + x) true gears
                    | Number x, _ ->
                        yield! getPartNumbersR cells.Tail (currentNum * 10 + x) hasAdjacent gears
                    | _ ->
                        if hasAdjacent then yield (currentNum, gears)
                        yield! getPartNumbersR cells.Tail 0 false Set.empty
            }

        getPartNumbersR cells 0 false Set.empty

    let getParts() =
        let grid = parseInput()

        let width = grid |> Seq.map (fun (x, _) -> x.X) |> Seq.max
        let height = grid |> Seq.map (fun (x, _) -> x.Y) |> Seq.max

        let cells = List.allPairs [0..height] [0..width] |> List.map (fun (y, x) -> { X = x; Y = y })
        let cellMap = grid |> readOnlyDict

        getPartNumbers cellMap cells

    // What is the sum of all of the part numbers in the engine schematic?
    let Part1() = getParts() |> Seq.sumBy fst

    // What is the sum of all of the gear ratios in your engine schematic?
    let Part2() = 
       getParts()
       |> Seq.collect (fun (x, y) -> y |> Set.map (fun p -> (p, x)))
       |> groupBy fst (fun _ v -> v |> Seq.map snd)
       |> Seq.filter (fun x -> Seq.length x = 2)
       |> Seq.sumBy (Seq.reduce (*))