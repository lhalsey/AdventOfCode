namespace AdventOfCode.Days.Y2021

open AdventOfCode.Shared.Utility
open FSharp.Collections.ParallelSeq
open System.Collections.Generic

/// Day 18: Snailfish
/// https://adventofcode.com/2021/day/18
/// You descend into the ocean trench and encounter some snailfish.
module Day18 =

    let [<Literal>] ExplodeThreshold = 4
    let [<Literal>] SplitThreshold = 10

    type SnailNumber = Regular of int | Pair of SnailNumber * SnailNumber

    type State = { 
        Current: SnailNumber
        Index: int
        Level: int
        Before: (int * int) option
        Pair: (int * int * int) option
        After: (int * int) option }

    // Convert a string to binary tree representation of SnailNumber
    let buildTree (s: string) =
        let rec buildTreeR = function
            | '['::xs ->
                let (left, xs) = buildTreeR xs
                let (right, xs) = buildTreeR xs
                Pair (left, right), xs
            | (CInt x)::xs -> Regular x, xs
            | _::xs -> buildTreeR xs // Skip ',' or ']'
            | [] -> failwithf "Invalid input %s" s

        buildTreeR (Seq.toList s) |> fst

    let parseInput() = getFile (2021, 18) |> readLinesAs buildTree |> Seq.toList

    // Replace nodes at specified indexes in tree with corresponding values
    let replaceNodes (num: SnailNumber) (indexValueMap: IReadOnlyDictionary<int, SnailNumber>) =
        let rec replaceNodesR (num: SnailNumber) (index: int) =
            match num, tryFind index indexValueMap with
            | _, Some value -> index, value
            | Regular r, _ -> index, Regular r
            | Pair (l, r), _ -> 
                let (index, left) = replaceNodesR l (index + 1)
                let (index, right) = replaceNodesR r (index + 1)
                index, Pair (left, right)

        replaceNodesR num 0 |> snd

    // To explode a pair, the pair's left value is added to the first regular number to the left
    // of the exploding pair (if any), and the pair's right value is added to the first regular number
    // to the right of the exploding pair (if any).
    let explode (num: SnailNumber) =
        let rec prepExplodeR (num: SnailNumber) (state: State) =
            match num, state with
            | Regular r, { Pair = None } -> { state with Before = Some (state.Index, r) }
            | Regular r, { Pair = Some _; After = None } -> { state with After = Some (state.Index, r) }
            | Regular _, _ -> state
            | Pair (Regular l, Regular r), { Level = ExplodeThreshold; Pair = None } ->
                { state with Pair = Some (state.Index, l, r) }
            | Pair (l, r), _ -> 
                let level = state.Level
                let state = prepExplodeR l { state with Level = level + 1; Index = state.Index + 1 }
                let state = prepExplodeR r { state with Level = level + 1; Index = state.Index + 1 }
                state

        // Do this in two passes as I'm not smart enough to do it in one!
        let state = prepExplodeR num { Current = Regular -1; Index = 0; Level = 0; Before = None; Pair = None; After = None }

        let replacements =
            state.Pair // If we find a pair to explode then determine which nodes to replace
            |> Option.map (fun (i, l, r) ->
                [ Some (i, Regular 0)
                  state.Before |> Option.map (fun (ib, b) -> ib, Regular (b + l))
                  state.After |> Option.map (fun (ia, a) -> ia, Regular(a + r)) ]
                  |> List.choose id
                  |> readOnlyDict)

        match replacements with
        | None -> (false, num)
        | Some r -> (true, replaceNodes num r)

    let traverse (num: SnailNumber) =
        let getChildren = function
            | { Current = Pair (l, r) } as state ->
                seq [ { state with Current = l; Level = state.Level + 1 }
                      { state with Current = r; Level = state.Level + 1 } ]
            | _ -> []

        { Current = num; Index = 0; Level = 0; Before = None; Pair = None; After = None }
        |> bfs getChildren
        |> Seq.mapi (fun i s -> { s with Index = i })


    // To split a regular number, replace it with a pair; the left element of the pair should be
    // the regular number divided by two and rounded down, while the right element of the pair should be
    // the regular number divided by two and rounded up.
    //let split (num: SnailNumber) =
    //    let rec splitR (num: SnailNumber) (hasSplit: bool) =
    //        match hasSplit, num with // Only one split per traversal
    //        | false, Regular r when r >= SplitThreshold -> true, Pair (Regular (r / 2), Regular ((r + 1) / 2))
    //        | _, Regular r -> hasSplit, Regular r
    //        | _, Pair (l, r) ->
    //            let (hasSplit, left) = splitR l hasSplit
    //            let (hasSplit, right) = splitR r hasSplit
    //            hasSplit, Pair (left, right)

    //    splitR num false

    let split (num: SnailNumber) =       
        let getReplacementMap (i, r) = 
            let pair = Pair (Regular (r / 2), Regular ((r + 1) / 2))

            [i, pair] |> readOnlyDict
        
        let replacement =
            traverse num
            |> Seq.tryPick (function { Current = Regular r; Index = i } when r >= SplitThreshold -> Some (i, r)  | _ -> None)
            |> Option.map getReplacementMap

        match replacement with
        | None -> false, num
        | Some r -> true, replaceNodes num r

    // During reduction, at most one action applies, after which the process returns to the top
    // of the list of actions. For example, if split produces a pair that meets the explode criteria,
    // that pair explodes before other splits occur.
    let rec reduce (num: SnailNumber) =
        match explode num with
        | true, n -> reduce n
        | _ -> match split num with
               | true, n -> reduce n
               | _ -> num

    // To add two snailfish numbers, form a pair from the left and right parameters of the addition operator.
    let add (n1: SnailNumber) (n2: SnailNumber) = Pair (n1, n2) |> reduce

    // The magnitude of a pair is 3 times the magnitude of its left element plus 2 times the magnitude
    // of its right element. The magnitude of a regular number is just that number.
    let rec getMagnitude = function Regular r -> r | Pair (l, r) -> (3 * getMagnitude l) + (2 * getMagnitude r)

    // Helper function for debugging
    let rec print = function Regular r -> $"{r}" | Pair (l, r) -> $"[{print l},{print r}]"

    // Add up all of the snailfish numbers from the homework assignment in the order they appear.
    // What is the magnitude of the final sum?
    let Part1() =
        parseInput()
        |> List.reduce add
        |> getMagnitude

    // What is the largest magnitude of any sum of two different snailfish numbers from the homework assignment?
    let Part2() =
        let input = parseInput()

        let t = traverse input.[0]

        List.allPairs input input
        |> PSeq.map (fun (x, y) -> add x y |> getMagnitude)
        |> PSeq.max