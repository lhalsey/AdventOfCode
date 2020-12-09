namespace AdventOfCode.Shared

open System
open System.IO
open System.Text.RegularExpressions
open System.Collections.Generic
open MoreLinq.Extensions
open FSharp.Collections.ParallelSeq

module Utility =

    // IO
    let (++) x y = Path.Combine(x, y)

    let getFile (year, day) =
        __SOURCE_DIRECTORY__ ++ $"..\\Days\\{year}\\Input\\{day:D2}.txt"

    let readAllText filename =
        filename
        |> File.ReadAllText
        |> fun x -> x.Trim()

    let readLines filename =
        filename
        |> File.ReadLines

    let readAllLines filename =
        filename
        |> File.ReadAllLines

    let readLinesAs f filename =
        filename
        |> File.ReadLines
        |> Seq.map f

    let readCsv filename =
        filename
        |> File.ReadAllText
        |> fun x -> x.Split [|','|]
        |> Array.toList

    // String
    let split (delimiter: char) (s: string) = s.Split [| delimiter |]

    let splitOn (delimiter: string) (s: string) = s.Split delimiter

    let splitAny (delimiters: string) (s: string) =
        s.Split(Seq.toArray delimiters, StringSplitOptions.RemoveEmptyEntries)

    let splitIntoPair (delimiter: string) (s:string) =
        let tokens = s.Split(delimiter)
        tokens.[0], tokens.[1]

    let splitIntoPairAs (delimiter: string) f (s:string) =
        let tokens = s.Split(delimiter)
        f tokens.[0], f tokens.[1]

    let tryParseAsInt (s: string) = match Int32.TryParse s with (true, v) -> Some v | _ -> None

    let tryParseAsFloat (s: string) = match Double.TryParse s with (true, v) -> Some v | _ -> None

    let trim (s: string) = s.Trim()

    let contains (sub: string) (s: string) = s.Contains (sub, StringComparison.CurrentCultureIgnoreCase)

    let charsToStr (c: char seq) = c |> Seq.toArray |> String

    let parseGrid (parse: char -> 'a) (s: string) =
        s
        |> readLines
        |> Seq.mapi (fun row str -> str |> Seq.mapi (fun col c -> ({ X = col; Y = row }, parse c)))
        |> Seq.concat

    // Sequence

    // Morelinq implementation is way faster than implementation on Rosetta code
    let runLengthEncode (x: 'a seq) = x.RunLengthEncode()

    let hasAtLeast n (x: 'a seq) = x.AtLeast n

    let countIf f = Seq.filter f >> Seq.length

    let parallelCountIf f = PSeq.filter f >> PSeq.length

    let subsets (x: 'a seq) = x.Subsets()

    let subsetsWithSize (size: int) (x: 'a seq) = x.Subsets(size)

    let partialSort n (x: 'a seq) = x.PartialSort(n)

    let takeEvery n (x: 'a seq) = x.TakeEvery n

    let findDuplicate (x: 'a seq) =
        let cache = HashSet<'a>()
        x |> Seq.find (cache.Add >> not)

    let toReadOnlyHashSet (elems: 'a seq) = HashSet<'a>(elems) :> IReadOnlySet<'a>

    // Data structure
    let mapIntersect a b = seq {
        for KeyValue(k, va) in a do
            match Map.tryFind k b with
            | Some vb -> yield (va, vb)
            | None    -> () }

    // List
    // From: http://stackoverflow.com/questions/286427/calculating-permutations-in-f
    let rec permutations = function
        | []      -> seq [List.empty]
        | x :: xs -> Seq.collect (insertions x) (permutations xs)
    and insertions x = function
        | []             -> [[x]]
        | (y :: ys) as xs -> (x::xs)::(List.map (fun x -> y::x) (insertions x ys))

    let partitionSingle x =
        let rec partitionSingleR x acc = 
            seq {
                match x with
                | [] -> ()
                | h::t -> yield h, acc @ t
                          yield! partitionSingleR t (h::acc)
                }

        partitionSingleR x []

    let rec uniquePairs elems = 
        seq { 
            match elems with
            | h::t -> for elem in t -> h, elem
                      yield! uniquePairs t
            | _ -> () }

    // Dictionary
    let tryFind (item: 'a) (dict: IReadOnlyDictionary<'a,_>) =
        match dict.TryGetValue item with
        | true, v -> Some v
        | _ -> None

    // Function
    let memoise f =
        let cache = Dictionary<_, _>()

        fun x ->
            match cache.TryGetValue x with
            | true, v -> v
            | _ -> let v = f x
                   cache.Add (x, v)
                   v

    // Graph
    let breadthFirstSearch getChildren (nodes: 'a list) =
        let visited = HashSet<'a>(nodes)

        let getUnseenChildren node = 
            let children =
                getChildren node
                |> List.filter (visited.Contains >> not)

            children |> List.iter (visited.Add >> ignore)
            children

        let rec breadthFirstSearchR nodes = 
            seq {
                match nodes with
                | [] -> ()
                | h::t ->
                    yield h
                    yield! breadthFirstSearchR (t @ (getUnseenChildren h))
            }

        breadthFirstSearchR nodes

    let breadthFirstSearchIterative getChildren (nodes: 'a list) =
        let visited = HashSet<'a>(nodes)
        let queue = new Queue<'a>(nodes)

        let getUnseenChildren node = 
            let children =
                getChildren node
                |> List.filter (visited.Contains >> not)

            children |> List.iter (visited.Add >> ignore)
            children

        seq {
            while (queue.Count > 0) do
                let current = queue.Dequeue()
                yield current

                for child in getUnseenChildren current do
                    queue.Enqueue child
                    visited.Add child |> ignore
            }

    // Active patterns
    let (|Int|_|) = tryParseAsInt

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    // Custom Operators
    let inline (>=<) lowest highest value = value >= lowest && value <= highest
