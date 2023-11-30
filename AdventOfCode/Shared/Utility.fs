namespace AdventOfCode.Shared

open System
open System.IO
open System.Text.RegularExpressions
open System.Collections.Generic
open MoreLinq.Extensions
open FSharp.Collections.ParallelSeq
open Priority_Queue

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
    let split (delimiter: char) (s: string) = s.Split([| delimiter |], StringSplitOptions.RemoveEmptyEntries)

    let splitOn (delimiter: string) (s: string) = s.Split delimiter

    let splitAny (delimiters: string) (s: string) =
        s.Split(Seq.toArray delimiters, StringSplitOptions.RemoveEmptyEntries)

    let splitIntoPair (delimiter: string) (s:string) =
        let tokens = s.Split(delimiter)
        tokens.[0], tokens.[1]

    let splitIntoPairAs (delimiter: string) f (s:string) =
        let tokens = s.Split(delimiter)
        f tokens.[0], f tokens.[1]

    let splitBy (token: 'a) (elems: 'a seq) = elems.Split(token)

    let replace (source: string) (dest: string) (s: string) = s.Replace (source, dest)

    let tryParseAsInt (s: string) = match Int32.TryParse s with (true, v) -> Some v | _ -> None

    let tryParseAsInt64 (s: string) = match Int64.TryParse s with (true, v) -> Some v | _ -> None

    let tryParseAsBigInt (s: string) = match bigint.TryParse s with (true, v) -> Some v | _ -> None

    let tryParseAsFloat (s: string) = match Double.TryParse s with (true, v) -> Some v | _ -> None

    let tryParseAsDateTime (s: string) = match DateTime.TryParse s with (true, v) -> Some v | _ -> None

    let trim (s: string) = s.Trim()

    let contains (sub: string) (s: string) = s.Contains (sub, StringComparison.CurrentCultureIgnoreCase)

    let charsToStr (c: char seq) = c |> Seq.toArray |> String

    let reverse (s: string) = s |> Seq.rev |> Seq.toArray  |> String

    let parseGrid (parse: char -> 'a) (s: string) =
        s
        |> readLines
        |> Seq.mapi (fun row str -> str |> Seq.mapi (fun col c -> ({ X = col; Y = row }, parse c)))
        |> Seq.concat

    // Sequence

    // Morelinq implementation is way faster than implementation on Rosetta code
    let runLengthEncode (x: 'a seq) = x.RunLengthEncode()

    let hasAtLeast n (x: 'a seq) = x.AtLeast n

    let hasAtMost n (x: 'a seq) = x.AtMost n

    let countIf f = Seq.filter f >> Seq.length

    let parallelCountIf f = PSeq.filter f >> PSeq.length

    let subsets (x: 'a seq) = x.Subsets()

    let subsetsWithSize (size: int) (x: 'a seq) = x.Subsets(size)

    let partialSort n (x: 'a seq) = x.PartialSort(n)

    let partialSortDesc n (x: 'a seq) = x.PartialSort(n, MoreLinq.OrderByDirection.Descending)

    let takeEvery n (x: 'a seq) = x.TakeEvery n

    let takeUntil (f: 'a -> bool) (x: 'a seq) = x.TakeUntil f

    let countBetween min max (x: 'a seq) = x.CountBetween (min, max)

    let hasExactly n (x: 'a seq) = x.Exactly n

    let findDuplicate (elems: 'a seq) =
        let cache = HashSet<'a>()
        elems |> Seq.find (cache.Add >> not)

    let tryFindDuplicate (elems: 'a seq) =
        let cache = HashSet<'a>()
        elems |> Seq.tryFind (cache.Add >> not)

    let findDuplicateBy f (elems: 'a seq) =
        let cache = HashSet<'b>()
        elems |> Seq.find (fun x -> cache.Add (f x) |> not)

    let findIndexes pred (elems: 'a seq) =
        elems
        |> Seq.indexed
        |> Seq.filter (fun (_, i) -> pred i)
        |> Seq.map fst

    let rec repeatInfinite (elems: 'a seq) = seq { yield! elems; yield! repeatInfinite elems }

    let toHashSet (elems: 'a seq) = HashSet<'a>(elems)

    let toReadOnlyHashSet (elems: 'a seq) = HashSet<'a>(elems) :> IReadOnlySet<'a>

    let median (elems: double seq) = MathNet.Numerics.Statistics.Statistics.Median(elems)

    let remainder a b = (a % b + b) % b

    // Data structure
    let mapIntersect a b = seq {
        for KeyValue(k, va) in a do
            match Map.tryFind k b with
            | Some vb -> yield (va, vb)
            | None    -> () }

    let mapCombine (source: Map<'a, 'b>) (target: Map<'a, 'b>) f = 
        let combine (acc: Map<'a, 'b>) (x: KeyValuePair<'a, 'b>) =
            match Map.tryFind x.Key acc with
            | Some v -> acc.Add(x.Key, (f x.Value v))
            | None    -> acc.Add(x.Key, x.Value)

        (source, target) ||> Seq.fold combine

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

    let countDistinct elems =
        let hs = new HashSet<_>()

        elems |> countIf hs.Add

    let countUntilDuplicate elems =
        let hs = new HashSet<_>()

        elems |> Seq.takeWhile hs.Add |> Seq.length

    let countUntilDuplicateBy f elems =
        let hs = new HashSet<_>()

        elems |> Seq.takeWhile (fun x -> hs.Add (f x)) |> Seq.length

    let areAllDistinct elems =
        let hs = new HashSet<_>()
        elems |> Seq.forall hs.Add

    let areAllDistinctBy f elems =
        let hs = new HashSet<_>()
        elems |> Seq.forall (fun x -> hs.Add (f x))

    // Dictionary
    let tryFind (item: 'a) (dict: IReadOnlyDictionary<'a,_>) =
        match dict.TryGetValue item with
        | true, v -> Some v
        | _ -> None

    let dictIntersect a (b: IReadOnlyDictionary<'a, 'b>) = seq {
        for KeyValue(k, va) in a do
            match tryFind k b with
            | Some vb -> yield (va, vb)
            | None    -> () }

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
    let bfs (getChildren: Func<'a, IEnumerable<'a>>) root = MoreLinq.MoreEnumerable.TraverseBreadthFirst(root, getChildren)

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

    // https://en.wikipedia.org/wiki/A*_search_algorithm
    // Search graph prioritising best nodes based on current cost + estimated cost to target
    let aStar
        (start: 'a) 
        (isGoal: 'a -> bool)
        (getChildren: 'a -> ('a * int) seq)
        (getEstimate: 'a -> int) =
            let queue = SimplePriorityQueue<'a, int>()
            let costMap = Dictionary<'a, int>()
    
            costMap.Add(start, 0)
            queue.Enqueue(start, getEstimate start)

            seq {
                while (queue.Count > 0) do
                    let state = queue.Dequeue()
        
                    if isGoal state then yield costMap.[state]

                    let currCost = costMap.[state]

                    for node, moveCost in getChildren state do
                        let cost = currCost + moveCost
                        let priority = cost + getEstimate node

                        match costMap.TryGetValue node with
                        | true, v when cost < v -> // Found a better way to reach this node
                            costMap.[node] <- cost
                            queue.UpdatePriority(node, priority)
                        | false, _ -> // First time visiting this node
                            costMap.Add(node, cost)
                            queue.Enqueue(node, priority)
                        | _ -> ()
                 }

    // Active patterns
    let (|Int|_|) = tryParseAsInt

    let (|Int64|_|) = tryParseAsInt64

    let (|BigInt|_|) = tryParseAsBigInt

    let (|Date|_|) = tryParseAsDateTime

    let (|CInt|_|) c = match Char.GetNumericValue c with -1.0 -> None | x -> Some (int x)

    let (|CInt64|_|) c = match Char.GetNumericValue c with -1.0 -> None | x -> Some (int64 x)

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let (|LowerCase|_|) (s: string) = if s |> Seq.forall Char.IsLower then Some s else None

    // Custom Operators
    let inline (>=<) lowest highest value = value >= lowest && value <= highest
