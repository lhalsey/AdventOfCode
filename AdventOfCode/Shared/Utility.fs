namespace AdventOfCode.Shared

open System.IO
open System.Text.RegularExpressions
open System.Collections.Generic

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
    let split (delimiter: char) (s: string) = s.Split([| delimiter |])

    let trim (s: string) = s.Trim()

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let parseGrid (parse: char -> 'a) (s: string) =
        s
        |> readLines
        |> Seq.mapi (fun row str -> str |> Seq.mapi (fun col c -> ({ X = col; Y = row }, parse c)))
        |> Seq.concat

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

    let rec partitionSingle x acc = 
        seq {
            match x with
            | [] -> ()
            | h::t -> yield h, acc @ t
                      yield! partitionSingle t (h::acc)
            }

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

