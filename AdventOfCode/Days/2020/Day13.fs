namespace AdventOfCode.Days.Y2020

open AdventOfCode.Shared.Utility
//open FSharpx.Collections
open MathNet.Numerics


/// Day 13: Shuttle Search
/// https://adventofcode.com/2020/day/13
/// Your ferry can make it safely to a nearby port, but it won't get much further.
module Day13 =

    let parseInput() =
        let lines = getFile (2020, 13) |> readLines |> Seq.toList
        let buses =
            lines.[1]
            |> split ','
            |> Array.indexed
            |> Array.filter (fun (_, x) -> x <> "x")
            |> Array.map (fun (i, x) -> int64 i, int64 x)
            |> Array.toList

        (int64 lines.[0], buses)

    // What is the ID of the earliest bus you can take to the airport multiplied by the
    // number of minutes you'll need to wait for that bus?
    let Part1() =
        let (target, buses) = parseInput()

        buses
        |> List.map (fun (_, id) -> id, id - target % id)
        |> List.minBy snd
        |> fun (id, time) -> id * time

    //let chineseRemainder (elems: (int64 * int64) list) =
    //    let (bs, ns) = List.unzip elems
    //    let getMs(a: int64) (b: int64) = Euclid.ExtendedGreatestCommonDivisor (a, b) |> fun (_, _, x) -> x
    //    let N = ns |> List.fold (*) 1L
    //    let Ns = ns |> List.map (fun n -> N / n)
    //    let ms = List.map2 getMs bs ns
    //    let As = List.map3 (fun x y z -> x * y * z) bs ms Ns
    //    Euclid.Modulus (List.sum As, N)

    let chineseRemainder (nums: (int64 * int64) list) =
        let nums =
            nums
            |> List.map (fun (x, y) -> Euclid.Modulus(-x, y), y)
            |> List.sortByDescending snd

        // 19 + 20 = 39 mod 3 -> 0
        // res + offset mod a -> n
        let findM res offset a n =
            let rec findMR res =
                if res % a = n then res else findMR (res + offset)
                
            findMR res

        let findRes elems =
            let rec findResR elems res offset =
                match elems with
                | [] -> res
                | (a, n)::t ->
                    let res = findM res offset n a
                    findResR t res (offset * n)
                    
               
            match elems with
            | (a, n)::t -> findResR t a n
            | _ -> -1L
        
           
        findRes nums


        //let findM (acc, step) (a, n) =
        //    acc
        //    |> Seq.unfold (fun x -> Some(x, x + step))
        //    |> Seq.find (fun x -> x % n = a)
        //    |> fun x -> x, step

        //let rec findResR elems =
        //    match elems with
        //    | [] -> res
        //    | (a1, n2)::(a2, n2)::t ->
        //        let res = findM res offset n a
        //        findResR t res (offset * n)

        //    match elems with
        //    | (a, n)::t -> findResR t a n
        //    | _ -> -1L

        //findResR nums
                    

        //((0L, 1L), nums)
        //||> List.fold (fun (acc, step) (a, n) -> findM (acc, step * a) (a, n)) 
        
        // 19 + 20 = 39 mod 3 -> 0
        // res + offset mod a -> n
        //let findM res offset a n =
        //    let rec findMR res =
        //        if res % a = n then res else findMR (res + offset)
                
        //    findMR res
        
        //let findRes elems =
        //    let rec findResR elems res offset =
        //        match elems with
        //        | [] -> res
        //        | (a, n)::t ->
        //            let res = findM res offset n a
        //            findResR t res (offset * n)
                    
               
        //    match elems with
        //    | (a, n)::t -> findResR t a n
        //    | _ -> -1L
        
           
        //findRes nums

    // What is the earliest timestamp such that all of the listed bus IDs depart at offsets matching
    // their positions in the list?
    let Part2() =
        let (_, buses) = parseInput()

        chineseRemainder buses

        //let f x step m t =
        //    x
        //    |> Seq.unfold (fun x -> Some(x, x + step))
        //    |> Seq.find (fun x -> x % m = t)

        //(1, buses)
        //||> List.fold (fun acc (i, x) -> f x (acc * x) 