namespace AdventOfCode.Days.Y2021

open AdventOfCode.Shared
open AdventOfCode.Shared.Utility
open System.Collections.Generic
open FSharp.Collections.ParallelSeq

/// Day 19: Beacon Scanner
/// https://adventofcode.com/2021/day/19
/// As your probe drifted down through this area, it released an assortment of beacons and scanners into the water.
module Day19 =

    let [<Literal>] MinimumCommonBeacons = 12

    type Scanner = { Id: int; Origin: Point3d; Beacons: Point3d list }

    let parseLines (lines: string list) = 
        let rec parseLinesR lines beacons =
            seq {
                match lines with
                | (Regex "--- scanner (\d+) ---" _)::xs -> yield! parseLinesR xs []
                | (Regex "([+-]*\d+),([+-]*\d+),([+-]*\d+)" [Int x; Int y; Int z])::xs ->
                    let beacon = { X = x; Y = y; Z = z }
                    yield! parseLinesR xs (beacon::beacons)
                | [] -> yield beacons
                | _::xs -> yield beacons         
                           yield! parseLinesR xs []
                }
        parseLinesR lines [] |> Seq.toList |> List.map (List.sortBy (fun x -> x.ManhattanDistance))

    let parseInput() =
        getFile (2021, 19)
        |> readLines
        |> Seq.toList
        |> parseLines
        |> List.mapi (fun i x -> { Id = i; Origin = Point3d.Origin; Beacons = x })

    let normalise (pivot: Point3d) (beacons: Point3d list) =
        beacons
        |> List.map (fun b -> b.ManhattanDistanceTo pivot)

    let perms =
        let axes = permutations [0..2] |> Seq.toList
        let directions = [0..7]
        let combos = List.allPairs axes directions

        let getPerm (a: int) (d: int) =
            match (a, d) with
            | (0, d) -> fun (p: Point3d) -> p.X * (if d % 2 = 0 then 1 else -1)
            | (1, d) -> fun p -> p.Y * (if (d/2) % 2 = 0 then 1 else -1)
            | (2, d) -> fun p -> p.Z * (if (d/4) % 2 = 0 then 1 else -1)
            | x -> failwithf "Invalid input %A" x

        let getTransform (fs: (Point3d -> int) list) =
            fun (p: Point3d) -> { X = fs.[0] p; Y = fs.[1] p; Z = fs.[2] p }

        combos
        |> List.map (fun (a, d) -> a |> List.map (fun x -> getPerm x d) |> getTransform)

    let tryTransform (transformed: Scanner, untransformed: Scanner) =
        let(s1, s2) = transformed.Beacons, untransformed.Beacons

        let getEquivalent (common: int Set) (n1: int list) (n2: int list) =
            let ns = common |> Set.toList
            let i1 = ns |> List.map (fun x -> n1 |> List.findIndex (fun y -> y = x))
            let i2 = ns |> List.map (fun x -> n2 |> List.findIndex (fun y -> y = x))

            (i1, i2) 
            ||> List.zip
            |> List.map (fun (x, y) -> s1.[x], s2.[y])

        let overlap (p1: Point3d, p2: Point3d) =
            let n1 = normalise p1 s1
            let n2 = normalise p2 s2
            let common = Set.intersect (set n1) (set n2)

            if common.Count < MinimumCommonBeacons
            then None
            else getEquivalent common n1 n2 |> Some


        let isValid (s1: (Point3d * Point3d) list) (transform: Point3d -> Point3d) =
            let (p1, p2) = s1.[0]
            let offset = p1 - transform p2

            let allPointsMatch = s1 |> List.forall (fun (pp1, pp2) -> pp1 = transform pp2 + offset)

            if allPointsMatch then Some (transform, offset) else None

        let getValid (s1: (Point3d * Point3d) list) = perms |> List.tryPick (isValid s1)

        let transformScanner (transform, offset) =
            let beacons = untransformed.Beacons |> List.map (fun p -> transform p + offset)
            { untransformed with Origin = offset; Beacons = beacons }

        Seq.allPairs s1 s2
        |> Seq.choose overlap
        |> Seq.tryPick getValid
        |> Option.map transformScanner


    let getTransformedScanners() =
        let scanners = parseInput()
         
        // Compare each untransformed scanner against the transformed scanners to find overlaps
        let rec compareAll (untransformed: Scanner list) (transformed: Scanner list) =
       
            match untransformed with
            | [] -> transformed
            | u::us -> 
                transformed
                |> PSeq.map (fun t -> tryTransform (t, u))
                |> Seq.tryPick id
                |> function
                    | None -> compareAll (us @ [u]) transformed // No current match so put to end and try next
                    | Some x -> compareAll us (x::transformed) // Matched so add to transformed and try next

        // All "absolute" positions will be expressed relative to scanner 0
        // (using the orientation of scanner 0 and as if scanner 0 is at coordinates 0,0,0).
        let (transformed, untransformed) = scanners |> List.splitAt 1
        
        compareAll untransformed transformed

    // Assemble the full map of beacons. How many beacons are there?
    let Part1() =
        getTransformedScanners()
        |> List.collect (fun x -> x.Beacons)
        |> countDistinct

    // What is the largest Manhattan distance between any two scanners?
    let Part2() =
        getTransformedScanners()
        |> List.map (fun x -> x.Origin)
        |> uniquePairs
        |> Seq.map (fun (x, y) -> x.ManhattanDistanceTo(y))
        |> Seq.max