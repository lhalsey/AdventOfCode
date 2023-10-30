namespace AdventOfCode.Days.Y2022

open AdventOfCode.Shared.Utility

/// Day 13: Distress Signal
/// https://adventofcode.com/2022/day/13
/// You climb the hill and again try contacting the Elves. However, you instead receive a signal
/// you weren't expecting: a distress signal.
module Day13 =

    type Packet = Single of int | List of Packet list

    let parse (s: string) =
        let rec parseR (chars: char list) (packets: Packet list) (currValue: int option) =
            seq {
                match chars, currValue with
                | [], _ -> ()
                | '['::t, _ -> yield! parseR t [] None
                | (CInt c)::t, Some cv -> yield! parseR t packets (Some (cv * 10 + c))
                | (CInt c)::t, None -> yield! parseR t packets (Some c)
                | ','::t, Some cv -> yield! parseR t (Single cv::packets) None
                | ','::t, None -> yield! parseR t packets None
                | ']'::t, Some cv ->
                    let packet = packets.Head
                    yield (List.rev (Single cv::packets))
                    yield! parseR t [] None
                | ']'::t, None ->
                    yield (List.rev packets)
                    yield! parseR t [] None
                | x -> failwithf "Invalid state: %A" x
            }

        parseR (Seq.toList s) [] None |> Seq.toList |> List.map List |> List

    let parseInput() =
        getFile (2022, 13)
        |> readLines
        |> Seq.chunkBySize 3
        |> Seq.map (fun x -> parse x[0], parse x[1])

    let rec isOrdered (p1: Packet, p2: Packet) =
        match p1, p2 with
        | Single s1, Single s2 -> s1 <= s2
        | Single s1, List l2 -> isOrdered ((List [Single s1]), (List l2))
        | List l1, Single s2 -> isOrdered ((List l1), (List [Single s2]))
        | List [], _ -> true
        | List _, List [] -> false
        | List l1, List l2 -> Seq.zip l1 l2 |> Seq.forall isOrdered

    let Part1() =
        //let (row1: Packet) = List [ Single 1; Single 1; Single 3; Single 1; Single 1 ]
        //let (row3: Packet) = List [ List [ Single 1 ]; List [ Single 2; Single 3; Single 4 ] ]

        //let b1 = isOrdered (row1, row3)

        let input = parseInput() |> Seq.toList

        let indexes =
            input
            |> List.indexed
            |> List.filter (fun (_, x) -> isOrdered x)
            |> List.map (fun (i, _) -> i + 1)

        List.sum indexes

    let Part2() =
        0