namespace AdventOfCode.Days.Y2024

open AdventOfCode.Shared.Utility

/// Day 9: Disk Fragmenter
/// https://adventofcode.com/2024/day/9
/// Another push of the button leaves you in the familiar hallways of some friendly amphipods!
module Day09 =

    let parseInput() = getFile (2024, 9) |> readAllText

    let moveBlocks (arr: int array) =
        let rec moveBlocksR p1 p2 =
            if p1 > p2 then arr
            else if arr[p1] = -1 && arr[p2] <> -1 then
                arr[p1] <- arr[p2]
                arr[p2] <- -1
                moveBlocksR (p1 + 1) (p2 - 1)
            else if arr[p2] = -1 then moveBlocksR p1 (p2 - 1)
            else moveBlocksR (p1 + 1) p2

        moveBlocksR 0 (arr.Length - 1)

    //let getCheckSum (arr: int array) =
    //    let rec getCheckSumR p1 p2 acc =
    //        if p1 > p2 then acc
    //        else if arr[p1] = -1 && arr[p2] <> -1 then
    //            //arr[p1] <- arr[p2]
    //            //arr[p2] <- -1
    //            getCheckSumR (p1 + 1) (p2 - 1) (acc + (int64 p1) * (int64 arr[p2]))
    //        else if arr[p2] = -1 then getCheckSumR p1 (p2 - 1) acc
    //        else getCheckSumR (p1 + 1) p2 (acc + (int64 p1) * (int64 arr[p1]))
    //
    //    getCheckSumR 0 (arr.Length - 1) 0L

    let getBlockSum (i: int) (c: int) =
        ((i * 2 + c - 1) * c) / 2 |> int64

    let getCheckSum2 (gaps: int list) (blocks: (int * int) list) =
        let rec getCheckSum2R gaps blocks acc index =
            match gaps, blocks with
            | [], _ -> acc // All gaps filled
            | _, [] -> acc // All possible blocks moved
            | 0::gs, bs -> getCheckSum2R gs bs acc index // Discard zero length gap
            | g1::gs, (i, c)::bs when c <= g1 -> // Move block
                getCheckSum2R ((g1 - c)::gs) bs (acc + (getBlockSum index c) * int64 i) (index + c)
            | _, b1::bs -> getCheckSum2R gaps bs acc index // Try next block
            | x -> failwithf "Invalid input: %A" x

        let i = blocks |> List.rev |> List.head |> snd
        getCheckSum2R gaps blocks 0L i

    let Part1() =
        let input = parseInput()

        let nums = input |> Seq.map (System.Char.GetNumericValue >> int)

        let arr =
            nums
            |> Seq.mapi (fun i x -> if i % 2 = 0 then Seq.replicate x (i / 2) else Seq.replicate x -1)
            |> Seq.concat
            |> Seq.toArray

        let arr2 = moveBlocks arr

        arr2
        |> Seq.mapi (fun i x -> if x = -1 then 0 else i * x)
        |> Seq.fold (fun acc x -> acc + int64 x) 0L

    let Part2() =
        let input = parseInput()

        let nums =
            input
            |> Seq.map (System.Char.GetNumericValue >> int)
            |> Seq.toList

        let (blocks, gaps) =
            nums
            |> List.indexed
            |> List.partition (fun (i, x) -> i % 2 = 0)

        let gaps = gaps |> List.map snd |> List.filter (fun x -> x <> 0)
        let blocks = blocks |> List.map (fun (x, y) -> (x / 2, y)) |> List.rev

        getCheckSum2 gaps blocks