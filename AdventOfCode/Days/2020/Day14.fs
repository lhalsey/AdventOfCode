namespace AdventOfCode.Days.Y2020

open AdventOfCode.Shared.Utility

/// Day 14: Docking Data
/// https://adventofcode.com/2020/day/14
/// As your ferry approaches the sea port, the captain asks for your help again
module Day14 =

    type Write = { Address: int64; Value: int64 }
    type MaskBit = One | Zero | X
    type MaskBits = (int * MaskBit) list
    type Instruction = Mask of MaskBits | Write of Write

    let getMask (s: string) = 
        s
        |> Seq.rev // Reverse as most significant bit is on the left
        |> Seq.map (function '0' -> Zero | '1' -> One | 'X' -> X | x -> failwithf "Invalid: %c" x)
        |> Seq.indexed
        |> Seq.toList
        |> Mask

    // E.g. "mask = 11100010111110X010100001X00000011XXX" or "mem[6540] = 1053547115"
    let parse (s: string) : Instruction =
        match s with
        | Regex "mask = (.+)" [m]  -> getMask m
        | Regex "mem\[(\d+)\] = (\d+)" [a; v] -> Write { Address = int64 a; Value = int64 v }
        | _ -> failwithf "Invalid input: %s" s
        
    let parseInput() = getFile (2020, 14) |> readLinesAs parse |> Seq.toList

    let setBit num i = num ||| (1L <<< i)

    let clearBit num i = num &&& ~~~(1L <<< i)

    // 0 or 1 overwrites the corresponding bit in the value, while an X leaves the bit in the value unchanged
    let decodeV1 (mask: MaskBits) (w: Write) = 
        let decode value (i, bit) =
            match bit with
            | X -> value
            | One -> setBit value i
            | Zero -> clearBit value i
        
        let value = mask |> List.fold decode w.Value
        (w.Address, value) |> Seq.singleton

    // 0 is unchanged, 1 is overwritten with 1, X is floating bit. The floating bits will take on all possible
    // values, potentially causing many memory addresses to be written all at once!
    let decodeV2 (mask: MaskBits) (w: Write) = 
        let decode value (i, bit) =
            match bit with
            | X -> [ clearBit value i; setBit value i ]
            | One -> [ setBit value i ]
            | Zero -> [ value ]

        let rec getAddresses bits acc =
            seq {
                match bits with
                | [] -> yield acc
                | h::t -> for d in decode acc h do yield! getAddresses t d }

        getAddresses mask w.Address
        |> Seq.map (fun address -> address, w.Value)

    let execute decode input =
        let rec executeR input (mask: MaskBits) =
            seq {
                match input with
                | [] -> ()
                | Mask m::t -> yield! executeR t m
                | Write w::t -> yield! decode mask w; yield! executeR t mask }

        // It's easier to return a sequence than maintain a map
        // Just need to reverse to get last value at each address
        executeR input []
        |> Seq.rev
        |> Seq.distinctBy fst
        |> Seq.sumBy snd


    // Execute the initialization program. What is the sum of all values left in memory after it completes?
    let Part1() = parseInput() |> execute decodeV1

    // Execute the initialization program using an emulator for a version 2 decoder chip.
    // What is the sum of all values left in memory after it completes?
    let Part2() = parseInput() |> execute decodeV2