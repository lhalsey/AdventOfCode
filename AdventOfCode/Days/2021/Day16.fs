namespace AdventOfCode.Days.Y2021

open AdventOfCode.Shared.Utility
open System

/// Day 16: Packet Decoder
/// https://adventofcode.com/2021/day/16
/// As you leave the cave and reach open waters, you receive a transmission from the Elves back on the ship.
module Day16 =

    type Content = Literal of int64 | Operator of int * Packet list
    and Packet = { Version: int; Content: Content }

    let hexToBinary (c: char) =
        Convert.ToInt32(string c, 16)
        |> fun x -> Convert.ToString(x, 2).PadLeft(4, '0')

    let binaryToDecimal x = Convert.ToInt32(x, 2)

    let getBits (n: int) (s: string) = binaryToDecimal s.[..n-1], s.[n..]

    let parseInput() =
        getFile (2021, 16) 
        |> readAllText 
        |> Seq.map hexToBinary
        |> Seq.toArray
        |> String.concat ""

    let decodeLiteral (bits: string): Content * string =
        let rec decodeLiteralR (b: string) (num: int64) =
            let (hasMore, b) = b |> getBits 1
            let (hex, b) = b |> getBits 4
            let num = num * 16L + (int64 hex)

            match hasMore with
            | 1 -> decodeLiteralR b num
            | 0 -> Literal num, b
            | _ -> failwith "non-binary"

        decodeLiteralR bits 0

    let rec decodeOperator (bits: string) (packetType: int) =
        let (lengthType, b) = bits |> getBits 1
       
        match lengthType with
        | 0 -> let (bitLength, b) = b |> getBits 15
               let (packetsBits, b) = b.[..bitLength - 1], b.[bitLength ..]
               let (packets, _) = getPackets packetsBits None
               Operator (packetType, packets), b
        | 1 -> let (numPackets, b) = b |> getBits 11
               let (packets, b) = getPackets b (Some numPackets)
               Operator (packetType, packets), b
        | _ -> failwithf "Invalid length type: %d" lengthType
               
    and getPackets (bits: string) (numPackets: int option) =
        let rec getPacketsR (b: string) (packets: Packet list) =
            match numPackets with
            | Some x when packets.Length = x -> List.rev packets, b
            | _ when b.Length < 3 -> List.rev packets, b
            | _ ->
                let (version, b) = b |> getBits 3
                let (packetType, b) = b |> getBits 3

                let (content, rest) = 
                    match packetType with
                    | 4 -> decodeLiteral b
                    | _ -> decodeOperator b packetType

                let packet = { Version = version; Content = content }

                getPacketsR rest (packet::packets)

        getPacketsR bits []


    let sumBy sumF (packet: Packet) =
        let getChildren (p: Packet) = match p.Content with Operator (_, xs) -> seq xs | _ -> seq []
        
        packet
        |> bfs getChildren
        |> Seq.sumBy sumF

    let rec getValue (packet: Packet) =
        match packet.Content with
        | Literal x -> x
        | Operator (0, ps) -> ps |> List.map getValue |> List.reduce (+)
        | Operator (1, ps) -> ps |> List.map getValue |> List.reduce (*)
        | Operator (2, ps) -> ps |> List.map getValue |> List.min
        | Operator (3, ps) -> ps |> List.map getValue |> List.max
        | Operator (5, ps) -> ps |> List.map getValue |> fun x -> if x.[0] > x.[1] then 1L else 0L
        | Operator (6, ps) -> ps |> List.map getValue |> fun x -> if x.[0] < x.[1] then 1L else 0L
        | Operator (7, ps) -> ps |> List.map getValue |> fun x -> if x.[0] = x.[1] then 1L else 0L
        | _ -> failwithf "Unexpected input: %A" packet

    let getRootPacket() =
        let bits = parseInput()
        let (packets, _) = getPackets bits (Some 1)
        packets.Head

    // Decode the structure of your hexadecimal-encoded BITS transmission
    // What do you get if you add up the version numbers in all packets?
    let Part1() = getRootPacket() |> sumBy (fun x -> x.Version)

    // What do you get if you evaluate the expression represented by your hexadecimal-encoded BITS transmission?
    let Part2() = getRootPacket() |> getValue