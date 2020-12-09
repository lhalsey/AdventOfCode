namespace AdventOfCode.Days.Y2019

open AdventOfCode.Shared.Utility
open AdventOfCode.Days.Y2019.Shared.IntCodeInterpreter

/// Day 23: Category Six
/// https://adventofcode.com/2019/day/23
/// The droids have finished repairing as much of the ship as they can.
module Day23 =

    let [<Literal>] NATAddress = 255L
    let [<Literal>] EmptyValue = -1L
    let [<Literal>] IdleThreshold = 2

    type Computer = { Id: int64; State: ProgramState }

    type Packet = { From: int64; To: int64; X: int64; Y: int64 }

    type SystemState = { PacketMap: Map<int64, (int64 * int64) list>; IdleTime: int }

    let getInterpreter() = getFile (2019, 23) |> Interpreter.Create

    let getPackets (packetMap: Map<int64, (int64 * int64) list>) address =
        match packetMap.TryFind address with
        | Some packets -> packets
        | None -> []

    let extractPackets id output =
        let extractPacket = function
            | [ address; x; y ] -> { From = id; To = address; X = x; Y = y }
            | x -> failwithf "Expected 3 values in packet: %A" x
        
        output
        |> List.chunkBySize 3
        |> List.map extractPacket

    let sendPackets (systemState: SystemState) (packets: Packet list) =
        let enqueuePacket systemState packet =
            let existingPackets = getPackets systemState.PacketMap packet.To
            let newPackets = existingPackets @ [(packet.X, packet.Y)]
            { systemState with PacketMap = systemState.PacketMap.Add(packet.To, newPackets) }
                    
        let systemState = (systemState, packets) ||> List.fold enqueuePacket

        { systemState with IdleTime = 0 }

    let handleInput (systemState: SystemState) id f =
        match getPackets systemState.PacketMap id with
        | [] -> (f EmptyValue, { systemState with IdleTime = systemState.IdleTime + 1 })
        | (x, y)::t ->
            let state = f x |> provideInput y // Provide X and Y input
            let packetMap = systemState.PacketMap.Add(id, t) // Remove X and Y from map
            (state, { systemState with PacketMap = packetMap; IdleTime = 0 })

    // Run one computer at a time, sending or receiving values via the packet map
    let rec runAll (computers: Computer list) (systemState: SystemState) =
        seq {
            match computers with
            | { Id = id; State = Output(output, state) }::t ->
                // Add packets to map and move computer to end of list
                let packets = extractPackets id output
                let systemState = sendPackets systemState packets
                yield! packets
                let computers = t @ [{ Id = id; State = state } ]
                yield! runAll computers systemState
            | { Id = 0L; State = Input f }::_
                    when systemState.IdleTime >= computers.Length * IdleThreshold ->
                // All computers waiting for input so send last NAT value to computer 0
                let (x, y) = getPackets systemState.PacketMap NATAddress |> List.last
                let lastNATPacket = { From = NATAddress; To = 0L; X = x; Y = y }
                let systemState = sendPackets systemState [ lastNATPacket ]
                yield lastNATPacket
                let packetMap = systemState.PacketMap.Add(NATAddress, [(x, y)]) // Just keep last
                let systemState = { systemState with PacketMap = packetMap }
                yield! runAll computers systemState
            | { Id = id; State = Input f }::t ->
                // Provide input (or -1 if none) to computer and move to end of list
                let (state, systemState) = handleInput systemState id f
                let computers = t @ [{ Id = id; State = state} ]
                yield! runAll computers systemState
            | x -> failwithf "Unexpected state: %A" x
        }
        
    let run() =
        let interpreter = getInterpreter() |> run

        let getComputer id = { Id = id; State = interpreter |> provideInput id }

        let computers = List.init 50 (int64 >> getComputer)
        
        runAll computers { PacketMap = Map.empty; IdleTime = 0 }


    // Boot up all 50 computers and attach them to your network.
    // What is the Y value of the first packet sent to address 255?
    let Part1() =
        run()
        |> Seq.find (fun packet -> packet.To = NATAddress)
        |> fun packet -> packet.Y

    // Monitor packets released to the computer at address 0 by the NAT. What is the first Y value
    // delivered by the NAT to the computer at address 0 twice in a row?
    let Part2() =
        run()
        |> Seq.filter (fun packet -> packet.From = NATAddress)
        |> Seq.pairwise
        |> Seq.find (fun (p1, p2) -> p1.Y = p2.Y)
        |> fun (packet, _) -> packet.Y