namespace AdventOfCode.Days.Y2020

/// Day 25: Combo Breaker
/// https://adventofcode.com/2020/day/25
/// You finally reach the check-in desk.
module Day25 =

    let CardPublicKey = 14_222_596L
    let DoorPublicKey = 4_057_428L

    let getSequence subject = 1L |> Seq.unfold (fun x -> Some(x, (x * subject) % 20_201_227L))

    let getLoopSize target = getSequence 7L |> Seq.findIndex ((=) target)

    let getEncryptionKey subject loopSize = getSequence subject |> Seq.item loopSize

    // What encryption key is the handshake trying to establish?
    let Part1() =
        CardPublicKey
        |> getLoopSize
        |> getEncryptionKey DoorPublicKey

    let Part2() = 1 // Check on your deposit