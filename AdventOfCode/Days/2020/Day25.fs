namespace AdventOfCode.Days.Y2020

/// Day 25: Combo Breaker
/// https://adventofcode.com/2020/day/25
/// You finally reach the check-in desk.
module Day25 =

    let CardPublicKey = 14_222_596L
    let DoorPublicKey = 4_057_428L
    let Divisor = 20_201_227L
    let Multiplier = 7L

    // When value matches first public key then we will have iterated
    // loopsize times and key will hold the value of the encryption key
    let getEncryptionKey pk1 pk2 =
        let rec getEncryptionKeyR value key =
            match value with
            | v when v = pk1 -> key
            | _ -> let value = (value * Multiplier) % Divisor
                   let key = (key * pk2) % Divisor
                   getEncryptionKeyR value key

        getEncryptionKeyR 1L 1L


    // What encryption key is the handshake trying to establish?
    let Part1() = getEncryptionKey CardPublicKey DoorPublicKey

    let Part2() = 1 // Check on your deposit