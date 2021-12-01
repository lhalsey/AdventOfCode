namespace AdventOfCode.Days.Y2016

open AdventOfCode.Shared.Utility
open System.Security.Cryptography
open System.Text
open System
open FSharp.Collections.ParallelSeq
open System.Threading

/// Day 5: How About a Nice Game of Chess?
/// https://adventofcode.com/2016/day/5
/// You are faced with a security door designed by Easter Bunny engineers that seem to
/// have acquired most of their security knowledge by watching hacking movies.
module Day05 =

    type Mode = V1 | V2

    let [<Literal>] Key = "ojvtpuvg"
    let [<Literal>] PasswordLength = 8

    // Don't really have much choice other than to brute force so do in parallel
    let findPassword thirdHexadecimalUpperBound (mode: Mode) =
        let mutable hashIndex = 0
        let mutable charIndex = 0
        let mutable password = Array.replicate PasswordLength '?'
        let lock1 = new Object()

        // A valid hash indicates the next character
        let updatePasswordV1 (hash: byte[]) =
            let value = hash.[2].ToString("x").[0]
            password.[charIndex] <- value
            charIndex <- charIndex + 1

        // The sixth character represents the position (0-7), and the seventh character
        // is the character to put in that position
        let updatePasswordV2 (hash: byte[]) =
            let i = int hash.[2]
            let value = hash.[3].ToString("x2").[0]
            if password.[i] = '?' then password.[i] <- value

        let updatePassword =
            match mode with
            | V1 -> updatePasswordV1
            | V2 -> updatePasswordV2

        let findHashes() =
            use hash = MD5.Create()

            let keyBytes = Encoding.ASCII.GetBytes Key

            let getHash n = 
                Encoding.ASCII.GetBytes (string n)
                |> Array.append keyBytes
                |> hash.ComputeHash

            let isValid (hash: byte[]) =
                hash.[0] = 0uy && hash.[1] = 0uy && hash.[2] < thirdHexadecimalUpperBound

            let rec loop() =
                let i = Interlocked.Increment &hashIndex
                let hash = getHash i
                if isValid hash then
                    lock (lock1) (fun () -> updatePassword hash)
                    
                if password |> Array.contains '?' then loop()

            loop()

        [ 1 .. Environment.ProcessorCount ] |> PSeq.iter (fun _ -> findHashes())

        password |> charsToStr


    // The sixth character in the valid hash is the next character of the password.
    // Given the actual Door ID, what is the password?
    let Part1() = findPassword (byte 0x10) V1

    // Instead of simply filling in the password from left to right, the hash now also
    // indicates the position within the password to fill.
    // Given the actual Door ID and this new method, what is the password?
    let Part2() = findPassword (byte 0x08) V2
