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

    let [<Literal>] Key = "ojvtpuvg"
    let [<Literal>] PasswordLength = 8

    //let findHashes thirdHexadecimalUpperBound =
    //    let hash = MD5.Create() // TODO: Dispose!

    //    let keyBytes = Encoding.ASCII.GetBytes Key

    //    let getHash n = 
    //        Encoding.ASCII.GetBytes (string n)
    //        |> Array.append keyBytes
    //        |> hash.ComputeHash

    //    let isValid (value: byte[]) =
    //        value.[0] = 0uy && value.[1] = 0uy && value.[2] < thirdHexadecimalUpperBound

    //    Seq.initInfinite id
    //    |> Seq.map getHash
    //    |> Seq.filter isValid
    //    |> Seq.map (fun x -> x.[2].ToString("x"), x.[3].ToString("x2"))

    let findHashesParallel thirdHexadecimalUpperBound =
        let mutable index = 0
        let mutable password = ""
        let lock1 = new Object()

        let findHashes() =
            use hash = MD5.Create()

            let keyBytes = Encoding.ASCII.GetBytes Key

            let getHash n = 
                Encoding.ASCII.GetBytes (string n)
                |> Array.append keyBytes
                |> hash.ComputeHash

            let isValid (value: byte[]) =
                value.[0] = 0uy && value.[1] = 0uy && value.[2] < thirdHexadecimalUpperBound

            //Seq.initInfinite (fun _ -> Interlocked.Increment(&index))
            //|> Seq.map getHash
            //|> Seq.filter isValid
            //|> Seq.map (fun x -> x.[2].ToString("x"), x.[3].ToString("x2"))

            let rec loop() =
                let i = Interlocked.Increment &index
                let hash = getHash i
                if isValid hash then 
                    lock (lock1) (fun () -> password <- password + hash.[2].ToString("x"))
                if password.Length < 8 then loop()

            loop()

        [ 1 .. Environment.ProcessorCount ] |> PSeq.iter (fun _ -> findHashes())

        password


    // The sixth character in the valid hash is the next character of the password.
    // Given the actual Door ID, what is the password?
    let Part1() =
        findHashesParallel (byte 0x10) //|> Seq.head
        //|> Seq.take PasswordLength
        //|> Seq.map (fun (thirdByte, _) -> thirdByte.[0])
        //|> charsToStr

    // Instead of simply filling in the password from left to right, the hash now also
    // indicates the position within the password to fill.
    // Given the actual Door ID and this new method, what is the password?
    let Part2() = 1
        //findHashesParallel (byte 0x08)
        //|> Seq.distinctBy fst
        //|> Seq.take PasswordLength
        //|> Seq.sortBy fst
        //|> Seq.map (fun (_, fourthByte) -> fourthByte.[0])
        //|> charsToStr