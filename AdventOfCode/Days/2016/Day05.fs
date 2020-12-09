namespace AdventOfCode.Days.Y2016

open AdventOfCode.Shared.Utility
open System.Security.Cryptography
open System.Text

/// Day 5: How About a Nice Game of Chess?
/// https://adventofcode.com/2016/day/5
/// You are faced with a security door designed by Easter Bunny engineers that seem to
/// have acquired most of their security knowledge by watching hacking movies.
module Day05 =

    let [<Literal>] Key = "ojvtpuvg"
    let [<Literal>] PasswordLength = 8

    let findHashes thirdHexadecimalUpperBound =
        use hash = MD5.Create()

        let keyBytes = Encoding.ASCII.GetBytes Key

        let getHash n = 
            Encoding.ASCII.GetBytes (string n)
            |> Array.append keyBytes
            |> hash.ComputeHash

        let isValid (value: byte[]) =
            value.[0] = 0uy && value.[1] = 0uy && value.[2] < thirdHexadecimalUpperBound

        Seq.initInfinite id
        |> Seq.map getHash
        |> Seq.filter isValid
        |> Seq.take PasswordLength
        |> Seq.toList

    // The sixth character in the valid hash is the next character of the password.
    // Given the actual Door ID, what is the password?
    let Part1() =
        findHashes (byte 0x10)
        |> List.map (fun x -> x.[2].ToString("x"))
        |> String.concat ""

    // Instead of simply filling in the password from left to right, the hash now also
    // indicates the position within the password to fill.
    // Given the actual Door ID and this new method, what is the password?
    let Part2() =
        findHashes (byte 0x08)
        |> List.map (fun x -> x.[2].ToString("x"))
        //|> List.permute 
        |> String.concat ""