namespace AdventOfCode.Days.Y2015

open System.Security.Cryptography
open System.Text

/// Day 4: The Ideal Stocking Stuffer
/// https://adventofcode.com/2015/day/4
/// Santa needs help mining some AdventCoins (very similar to bitcoins) to use as gifts
// for all the economically forward-thinking little girls and boys.
module Day04 =

    let [<Literal>] Key = "bgvyzdsv"

    // TODO: Consider parallelising with a separate MD5 object in each thread 
    let findHash thirdHexadecimalUpperBound =
        use hash = MD5.Create()

        let keyBytes = Encoding.ASCII.GetBytes Key

        let isValid n = 
            let value =
                Encoding.ASCII.GetBytes (string n)
                |> Array.append keyBytes
                |> hash.ComputeHash

            value.[0] = 0uy && value.[1] = 0uy && value.[2] <= thirdHexadecimalUpperBound

        Seq.initInfinite id |> Seq.find isValid

    // Santa needs to find MD5 hashes which, in hexadecimal, start with at least five zeroes.
    // The input to the MD5 hash is some secret key (your puzzle input, given below) followed
    // by a number in decimal. To mine AdventCoins, you must find Santa the lowest positive number
    // (no leading zeroes: 1, 2, 3, ...) that produces such a hash.
    let Part1() = findHash (byte 0x0f)

    // Now find one that starts with six zeroes.
    let Part2() = findHash (byte 0x00)