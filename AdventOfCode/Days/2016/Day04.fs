namespace AdventOfCode.Days.Y2016

open AdventOfCode.Shared.Utility

/// Day 4: Security Through Obscurity
/// https://adventofcode.com/2016/day/4
/// Finally, you come across an information kiosk with a list of rooms.
module Day04 =

    // Each room consists of an encrypted name (lowercase letters separated by dashes) followed by
    // a dash, a sector ID, and a checksum in square brackets.
    // E.g. "aczupnetwp-mfyyj-opalcexpye-977[peyac]"
    let parse (s: string) =
        let i = s.LastIndexOf '-'
        let encryptedName = s.[..i] |> Seq.filter (fun x -> x <> '-')
        let tokens = s.[i+1..] |> split '['
        let (sectorId, checksum) = int tokens.[0], tokens.[1].TrimEnd ']'

        let mostCommonLetters =
            encryptedName
            |> Seq.countBy id
            |> Seq.map (fun (c, count) -> -count, c) // Most common first, then by alphabetical
            |> partialSort 5
            |> Seq.map snd
            |> charsToStr

        if mostCommonLetters = checksum then Some (encryptedName, sectorId) else None

    let rotate shift (c: char) = (int c - int 'a' + shift) % 26 + int 'a' |> char

    let decrypt (encryptedName, sectorId) =
        encryptedName |> Seq.map (rotate sectorId) |> charsToStr

    let parseInput() = getFile (2016, 4) |> readLinesAs parse

     // What is the sum of the sector IDs of the real rooms?
    let Part1() =
        parseInput()
        |> Seq.choose id
        |> Seq.sumBy snd

    // What is the sector ID of the room where North Pole objects are stored?
    let Part2() =
        parseInput()
        |> Seq.choose id
        |> Seq.find (fun (code, id) -> decrypt (code, id) |> contains "north")
        |> snd