namespace AdventOfCode.Days.Y2016

open System
open AdventOfCode.Shared.Utility

/// Day 7: Internet Protocol Version 7
/// https://adventofcode.com/2016/day/7
/// While snooping around the local network of EBHQ, you compile a list of IP addresses (they're IPv7, of course; IPv6 is much too limited).
module Day07 =

    type IPAddress = { Hypernet: string list; Supernet: string list }

    // Put odd indexed elements in first list and even indexed elements in second list
    let unweave elems = Array.foldBack (fun x (a, b) -> (b, x::a)) elems ([], [])

    // E.g. "dnwtsgywerfamfv[gwrhdujbiowtcirq]bjbhmuxdcasenlctwgh"
    let parse (s: string) =
        s
        |> splitAny "[]"
        |> unweave
        |> fun (x, y) -> { Hypernet = x; Supernet = y }

    let parseInput() = getFile (2016, 7) |> readLinesAs parse

    // An ABBA is any four-character sequence which consists of a pair of two different
    // characters followed by the reverse of that pair, such as xyyx or abba.
    let hasAbba =
        Seq.windowed 4
        >> Seq.exists (fun x -> x.[0] = x.[3] && x.[1] = x.[2] && x.[0] <> x.[1])

    // An ABA is any three-character sequence which consists of the same character twice
    // with a different character between them, such as xyx or aba.
    let getAbas =
        Seq.windowed 3
        >> Seq.filter (fun x -> x.[0] = x.[2] && x.[0] <> x.[1])
        >> Seq.map String

    let abaToBab (s: string) = $"{s.[1]}{s.[0]}{s.[1]}"
    
    // An IP supports TLS if it has an Autonomous Bridge Bypass Annotation, or ABBA.
    // However, the IP also must not have an ABBA within any hypernet sequences, which
    // are contained by square brackets.
    let supportsTLS ipAddress = 
         Seq.exists hasAbba ipAddress.Supernet &&
         Seq.exists hasAbba ipAddress.Hypernet |> not

    // An IP supports SSL if it has an Area-Broadcast Accessor, or ABA, anywhere in the
    // supernet sequences (outside any square bracketed sections), and a corresponding Byte
    // Allocation Block, or BAB, anywhere in the hypernet sequences.
    let supportsSSL ipAddress =
        let abas = ipAddress.Supernet |> Seq.collect getAbas |> set
        let babs = ipAddress.Hypernet |> Seq.collect getAbas |> Seq.map abaToBab

        babs |> Seq.exists abas.Contains

    // How many IPs in your puzzle input support TLS?
    let Part1() = parseInput() |> parallelCountIf supportsTLS

    // How many IPs in your puzzle input support SSL?
    let Part2() = parseInput() |> parallelCountIf supportsSSL