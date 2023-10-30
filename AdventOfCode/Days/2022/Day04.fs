namespace AdventOfCode.Days.Y2022

open AdventOfCode.Shared.Utility

/// Day 4: Camp Cleanup
/// https://adventofcode.com/2022/day/4
/// Space needs to be cleared before the last supplies can be unloaded from the ships,
/// and so several Elves have been assigned the job of cleaning up sections of the camp.
module Day04 =

    type Elf = { Start: int; End: int } with
        member __.HasFullOverlap (elf: Elf) = __.Start <= elf.Start && __.End >= elf.End
        member __.HasStartOverlap (elf: Elf) = __.Start >= elf.Start && __.Start <= elf.End

    let getElves (s: string) = // E.g. 4-90,1-4
        let getElf token =
            let (s, e) = splitIntoPairAs "-" int token
            { Start = s; End = e }

        splitIntoPairAs "," getElf s

    let parseInput() = getFile (2022, 4) |> readLinesAs getElves

    // In how many assignment pairs does one range fully contain the other?
    let Part1() = parseInput() |> countIf (fun (elf1, elf2) -> elf1.HasFullOverlap(elf2) || elf2.HasFullOverlap(elf1))

    // In how many assignment pairs do the ranges overlap?
    let Part2() = parseInput() |> countIf (fun (elf1, elf2) -> elf1.HasStartOverlap(elf2) || elf2.HasStartOverlap(elf1))