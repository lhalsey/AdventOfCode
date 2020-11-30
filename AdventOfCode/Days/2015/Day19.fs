namespace AdventOfCode.Days.Y2015

open System.Text.RegularExpressions
open AdventOfCode.Shared.Utility

/// Day 19: Medicine for Rudolph
/// https://adventofcode.com/2015/day/19
/// Rudolph the Red-Nosed Reindeer is sick!
module Day19 =

    // E.g. "H => CRnFYFYFAr"
    let parse (s: string) = s |> splitIntoPair " => "
        
    let parseInput() =
        let input = getFile (2015, 19) |> readLines
        
        let replacements =
            input
            |> Seq.takeWhile (fun s -> s.Contains " => ")
            |> Seq.map parse

        (Seq.last input, replacements)

    // Return all combinations of molecule string where a single instance
    // of the from substring is replaced with the to substring
    // E.g. ABCABD with AB => E returns ECABD and ABCED
    let replace (molecule: string) (input: string, output: string) =
        let r = Regex(input)

        r.Matches(molecule)
        |> Seq.map (fun m -> r.Replace(molecule, output, 1, m.Index))

    let getDistinctMolecules() =
        let (molecule, replacements) = parseInput()
        
        replacements
        |> Seq.collect (replace molecule)
        |> Seq.distinct
        |> Seq.length

    let getFewestReplacements() =
        let (molecule, replacements) = parseInput()
        
        // Use a greedy search to replace longest substrings first
        let replacements =
            replacements
            |> Seq.sortByDescending (fun (_, output) -> output.Length)
            |> Seq.toList
        
        // Work backwards from the molecule to "e" (output -> input)
        let rec getFewestReplacementsR molecule count = 
            match molecule with
            | "e" -> count
            | _ ->
                let (input, output) = 
                    replacements
                    |> Seq.find (fun (_, output) -> molecule.Contains output)
        
                let count = count + Regex.Matches(molecule, output).Count
                let molecule = molecule.Replace(output, input) // Replace all instances
        
                getFewestReplacementsR molecule count
        
        getFewestReplacementsR molecule 0


    // How many distinct molecules can be created after all the different ways
    // you can do one replacement on the medicine molecule?
    let Part1() = getDistinctMolecules()        

    // How long will it take to make the medicine? Given the available replacements
    // and the medicine molecule in your puzzle input, what is the fewest number of
    // steps to go from e to the medicine molecule?
    let Part2() = getFewestReplacements()