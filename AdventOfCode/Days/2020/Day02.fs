namespace AdventOfCode.Days.Y2020

open AdventOfCode.Shared.Utility

/// Day 2: Password Philosophy
/// https://adventofcode.com/2020/day/2
/// Your flight departs in a few days from the coastal airport; the easiest way down
/// to the coast from here is via toboggan.
module Day02 =

    type Policy = { Min: int; Max: int; Letter: char }

    // E.g. "15-19 k: kkkkkkkkkkkkzkkkkkkk"
    let parse = splitAny " -:" >> function
        | [| Int min; Int max; letter; pw |] -> pw, { Min = min; Max = max; Letter = letter.[0] }
        | x -> failwithf "Invalid input: %A" x

    let parseInput() = getFile (2020, 2) |> readLinesAs parse

    // The password policy indicates the lowest and highest number of times a given letter
    // must appear for the password to be valid.
    let isValid ((password: string), policy) = 
        password
        |> countIf ((=) policy.Letter)
        |> (policy.Min >=< policy.Max)

    // Each policy actually describes two positions in the password. Exactly one of these
    // positions must contain the given letter.
    let isValidNew ((password: string), policy) = 
        (password.[policy.Min - 1] = policy.Letter) <> (password.[policy.Max - 1] = policy.Letter)


    // How many passwords are valid according to their policies?
    let Part1() = parseInput() |> countIf isValid

    // How many passwords are valid according to the new interpretation of the policies?
    let Part2() = parseInput() |> countIf isValidNew