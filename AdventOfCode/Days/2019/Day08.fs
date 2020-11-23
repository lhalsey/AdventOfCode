namespace AdventOfCode.Days.Y2019

open System

open AdventOfCode.Shared.Utility

/// Day 8: Space Image Format
/// https://adventofcode.com/2019/day/8
/// The Elves' spirits are lifted when they realize you have an opportunity to reboot one
/// of their Mars rovers, and so they are curious if you would spend a brief sojourn on Mars.
module Day08 =

    let (Width, Height) = 25, 6
    let Size = Width * Height

    type Colour = Black | White | Transparent

    let getColour = function
        | '0' -> Black
        | '1' -> White
        | '2' -> Transparent
        | x   -> failwithf "Invalid cell value: %c" x

    let countOf n = Array.filter ((=) n) >> Array.length

    // Find the first non-transparent cell
    let getCell (layers: Colour[] seq) pos =
        layers
        |> Seq.map (fun layer -> layer.[pos])
        |> Seq.find ((<>) Transparent)

    // E.g. "2122222022212222..."
    let getInput() = 
        getFile (2019, 8)
        |> readAllText
        |> trim
        |> Seq.map getColour
        |> Seq.chunkBySize Size
        |> Seq.toList

    // To make sure the image wasn't corrupted during transmission, the Elves
    // would like you to find the layer that contains the fewest 0 digits. On that
    // layer, what is the number of 1 digits multiplied by the number of 2 digits?
    let Part1() =
        let layers = getInput()

        layers
        |> List.minBy (countOf Black)
        |> fun layer -> (countOf White layer) * (countOf Transparent layer)

    // What message is produced after decoding your image?
    let Part2() =
        let layers = getInput()

        List.init Size (getCell layers)
        |> List.map (fun colour -> if colour = Black then ' ' else '█')
        |> List.chunkBySize Width
        |> List.map (List.toArray >> String)
        |> String.concat "\n"