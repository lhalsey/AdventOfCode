namespace AdventOfCode.Days.Y2022

open AdventOfCode.Shared.Utility

/// Day 20: Grove Positioning System
/// https://adventofcode.com/2022/day/20
/// It's finally time to meet back up with the Elves. When you try to contact them, however,
/// you get no reply. Perhaps you're out of range?
module Day20 =

    let parseInput() = getFile (2022, 20) |> readLinesAs int |> Seq.toList

    let getResult (elems: int list) =
        let zeroIndex = elems |> List.findIndex (fun x -> x = 0)
        let indexes = [ 1_000; 2_000; 3_000 ] |> List.map (fun x -> (x + zeroIndex) % elems.Length)
        indexes |> List.sumBy (fun x -> elems[x])

    let mixElement (elems: int list) (elem: int) =
        let index = elems |> List.findIndex (fun x -> x = elem)
        let newIndex = remainder (index + elem) elems.Length
        //let newIndex = if index + elem < 0 then newIndex - 1 else newIndex
        
        let newElems =
            match compare newIndex index with
            | 0 -> elems
            | 1 -> elems[0..index - 1] @ elems[index + 1..newIndex] @ [elem] @ elems[newIndex + 1..]
            | _ -> elems[0..newIndex - 1] @ [elem] @ elems[newIndex..index - 1] @ elems[index + 1 ..]

        newElems


    let mix (elems: int list) =
        (elems, elems)
        ||> List.fold mixElement

    let Part1() =
        let input = parseInput()

        let output = input |> mix

        getResult output

    let Part2() =
        0