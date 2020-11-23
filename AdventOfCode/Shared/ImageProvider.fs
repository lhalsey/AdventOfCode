namespace AdventOfCode.Shared

open System
open System.Collections.Generic

module ImageProvider =

    let getImage getCell (map: IReadOnlyDictionary<Point2d, 'a>) =
        let x1 = map |> Seq.map (fun x -> x.Key.X) |> Seq.min
        let x2 = map |> Seq.map (fun x -> x.Key.X) |> Seq.max
        let y1 = map |> Seq.map (fun x -> x.Key.Y) |> Seq.min
        let y2 = map |> Seq.map (fun x -> x.Key.Y) |> Seq.max

        let width = (x2 - x1) + 1

        let getCell (y, x) =
            match map.TryGetValue { X = x; Y = y } with
            | true, v  -> getCell v
            | _ -> ' '

        let cells =
            List.allPairs [y1..y2] [x1..x2] 
            |> List.map getCell

        cells
        |> List.chunkBySize width
        |> List.map (Seq.toArray >> String)
        |> String.concat "\n"

