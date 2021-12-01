namespace AdventOfCode.Days.Y2018

open AdventOfCode.Shared.Utility
open AdventOfCode.Shared

/// Day 23: Experimental Emergency Teleportation
/// https://adventofcode.com/2018/day/23
/// Using your torch to search the darkness of the rocky cavern, you finally locate the man's
/// friend: a small reindeer.
module Day23 =

    type Bot = { Location: Point3d; Radius: int }

    // E.g. "pos=<-13936329,35619897,41211497>, r=68603272"
    let parse = function
        | Regex "pos=<([+-]*\d+),([+-]*\d+),([+-]*\d+)>, r=(\d+)" [Int x; Int y; Int z; Int r] ->
            { Location = { X = x; Y = y; Z = z }; Radius = r }
        | x -> failwithf "Invalid input: %s" x

    let parseInput() = getFile (2018, 23) |> readLinesAs parse

    let Part1() =
        let bots = parseInput() |> Seq.toList
        let keyBot = bots |> List.maxBy (fun x -> x.Radius)

        let inRange (bot: Bot) = bot.Location.ManhattanDistanceTo keyBot.Location <= keyBot.Radius

        bots |> countIf inRange


    let Part2() =
        let bots = parseInput() |> Seq.toList

        let xs = bots |> List.map (fun bot -> bot.Location.X)
        let ys = bots |> List.map (fun bot -> bot.Location.Y)
        let zs = bots |> List.map (fun bot -> bot.Location.Z)
        let (minX, maxX) = List.min xs, List.max xs
        let (minY, maxY) = List.min ys, List.max ys
        let (minZ, maxZ) = List.min zs, List.max zs
        let midX = (minX + maxX) / 2
        let midY = (minY + maxY) / 2
        let midZ = (minZ + maxZ) / 2
        
        let getAvgDist (point: Point3d) =
            let distances =
                bots
                |> List.map (fun bot -> max 0 (bot.Location.ManhattanDistanceTo point - bot.Radius))

            distances |> List.map float |> List.average

        let me = { X = midX; Y = midY; Z = midZ }

        let points =
            [ { X = minX; Y = minY; Z = minZ }
              { X = minX; Y = minY; Z = maxZ }
              { X = minX; Y = maxY; Z = minZ }
              { X = minX; Y = maxY; Z = maxZ }
              { X = maxX; Y = minY; Z = minZ }
              { X = maxX; Y = minY; Z = maxZ }
              { X = maxX; Y = maxY; Z = minZ }
              { X = maxX; Y = maxY; Z = maxZ }
              me
            ]
            
        let avgDists = 
            points
            |> List.map getAvgDist
        

        1