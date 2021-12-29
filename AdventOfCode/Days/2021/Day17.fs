namespace AdventOfCode.Days.Y2021

open AdventOfCode.Shared.Utility
open AdventOfCode.Shared

/// Day 17: Trick Shot
/// https://adventofcode.com/2021/day/17
/// You finally decode the Elves' message. HI, the message says. You continue searching for the sleigh keys.
module Day17 =

    type Status = PastTarget | OnTarget | BeforeTarget

    let parse = function
        | Regex "target area: x=([+-]*\d+)..([+-]*\d+), y=([+-]*\d+)..([+-]*\d+)" [Int x1; Int x2; Int y1; Int y2 ]
            -> ( { X = min x1 x2; Y = max y1 y2 }, { X = max x1 x2; Y = min y1 y2 })
        | x -> failwithf "Invalid input: %s" x

    let parseInput() = getFile (2021, 17) |> readAllText |> parse

    let getPosition ((pos: Point2d), (dir: Direction2d)) = 
        let pos = pos + dir
        let dirX = dir.X + (compare 0 dir.X)
        let dirY = dir.Y - 1
        (pos, { Direction2d.X = dirX; Y = dirY })

    let getStatus (p1: Point2d) (p2: Point2d) (pos: Point2d) =
        match (pos.X, pos.Y) with
        | (x, y) when x > p2.X || y < p2.Y -> PastTarget
        | (x, y) when x < p1.X || y > p1.Y -> BeforeTarget
        | _ -> OnTarget

    let tryVelocity t1 t2 (dx, dy) =
        let dir = { Direction2d.X = dx; Y = dy }

        let steps =
            (Point2d.Origin, dir)
            |> Seq.unfold (fun (p, d) -> Some(p, getPosition (p, d)))
            |> Seq.map (fun p -> getStatus t1 t2 p, p)
            |> Seq.takeWhile (fun (s, _) -> s <> PastTarget)
            |> Seq.toList

        steps
        |> List.tryFind (fun (s, _) -> s = OnTarget)
        |> Option.map (fun _ -> steps)

    let getValidTrajectories() =
        let (t1, t2) = parseInput()
        
        List.allPairs [1 .. t2.X] [t2.Y .. -t2.Y]
        |> List.choose (tryVelocity t1 t2)

    let getHighestPoint = List.map (fun (_, p) -> p.Y) >> List.max

    // What is the highest y position it reaches on this trajectory?
    let Part1() = getValidTrajectories() |> List.map getHighestPoint |> List.max

    // How many distinct initial velocity values cause the probe to be within the target area after any step?
    let Part2() = getValidTrajectories() |> List.length