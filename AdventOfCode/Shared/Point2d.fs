namespace AdventOfCode.Shared

open System

type Point2d = { X: int; Y: int } with

    static member (+) (a, b) = { X = a.X + b.X; Y = a.Y + b.Y }

    static member (-) (a, b) = { X = a.X - b.X; Y = a.Y - b.Y }

    static member (+) (a, b: Direction2d) = { X = a.X + b.X; Y = a.Y + b.Y }

    static member (*) (a, b) = { X = a.X * b; Y = a.Y * b }

    static member Origin = { X = 0; Y = 0 }

    member __.ManhattanDistance = abs __.X + abs __.Y

    member __.ManhattanDistanceTo (p: Point2d) = abs (p.X - __.X) + abs (p.Y - __.Y)

    member __.AngleTo (p: Point2d) =
        let radiansToDegrees = (*) (180.0 / Math.PI)

        let dx = float __.X - float p.X
        let dy = float __.Y - float p.Y
        let degrees = atan2 dy dx |> radiansToDegrees
        (degrees + 270.0) % 360.0 // Make straight up zero degrees

    member __.EuclidianDistanceSquaredTo (p: Point2d) =
        (p.X - __.X) * (p.X - __.X) + (p.Y - __.Y) * (p.Y - __.Y)

    member __.GetAdjacent() =
        seq {
            yield __ + Direction2d.North
            yield __ + Direction2d.East
            yield __ + Direction2d.South
            yield __ + Direction2d.West
        }

    member __.GetDiagonalAdjacent() =
        seq {
            yield __ + Direction2d.NorthWest
            yield __ + Direction2d.NorthEast
            yield __ + Direction2d.SouthWest
            yield __ + Direction2d.SouthEast
        }

    member __.GetAllAdjacent() = seq { yield! __.GetAdjacent(); yield! __.GetDiagonalAdjacent() }

    member __.GetPointsInDirection (dir: Direction2d) = __ |> Seq.unfold (fun x -> Some(x, x + dir))