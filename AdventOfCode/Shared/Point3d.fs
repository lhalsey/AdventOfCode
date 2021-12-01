namespace AdventOfCode.Shared

open System

type Point3d = { X: int; Y: int; Z: int } with

    static member (+) (a, b) = { X = a.X + b.X; Y = a.Y + b.Y; Z = a.Z + b.Z }

    static member (-) (a, b) = { X = a.X - b.X; Y = a.Y - b.Y; Z = a.Z - b.Z }

    static member Origin = { X = 0; Y = 0; Z = 0 }

    member __.ManhattanDistance = abs __.X + abs __.Y + abs __.Z

    member __.ManhattanDistanceTo (p: Point3d) = abs (p.X - __.X) + abs (p.Y - __.Y) + abs (p.Z - __.Z)