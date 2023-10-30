namespace AdventOfCode.Shared

type Direction3d = { X: int; Y: int; Z: int } with

    static member West = { X = -1; Y = 0; Z = 0 }
    static member East = { X = 1; Y = 0; Z = 0 }
    static member South = { X = 0; Y = -1; Z = 0 }
    static member North = { X = 0; Y = 1; Z = 0 }
    static member Down = { X = 0; Y = 0; Z = -1 }
    static member Up = { X = 0; Y = 0; Z = 1 }

    static member AllDirections = 
        seq { Direction3d.North
              Direction3d.East
              Direction3d.South
              Direction3d.West
              Direction3d.Up
              Direction3d.Down }

    static member (*) (a, b) = { X = a.X * b; Y = a.Y * b; Z = a.Z * b }

    member __.Opposite = __ * -1
  