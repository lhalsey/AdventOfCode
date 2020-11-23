namespace AdventOfCode.Shared

type Direction2d = { X: int; Y: int } with
    static member North = { X = 0; Y = -1 }
    static member South = { X = 0; Y = 1 }
    static member East = { X = 1; Y = 0 }
    static member West = { X = -1; Y = 0 }
    static member NorthWest = { X = -1; Y = -1 }
    static member NorthEast = { X = 1; Y = -1 }
    static member SouthWest = { X = -1; Y = 1 }
    static member SouthEast = { X = 1; Y = 1 }

    static member GetDirection dir =
        match dir with
        | 'N' | 'U' -> Direction2d.North
        | 'S' | 'D' -> Direction2d.South
        | 'W' | 'L' -> Direction2d.West
        | 'E' | 'R' -> Direction2d.East
        | _   -> failwithf "Unexpected token %c" dir

    static member (+) (a, b) = { X = a.X + b.X; Y = a.Y + b.Y }

    static member GoStraight dir = { X = dir.X; Y = dir.Y }

    static member TurnLeft dir = { X = dir.Y; Y = -dir.X }

    static member TurnRight dir = { X = -dir.Y; Y = dir.X }

    static member Zero = { X = 0; Y = 0 }

