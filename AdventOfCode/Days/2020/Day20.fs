namespace AdventOfCode.Days.Y2020

open System
open AdventOfCode.Shared.Utility

/// Day 20: Jurassic Jigsaw
/// https://adventofcode.com/2020/day/20
/// The high-speed train leaves the forest and quickly carries you south.
module Day20 =

    type Edge = string
    type Cell = char

    let getInner (elems :'a list) = 
        elems |> List.tail |> List.take (elems.Length - 2)

    type Tile = { Id: int64; Cells: Cell list list; OuterEdges: Edge list } with
        member __.North = List.head __.Cells |> charsToStr
        member __.South = List.last __.Cells |> List.rev |> charsToStr
        member __.East = __.Cells |> List.map List.last |> charsToStr
        member __.West = __.Cells |> List.map List.head |> List.rev |> charsToStr
        member __.Edges = [ __.North; __.East; __.South; __.West ]
        member __.InnerCells = getInner __.Cells |> List.map getInner

    let rev (s: string) = s |> Seq.rev |> Seq.toArray |> String

    let flipHorizontal (tile: Tile) =
        { tile with Cells = tile.Cells |> List.map List.rev }

    let flipVertical (tile: Tile) =
        { tile with Cells = tile.Cells |> List.rev }

    let rotate (tile: Tile) = // Rotate = transpose + flip?
        { tile with Cells = tile.Cells |> List.transpose |> List.map List.rev }

    let edgeToInt (s: string) = Convert.ToInt32(s.Replace('.', '0').Replace('#', '1'), 2)

    let parseGrid (s: string) =
        let lines = s |> split '\n'
        let id = lines.[0] |> split ' ' |> fun x -> x.[1].Trim(':') |> int64
        let tiles = lines.[1..] |> Array.map (Seq.toList) |> Array.toList

        { Id = id; Cells = tiles; OuterEdges = [] }

    let parseInput() =
        getFile (2020, 20)
        |> readAllText
        |> splitOn "\n\n"
        |> Array.map parseGrid
        |> Array.toList

    let getTiles() =
        let grids = parseInput()
        
        let uniqueEdges =
            grids
            |> List.collect (fun x -> x.Edges)
            |> List.countBy id
            |> List.filter (fun (_, v) -> v = 1)
            |> List.map fst
        
        let flippedMatches =
            uniquePairs uniqueEdges
            |> Seq.filter (fun (x, y) -> x = rev y)
            |> Seq.collect (fun (x, y) -> [x; y])
            |> Seq.toList
        
        let unique = set uniqueEdges - set flippedMatches

        let tileMap =
            grids
            |> List.map (fun grid -> { grid with OuterEdges = grid.Edges |> List.filter unique.Contains })
            |> List.groupBy (fun x -> x.OuterEdges.Length)
            |> Map

        (tileMap.[2], tileMap.[1], tileMap.[0])

    let rotations (tile: Tile) = tile |> Seq.unfold (fun x -> Some(x, rotate x)) |> Seq.take 4

    let flipsAndRotations (tile: Tile) =
        [ tile; flipHorizontal tile; flipVertical tile ]
        |> Seq.collect rotations


    // Assemble the tiles into an image.
    // What do you get if you multiply together the IDs of the four corner tiles?
    let Part1() =
        let (corners, _, _) = getTiles()

        corners |> List.map (fun x -> x.Id) |> List.reduce (*)

        
    // How many # are not part of a sea monster?
    let Part2() =
        let (corners, outer, inner) = getTiles()

        let numTiles = corners.Length + outer.Length + inner.Length
        let imageWidth = Math.Sqrt (float numTiles) |> int

        let matchEdge (e1: Edge) (e2: Edge) = e1 = e2 || e1 = rev e2

        let getTile (left: Tile option) (top: Tile option) (unused: Tile Set) =
            match left, top with
            | None, None ->
                let tile =
                    unused
                    |> Set.filter (fun x -> x.OuterEdges.Length = 2)
                    |> Set.minElement

                let flippedTile = 
                    tile
                    |> flipsAndRotations
                    |> Seq.find (fun x -> matchEdge x.North x.OuterEdges.[0] && matchEdge x.West x.OuterEdges.[1])

                flippedTile, unused.Remove tile

            | Some l, None ->
                let tile =
                    unused
                    |> Set.filter (fun x -> x.OuterEdges.Length >= 1 && (x.Edges |> List.contains l.East || x.Edges |> List.contains (rev l.East)))
                    |> Set.minElement

                let flippedTile = 
                    tile
                    |> flipsAndRotations
                    |> Seq.find (fun x -> (x.OuterEdges |> List.contains x.North || x.OuterEdges |> List.contains (rev x.North)) && matchEdge x.West l.East)

                flippedTile, unused.Remove tile

            | None, Some t ->
                let tile =
                    unused
                    |> Set.filter (fun x -> x.OuterEdges.Length >= 1 && (x.Edges |> List.contains t.South || x.Edges |> List.contains (rev t.South)))
                    |> Set.minElement

                let flippedTile = 
                    tile
                    |> flipsAndRotations
                    |> Seq.find (fun x -> matchEdge x.North t.South && (x.OuterEdges |> List.contains x.West || x.OuterEdges |> List.contains (rev x.West)))

                flippedTile, unused.Remove tile

            | Some l, Some t ->
                let tile =
                    unused
                    |> Set.filter (fun x -> (x.Edges |> List.contains l.East || x.Edges |> List.contains (rev l.East))
                                            && (x.Edges |> List.contains t.South || x.Edges |> List.contains (rev t.South)))
                    |> Set.minElement

                let flippedTile = 
                    tile
                    |> flipsAndRotations
                    |> Seq.find (fun x -> matchEdge x.North t.South && matchEdge x.West l.East)

                flippedTile, unused.Remove tile

     
        // Start assembly of image
        let unused = set (corners @ outer @ inner)

        let rec getRow (lastTile: Tile option) (topTiles: Tile option list) (unused: Tile Set) (acc: Tile list) =
            match topTiles with
            | [] -> (List.rev acc, unused)
            | h::t ->
                let (tile, unused) = getTile lastTile h unused
                getRow (Some tile) t (unused.Remove tile) (tile::acc)

        let rec getRows (lastRow: Tile list) (unused: Tile Set) (rows: Tile list list) =
            if rows.Length = imageWidth then List.rev rows else
            let (row, unused) = getRow None (lastRow |> List.map Some) unused []
            getRows row unused (row::rows)


        let blanks = List.replicate (imageWidth) None
        let (topRow, unused) = getRow None blanks unused []

        let allRows = getRows topRow unused [ topRow ]

        let getAllCells (rows: Tile list list) =
            let getRow (row: Tile list) =
                row
                |> List.collect (fun x -> x.InnerCells |> List.transpose)
                |> List.transpose

            rows |> List.collect getRow

        let image = getAllCells allRows

        let tile = { Id = 0L; Cells = image; OuterEdges = [] }

        let charsToInt (s: string) =
            let digits = s.Replace(' ', '0').Replace('.', '0').Replace('#', '1')
            Convert.ToInt32(digits, 2)

        // When looking for this pattern in the image, the spaces can be anything; only the # need to match.
        let findMonsters (tile: Tile) =
            let monsterLength = 20
            let m1 = "                  # " |> charsToInt
            let m2 = "#    ##    ##    ###" |> charsToInt
            let m3 = " #  #  #  #  #  #   " |> charsToInt
            let ms = [ m1; m2; m3 ]

            // Convert area to ints and do bitwise And to see if #'s overlap
            let checkArea (area: Cell list list) = 
                let values = area |> List.transpose |> List.map (charsToStr >> charsToInt)
                let andValues = List.map2 (fun x y -> x &&& y) values ms 
                if andValues = ms then 1 else 0

            let findMonster (rows: Cell list list) =
                rows
                |> List.transpose
                |> List.windowed monsterLength
                |> List.sumBy checkArea
                
            tile.Cells
            |> List.windowed 3 // Monster height
            |> List.sumBy findMonster

        let monsters =
            tile
            |> flipsAndRotations
            |> Seq.map findMonsters
            |> Seq.max

        let water = tile.Cells |> List.concat |> countIf ((=) '#')

        let monsterCells = 15
        water - (monsterCells * monsters)