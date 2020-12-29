namespace AdventOfCode.Days.Y2020

open System
open AdventOfCode.Shared.Utility

/// Day 20: Jurassic Jigsaw
/// https://adventofcode.com/2020/day/20
/// The high-speed train leaves the forest and quickly carries you south.
module Day20 =

    type Edge = string
    type Cell = char

    let getInner (elems :'a list) =  elems |> List.tail |> List.take (elems.Length - 2)

    let flipHorizontal (elems: 'a list list) = elems |> List.map List.rev

    let rotate (elems: 'a list list) = elems |> List.transpose |> List.map List.rev

    let rotations (elems: 'a list list) =
        elems |> Seq.unfold (fun x -> Some (x, rotate x)) |> Seq.take 4

    let getOrientations (elems: 'a list list) = // Get 8 unique orientations
        [ yield! elems |> rotations
          yield! elems |> flipHorizontal |> rotations ]

    type Tile = { Id: int64; Cells: Cell list list; OuterEdges: Edge list } with
        member __.North = List.head __.Cells |> charsToStr
        member __.South = List.last __.Cells |> charsToStr
        member __.East = __.Cells |> List.map List.last |> charsToStr
        member __.West = __.Cells |> List.map List.head |> charsToStr
        member __.Edges = [ __.North; __.East; __.South; __.West ]
        member __.InnerCells = getInner __.Cells |> List.map getInner
        member __.Orientations = getOrientations __.Cells |> List.map (fun x -> { __ with Cells = x })

    let rev (s: string) = s |> Seq.rev |> Seq.toArray |> String

    // E.g. "Tile 2311:\n..##.#..#., etc."
    let parseTile (s: string) = 
        let lines = s |> split '\n'
        let id = lines.[0] |> split ' ' |> fun x -> x.[1].Trim(':') |> int64
        let tiles = lines.[1..] |> Array.map (Seq.toList) |> Array.toList

        { Id = id; Cells = tiles; OuterEdges = [] }

    let parseInput() =
        getFile (2020, 20)
        |> readAllText
        |> splitOn "\n\n"
        |> Array.map parseTile
        |> Array.toList

    // Read tiles and determine which pieces must have outer edges as they cannot match any other edge
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

        grids
        |> List.map (fun grid -> { grid with OuterEdges = grid.Edges |> List.filter unique.Contains })

    let matchEdge (e1: Edge) (e2: Edge) = e1 = e2 || e1 = rev e2

    let hasEdge (e: Edge) (edges: Edge list) = List.contains e edges || List.contains (rev e) edges

    // Select the next tile to place and flip/rotate it to match the edges to the left and above
    let getTile (left: Tile option) (top: Tile option) (unused: Tile Set) =
        let (selectTile, orientateTile) =
            match left, top with
            | None, None -> // Pick any corner to start and place in top left
                (fun (x: Tile) -> x.OuterEdges.Length = 2),
                (fun (x: Tile) -> matchEdge x.North x.OuterEdges.[0] && matchEdge x.West x.OuterEdges.[1])
            | Some left, None -> // Top edge piece
                (fun (x: Tile) -> hasEdge left.East x.Edges),
                (fun (x: Tile) -> hasEdge x.North x.OuterEdges && matchEdge x.West left.East)
            | None, Some top -> // Left edge piece
                (fun (x: Tile) -> hasEdge top.South x.Edges),
                (fun (x: Tile) -> matchEdge x.North top.South && hasEdge x.West x.OuterEdges)
            | Some left, Some top -> // Inner piece
                (fun (x: Tile) -> hasEdge left.East x.Edges && hasEdge top.South x.Edges),
                (fun (x: Tile) ->  matchEdge x.North top.South && matchEdge x.West left.East)

        let tile = unused |> Set.filter selectTile |> Set.minElement

        List.find orientateTile tile.Orientations, unused.Remove tile

    // Place tiles on new row using row above as guidance
    let rec getRow (lastTile: Tile option) (topTiles: Tile option list) (unused: Tile Set) (acc: Tile list) =
        match topTiles with
        | [] -> (List.rev acc, unused)
        | h::t ->
            let (tile, unused) = getTile lastTile h unused
            getRow (Some tile) t (unused.Remove tile) (tile::acc)

    let rec getRows (lastRow: Tile option list) (unused: Tile Set) =
        seq { let (row, unused) = getRow None lastRow unused []
              yield row
              yield! getRows (row |> List.map Some) unused }

    // Remove the borders and combine the tiles
    let getImageRow (row: Tile list) =
        row
        |> List.collect (fun x -> x.InnerCells |> List.transpose)
        |> List.transpose

    let getImage() =
        let tiles = getTiles() |> set
        let imageSize = Math.Sqrt (float tiles.Count) |> int
        let blankRow = List.replicate imageSize None

        let allRows = getRows blankRow tiles |> Seq.take imageSize |> Seq.toList

        allRows |> List.collect getImageRow

    let charsToInt (s: string) =
        let digits = s.Replace(' ', '0').Replace('.', '0').Replace('#', '1')
        Convert.ToInt32(digits, 2)

    // When looking for this pattern in the image, the spaces can be anything; only the # need to match.
    let countMonsterCells (cells: Cell list list) =
        let m1 = "                  # "
        let m2 = "#    ##    ##    ###"
        let m3 = " #  #  #  #  #  #   "
        let target = [ m1; m2; m3 ] |> List.map charsToInt
        let monsterLength = m1.Length
        let monsterHeight = target.Length
        let monsterCells = m1 + m2 + m3 |> countIf ((=) '#')

        // Convert area to ints and do bitwise And to see if #'s overlap
        // We assume the monsters don't overlap each other
        let checkArea (area: Cell list list) = 
            let values = area |> List.transpose |> List.map (charsToStr >> charsToInt)
            let andValues = List.map2 (fun x y -> x &&& y) values target 
            if andValues = target then monsterCells else 0

        let findMonster (rows: Cell list list) =
            rows
            |> List.transpose
            |> List.windowed monsterLength
            |> List.sumBy checkArea
            
        cells
        |> List.windowed monsterHeight
        |> List.sumBy findMonster


    // Assemble the tiles into an image.
    // What do you get if you multiply together the IDs of the four corner tiles?
    let Part1() =
        getTiles()
        |> List.filter (fun x -> x.OuterEdges.Length = 2)
        |> List.map (fun x -> x.Id)
        |> List.reduce (*)

    // How many # are not part of a sea monster?
    let Part2() =
        let image = getImage()

        let monsterCells =
            image
            |> getOrientations
            |> Seq.map countMonsterCells
            |> Seq.find (fun x -> x > 0)

        let waterCells = image |> List.concat |> countIf ((=) '#')

        waterCells - monsterCells