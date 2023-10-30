namespace AdventOfCode.Days.Y2022

open AdventOfCode.Shared.Utility
open System.IO

/// Day 7: No Space Left On Device
/// https://adventofcode.com/2022/day/7
/// You can hear birds chirping and raindrops hitting leaves as the expedition proceeds.
module Day07 =

    let [<Literal>] RootDir = "/"
    let [<Literal>] UpDir = ".."
    let [<Literal>] TotalDiskSpace = 70_000_000
    let [<Literal>] RequiredSpace = 30_000_000
    
    type Output = ChangeDirectory of string | List | Dir of string | File of int * string

    type File = { Name: string; Size: int }

    type Directory = { Name: string; Parent: string; Files: File Set; SubDirectories: string Set }

    let parse = function
        | Regex "[$] cd (.+)" [dir] -> ChangeDirectory dir
        | Regex "[$] ls" [] -> List
        | Regex "dir (\w+)" [dir] -> Dir dir
        | Regex "(\d+) (.+)" [Int size; file] -> File (size, file)
        | x -> failwithf "Cannot parse %s" x

    let parseInput() = getFile (2022, 7) |> readLinesAs parse

    let processOutput (currDirectory: string, dirMap: Map<string, Directory>) (currOutput: Output) =
        match currOutput with
        | List -> (currDirectory, dirMap)
        | ChangeDirectory UpDir -> (dirMap[currDirectory].Parent, dirMap)
        | ChangeDirectory dir ->
            let fullDirPath = if dir = RootDir then RootDir else Path.Combine [| currDirectory; dir |]
            let newDir = { Name = dir; Parent = currDirectory; Files = Set.empty; SubDirectories = Set.empty }
            let dirMap = if dirMap.ContainsKey fullDirPath then dirMap else dirMap.Add (fullDirPath, newDir)
            (fullDirPath, dirMap)
        | Dir dir ->
            let currDir = dirMap[currDirectory]
            let fullDirPath = Path.Combine [| currDirectory; dir |]
            let newDir = { currDir with SubDirectories = currDir.SubDirectories.Add fullDirPath }
            let dirMap = dirMap.Add (currDirectory, newDir)
            (currDirectory, dirMap)
        | File (size, name) -> 
            let currDir = dirMap[currDirectory]
            let file = { Name = name; Size = size }
            let newDir = { currDir with Files = currDir.Files.Add file }
            let dirMap = dirMap.Add (currDirectory, newDir)
            (currDirectory, dirMap)

    let getSizes (dirMap: Map<string, Directory>) =
        let rec getSize (dirName: string) =
            let dir = dirMap[dirName]
            let fileSize = dir.Files |> Seq.sumBy (fun x -> x.Size)
            let subDirSize = dir.SubDirectories |> Seq.sumBy getSizeMemo

            fileSize + subDirSize
        and getSizeMemo = memoise getSize

        dirMap.Keys |> Seq.map getSize

    let getDirectorySizes() =
        let output = parseInput()

        let (_, dirMap) =
            ((RootDir, Map.empty), output)
            ||> Seq.fold processOutput

        getSizes dirMap |> Seq.toList
        
    // Find all of the directories with a total size of at most 100,000.
    // What is the sum of the total sizes of those directories?
    let Part1() = getDirectorySizes() |> List.filter (fun x -> x <= 100_000) |> List.sum 

    // Find the smallest directory that, if deleted, would free up enough space on the filesystem to run the update.
    // What is the total size of that directory?
    let Part2() =
        let sizes = getDirectorySizes()
        let rootSize = List.max sizes
        let unused = TotalDiskSpace - rootSize
        let extraRequired = RequiredSpace - unused

        sizes |> List.filter (fun x -> x >= extraRequired) |> List.min