namespace AdventOfCode.Days.Y2021

open AdventOfCode.Shared
open AdventOfCode.Shared.Utility
open System.Collections.Generic
open FSharp.Collections.ParallelSeq

/// Day 20: Title TBC
/// https://adventofcode.com/2021/day/20
/// Description TBC
module Day20 =

    type Image = { Pixels: IReadOnlyDictionary<Point2d, int>; MinX: int; MaxX: int }

    let getPixelValue = function '.' -> 0 | '#' -> 1 | x -> failwithf "Invalid character: %c" x 

    let getInput (lines: string list) =
        let pixelMap = lines.[0] |> Seq.map getPixelValue |> Seq.toArray

        let pixels =
            lines.[2..]
            |> List.mapi (fun row str -> str |> Seq.mapi (fun col c -> ({ X = col; Y = row }, getPixelValue c)))
            |> Seq.concat
            |> readOnlyDict

        pixelMap, { Pixels = pixels; MinX = 0; MaxX = lines.[2].Length - 1 }

    let parseInput() = getFile (2021, 20) |> readLines |> Seq.toList |> getInput

    let gridCells = List.allPairs [-1..1] [-1..1]

    let getEnhancedPixel
        (pixelMap: int[])
        (image: IReadOnlyDictionary<Point2d, int>)
        (point: Point2d)
        (outerValue: int) =

        let getValue (y, x) = tryFind { X = point.X + x; Y = point.Y + y } image |> Option.defaultValue outerValue

        let index = gridCells |> List.fold (fun acc x -> acc * 2 + getValue x) 0

        (point, pixelMap.[index])

    let enhanceImage (pixelMap: int[]) (image: Image) (iteration: int) =
        let (minX, maxX) = image.MinX - 1, image.MaxX + 1

        // Image extends infinitely and pixels outside initial image can flip between on and off each iteration
        let outerValue = 
            match pixelMap.[0], iteration % 2 with
            | 0, _ -> 0
            | 1, 1 -> 1
            | _ -> 0

        let pixels =
            Seq.allPairs [minX .. maxX] [minX .. maxX]
            |> PSeq.map (fun (y, x) -> getEnhancedPixel pixelMap image.Pixels { X = x; Y = y } outerValue)
            |> readOnlyDict

        { Pixels = pixels; MinX = minX; MaxX = maxX }

    let getLitPixelCount (iterations: int) =
        let (pixelMap, image) = parseInput()
        
        let enhance (iteration, img) = Some (img, (iteration + 1, enhanceImage pixelMap img iteration))
        
        let finalImage = (0, image) |> Seq.unfold enhance |> Seq.item iterations

        finalImage.Pixels |> countIf (fun x -> x.Value = 1)

    // Start with the original input image and apply the image enhancement algorithm twice, being careful
    // to account for the infinite size of the images.
    // How many pixels are lit in the resulting image?
    let Part1() = getLitPixelCount 2

    // Start again with the original input image and apply the image enhancement algorithm 50 times.
    // How many pixels are lit in the resulting image?
    let Part2() = getLitPixelCount 50