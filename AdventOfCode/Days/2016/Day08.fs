namespace AdventOfCode.Days.Y2016

open System
open AdventOfCode.Shared.Utility

/// Day 8: Two-Factor Authentication
/// https://adventofcode.com/2016/day/8
/// You come across a door implementing what you can only assume is an implementation of two-factor authentication after a long game of requirementstelephone.
module Day08 =

    let [<Literal>] Width = 50
    let [<Literal>] Height = 6

    type Instruction = 
        | Rect of int * int
        | RotateRow of int * int
        | RotateColumn of int * int

    // E.g. "rotate row y=0 by 10", "rotate column x=0 by 1", "rect 9x1"
    let parse = function
        | Regex "rotate row y=(\d+) by (\d+)" [y; a] -> RotateRow (int y, int a)
        | Regex "rotate column x=(\d+) by (\d+)" [x; a] -> RotateColumn (int x, int a)
        | Regex "rect (\d+)x(\d+)" [x; y] -> Rect (int x, int y)
        | x -> failwithf "Invalid input: %s" x
        
    let parseInput() = getFile (2016, 8) |> readLinesAs parse

    // rect AxB turns on all of the pixels in a rectangle at the top-left
    // of the screen which is A wide and B tall
    let setRect x y (grid: int [,]) =
        grid.[0..(y - 1), 0..(x - 1)] <- Array2D.create y x 1
        grid

    // Rotate row or column by duplicating and skipping to handle wrapping
    let rotate amount vector =
        vector
        |> Array.append vector
        |> Array.skip amount
        |> Array.take vector.Length

    // rotate row y=A by B shifts all of the pixels in row A (0 is the top row) right by B pixels
    // Pixels that would fall off the right end appear at the left end of the row
    let rotateRow y a (grid: int [,]) =
        grid.[y, *] <- grid.[y, *] |> rotate (Width - a)
        grid

    // rotate column x=A by B shifts all of the pixels in column A (0 is the left column) down by B pixels
    // Pixels that would fall off the bottom appear at the top of the column
    let rotateColumn x a (grid: int [,]) =
        grid.[*, x] <- grid.[*, x] |> rotate (Height - a)
        grid

    let applyInstruction (grid: int [,]) = function
        | Rect (x, y) -> setRect x y grid
        | RotateRow (y, a) -> rotateRow y a grid
        | RotateColumn (x, a) -> rotateColumn x a grid
    
    let getPixels() =
        let input = parseInput()
        let grid = Array2D.zeroCreate Height Width

        (grid, input)
        ||> Seq.fold applyInstruction


    // There seems to be an intermediate check of the voltage used by the display:
    // after you swipe your card, if the screen did work, how many pixels should be lit?
    let Part1() = getPixels() |> Seq.cast<int> |> Seq.sum

    // After you swipe your card, what code is the screen trying to display?
    let Part2() =
        getPixels()
        |> Seq.cast<int>
        |> Seq.map (function 1 -> '█' | _ -> ' ')
        |> Seq.chunkBySize Width
        |> Seq.map String
        |> String.concat "\n"