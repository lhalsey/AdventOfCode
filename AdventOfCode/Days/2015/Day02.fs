namespace AdventOfCode.Days.Y2015

open AdventOfCode.Shared.Utility

/// Day 2: I Was Told There Would Be No Math
/// https://adventofcode.com/2015/day/2
/// The elves are running low on wrapping paper, and so they need to submit an order for more.
module Day02 =

    // Every present is a box (a perfect right rectangular prism), which makes
    // calculating the required wrapping paper for each gift a little easier:
    // find the surface area of the box, which is 2*l*w + 2*w*h + 2*h*l.
    // The elves also need a little extra paper for each present: the area of the smallest side.
    let getWrappingPaper (l, w, h) = 
        let sides = [l * w; w * h; h * l]
        let area = List.sum sides * 2
        let smallestSideArea = List.min sides
        area + smallestSideArea

    // The ribbon required to wrap a present is the shortest distance around its sides,
    // or the smallest perimeter of any one face. Each present also requires a bow made
    // out of ribbon as well; the feet of ribbon required for the perfect bow is equal
    // to the cubic feet of volume of the present.
    let getRibbon (l, w, h) =
        let volume = l * w * h
        let longestDimension = List.max [l; w; h]
        let smallestSidePerimeter = (l + w + h - longestDimension) * 2
        volume + smallestSidePerimeter

    // E.g. "29x13x26"
    let parse box =
        box
        |> split 'x'
        |> fun dims -> (int dims.[0], int dims.[1], int dims.[2])

    let parseInput() = getFile (2015, 2) |> readLinesAs parse


    // How many total square feet of wrapping paper should they order?
    let Part1() = parseInput() |> Seq.sumBy getWrappingPaper

    // How many total feet of ribbon should they order?
    let Part2() = parseInput() |> Seq.sumBy getRibbon