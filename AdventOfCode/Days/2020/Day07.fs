namespace AdventOfCode.Days.Y2020

open AdventOfCode.Shared.Utility
open System.Collections.Generic

/// Day 7: Handy Haversacks
/// https://adventofcode.com/2020/day/7
/// You land at the regional airport in time for your next flight.
module Day07 =

    type BagMap = IReadOnlyDictionary<string, (int * string) []>

    let [<Literal>] ShinyGold = "shiny gold"
        
    // E.g. "light red bags contain 2 clear indigo bags, 3 light lime bags."
    let parse (s: string) = 
        let parseBags = split ' ' >> function
            | [| Int c; b1; b2; _ |] -> Some (c, $"{b1} {b2}")
            | _ -> None // "no other bags"

        let (bag, bags) = s |> splitIntoPair " bags contain "
        bag, bags |> splitOn ", " |> Array.choose parseBags

    let parseInput() = getFile (2020, 7) |> readLinesAs parse |> readOnlyDict

    // Count number of bags that contain target (using memoisation for performance)
    // We could reverse the map to traverse up the tree, but this performs well
    let countBagsContaining target (bagMap: BagMap) =
        let rec countBagsContainingR bag =
            bag = target || bagMap.[bag] |> Array.exists (fun (_, bags) -> containsBagMemo bags)
        and containsBagMemo = memoise countBagsContainingR

        bagMap.Keys |> countIf containsBagMemo

    // Count total number of bags within a bag
    let rec countContainedBags bag (bagMap: BagMap)  =
        bagMap.[bag]
        |> Array.sumBy (fun (count, bags) -> count * countContainedBags bags bagMap)
        |> (+) 1 // Include bag itself


    // How many bag colors can eventually contain at least one shiny gold bag?
    let Part1() = parseInput() |> countBagsContaining ShinyGold |> (+) -1 // Don't count shiny gold bag
        
    // How many individual bags are required inside your single shiny gold bag?
    let Part2() = parseInput() |> countContainedBags ShinyGold |> (+) -1 // Don't count shiny gold bag