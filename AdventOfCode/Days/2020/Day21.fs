namespace AdventOfCode.Days.Y2020

open AdventOfCode.Shared.Utility

/// Day 21: Allergen Assessment
/// https://adventofcode.com/2020/day/21
/// You reach the train's last stop and the closest you can get to your vacation island without getting wet.
module Day21 =

    type Food = { Ingredients: string list; Allergens: string list }

    let parse (s:string) = 
        let tokens = s |> splitOn " (contains "
        let ingredients = tokens.[0] |> split ' ' |> Array.toList
        let allergens = tokens.[1].Trim(')') |> splitOn ", " |> Array.toList

        { Ingredients = ingredients; Allergens = allergens }

    let parseInput() = getFile (2020, 21) |> readLinesAs parse |> Seq.toList

    let getIngredientCountByAllergen foodList = 
        foodList
        |> List.collect (fun x -> List.allPairs x.Allergens x.Ingredients)
        |> List.groupBy fst
        |> List.map (fun (k, v) -> k, v |> List.map snd |> List.countBy id)

    // If an ingredient appears every time that an allergen does then it's a possible match
    let getPossible (allergen, (counts: (string * int) list)) =
        let max = counts |> List.map snd |> List.max
    
        let ingredients =
            counts
            |> List.filter (fun (_, c) -> c = max)
            |> List.map fst

        (allergen, ingredients)

    // Determine which ingredients cannot possibly contain any of the allergens in your list.
    // How many times do any of those ingredients appear?
    let Part1() =
        let input = parseInput()

        let possible =
            getIngredientCountByAllergen input
            |> List.collect (getPossible >> snd)
            |> set

        input
        |> List.collect (fun x -> x.Ingredients)
        |> countIf (possible.Contains >> not)

    // Arrange the ingredients alphabetically by their allergen and separate them by commas to produce
    // your canonical dangerous ingredient list. What is your canonical dangerous ingredient list?
    let Part2() =
        let input = parseInput()
               
        let possible =
            getIngredientCountByAllergen input
            |> List.map getPossible
            |> List.map (fun (k, v) -> k, set v)

        let rec solve (possible: (string * (string Set)) list) (known: (string * string) list) =
            let possible = possible |> List.sortBy (fun (_, i) -> i.Count)

            match possible with
            | [] -> known
            | (allergen, ingredients)::t when ingredients.Count = 1 ->
                let ingredient = ingredients.MinimumElement
                let remainingPossible = t |> List.map (fun (a, i) -> a, i.Remove ingredient)

                solve remainingPossible ((allergen, ingredient)::known)
            | x -> failwithf "Multiple options: %A" x // Could handle this recursively, but doesn't happen
                
        solve possible []
        |> List.sortBy fst
        |> List.map snd
        |> String.concat ","