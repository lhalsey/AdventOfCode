namespace AdventOfCode.Days.Y2020

open AdventOfCode.Shared.Utility
open System.Collections.Generic

/// Day 16: Ticket Translation
/// https://adventofcode.com/2020/day/16
/// As you're walking to yet another connecting flight, you realize that one of the legs of your
/// re-routed trip coming up is on a high-speed train.
module Day16 =

    type Rule = { Id: int; Name: string; ValidValues: IReadOnlySet<int> }

    // E.g. "zone: 36-621 or 637-963"
    let parseRule index = function
        | Regex "(.+): (\d+)-(\d+) or (\d+)-(\d+)" [ name; Int min1; Int max1; Int min2; Int max2 ] ->
            { Id = index; Name = name; ValidValues = [ min1 .. max1 ] @ [ min2 .. max2 ] |> toReadOnlyHashSet }
        | x -> failwithf "Invalid rule format: %s" x
       
    // 83,127,131,137,113,73,139,101,67,53,107,103,59,149,109,61,79,71,97,89
    let parseTicket (s: string) = s |> split ',' |> Array.map int |> Array.toList
    
    let parseInput() =
        let lines = getFile (2020, 16) |> readAllLines
        let yourTicketIndex = lines |> Array.findIndex (fun x -> x = "your ticket:")

        let rules = lines.[0..yourTicketIndex - 2] |> Array.mapi parseRule |> Array.toList
        let yourTicket = lines.[yourTicketIndex + 1] |> parseTicket
        let otherTickets = lines.[yourTicketIndex + 4 ..] |> Array.map parseTicket |> Array.toList

        rules, yourTicket, otherTickets
     

    // Consider the validity of the nearby tickets you scanned. What is your ticket scanning error rate?
    let Part1() =
        let (rules, _, otherTickets) = parseInput()

        let isValid x = rules |> List.exists (fun r -> r.ValidValues.Contains x)

        otherTickets
        |> List.concat
        |> List.sumBy (fun x -> if isValid x then 0 else x)

    // Once you work out which field is which, look for the six fields on your ticket that start with
    // the word departure. What do you get if you multiply those six values together?
    let Part2() =
        let (rules, yourTicket, otherTickets) = parseInput()

        let isValid x = rules |> List.exists (fun r -> r.ValidValues.Contains x)
        
        let validTickets = otherTickets |> List.filter (List.forall isValid)

        // For each field position (column) get set of distinct values that must be in rule range
        let columns = validTickets |> List.transpose |> List.map Set

        // Determine which columns the rule can satisfy
        let getColumnMatches rule =
            columns
            |> findIndexes (rule.ValidValues.IsSupersetOf)
            |> set

        // Sort rules with fewest matching columns first to narrow search
        let ruleColumnMatches =
            rules
            |> List.map (fun rule -> rule, getColumnMatches rule)
            |> List.sortBy (fun (_, m) -> m.Count)

        // Depth first search until we find a valid column for each rule
        let rec solve ruleColumnMatches colIndexes =
            match ruleColumnMatches with
            | [] -> List.rev colIndexes // Rules met - reverse as we prepend for efficiency
            | (_, cols)::t -> (cols - Set colIndexes)
                              |> Seq.map (fun col -> solve t (col::colIndexes))
                              |> Seq.head
                    
        let colOrder = solve ruleColumnMatches []

        // Match the column indexes up to the rules and multiply values for depature fields
        (ruleColumnMatches, colOrder)
        ||> List.map2 (fun (rule, _) col -> rule.Name, col)
        |> List.filter (fun (name, _) -> name.StartsWith "departure")
        |> List.map (fun (_, col) -> int64 yourTicket.[col])
        |> List.reduce (*)