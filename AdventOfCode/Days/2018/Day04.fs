namespace AdventOfCode.Days.Y2018

open AdventOfCode.Shared.Utility
open System

/// Day 4: Repose Record
/// https://adventofcode.com/2018/day/4
/// You've sneaked into another supply closet - this time, it's across from the prototype suit manufacturing lab.
module Day04 =

    type Action = StartsShift | FallsAsleep | WakesUp

    type Event = { Time: DateTime; GuardId: int option; Action: Action }

    let parse = function // E.g. [1518-10-12 23:58] Guard #421 begins shift
        | Regex "\[(.+)\] Guard #(\d+) begins shift" [Date date; Int id] -> { Time = date; GuardId = Some id; Action = StartsShift }
        | Regex "\[(.+)\] falls asleep" [Date date] -> { Time = date; GuardId = None; Action = FallsAsleep }
        | Regex "\[(.+)\] wakes up" [Date date] -> { Time = date; GuardId = None; Action = WakesUp }
        | x -> failwithf "Invalid input: %s" x

    let parseInput() = getFile (2018, 4) |> readLinesAs parse

    let getSleepMinutes (events: Event list) =
        let rec getSleepMinutesR (events: Event list) (guardId: int option) (startMinute: int option) =
            seq {
                match events, guardId, startMinute with
                | [], _, _ -> ()
                | { GuardId = guardId; Action = a }::t, _, _ when a = StartsShift ->
                    yield! getSleepMinutesR t guardId None // Record guard id
                | { Time = time; Action = a }::t, _, _ when a = FallsAsleep ->
                    yield! getSleepMinutesR t guardId (Some time.Minute) // Record sleep start time
                | { Time = time; Action = a }::t, Some id, Some startMin when a = WakesUp -> 
                    yield! [startMin .. time.Minute - 1] |> List.map (fun x -> (id, x)) // Yield all sleep minutes
                    yield! getSleepMinutesR t guardId None // Unset sleep start time
                | x -> failwithf "Invalid input: %A" x
            }

        getSleepMinutesR events None None

    // Find the guard that has the most minutes asleep. What minute does that guard spend asleep the most?
    // What is the ID of the guard you chose multiplied by the minute you chose?
    let Part1() =
        let events = parseInput() |> Seq.toList |> List.sort

        let minutes = getSleepMinutes events |> Seq.toList

        let sleepiestGuardId =
            minutes
            |> List.countBy fst
            |> List.maxBy snd
            |> fst

        let minutesSlept =
            minutes
            |> List.filter (fun (id, m) -> id = sleepiestGuardId)
            |> List.countBy snd
            |> List.maxBy snd
            |> fst

        sleepiestGuardId * minutesSlept

    // Of all guards, which guard is most frequently asleep on the same minute?
    // What is the ID of the guard you chose multiplied by the minute you chose?
    let Part2() =
        let events = parseInput() |> Seq.toList |> List.sort

        let minutes = getSleepMinutes events |> Seq.toList

        let getSleepiestMinute (guardId, guardMinutes) =
            let (min, count) =
                guardMinutes
                |> List.countBy snd
                |> List.maxBy snd

            (guardId, min, count)

        let guardMinutes =
            minutes
            |> List.groupBy fst
            |> List.map getSleepiestMinute

        let (guardId, min) =
            guardMinutes
            |> List.maxBy (fun (_, _, x) -> x)
            |> fun (g, m, _) -> g, m

        guardId * min