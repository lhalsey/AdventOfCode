module CodeGenerator

open FSharp.Data

open System.IO

let [<Literal>] BaseUrl = "https://adventofcode.com"
let [<Literal>] SampleUrl = "https://adventofcode.com/2015/day/1"

type DayType = HtmlProvider<SampleUrl>

type DaysType = CsvProvider<"Days.csv", Separators = "|">

// This isn't terribly robust obviously as it relies on the HTML structure remaining consistent
let getDayDescription (url: string) =
    try
        let dayElements =
            DayType.Load(url).Html.Descendants (fun x -> x.HasClass "day-desc")
            |> Seq.head
            |> fun x -> x.Elements()

        // E.g. "--- Day 17: Set and Forget ---"
        let title = 
            dayElements.[0].InnerText()
            |> fun x -> x.Trim([|'-'; ' '|]).Split(':').[1].Trim()

        // Get first sentence (or more depending on punctuation) of description
        let desc = dayElements.[1].InnerText()
        let index = desc.IndexOfAny([|'.'; '!'|])
        let desc1 = if index > 0 then desc.[0..index] else desc

        title, desc1
        
    with _ -> "Title TBC", "Description TBC" // Might want to generate code in advance for future years


let getDay (year, day) =
    let url = sprintf "%s/%i/day/%i" BaseUrl year day
    let (title, desc) = getDayDescription url

    sprintf "%i|%i|%s|%s|%s" year day title desc url


// Use to generate Days.csv
let GetDays() =
    let days =
        ([2015 .. 2020], [1 .. 25])
        ||> List.allPairs 
        |> List.map getDay

    let header = "Year|Day|Title|Desc|Url"

    header::days |> String.concat "\n"


let genClass (x: DaysType.Row) =

    let code = 
        [ sprintf "namespace AdventOfCode.Days.Y%i\n" x.Year
          "open AdventOfCode.Shared.Utility\n"
          sprintf "/// Day %i: %s" x.Day x.Title
          sprintf "/// %s" x.Url
          sprintf "/// %s" x.Desc
          sprintf "module Day%02i =\n" x.Day
          sprintf "    let parseInput() = getFile (%i, %i) |> readLines\n" x.Year x.Day
          "    let Part1() ="
          "        0\n"
          "    let Part2() ="
          "        0"
          ]
         |> String.concat "\n"

    let path = Path.Combine(__SOURCE_DIRECTORY__, "Output")
    let path = Path.Combine(path, string x.Year)
    let path = Path.Combine(path, sprintf "Day%02i.fs" x.Day)

    File.WriteAllText(path, code)


let Generate() =

    DaysType.Load("Days.csv").Rows
    //|> Seq.filter (fun x -> x.Year = 2019)
    |> Seq.iter genClass
