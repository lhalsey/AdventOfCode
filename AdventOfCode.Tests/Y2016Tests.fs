namespace AdventOfCode.Tests.Y2016

open AdventOfCode.Days.Y2016
open Xunit
open FsUnit.Xunit

module ``Y2016 Tests`` =

    let [<Literal>] Unknown = -99

    [<Fact>]
    let ``Day 01 Part 1`` () = Day01.Part1() |> should equal 278

    [<Fact>]
    let ``Day 01 Part 2`` () = Day01.Part2() |> should equal 161

    [<Fact>]
    let ``Day 02 Part 1`` () = Day02.Part1() |> should equal "98575"

    [<Fact>]
    let ``Day 02 Part 2`` () = Day02.Part2() |> should equal "CD8D4"

    [<Fact>]
    let ``Day 03 Part 1`` () = Day03.Part1() |> should equal 862
        
    [<Fact>]
    let ``Day 03 Part 2`` () = Day03.Part2() |> should equal 1_577
    
    [<Fact>]
    let ``Day 04 Part 1`` () = Day04.Part1() |> should equal 361_724
        
    [<Fact>]
    let ``Day 04 Part 2`` () = Day04.Part2() |> should equal 482
    
    [<Fact>]
    let ``Day 05 Part 1`` () = Day05.Part1() |> should equal "4543c154"
        
    [<Fact>]
    let ``Day 05 Part 2`` () = Day05.Part2() |> should equal "1050cbbd"
    
    [<Fact>]
    let ``Day 06 Part 1`` () = Day06.Part1() |> should equal "kqsdmzft"
        
    [<Fact>]
    let ``Day 06 Part 2`` () = Day06.Part2() |> should equal "tpooccyo"
    
    [<Fact>]
    let ``Day 07 Part 1`` () = Day07.Part1() |> should equal 105
            
    [<Fact>]
    let ``Day 07 Part 2`` () = Day07.Part2() |> should equal 258
    
    [<Fact>]
    let ``Day 08 Part 1`` () = Day08.Part1() |> should equal 121
                
    [<Fact>]
    let ``Day 08 Part 2`` () =

        let expected =
            "███  █  █ ███  █  █  ██  ████  ██  ████  ███ █    \n" +
            "█  █ █  █ █  █ █  █ █  █ █    █  █ █      █  █    \n" +   
            "█  █ █  █ █  █ █  █ █    ███  █  █ ███    █  █    \n" +   
            "███  █  █ ███  █  █ █    █    █  █ █      █  █    \n" +   
            "█ █  █  █ █ █  █  █ █  █ █    █  █ █      █  █    \n" +   
            "█  █  ██  █  █  ██   ██  ████  ██  ████  ███ ████ "

        Day08.Part2() |> should equal expected
    
    [<Fact>]
    let ``Day 09 Part 1`` () = Day09.Part1() |> should equal 120_765L
                
    [<Fact>]
    let ``Day 09 Part 2`` () = Day09.Part2() |> should equal 11_658_395_076L
    
    [<Fact>]
    let ``Day 10 Part 1`` () = Day10.Part1() |> should equal 113
                
    [<Fact>]
    let ``Day 10 Part 2`` () = Day10.Part2() |> should equal 12_803
    
    [<Fact>]
    let ``Day 11 Part 1`` () = Day11.Part1() |> should equal 33
                
    [<Fact>]
    let ``Day 11 Part 2`` () = Day11.Part2() |> should equal 57
    
    [<Fact>]
    let ``Day 12 Part 1`` () = Day12.Part1() |> should equal 318_077
                
    [<Fact>]
    let ``Day 12 Part 2`` () = Day12.Part2() |> should equal 9_227_731
    
    [<Fact>]
    let ``Day 13 Part 1`` () = Day13.Part1() |> should equal 96
                
    [<Fact>]
    let ``Day 13 Part 2`` () = Day13.Part2() |> should equal 141
    
    [<Fact>]
    let ``Day 14 Part 1`` () = Day14.Part1() |> should equal 25_427
                
    [<Fact>]
    let ``Day 14 Part 2`` () = Day14.Part2() |> should equal 22_045
    
    [<Fact>]
    let ``Day 15 Part 1`` () = Day15.Part1() |> should equal 203_660
                
    [<Fact>]
    let ``Day 15 Part 2`` () = Day15.Part2() |> should equal 2_408_135
    
    [<Fact>]
    let ``Day 16 Part 1`` () = Day16.Part1() |> should equal "01110011101111011"
                
    [<Fact>]
    let ``Day 16 Part 2`` () = Day16.Part2() |> should equal "11001111011000111"
    
    [<Fact>]
    let ``Day 17 Part 1`` () = Day17.Part1() |> should equal "RDRLDRDURD"
                
    [<Fact>]
    let ``Day 17 Part 2`` () = Day17.Part2() |> should equal 596
    
    [<Fact>]
    let ``Day 18 Part 1`` () = Day18.Part1() |> should equal 1_963
                
    [<Fact>]
    let ``Day 18 Part 2`` () = Day18.Part2() |> should equal 20_009_568
    
    [<Fact>]
    let ``Day 19 Part 1`` () = Day19.Part1() |> should equal 1_834_903
                
    [<Fact>]
    let ``Day 19 Part 2`` () = Day19.Part2() |> should equal 1_420_280
    
    [<Fact>]
    let ``Day 20 Part 1`` () = Day20.Part1() |> should equal 14_975_795u
                
    [<Fact>]
    let ``Day 20 Part 2`` () = Day20.Part2() |> should equal 101
    
    [<Fact>]
    let ``Day 21 Part 1`` () = Day21.Part1() |> should equal "dgfaehcb"
            
    [<Fact>]
    let ``Day 21 Part 2`` () = Day21.Part2() |> should equal "fdhgacbe"
    
    [<Fact>]
    let ``Day 22 Part 1`` () = Day22.Part1() |> should equal 860
                
    [<Fact>]
    let ``Day 22 Part 2`` () = Day22.Part2() |> should equal 200
    
    [<Fact>]
    let ``Day 23 Part 1`` () = Day23.Part1() |> should equal 12_762
                
    [<Fact>]
    let ``Day 23 Part 2`` () = Day23.Part2() |> should equal 479_009_322
    
    [<Fact>]
    let ``Day 24 Part 1`` () = Day24.Part1() |> should equal 442L
                
    [<Fact>]
    let ``Day 24 Part 2`` () = Day24.Part2() |> should equal 660L
    
    [<Fact>]
    let ``Day 25 Part 1`` () = Day25.Part1() |> should equal 198
                
    [<Fact>]
    let ``Day 25 Part 2`` () = Day25.Part2() |> should equal 1