namespace AdventOfCode.Tests.Y2021

open AdventOfCode.Days.Y2021
open Xunit
open FsUnit.Xunit

module ``Y2021 Tests`` =

    let [<Literal>] Unknown = -99

    [<Fact>]
    let ``Day 01 Part 1`` () = Day01.Part1() |> should equal 1752

    [<Fact>]
    let ``Day 01 Part 2`` () = Day01.Part2() |> should equal 1781

    [<Fact>]
    let ``Day 02 Part 1`` () = Day02.Part1() |> should equal 2_322_630

    [<Fact>]
    let ``Day 02 Part 2`` () = Day02.Part2() |> should equal 2_105_273_490

    [<Fact>]
    let ``Day 03 Part 1`` () = Day03.Part1() |> should equal 4_191_876
        
    [<Fact>]
    let ``Day 03 Part 2`` () = Day03.Part2() |> should equal 3_414_905
    
    [<Fact>]
    let ``Day 04 Part 1`` () = Day04.Part1() |> should equal 25_023
        
    [<Fact>]
    let ``Day 04 Part 2`` () = Day04.Part2() |> should equal 2_634
    
    [<Fact>]
    let ``Day 05 Part 1`` () = Day05.Part1() |> should equal 6_397
        
    [<Fact>]
    let ``Day 05 Part 2`` () = Day05.Part2() |> should equal 22_335
    
    [<Fact>]
    let ``Day 06 Part 1`` () = Day06.Part1() |> should equal 351_188L
        
    [<Fact>]
    let ``Day 06 Part 2`` () = Day06.Part2() |> should equal 1_595_779_846_729L
    
    [<Fact>]
    let ``Day 07 Part 1`` () = Day07.Part1() |> should equal 344_297
            
    [<Fact>]
    let ``Day 07 Part 2`` () = Day07.Part2() |> should equal 97_164_301
    
    [<Fact>]
    let ``Day 08 Part 1`` () = Day08.Part1() |> should equal 473
                
    [<Fact>]
    let ``Day 08 Part 2`` () = Day08.Part2() |> should equal 1_097_568
    
    [<Fact>]
    let ``Day 09 Part 1`` () = Day09.Part1() |> should equal 631
                
    [<Fact>]
    let ``Day 09 Part 2`` () = Day09.Part2() |> should equal 821_560
    
    [<Fact>]
    let ``Day 10 Part 1`` () = Day10.Part1() |> should equal 215_229
                
    [<Fact>]
    let ``Day 10 Part 2`` () = Day10.Part2() |> should equal 1_105_996_483L
    
    [<Fact>]
    let ``Day 11 Part 1`` () = Day11.Part1() |> should equal 1_652
                
    [<Fact>]
    let ``Day 11 Part 2`` () = Day11.Part2() |> should equal 220
    
    [<Fact>]
    let ``Day 12 Part 1`` () = Day12.Part1() |> should equal 4_304
                
    [<Fact>]
    let ``Day 12 Part 2`` () = Day12.Part2() |> should equal 118_242
    
    [<Fact>]
    let ``Day 13 Part 1`` () = Day13.Part1() |> should equal 724
                
    [<Fact>]
    let ``Day 13 Part 2`` () =
        let expected = 
            " ██  ███    ██ ███  ████ ███  █  █ █   \n" +
            "█  █ █  █    █ █  █ █    █  █ █  █ █   \n" + 
            "█    █  █    █ ███  ███  █  █ █  █ █   \n" + 
            "█    ███     █ █  █ █    ███  █  █ █   \n" + 
            "█  █ █    █  █ █  █ █    █ █  █  █ █   \n" + 
            " ██  █     ██  ███  ████ █  █  ██  ████"
    
        Day13.Part2() |> should equal expected // CPJBERUL
    
    [<Fact>]
    let ``Day 14 Part 1`` () = Day14.Part1() |> should equal 2_915L
                
    [<Fact>]
    let ``Day 14 Part 2`` () = Day14.Part2() |> should equal 3_353_146_900_153L
    
    [<Fact>]
    let ``Day 15 Part 1`` () = Day15.Part1() |> should equal 410
                
    [<Fact>]
    let ``Day 15 Part 2`` () = Day15.Part2() |> should equal 2_809
    
    [<Fact>]
    let ``Day 16 Part 1`` () = Day16.Part1() |> should equal 969
                
    [<Fact>]
    let ``Day 16 Part 2`` () = Day16.Part2() |> should equal 124_921_618_408L
    
    [<Fact>]
    let ``Day 17 Part 1`` () = Day17.Part1() |> should equal 3_003
                
    [<Fact>]
    let ``Day 17 Part 2`` () = Day17.Part2() |> should equal 940
    
    [<Fact>]
    let ``Day 18 Part 1`` () = Day18.Part1() |> should equal 4_641
                
    [<Fact>]
    let ``Day 18 Part 2`` () = Day18.Part2() |> should equal 4_624
    
    [<Fact>]
    let ``Day 19 Part 1`` () = Day19.Part1() |> should equal 318
                
    [<Fact>]
    let ``Day 19 Part 2`` () = Day19.Part2() |> should equal 12_166
    
    [<Fact>]
    let ``Day 20 Part 1`` () = Day20.Part1() |> should equal 5_339
                
    [<Fact>]
    let ``Day 20 Part 2`` () = Day20.Part2() |> should equal 18_395
    
    [<Fact>]
    let ``Day 21 Part 1`` () = Day21.Part1() |> should equal 916_083
            
    [<Fact>]
    let ``Day 21 Part 2`` () = Day21.Part2() |> should equal 49_982_165_861_983L
    
    [<Fact>]
    let ``Day 22 Part 1`` () = Day22.Part1() |> should equal 602_574
                
    [<Fact>]
    let ``Day 22 Part 2`` () = Day22.Part2() |> should equal Unknown
    
    [<Fact>]
    let ``Day 23 Part 1`` () = Day23.Part1() |> should equal 16_244
                
    [<Fact>]
    let ``Day 23 Part 2`` () = Day23.Part2() |> should equal 43_226
    
    [<Fact>]
    let ``Day 24 Part 1`` () = Day24.Part1() |> should equal Unknown
                
    [<Fact>]
    let ``Day 24 Part 2`` () = Day24.Part2() |> should equal Unknown
    
    [<Fact>]
    let ``Day 25 Part 1`` () = Day25.Part1() |> should equal 295
                
    [<Fact>]
    let ``Day 25 Part 2`` () = Day25.Part2() |> should equal 1