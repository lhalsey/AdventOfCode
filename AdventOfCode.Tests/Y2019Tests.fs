namespace AdventOfCode.Tests.Y2019

open AdventOfCode.Days.Y2019
open Xunit
open FsUnit.Xunit

module ``Y2019 Tests`` =

    [<Fact>]
    let ``Day 01 Part 1`` () = Day01.Part1()  |> should equal 3_305_115

    [<Fact>]
    let ``Day 01 Part 2`` () = Day01.Part2() |> should equal 4_954_799

    [<Fact>]
    let ``Day 02 Part 1`` () = Day02.Part1()  |> should equal 6_327_510L

    [<Fact>]
    let ``Day 02 Part 2`` () = Day02.Part2() |> should equal 4_112L

    [<Fact>]
    let ``Day 03 Part 1`` () = Day03.Part1()  |> should equal 855
        
    [<Fact>]
    let ``Day 03 Part 2`` () = Day03.Part2() |> should equal 11_238
    
    [<Fact>]
    let ``Day 04 Part 1`` () = Day04.Part1()  |> should equal 495
        
    [<Fact>]
    let ``Day 04 Part 2`` () = Day04.Part2() |> should equal 305
    
    [<Fact>]
    let ``Day 05 Part 1`` () = Day05.Part1()  |> should equal 9_025_675L
        
    [<Fact>]
    let ``Day 05 Part 2`` () = Day05.Part2() |> should equal 11_981_754L
    
    [<Fact>]
    let ``Day 06 Part 1`` () = Day06.Part1()  |> should equal 271_151
        
    [<Fact>]
    let ``Day 06 Part 2`` () = Day06.Part2() |> should equal 388
    
    [<Fact>]
    let ``Day 07 Part 1`` () = Day07.Part1()  |> should equal 77_500L
            
    [<Fact>]
    let ``Day 07 Part 2`` () = Day07.Part2() |> should equal 22_476_942L
    
    [<Fact>]
    let ``Day 08 Part 1`` () = Day08.Part1()  |> should equal 2_080
                
    [<Fact>]
    let ``Day 08 Part 2`` () =
        let expected =
            " ██  █  █ ███   ██  █   █\n" +
            "█  █ █  █ █  █ █  █ █   █\n" +
            "█  █ █  █ █  █ █     █ █ \n" +
            "████ █  █ ███  █      █  \n" +
            "█  █ █  █ █ █  █  █   █  \n" +
            "█  █  ██  █  █  ██    █  ";

        Day08.Part2() |> should equal expected // "AURCY"
    
    [<Fact>]
    let ``Day 09 Part 1`` () = Day09.Part1()  |> should equal 3_638_931_938L
                
    [<Fact>]
    let ``Day 09 Part 2`` () = Day09.Part2() |> should equal 86_025L
    
    [<Fact>]
    let ``Day 10 Part 1`` () = Day10.Part1()  |> should equal 292
                
    [<Fact>]
    let ``Day 10 Part 2`` () = Day10.Part2() |> should equal 317
    
    [<Fact>]
    let ``Day 11 Part 1`` () = Day11.Part1()  |> should equal 1_681
                
    [<Fact>]
    let ``Day 11 Part 2`` () =
        let expected =
            " ████  ██  ████  ██  ███  █  █  ██  █  █   \n" + 
            " █    █  █    █ █  █ █  █ █ █  █  █ █ █    \n" + 
            " ███  █      █  █    █  █ ██   █    ██     \n" + 
            " █    █ ██  █   █    ███  █ █  █ ██ █ █    \n" + 
            " █    █  █ █    █  █ █ █  █ █  █  █ █ █    \n" + 
            " ████  ███ ████  ██  █  █ █  █  ███ █  █   "
    
        Day11.Part2() |> should equal expected // "EGZCRKGK"
    
    [<Fact>]
    let ``Day 12 Part 1`` () = Day12.Part1() |> should equal 12_644
                
    [<Fact>]
    let ``Day 12 Part 2`` () = Day12.Part2() |> should equal 290_314_621_566_528L
    
    [<Fact>]
    let ``Day 13 Part 1`` () = Day13.Part1() |> should equal 324
                
    [<Fact>]
    let ``Day 13 Part 2`` () = Day13.Part2() |> should equal 15_957
    
    [<Fact>]
    let ``Day 14 Part 1`` () = Day14.Part1() |> should equal 469_536L
                
    [<Fact>]
    let ``Day 14 Part 2`` () = Day14.Part2() |> should equal 3_343_477L
    
    [<Fact>]
    let ``Day 15 Part 1`` () = Day15.Part1() |> should equal 272
                
    [<Fact>]
    let ``Day 15 Part 2`` () = Day15.Part2() |> should equal 398
    
    [<Fact>]
    let ``Day 16 Part 1`` () = Day16.Part1()  |> should equal 29_956_495
                
    [<Fact>]
    let ``Day 16 Part 2`` () = Day16.Part2() |> should equal 73_556_504
    
    [<Fact>]
    let ``Day 17 Part 1`` () = Day17.Part1() |> should equal 7_780
                
    [<Fact>]
    let ``Day 17 Part 2`` () = Day17.Part2() |> should equal 1_075_882L
    
    [<Fact>]
    let ``Day 18 Part 1`` () = Day18.Part1() |> should equal 5_402
                
    [<Fact>]
    let ``Day 18 Part 2`` () = Day18.Part2() |> should equal 2_138
    
    [<Fact>]
    let ``Day 19 Part 1`` () = Day19.Part1()  |> should equal 166
                
    [<Fact>]
    let ``Day 19 Part 2`` () = Day19.Part2() |> should equal 3_790_981
    
    [<Fact>]
    let ``Day 20 Part 1`` () = Day20.Part1()  |> should equal 588
                
    [<Fact>]
    let ``Day 20 Part 2`` () = Day20.Part2() |> should equal 6_834
    
    [<Fact>]
    let ``Day 21 Part 1`` () = Day21.Part1()  |> should equal 19_357_761L
            
    [<Fact>]
    let ``Day 21 Part 2`` () = Day21.Part2() |> should equal 1_142_249_706L
    
    [<Fact>]
    let ``Day 22 Part 1`` () = Day22.Part1()  |> should equal 6_850
                
    [<Fact>]
    let ``Day 22 Part 2`` () = Day22.Part2() |> should equal 13_224_103_523_662I
    
    [<Fact>]
    let ``Day 23 Part 1`` () = Day23.Part1()  |> should equal 16_660L
                
    [<Fact>]
    let ``Day 23 Part 2`` () = Day23.Part2() |> should equal 11_504L
    
    [<Fact>]
    let ``Day 24 Part 1`` () = Day24.Part1()  |> should equal 32_526_865
                
    [<Fact>]
    let ``Day 24 Part 2`` () = Day24.Part2() |> should equal 2_009
    
    [<Fact>]
    let ``Day 25 Part 1`` () = Day25.Part1()  |> should equal 2_214_608_912L
                
    [<Fact>]
    let ``Day 25 Part 2`` () = Day25.Part2() |> should equal 1L