namespace AdventOfCode.Tests.Y2015

open AdventOfCode.Days.Y2015
open Xunit
open FsUnit.Xunit

module ``Y2015 Tests`` =

    [<Fact>]
    let ``Day 01 Part 1`` () = Day01.Part1() |> should equal 232

    [<Fact>]
    let ``Day 01 Part 2`` () = Day01.Part2() |> should equal 1_783

    [<Fact>]
    let ``Day 02 Part 1`` () = Day02.Part1() |> should equal 1_586_300

    [<Fact>]
    let ``Day 02 Part 2`` () = Day02.Part2() |> should equal 3_737_498

    [<Fact>]
    let ``Day 03 Part 1`` () = Day03.Part1() |> should equal 2_592
        
    [<Fact>]
    let ``Day 03 Part 2`` () = Day03.Part2() |> should equal 2_360
    
    [<Fact>]
    let ``Day 04 Part 1`` () = Day04.Part1() |> should equal 254_575
        
    [<Fact>]
    let ``Day 04 Part 2`` () = Day04.Part2() |> should equal 1_038_736
    
    [<Fact>]
    let ``Day 05 Part 1`` () = Day05.Part1() |> should equal 255
        
    [<Fact>]
    let ``Day 05 Part 2`` () = Day05.Part2() |> should equal 55
    
    [<Fact>]
    let ``Day 06 Part 1`` () = Day06.Part1() |> should equal 543_903
        
    [<Fact>]
    let ``Day 06 Part 2`` () = Day06.Part2() |> should equal 14_687_245
    
    [<Fact>]
    let ``Day 07 Part 1`` () = Day07.Part1() |> should equal 956
            
    [<Fact>]
    let ``Day 07 Part 2`` () = Day07.Part2() |> should equal 40_149
    
    [<Fact>]
    let ``Day 08 Part 1`` () = Day08.Part1() |> should equal 1_333
                
    [<Fact>]
    let ``Day 08 Part 2`` () = Day08.Part2() |> should equal 2_046
    
    [<Fact>]
    let ``Day 09 Part 1`` () = Day09.Part1() |> should equal 207
                
    [<Fact>]
    let ``Day 09 Part 2`` () = Day09.Part2() |> should equal 804
    
    [<Fact>]
    let ``Day 10 Part 1`` () = Day10.Part1() |> should equal 492_982
                
    [<Fact>]
    let ``Day 10 Part 2`` () = Day10.Part2() |> should equal 6_989_950
    
    [<Fact>]
    let ``Day 11 Part 1`` () = Day11.Part1() |> should equal "hxbxxyzz"
                
    [<Fact>]
    let ``Day 11 Part 2`` () = Day11.Part2() |> should equal "hxcaabcc"
    
    [<Fact>]
    let ``Day 12 Part 1`` () = Day12.Part1() |> should equal 119_433
                
    [<Fact>]
    let ``Day 12 Part 2`` () = Day12.Part2() |> should equal 68_466
    
    [<Fact>]
    let ``Day 13 Part 1`` () = Day13.Part1() |> should equal 618
                
    [<Fact>]
    let ``Day 13 Part 2`` () = Day13.Part2() |> should equal 601
    
    [<Fact>]
    let ``Day 14 Part 1`` () = Day14.Part1() |> should equal 2_655
                
    [<Fact>]
    let ``Day 14 Part 2`` () = Day14.Part2() |> should equal 1_059
    
    [<Fact>]
    let ``Day 15 Part 1`` () = Day15.Part1() |> should equal 18_965_440
                
    [<Fact>]
    let ``Day 15 Part 2`` () = Day15.Part2() |> should equal 15_862_900
    
    [<Fact>]
    let ``Day 16 Part 1`` () = Day16.Part1() |> should equal 213
                
    [<Fact>]
    let ``Day 16 Part 2`` () = Day16.Part2() |> should equal 323
    
    [<Fact>]
    let ``Day 17 Part 1`` () = Day17.Part1() |> should equal 1_638
                
    [<Fact>]
    let ``Day 17 Part 2`` () = Day17.Part2() |> should equal 17
    
    [<Fact>]
    let ``Day 18 Part 1`` () = Day18.Part1() |> should equal 1_061
                
    [<Fact>]
    let ``Day 18 Part 2`` () = Day18.Part2() |> should equal 1_006
    
    [<Fact>]
    let ``Day 19 Part 1`` () = Day19.Part1() |> should equal 518
                
    [<Fact>]
    let ``Day 19 Part 2`` () = Day19.Part2() |> should equal 200
    
    [<Fact>]
    let ``Day 20 Part 1`` () = Day20.Part1() |> should equal 776_160
                
    [<Fact>]
    let ``Day 20 Part 2`` () = Day20.Part2() |> should equal 786_240
    
    [<Fact>]
    let ``Day 21 Part 1`` () = Day21.Part1() |> should equal 78
            
    [<Fact>]
    let ``Day 21 Part 2`` () = Day21.Part2() |> should equal 148
    
    [<Fact>]
    let ``Day 22 Part 1`` () = Day22.Part1() |> should equal 953
                
    [<Fact>]
    let ``Day 22 Part 2`` () = Day22.Part2() |> should equal 1_289
    
    [<Fact>]
    let ``Day 23 Part 1`` () = Day23.Part1() |> should equal 307
                
    [<Fact>]
    let ``Day 23 Part 2`` () = Day23.Part2() |> should equal 160
    
    [<Fact>]
    let ``Day 24 Part 1`` () = Day24.Part1() |> should equal 11_266_889_531L
                
    [<Fact>]
    let ``Day 24 Part 2`` () = Day24.Part2() |> should equal 77_387_711L
    
    [<Fact>]
    let ``Day 25 Part 1`` () = Day25.Part1() |> should equal 9_132_360I
                
    [<Fact>]
    let ``Day 25 Part 2`` () = Day25.Part2() |> should equal 1