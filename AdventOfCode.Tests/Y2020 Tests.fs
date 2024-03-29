﻿namespace AdventOfCode.Tests.Y2020

open AdventOfCode.Days.Y2020
open AdventOfCode.Shared.Constants
open Xunit
open FsUnit.Xunit

module ``Y2020 Tests`` =

    [<Fact>]
    let ``Day 01 Part 1`` () = Day01.Part1() |> should equal 270_144

    [<Fact>]
    let ``Day 01 Part 2`` () = Day01.Part2() |> should equal 261_342_720

    [<Fact>]
    let ``Day 02 Part 1`` () = Day02.Part1() |> should equal 655

    [<Fact>]
    let ``Day 02 Part 2`` () = Day02.Part2() |> should equal 673

    [<Fact>]
    let ``Day 03 Part 1`` () = Day03.Part1() |> should equal 159L
        
    [<Fact>]
    let ``Day 03 Part 2`` () = Day03.Part2() |> should equal 6_419_669_520L
    
    [<Fact>]
    let ``Day 04 Part 1`` () = Day04.Part1() |> should equal 190
        
    [<Fact>]
    let ``Day 04 Part 2`` () = Day04.Part2() |> should equal 121
    
    [<Fact>]
    let ``Day 05 Part 1`` () = Day05.Part1() |> should equal 913
        
    [<Fact>]
    let ``Day 05 Part 2`` () = Day05.Part2() |> should equal 717
    
    [<Fact>]
    let ``Day 06 Part 1`` () = Day06.Part1() |> should equal 6_170
        
    [<Fact>]
    let ``Day 06 Part 2`` () = Day06.Part2() |> should equal 2_947
    
    [<Fact>]
    let ``Day 07 Part 1`` () = Day07.Part1() |> should equal 139
            
    [<Fact>]
    let ``Day 07 Part 2`` () = Day07.Part2() |> should equal 58_175
    
    [<Fact>]
    let ``Day 08 Part 1`` () = Day08.Part1() |> should equal 1_134
                
    [<Fact>]
    let ``Day 08 Part 2`` () = Day08.Part2() |> should equal 1_205
    
    [<Fact>]
    let ``Day 09 Part 1`` () = Day09.Part1() |> should equal 756_008_079L
                
    [<Fact>]
    let ``Day 09 Part 2`` () = Day09.Part2() |> should equal 9_372_7241L
    
    [<Fact>]
    let ``Day 10 Part 1`` () = Day10.Part1() |> should equal 2_664
                
    [<Fact>]
    let ``Day 10 Part 2`` () = Day10.Part2() |> should equal 148_098_383_347_712L
    
    [<Fact>]
    let ``Day 11 Part 1`` () = Day11.Part1() |> should equal 2_324
                
    [<Fact>]
    let ``Day 11 Part 2`` () = Day11.Part2() |> should equal 2_068
    
    [<Fact>]
    let ``Day 12 Part 1`` () = Day12.Part1() |> should equal 1_148
                
    [<Fact>]
    let ``Day 12 Part 2`` () = Day12.Part2() |> should equal 52_203
    
    [<Fact>]
    let ``Day 13 Part 1`` () = Day13.Part1() |> should equal 2_305L
                
    [<Fact>]
    let ``Day 13 Part 2`` () = Day13.Part2() |> should equal 552_612_234_243_498L
    
    [<Fact>]
    let ``Day 14 Part 1`` () = Day14.Part1() |> should equal 17_028_179_706_934L
                
    [<Fact>]
    let ``Day 14 Part 2`` () = Day14.Part2() |> should equal 3_683_236_147_222L
    
    [<Fact>]
    let ``Day 15 Part 1`` () = Day15.Part1() |> should equal 421
                
    [<Fact>]
    let ``Day 15 Part 2`` () = Day15.Part2() |> should equal 436
    
    [<Fact>]
    let ``Day 16 Part 1`` () = Day16.Part1() |> should equal 25_972
                
    [<Fact>]
    let ``Day 16 Part 2`` () = Day16.Part2() |> should equal 622_670_335_901L
    
    [<Fact>]
    let ``Day 17 Part 1`` () = Day17.Part1() |> should equal 322
                
    [<Fact>]
    let ``Day 17 Part 2`` () = Day17.Part2() |> should equal 2_000
    
    [<Fact>]
    let ``Day 18 Part 1`` () = Day18.Part1() |> should equal 131_076_645_626L
                
    [<Fact>]
    let ``Day 18 Part 2`` () = Day18.Part2() |> should equal 109_418_509_151_782L
    
    [<Fact>]
    let ``Day 19 Part 1`` () = Day19.Part1() |> should equal 171
                
    [<Fact>]
    let ``Day 19 Part 2`` () = Day19.Part2() |> should equal 369
    
    [<Fact>]
    let ``Day 20 Part 1`` () = Day20.Part1() |> should equal 66_020_135_789_767L
                
    [<Fact>]
    let ``Day 20 Part 2`` () = Day20.Part2() |> should equal 1_537
    
    [<Fact>]
    let ``Day 21 Part 1`` () = Day21.Part1() |> should equal 2_485
            
    [<Fact>]
    let ``Day 21 Part 2`` () = Day21.Part2() |> should equal "bqkndvb,zmb,bmrmhm,snhrpv,vflms,bqtvr,qzkjrtl,rkkrx"
    
    [<Fact>]
    let ``Day 22 Part 1`` () = Day22.Part1() |> should equal 31_957
                
    [<Fact>]
    let ``Day 22 Part 2`` () = Day22.Part2() |> should equal 33_212
    
    [<Fact>]
    let ``Day 23 Part 1`` () = Day23.Part1() |> should equal 68_245_739
                
    [<Fact>]
    let ``Day 23 Part 2`` () = Day23.Part2() |> should equal 219_634_632_000L
    
    [<Fact>]
    let ``Day 24 Part 1`` () = Day24.Part1() |> should equal 455
                
    [<Fact>]
    let ``Day 24 Part 2`` () = Day24.Part2() |> should equal 3_904
    
    [<Fact>]
    let ``Day 25 Part 1`` () = Day25.Part1() |> should equal 3_286_137L
                
    [<Fact>]
    let ``Day 25 Part 2`` () = Day25.Part2() |> should equal GotAllTheStars