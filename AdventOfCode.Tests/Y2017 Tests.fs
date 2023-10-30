namespace AdventOfCode.Tests.Y2017

open AdventOfCode.Days.Y2017
open AdventOfCode.Shared.Constants
open Xunit
open FsUnit.Xunit

module ``Y2017 Tests`` =

    [<Fact>]
    let ``Day 01 Part 1`` () = Day01.Part1() |> should equal 1_175

    [<Fact>]
    let ``Day 01 Part 2`` () = Day01.Part2() |> should equal 1_166

    [<Fact>]
    let ``Day 02 Part 1`` () = Day02.Part1() |> should equal 42_299

    [<Fact>]
    let ``Day 02 Part 2`` () = Day02.Part2() |> should equal 277

    [<Fact>]
    let ``Day 03 Part 1`` () = Day03.Part1() |> should equal 475
        
    [<Fact>]
    let ``Day 03 Part 2`` () = Day03.Part2() |> should equal 279_138
    
    [<Fact>]
    let ``Day 04 Part 1`` () = Day04.Part1() |> should equal 455
        
    [<Fact>]
    let ``Day 04 Part 2`` () = Day04.Part2() |> should equal 186
    
    [<Fact>]
    let ``Day 05 Part 1`` () = Day05.Part1() |> should equal 391_540
        
    [<Fact>]
    let ``Day 05 Part 2`` () = Day05.Part2() |> should equal 30_513_679
    
    [<Fact>]
    let ``Day 06 Part 1`` () = Day06.Part1() |> should equal 3_156
        
    [<Fact>]
    let ``Day 06 Part 2`` () = Day06.Part2() |> should equal 1_610
    
    [<Fact>]
    let ``Day 07 Part 1`` () = Day07.Part1() |> should equal "gynfwly"
            
    [<Fact>]
    let ``Day 07 Part 2`` () = Day07.Part2() |> should equal 1_526
    
    [<Fact>]
    let ``Day 08 Part 1`` () = Day08.Part1() |> should equal 5_215
                
    [<Fact>]
    let ``Day 08 Part 2`` () = Day08.Part2() |> should equal 6_419
    
    [<Fact>]
    let ``Day 09 Part 1`` () = Day09.Part1() |> should equal 14_212
                
    [<Fact>]
    let ``Day 09 Part 2`` () = Day09.Part2() |> should equal 6_569
    
    [<Fact>]
    let ``Day 10 Part 1`` () = Day10.Part1() |> should equal 5_577
                
    [<Fact>]
    let ``Day 10 Part 2`` () = Day10.Part2() |> should equal "44f4befb0f303c0bafd085f97741d51d"
    
    [<Fact>]
    let ``Day 11 Part 1`` () = Day11.Part1() |> should equal 773
                
    [<Fact>]
    let ``Day 11 Part 2`` () = Day11.Part2() |> should equal 1_560
    
    [<Fact>]
    let ``Day 12 Part 1`` () = Day12.Part1() |> should equal 113
                
    [<Fact>]
    let ``Day 12 Part 2`` () = Day12.Part2() |> should equal 202
    
    [<Fact>]
    let ``Day 13 Part 1`` () = Day13.Part1() |> should equal 1_900
                
    [<Fact>]
    let ``Day 13 Part 2`` () = Day13.Part2() |> should equal 3_966_414
    
    [<Fact>]
    let ``Day 14 Part 1`` () = Day14.Part1() |> should equal 8_222
                
    [<Fact>]
    let ``Day 14 Part 2`` () = Day14.Part2() |> should equal 1_086
    
    [<Fact>]
    let ``Day 15 Part 1`` () = Day15.Part1() |> should equal 600
                
    [<Fact>]
    let ``Day 15 Part 2`` () = Day15.Part2() |> should equal 313
    
    [<Fact>]
    let ``Day 16 Part 1`` () = Day16.Part1() |> should equal "nlciboghjmfdapek"
                
    [<Fact>]
    let ``Day 16 Part 2`` () = Day16.Part2() |> should equal "nlciboghmkedpfja"
    
    [<Fact>]
    let ``Day 17 Part 1`` () = Day17.Part1() |> should equal 600
                
    [<Fact>]
    let ``Day 17 Part 2`` () = Day17.Part2() |> should equal 31_220_910
    
    [<Fact>]
    let ``Day 18 Part 1`` () = Day18.Part1() |> should equal 1_187
                
    [<Fact>]
    let ``Day 18 Part 2`` () = Day18.Part2() |> should equal 5_969
    
    [<Fact>]
    let ``Day 19 Part 1`` () = Day19.Part1() |> should equal "XYFDJNRCQA"
                
    [<Fact>]
    let ``Day 19 Part 2`` () = Day19.Part2() |> should equal 17_450
    
    [<Fact>]
    let ``Day 20 Part 1`` () = Day20.Part1() |> should equal 157
                
    [<Fact>]
    let ``Day 20 Part 2`` () = Day20.Part2() |> should equal 499
    
    [<Fact>]
    let ``Day 21 Part 1`` () = Day21.Part1() |> should equal 144
            
    [<Fact>]
    let ``Day 21 Part 2`` () = Day21.Part2() |> should equal 2_169_301
    
    [<Fact>]
    let ``Day 22 Part 1`` () = Day22.Part1() |> should equal 5_565
                
    [<Fact>]
    let ``Day 22 Part 2`` () = Day22.Part2() |> should equal 2_511_978
    
    [<Fact>]
    let ``Day 23 Part 1`` () = Day23.Part1() |> should equal 3_969L
                
    [<Fact>]
    let ``Day 23 Part 2`` () = Day23.Part2() |> should equal 917
    
    [<Fact>]
    let ``Day 24 Part 1`` () = Day24.Part1() |> should equal 1_859
                
    [<Fact>]
    let ``Day 24 Part 2`` () = Day24.Part2() |> should equal 1_799
    
    [<Fact>]
    let ``Day 25 Part 1`` () = Day25.Part1() |> should equal 5_593
                
    [<Fact>]
    let ``Day 25 Part 2`` () = Day25.Part2() |> should equal GotAllTheStars