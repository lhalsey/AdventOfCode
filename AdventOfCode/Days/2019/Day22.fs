namespace AdventOfCode.Days.Y2019

open System
open AdventOfCode.Shared.Utility

/// Day 22: Slam Shuffle
/// https://adventofcode.com/2019/day/22
/// There isn't much to do while you wait for the droids to repair your ship.
module Day22 =

    type Shuffle = DealWithIncrement of int | Cut of int | DealIntoNewStack

    // Custom 2 x 2 matrix of bigints
    type BigIntMatrix = { A: bigint; B: bigint; C: bigint; D: bigint } with
        static member create (a, b) = { A = a; B = b; C = 0I; D = 1I }

        static member Identity = { A = 1I; B = 0I; C = 0I; D = 1I }

        static member combine numCards (m1: BigIntMatrix) (m2: BigIntMatrix) =
            {
                A = bigint.Remainder(m1.A * m2.A + m1.B * m2.C, numCards)
                B = bigint.Remainder(m1.A * m2.B + m1.B * m2.D, numCards)
                C = bigint.Remainder(m1.C * m2.A + m1.D * m2.C, numCards)
                D = bigint.Remainder(m1.C * m2.B + m1.D * m2.D, numCards)
            }

        // https://simple.wikipedia.org/wiki/Exponentiation_by_squaring
        static member pow numCards (m: BigIntMatrix) (exp: int64) =
            let rec powR (m: BigIntMatrix) (exp: int64) =
                match exp with
                | 0L -> BigIntMatrix.Identity
                | _ ->
                    let m2 = powR m (exp / 2L)
                    let m2 = BigIntMatrix.combine numCards m2 m2

                    if exp % 2L = 0L then m2 else BigIntMatrix.combine numCards m m2

            powR m exp          

    let dealWithIncrement n numCards index = (index * n) % numCards

    let cut n numCards index = (index - n + numCards) % numCards

    let dealIntoNewStack numCards index = numCards - 1 - index

    // // Can use Fermat's Little Theorem as NumCards is prime
    let inverseMod n numCards = bigint.ModPow(n, numCards - 2I, numCards)

    let getShuffle numCards = function
        | DealWithIncrement n -> dealWithIncrement n numCards
        | Cut n -> cut n numCards
        | DealIntoNewStack -> dealIntoNewStack numCards

    // TODO: Explain this black magic!
    let getInverseShuffle numCards = function
        | DealWithIncrement n -> BigIntMatrix.create((inverseMod (bigint n) numCards), 0I)
        | Cut n -> BigIntMatrix.create(1I, (bigint n))
        | DealIntoNewStack -> BigIntMatrix.create(-1I, -1I)

    let parse = function
        | Regex "deal with increment ([\d]+)" [n] -> DealWithIncrement (int n)
        | Regex "cut (-?[\d]+)" [n] -> Cut (int n)
        | "deal into new stack" -> DealIntoNewStack
        | x -> failwithf "Invalid instruction: %s" x

    let parseInput() = getFile (2019, 22) |> readLinesAs parse

    // After shuffling your factory order deck of 10007 cards, what is the position of card 2019?
    let Part1() =
        let NumCards = 10_007
        let StartIndex = 2_019

        let shuffles = parseInput() |> Seq.map (getShuffle NumCards)

        (StartIndex, shuffles)
        ||> Seq.fold (fun acc f -> f acc)

    // When you get back, you discover that the 3D printers have combined their power to create for
    // you a single, giant, brand new, factory order deck of 119,315,717,514,047 space cards.
    // You decide to apply your complete shuffle process (your puzzle input) to the deck
    // 101,741,582,076,661 times in a row.
    // After shuffling your new, giant, factory order deck that many times, what number is on the
    // card that ends up in position 2020?
    let Part2() =
        let NumShuffles = 101_741_582_076_661L
        let NumCards = 119_315_717_514_047I
        let EndIndex = 2_020I

        let shuffles = parseInput() |> Seq.map (getInverseShuffle NumCards)

        // Combine 100 shuffles from input into a single shuffle
        let singleShuffle = shuffles |> Seq.reduce (BigIntMatrix.combine NumCards)

        // Shuffle a "bajillion" times using exponentiation by squaring
        let multiShuffle = BigIntMatrix.pow NumCards singleShuffle NumShuffles

        (multiShuffle.A * EndIndex + multiShuffle.B) % NumCards