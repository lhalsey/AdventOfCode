namespace AdventOfCode.Days.Y2021

open AdventOfCode.Shared.Utility
open GeneticSharp.Domain
open GeneticSharp.Domain.Populations
open GeneticSharp.Domain.Chromosomes
open GeneticSharp.Domain.Fitnesses
open GeneticSharp.Domain.Selections
open GeneticSharp.Domain.Crossovers
open GeneticSharp.Domain.Mutations
open GeneticSharp.Domain.Terminations
open GeneticSharp.Domain.Randomizations

/// Day 24: Arithmetic Logic Unit
/// https://adventofcode.com/2021/day/24
/// Magic smoke starts leaking from the submarine's arithmetic logic unit (ALU).
module Day24 =

    type Result = string
    type Operand = string

    type Instruction =
        | Input of Result
        | Add of Result * Operand
        | Mul of Result * Operand
        | Div of Result * Operand
        | Mod of Result * Operand
        | Equal of Result * Operand

    let parse = function
        | Regex "inp (\w)" [r] -> Input r
        | Regex "add (\w) (.+)" [r; o] -> Add (r, o)
        | Regex "mul (\w) (.+)" [r; o] -> Mul (r, o)
        | Regex "div (\w) (.+)" [r; o] -> Div (r, o)
        | Regex "mod (\w) (.+)" [r; o] -> Mod (r, o)
        | Regex "eql (\w) (.+)" [r; o] -> Equal (r, o)
        | x -> failwithf "Invalid instruction: %s" x

    let parseInput() = getFile (2021, 24) |> readLinesAs parse |> Seq.toList

    let instructions = parseInput()

    let execute (instructions: Instruction list) (input: int64 list) =
        let rec executeR (instructions: Instruction list) (input: int64 list) (variables: Map<string, int64>) =

            let getValue (operand: string) =
                match operand with
                | Int64 x -> x
                | x -> variables.TryFind x |> Option.defaultValue 0L

            let executeInstruction (instruction: Instruction) =
                let apply (r, o) f = variables.Add(r, f (getValue r) (getValue o))

                match instruction with
                | Add (r, o) -> apply (r, o) (+)
                | Mul (r, o) -> apply (r, o) (*)
                | Div (r, o) -> apply (r, o) (/)
                | Mod (r, o) -> apply (r, o) (%)
                | Equal (r, o) -> apply (r, o) (fun r o -> if r = o then 1 else 0)
                | _ -> failwithf "Invalid instruction: %A" instruction
            
            match instructions, input with
            | [], _ -> variables.["z"]
            | (Input r)::xs, y::ys -> executeR xs ys (variables.Add (r, y))
            | (Input _)::_, [] -> failwith "Insufficient input"
            | x::xs, _ -> executeR xs input (executeInstruction x)

        executeR instructions input Map.empty



    type MyChromosome =
        inherit ChromosomeBase
        val s: unit
        new () = { inherit ChromosomeBase(14); s = base.CreateGenes() }

        override __.CreateNew() = new MyChromosome()
        override __.GenerateGene (geneIndex: int) =
            let v = RandomizationProvider.Current.GetInt(1, 9) |> int64
            new Gene(v)

    type MyFitness() =
        interface IFitness with
            member __.Evaluate (chromosome: IChromosome) =
                let input =
                    chromosome.GetGenes()
                    |> Array.map (fun x -> x.Value :?> int64)
                    |> Array.toList
                
                execute instructions input |> float |> (*) -1.0

    type MyMutation() =
        inherit MutationBase()

        override __.PerformMutate (chromosome: IChromosome, probability: float32) =
            if (RandomizationProvider.Current.GetDouble() <= 0.1)
            then 
                let index = RandomizationProvider.Current.GetInt(0, chromosome.Length)
                let currVal = chromosome.GetGene(index).Value :?> int64
                let mut = RandomizationProvider.Current.GetInt(-3, 3)
                let newVal = currVal + int64 mut |> min 9L |> max 1L
                chromosome.ReplaceGene(index, new Gene(int64 newVal))

    let Part1() =
        

        let selection = new EliteSelection()
        let crossover = new OnePointCrossover()
        let mutation = new MyMutation()
        let fitness = new MyFitness()
        let chromosome = new MyChromosome()
        let pop = new Population(500, 500, chromosome)
        let ga = new GeneticAlgorithm(pop, fitness, selection, crossover, mutation)
        ga.Termination <- new FitnessThresholdTermination(-10.0)

        ga.Start()

        0

    let Part2() =
        0