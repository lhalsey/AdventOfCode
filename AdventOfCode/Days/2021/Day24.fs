namespace AdventOfCode.Days.Y2021

open AdventOfCode.Shared.Utility

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
            | [], _ -> variables
            | (Input r)::xs, y::ys -> executeR xs ys (variables.Add (r, y))
            | (Input _)::_, [] -> failwith "Insufficient input"
            | x::xs, _ -> executeR xs input (executeInstruction x)


        executeR instructions input Map.empty

    let Part1() =
        let instructions = parseInput()
        let input = List.replicate 14 0L

        //let getNext (digits: int64 list) =
            

        let result = execute instructions input

        0

    let Part2() =
        0