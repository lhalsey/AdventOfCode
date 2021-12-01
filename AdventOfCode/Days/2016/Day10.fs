namespace AdventOfCode.Days.Y2016

open AdventOfCode.Shared.Utility

/// Day 10: Balance Bots
/// https://adventofcode.com/2016/day/10
/// You come upon a factory in which many robots are zooming around handing small microchips to each other.
module Day10 =

    type BotId = int
    type OutputId = int
    type Token = int

    type Recipient = Bot of BotId | Output of OutputId

    type Instruction =
        | Input of Token * BotId
        | Delivery of BotId * Recipient * Recipient

    type Bot = 
        { Id: int; Tokens: Token list; LowRecipient: Recipient; HighRecipient: Recipient } with
          member __.LowAndHigh =
            match __.Tokens with
            | t1::t2::[] -> if t1 < t2 then Some (t1, t2) else Some (t2, t1)
            | _ -> None

    type State = { BotMap: Map<int, Bot>; Output: Map<OutputId, Token> }

    let getRecipient (r: string) = 
        match split ' ' r with
        | [| "bot"; Int botId |] -> Bot botId
        | [| "output"; Int outputId |] -> Output outputId
        | x -> failwithf "Invalid recipient: %A" x

    let parse = function
        | Regex "value (\d+) goes to bot (\d+)" [Int token; Int botId] -> Input (token, botId)
        | Regex "bot (\d+) gives low to (\w+ \d+) and high to (\w+ \d+)" [Int botId; low; high] ->
            Delivery (botId, getRecipient low, getRecipient high)
        | x -> failwithf "Invalid input: %s" x

    let parseInput() = getFile (2016, 10) |> readLinesAs parse

    let getBots (instructions: Instruction list) =
        let (inputs, deliveries) = instructions |> List.partition (function Input _ -> true | _ -> false)

        let getBot (instruction: Instruction) =
            match instruction with
            | Delivery (botId, lowRecipient, highRecipient) -> 
                { Id = botId; Tokens = []; LowRecipient = lowRecipient; HighRecipient = highRecipient }
            | _ -> failwith "Not a delivery instruction"

        let addInput (botMap: Map<int, Bot>) (instruction: Instruction) =
            match instruction with
            | Input (token, botId) ->
                let bot = botMap.[botId]
                botMap.Add (botId, { bot with Tokens = token::bot.Tokens })
            | _ -> failwith "Not an input instruction"

        let botMap =
            deliveries
            |> List.map getBot
            |> List.map (fun bot -> bot.Id, bot)
            |> Map

        (botMap, inputs) ||> List.fold addInput

    let distributeTokenIfReady (state: State) (bot: Bot) =
        let update (recipient: Recipient) (token: Token) (state: State)  = 
            match recipient with
            | Output outputId -> { state with Output = state.Output.Add(outputId, token) }
            | Bot botId ->
                let bot = state.BotMap.[botId]
                let bot = { bot with Tokens = token::bot.Tokens }
                { state with BotMap = state.BotMap.Add (bot.Id, bot) }

        match bot.LowAndHigh with
        | None -> state
        | Some (low, high) ->
            state
            |> update bot.LowRecipient low
            |> update bot.HighRecipient high
            |> fun state -> { state with BotMap = state.BotMap.Remove bot.Id }

    let distributeTokens (state: State) =
        let bots = state.BotMap |> Seq.map (fun x -> x.Value)

        (state, bots) ||> Seq.fold distributeTokenIfReady

    let simulate() =
        let input = parseInput() |> Seq.toList
        let botMap = getBots input

        { BotMap = botMap; Output = Map.empty }
        |> Seq.unfold (fun x -> Some(x, distributeTokens x))


    // Based on your instructions, what is the number of the bot that is responsible for comparing
    // value-61 microchips with value-17 microchips?
    let Part1() =
        let tryFindBot low high (state: State) =
            state.BotMap
            |> Seq.map (fun x -> x.Value)
            |> Seq.tryFind (fun bot -> bot.LowAndHigh = Some (low, high))

        simulate()
        |> Seq.pick (tryFindBot 17 61)
        |> fun x -> x.Id

    // What do you get if you multiply together the values of one chip in each of outputs 0, 1, and 2?
    let Part2() =
        let tryFindOutputs keys (state: State) =
            keys
            |> List.choose state.Output.TryFind
            |> function output when output.Length = keys.Length -> Some output | _ -> None

        simulate()
        |> Seq.pick (tryFindOutputs [0; 1; 2])
        |> List.reduce (*)