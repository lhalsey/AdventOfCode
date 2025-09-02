namespace AdventOfCode.Days.Y2024

open AdventOfCode.Shared.Utility
open AdventOfCode.Shared

/// Day 14: Restroom Redoubt
/// https://adventofcode.com/2024/day/14
/// One of The Historians needs to use the bathroom; fortunately, you know there's a bathroom
/// near an unvisited location on their list, and so you're all quickly teleported directly to
/// the lobby of Easter Bunny Headquarters.
module Day14 =

    let [<Literal>] Columns = 101
    let [<Literal>] Rows = 103

    type Robot = { Location: Point2d; Vector: Point2d }

    // E.g. p=0,4 v=3,-3
    let parse = function
        | Regex "p=(-*\d+),(-*\d+) v=(-*\d+),(-*\d+)" [Int x; Int y; Int dx; Int dy] ->
            { Location = { X = x; Y = y }; Vector = { X = dx; Y = dy } }
        | x -> failwithf "Invalid input: %A" x

    let parseInput() = getFile (2024, 14) |> readLinesAs parse |> Seq.toList

    let getPositionAfter (seconds: int) (robot: Robot) =
        let pos = robot.Location + robot.Vector * seconds
        { X = remainder pos.X Columns; Y = remainder pos.Y Rows }

    let getQuadrant (point: Point2d) = 
        if point.X = (Columns / 2) || point.Y = (Rows / 2)
        then None
        else Some ((point.Y / ((Rows / 2) + 1)) * 2 + (point.X / ((Columns / 2) + 1)))

    let getThird (point: Point2d) = 
        if point.Y < 33 then 0
        else if point.Y > 65 then 2
        else 1

    let tryGetImage input (seconds: int) =
        
        let positions = input |> List.map (getPositionAfter seconds)

        let spread =
            positions
            |> List.map getThird
            |> List.countBy id
            |> List.map snd
            |> List.sortDescending

        let score = (spread[1] * 100) / positions.Length

        if score > 15 then None else
        positions
        |> List.map (fun pt -> (pt, '*'))
        |> readOnlyDict
        |> ImageProvider.getImage (function _ -> '█')
        |> (+) $"{seconds} - {score}\n"
        |> Some

    // Predict the motion of the robots in your list within a space which is 101 tiles wide and 103 tiles tall.
    // What will the safety factor be after exactly 100 seconds have elapsed?
    let Part1() =
        let positions = parseInput() |> List.map (getPositionAfter 100)

        let quadrantCount = positions |> List.choose getQuadrant |> List.countBy id

        quadrantCount |> List.map snd |> List.reduce (*)

    
    // What is the fewest number of seconds that must elapse for the robots to display the Easter egg?
    let Part2() =
        let input = parseInput()

        let images =
            Seq.initInfinite id
            |> Seq.choose (tryGetImage input)
            |> Seq.take 100
            |> String.concat "\n\n\n\n\n\n"

        7_338 // TODO: Automatically derive this!


//        █                                                                                         █         
//      █                              █       █                                                      
                                                                                                    
//                                                                                 █ █                
//               ███████████████████████████████                                                      
//               █                             █               █                                      
//               █                             █                                       █              
//               █                             █                          █                           
//█              █                             █                                         █            
//               █              █              █                                     █            █   
//               █             ███             █                                                      
// █             █            █████            █                       █                         █    
//               █           ███████           █                                                ██    
//               █          █████████          █                                              █       
//               █            █████            █          █                    █                     █
//             █ █           ███████           █                                                      
//               █          █████████          █                                                      
//               █         ███████████         █                                                      
//               █        █████████████        █             █             █       █                  
//               █          █████████          █   █                                                  
//               █         ███████████         █                                                      
//      █        █        █████████████        █                                         █            
//               █       ███████████████       █              ██                   █               █  
//               █      █████████████████      █                 █                                    
//               █        █████████████        █                                                      
//               █       ███████████████       █          █                                           
//               █      █████████████████      █              █                                       
//               █     ███████████████████     █                                                      
//               █    █████████████████████    █  █                                                   
//               █             ███             █                       █                              
//               █             ███             █                                                      
//             █ █             ███             █      █                      █                        
//         █     █                             █                                           █          
//               █                             █                      █                      █        
//               █                             █              █                                       
//               █                             █                                                  █   
//               ███████████████████████████████                                                      
//                                                                     █        █                     
                                                                                                    
                                                                                                    
//                                         █                                                          
                                                                                                    