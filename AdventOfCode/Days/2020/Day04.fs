namespace AdventOfCode.Days.Y2020

open AdventOfCode.Shared.Utility
open System
open System.Collections.Generic

/// Day 4: Passport Processing
/// https://adventofcode.com/2020/day/4
/// You arrive at the airport only to realize that you grabbed your North Pole Credentials instead of your passport
module Day04 =

    // Not really sure the refactor has improved things - it's quite verbose and there are still Regex
    // expressions - but I had a nice time!
    type Field = BirthYear | IssueYear | ExpirationYear | Height | HairColor | EyeColor | PassportId | CountryId

    type Passport = IReadOnlyDictionary<Field, string>

    let [<Literal>] Inches = "in"
    let [<Literal>] Centimetres = "cm"

    let getFieldType = function
        | "byr" -> BirthYear
        | "iyr" -> IssueYear
        | "eyr" -> ExpirationYear
        | "hgt" -> Height
        | "hcl" -> HairColor
        | "ecl" -> EyeColor
        | "pid" -> PassportId
        | "cid" -> CountryId
        | x -> failwithf "Invalid field: %s" x

    let RequiredFields = [ BirthYear; IssueYear; ExpirationYear; Height; HairColor; EyeColor; PassportId ]

    let EyeColors = set [ "amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth" ]

    // E.g. "iyr:1928 cid:150 pid:476113241 eyr:2039 hcl:a5ac0f ecl:#25f8d2 byr:2027 hgt:190"
    let parse =
        splitAny " \r\n"
        >> Array.map (splitIntoPair ":")
        >> Array.map (fun (k, v) -> getFieldType k, v)
        >> readOnlyDict

    let parseInput() =
        getFile (2020, 4)
        |> readAllText
        |> splitOn $"{Environment.NewLine}{Environment.NewLine}"
        |> Array.map parse

    let (|IsOneOf|_|) choices x = if choices |> Set.contains x then Some IsOneOf else None

    let (|InRange|_|) min max = tryParseAsInt >> Option.filter ((>=<) min max) >> Option.map (fun _ -> InRange)

    let (|In|_|) unit = function Regex (sprintf "(\d+)%s" unit) [ amount ] -> Some amount | _ -> None

    let (|HasDigits|_|) n = function Regex (sprintf "^\d{%i}$" n) _ -> Some HasDigits | _ -> None

    let (|Hex|_|) = function Regex "^#[0-9a-f]{6}$" _ -> Some Hex | _ -> None

    let isValidField = function
        | BirthYear, InRange 1920 2002
        | IssueYear, InRange 2010 2020
        | ExpirationYear, InRange 2020 2030
        | Height, In Centimetres (InRange 150 193)
        | Height, In Inches (InRange 59 76)
        | HairColor, Hex 
        | EyeColor, IsOneOf EyeColors
        | PassportId, HasDigits 9
        | CountryId, _ -> true
        | _ -> false

    let hasAllRequiredFields (passport: Passport) = RequiredFields |> List.forall passport.ContainsKey

    let isValid (passport: Passport)  =
        hasAllRequiredFields passport &&
        passport |> Seq.forall (fun x -> isValidField (x.Key, x.Value))


    // Count the number of valid passports - those that have all required fields. Treat cid as optional.
    // In your batch file, how many passports are valid?
    let Part1() = parseInput() |> parallelCountIf hasAllRequiredFields

    // Count the number of valid passports - those that have all required fields and valid values.
    // Continue to treat cid as optional. In your batch file, how many passports are valid?
    let Part2() = parseInput() |> parallelCountIf isValid