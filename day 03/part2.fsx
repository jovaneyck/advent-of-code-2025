#r "nuget: Unquote"
open Swensen.Unquote

let input =
    System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt"
    |> List.ofSeq

let example =
    """987654321111111
811111111111119
234234234234278
818181911112111""".Split("\n")
    |> Array.map (fun s -> s.Trim())
    |> List.ofSeq

let parseBank (line: string) =
    line
    |> Seq.map (string >> int)
    |> Seq.toList

let rec batteries nb  (bank : int list) : int list list =
    if nb = 0 then [[]]
    else
        [for (i, battery) in bank |> List.indexed do
            let rest = bank |> List.skip (i + 1)
            (batteries (nb - 1) rest) |> List.map (fun b -> battery :: b)]
        |> List.collect id
 
let joltage selection =
    selection
    |> List.map string
    |> String.concat ""
    |> int64
        
let banks = input |> List.map parseBank
banks |> List.map (fun bank ->
    printfn $"Starting on bank {bank}"
    let selections = batteries 12 bank |> List.distinct     
    selections |> List.map joltage |> List.max)
|> List.sum

let run () =
    printf "Testing.."
    test <@ 1 + 1 = 2 @>
    printfn "...done!"

run ()