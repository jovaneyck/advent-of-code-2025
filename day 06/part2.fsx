#r "nuget: Unquote"
open Swensen.Unquote

let input =
    System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt"
    |> List.ofSeq

let example =
    """123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   +  """.Split("\n")
    |> List.ofSeq

let splitindices =
    let operators = input |> List.last
    [ for i in 0 .. operators.Length - 1 do
        if operators[i] <> ' ' then
            yield i ]

let rec split indices (input : char list list) =
    match indices with
    | [] -> [input]
    | i :: is ->
        let splits =
            input
            |> List.map (fun line -> line |> List.ofSeq |> List.splitAt i)
        let problem = splits |> List.map fst
        let rest = splits |> List.map snd
        let nis = is |> List.map (fun ii -> ii - i)
        problem :: (split nis rest)

let rawProblems = split splitindices (input |> List.map List.ofSeq) |> List.skip 1        

let solve problem =
    let operator = problem |> List.last |> List.head
    let operands = problem[0..(problem.Length - 2)] |> List.transpose |> List.map ((List.map string) >> (String.concat "") >> (fun nb -> nb.TrimEnd())) |> List.filter ((<>) "") |> List.map int64
    match operator with
    | '*' -> operands |> List.reduce (*)
    | '+' -> operands |> List.reduce (+)
    | c -> failwith $"Unexpected operator: {c}"

let solutions = rawProblems |> List.map solve |> List.sum

let run () =
    printf "Testing.."
    test <@ 1 + 1 = 2 @>
    printfn "...done!"

run ()