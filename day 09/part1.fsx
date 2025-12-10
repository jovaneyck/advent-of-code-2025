#r "nuget: Unquote"
open Swensen.Unquote

let input =
    System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt"
    |> List.ofSeq

let example =
    """7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3""".Split("\n")
    |> Array.map (fun s -> s.Trim())
    |> List.ofSeq

let parse (input : string list) =
    input
    |> List.map (fun line ->
        let [|r;c|] = line.Split(',')
        (int64 r, int64 c))

let area ((r1 : int64,c1 : int64), (r2 : int64,c2 : int64)) : int64 =
    (1L + abs (r1 - r2)) * (1L + abs (c1 - c2))
    
let parsed = input |> parse
let pairs =
    [ for (i, p1) in List.indexed parsed do
          for p2 in parsed |> List.skip (i + 1) -> (p1,p2) ]
pairs |> List.map area |> List.max

let run () =
    printf "Testing.."
    test <@ area ((1,2),(1,3)) = 2 @>
    test <@ area ((1,2),(2,3)) = 4 @>
    printfn "...done!"

run ()