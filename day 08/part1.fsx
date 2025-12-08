#r "nuget: Unquote"
open Swensen.Unquote

let input =
    System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt"
    |> List.ofSeq

let example =
    """162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689""".Split("\n")
    |> Array.map (fun s -> s.Trim())
    |> List.ofSeq

type JunctionBox = int64 * int64 * int64
type Circuit = Set<JunctionBox>
type State = Circuit list

let parse (input : string list) : JunctionBox list =
    input
    |> List.map (fun line ->
        let parts = line.Split(',')
        (int64 parts[0], int64 parts[1], int64 parts[2])
    )
    
let allPairs xes =
    [for (i, j1) in List.indexed xes do
        for j2 in xes |> List.skip (i + 1) do
            j1,j2
    ]
 
let euclideanDistance (one : JunctionBox) (other : JunctionBox)=
    let (x1,y1,z1) = one  
    let (x2,y2,z2) = other
    let dx = x2 - x1
    let dy = y2 - y1
    let dz = z2 - z1
    sqrt (float (dx * dx + dy * dy + dz * dz))
 
let calculateDistances (boxes : JunctionBox list) =
    boxes
    |> allPairs
    |> List.map (fun (b1, b2) -> (b1,b2), euclideanDistance b1 b2)
    |> List.sortBy snd

let rec connect nb (state : State) lookup =
    if nb = 0 then
        state
    else
        let next :: rest = lookup
        let (b1, b2), dist = next
        let c1 = state |> List.find (fun c -> c.Contains b1)
        let c2 = state |> List.find (fun c -> c.Contains b2)
        let combined = c1 |> Set.union c2
        connect (nb - 1) (state |> List.except [c1;c2] |> List.append [combined]) rest

let junctionBoxes = parse input
let lookup = calculateDistances junctionBoxes
let initial : State = 
    junctionBoxes
    |> List.map Set.singleton
    
let connected = connect 1_000 initial lookup |> List.sortByDescending Set.count

connected
|> Seq.length

connected[0..2]
|> List.map _.Count
|> List.reduce (*) //24360

let run () =
    printf "Testing.."
    test <@ 1 + 1 = 2 @>
    printfn "...done!"

run ()