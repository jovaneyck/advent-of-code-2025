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

type Point = int64 * int64
module Point =
    let make (r:int64) (c:int64) : Point = (r,c)
    
type Line = Point * Point
module Line =
    let inline make (p1:Point) (p2:Point) : Line = (p1,p2)
    let points ((r1,c1) : Point, (r2,c2) : Point) : Point array =
        if r1 = r2 then
            // horizontal line
            [| for c in min c1 c2 .. max c1 c2 -> (r1,c) |]
        else
            // vertical line
            [| for r in min r1 r2 .. max r1 r2 -> (r,c1) |]
    let inline contains ((r,c) : Point) (((r1,c1), (r2,c2)) : Line) : bool =
        if r1 = r2 then
            r = r1 && c >= min c1 c2 && c <= max c1 c2
        else
            c = c1 && r >= min r1 r2 && r <= max r1 r2
            
    (*Checks for intersection between a horizontal and vertical line*)
    let inline intersects (horline : Line) (vertline : Line) : bool =
        let ((vr1,vc1),(vr2,_)) = vertline
        let ((hr1,hc1),(_,hc2)) = horline
        vc1 >= min hc1 hc2 && vc1 <= max hc1 hc2 &&
        hr1 >= min vr1 vr2 && hr1 <= max vr1 vr2

module Rectangle =
    let inline area ((r1 : int64,c1 : int64), (r2 : int64,c2 : int64)) : int64 =
        (1L + abs (r1 - r2)) * (1L + abs (c1 - c2))
    let enumerate ((r1,c1) : Point, (r2,c2) : Point) : Point list =
        let r1, r2 = min r1 r2, max r1 r2
        let c1, c2 = min c1 c2, max c1 c2
        
        let top = Line.points ((r1,c1),(r1,c2))
        let bottom = Line.points ((r2,c1),(r2,c2))
        let left = Line.points ((r1+1L,c1),(r2-1L,c1))
        let right = Line.points ((r1+1L,c2),(r2-1L,c2))
        Array.concat [| top; right; Array.rev bottom; Array.rev left |] |> Array.toList
    
    // Lazy sequence enumeration for early termination
    let enumerateSeq ((r1,c1) : Point, (r2,c2) : Point) : Point seq =
        let r1, r2 = min r1 r2, max r1 r2
        let c1, c2 = min c1 c2, max c1 c2
        seq {
            // top edge
            for c in c1 .. c2 do yield (r1, c)
            // right edge (excluding corners)
            for r in r1+1L .. r2-1L do yield (r, c2)
            // bottom edge (reversed)
            for c in c2 .. -1L .. c1 do yield (r2, c)
            // left edge (reversed, excluding corners)
            for r in r2-1L .. -1L .. r1+1L do yield (r, c1)
        }
        
module Polygon =
    // Fast raycast using pre-computed verticals array
    let inline raycastFast (verticals : Line array) ((r,c) : Point) : bool =
        //Ray-casting algorithm: count intersections with a ray emitting from p to the left
        //Even: outside, Odd: inside
        let ray = Line.make (r,c) (r, System.Int64.MinValue)
        let mutable nbCrossings = 0
        for i = 0 to verticals.Length - 1 do
            if Line.intersects ray verticals.[i] then
                nbCrossings <- nbCrossings + 1
        nbCrossings % 2 <> 0
    
    let inline onPerimeterFast (poly : Line array) (p : Point) : bool =
        let mutable found = false
        let mutable i = 0
        while not found && i < poly.Length do
            if Line.contains p poly.[i] then
                found <- true
            i <- i + 1
        found
        
    let inline containsFast (poly : Line array) (verticals : Line array) (p : Point) : bool =
        onPerimeterFast poly p || raycastFast verticals p
    
    // Keep original for backwards compatibility with tests
    let raycast (poly : Line list) ((r,c) : Point) : bool =
        let verticals =
            poly
            |> List.filter (fun ((_,c1),(_,c2)) -> c1 = c2)
            |> Array.ofList
        raycastFast verticals (r,c)
        
    let onPerimeter (poly : Line list) (p : Point) : bool =
        poly |> List.exists (Line.contains p)
        
    let contains (poly : Line list) (p : Point) : bool =
        onPerimeter poly p || raycast poly p
        
let parse (input : string list) : Point list=
    input
    |> List.map (fun line ->
        let [|r;c|] = line.Split(',')
        (int64 r, int64 c))

//Outline: loop over all pairs and check if the rectangle is within the spanning polygon
// - For each pair, enumerate all pixels on the perimeter
// - For each pixel, check if:
//   - it is on the perimeter of the polygon
//   - inside/outside the polygon (ray-casting using the vertical lines)
let solve input =
    let parsed = input |> parse |> Array.ofList
    let pairs =
        [| for i = 0 to parsed.Length - 1 do
              for j = i + 1 to parsed.Length - 1 do
                  yield (parsed.[i], parsed.[j]) |]
    
    // Use arrays for better performance
    let polygon =
        parsed 
        |> Array.zip (Array.append (Array.tail parsed) [|parsed.[0]|])
        |> Array.map (fun (a,b) -> Line.make a b)
    
    // Pre-compute verticals ONCE instead of on every raycast call
    let verticals = 
        polygon 
        |> Array.filter (fun ((_,c1),(_,c2)) -> c1 = c2)
    
    // Use parallel processing for better performance on multi-core
    pairs
    |> Array.mapi (fun i (p1,p2) ->
        if(i % 100 = 0) then
            printfn $"Processing rectangle %d{i+1}/%d{pairs.Length}"
        
        let rect = (p1,p2)
        
        // Use lazy Seq enumeration for early termination
        let allInside =
            Rectangle.enumerateSeq rect
            |> Seq.forall (Polygon.containsFast polygon verticals)

        if allInside then
            Rectangle.area rect
        else
            0L)
    |> Array.max

let run () =
    printf "Testing.."
    test <@ Rectangle.area ((1,2),(1,3)) = 2 @>
    test <@ Rectangle.area ((1,2),(2,3)) = 4 @>
    
    test <@ Rectangle.enumerate (((1L,1L),(3L,3L))) = [(1L, 1L); (1L, 2L); (1L, 3L); (2L, 3L); (3L, 3L); (3L, 2L); (3L, 1L); (2L, 1L)] @>
    test <@ Rectangle.enumerate (((3L,3L),(1L,1L))) = [(1L, 1L); (1L, 2L); (1L, 3L); (2L, 3L); (3L, 3L); (3L, 2L); (3L, 1L); (2L, 1L)] @>
    
    test <@ Line.points ((1L,1L),(1L,2L)) = [| (1L, 1L); (1L, 2L) |] @>
    test <@ Line.points ((1L,1L),(2L,1L)) = [| (1L, 1L); (2L, 1L) |] @>
    
    test <@ Line.contains (1L,2L) (Line.make (1L,1L) (1L,3L)) @>
    test <@ not <| Line.contains (1L,0L) (Line.make (1L,1L) (1L,3L)) @>
    test <@ not <| Line.contains (1L,4L) (Line.make (1L,1L) (1L,3L)) @>
    test <@ Line.contains (2L,1L) (Line.make (1L,1L) (3L,1L)) @>
    test <@ not <| Line.contains (0L,1L) (Line.make (1L,1L) (3L,1L)) @>
    test <@ not <| Line.contains (4L,1L) (Line.make (1L,1L) (3L,1L)) @>
    
    test <@ Line.intersects (Line.make (2,1) (2,10)) (Line.make (1,2) (10,2))  @>
    test <@ Line.intersects (Line.make (2,1) (2,10)) (Line.make (1,3) (10,3))  @>
    
    test <@ solve example = 24L @>
    
    printfn "...done!"

run ()

#time
//Real: 00:17:53.450, CPU: 00:17:37.875, GC gen0: 9194, gen1: 5, gen2: 4

solve input //192570426