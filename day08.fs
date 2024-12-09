module Day08

open System.IO

open Extensions
open Maths

let add (a0, a1) (b0, b1) = a0 + b0, a1 + b1
let sub (a0, a1) (b0, b1) = a0 - b0, a1 - b1
let div (a, b) d = a / d, b / d
let negate (a, b) = -a, -b


let product list =
    list |> List.allPairs list |> List.filter (fun (a, b) -> a <> b)


let within rows columns (a, b) =
    not (a < 0 || a >= rows || b < 0 || b >= columns)


let findAntinodes rows columns antennae =
    antennae
    |> List.collect (fun (a, b) -> [ add a (sub a b); add b (sub b a) ])
    |> List.filter (within rows columns)


let part1 rows columns antennae =
    antennae |> List.collect (findAntinodes rows columns) |> set |> Set.count


let drawLine rows columns (a, b) =
    let horizontal r =
        List.arange 0 (columns - 1) |> List.map (fun c -> (r, c))

    let vertical c =
        List.arange 0 (rows - 1) |> List.map (fun r -> (r, c))

    let rec line d xs =
        match xs with
        | x :: _ when within rows columns x -> line d (add x d :: xs)
        | _ -> List.tail xs

    let diff = sub b a

    match diff with
    | 0, _ -> horizontal (fst a)
    | _, 0 -> vertical (snd a)
    | d0, d1 when d0 > d1 ->
        let gcd = gcd d0 d1
        let s = div diff gcd
        List.append (line s [ a ]) (line (negate s) [ a ])
    | d0, d1 when d0 < d1 ->
        let gcd = gcd d1 d0
        let s = div diff gcd
        List.append (line s [ a ]) (line (negate s) [ a ])
    | _ ->
        let s = (1, 1)
        List.append (line s [ a ]) (line (negate s) [ a ])


let part2 rows columns antennae =
    antennae
    |> List.collect (List.collect (drawLine rows columns))
    |> set
    |> Set.count


let run =
    printfn "== Day 08 =="

    let grid =
        File.ReadLines("inputs/day08.txt")
        |> Seq.toList
        |> List.toGrid
        |> Grid.filterOut (Set.singleton '.')

    let antennae =
        grid.Table
        |> Map.toList
        |> List.groupBy snd
        |> List.map (fun (_, xs) -> xs |> List.map fst)
        |> List.map product

    printfn "Part 1: %A" (part1 grid.Rows grid.Columns antennae)
    printfn "Part 2: %A" (part2 grid.Rows grid.Columns antennae)
    printfn ""
