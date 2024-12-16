module Day08

open System.IO

open Extensions
open Maths


let product list =
    list |> List.allPairs list |> List.filter (fun (a, b) -> a <> b)


let findAntinodes grid antennae =
    antennae
    |> List.collect (fun (a, b) -> [ a + (a - b); b + (b - a) ])
    |> List.filter (Grid.isInside grid)


let part1 grid antennae =
    antennae |> List.collect (findAntinodes grid) |> set |> Set.count


let drawLine grid (a, b) =
    let horizontal r =
        List.arange 0 (grid.Columns - 1) |> List.map (fun c -> { Row = r; Column = c })

    let vertical c =
        List.arange 0 (grid.Rows - 1) |> List.map (fun r -> { Row = r; Column = c })

    let rec line d xs =
        match xs with
        | x :: _ when Grid.isInside grid x -> line d (x + d :: xs)
        | _ -> List.tail xs

    let diff = b - a

    match diff with
    | { Row = 0 } -> horizontal a.Row
    | { Column = 0 } -> vertical a.Column
    | { Row = d0; Column = d1 } when d0 > d1 ->
        let gcd = gcd d0 d1
        let s = diff / gcd
        List.append (line s [ a ]) (line (-s) [ a ])
    | { Row = d0; Column = d1 } when d0 < d1 ->
        let gcd = gcd d1 d0
        let s = diff / gcd
        List.append (line s [ a ]) (line (-s) [ a ])
    | _ ->
        let s = { Row = 1; Column = 1 }
        List.append (line s [ a ]) (line (-s) [ a ])


let part2 grid antennae =
    antennae |> List.collect (List.collect (drawLine grid)) |> set |> Set.count


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

    printfn "Part 1: %d" (part1 grid antennae)
    printfn "Part 2: %d" (part2 grid antennae)
    printfn ""
