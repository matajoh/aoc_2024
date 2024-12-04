module Day04

open System.IO
open Extensions

type Move =
    | West
    | NorthWest
    | North
    | NorthEast
    | East
    | SouthEast
    | South
    | SouthWest

    static member All = [ West; NorthWest; North; NorthEast; East; SouthEast; South; SouthWest ]

    static member diagonal dir =
        match dir with
        | NorthWest -> true
        | NorthEast -> true
        | SouthEast -> true
        | SouthWest -> true
        | _ -> false

    static member next (r, c) dir =
        match dir with
        | West -> (r, c - 1)
        | NorthWest -> (r - 1, c - 1)
        | North -> (r - 1, c)
        | NorthEast -> (r - 1, c + 1)
        | East -> (r, c + 1)
        | SouthEast -> (r + 1, c + 1)
        | South -> (r + 1, c)
        | SouthWest -> (r + 1, c - 1)

let rec follow word grid index dir =
    match Array2D.tryGet index grid, word with
    | Some(y), x :: [] when x = y -> true
    | Some(y), x :: xs when x = y -> follow xs grid (Move.next index dir) dir
    | _ -> false

let search word grid index =
    Move.All |> List.filter (follow word grid index)

let part1 grid =
    let word = Array.toList ("XMAS".ToCharArray())

    Array2D.rowColumnIndices grid
    |> Seq.toList
    |> List.map (search word grid)
    |> List.map List.length
    |> List.sum

let findA word grid index =
    search word grid index
    |> List.filter Move.diagonal
    |> List.map (Move.next index)

let part2 grid =
    let word = Array.toList ("MAS".ToCharArray())

    Array2D.rowColumnIndices grid
    |> Seq.toList
    |> List.collect (findA word grid)
    |> List.countBy id
    |> List.filter (fun (_, x) -> x = 2)
    |> List.length

let run =
    printfn "== Day 04 =="

    let grid = File.ReadAllLines("inputs/day04.txt") |> array2D

    printfn "Part 1: %d" (part1 grid)
    printfn "Part 2: %d" (part2 grid)
