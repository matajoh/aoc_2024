module Day01

open System
open System.IO

let splitAndConvert (s: String) =
    let parts = s.Split("   ")
    parts[0] |> int, parts[1] |> int

let part1 lhs rhs =
    List.zip (List.sort lhs) (List.sort rhs)
    |> List.map (fun (a, b) -> abs a - b)
    |> List.sum

let countInstances counts x =
    match Map.tryFind x counts with
    | Some(count) -> Map.add x (count + 1) counts
    | None -> Map.add x 1 counts

let similarityScore counts x =
    match Map.tryFind x counts with
    | Some(count) -> x * count
    | None -> 0

let part2 lhs rhs =
    let counts = List.fold countInstances Map.empty rhs
    lhs |> List.map (similarityScore counts) |> List.sum

let run =
    printfn "== Day 01 =="

    let lhs, rhs =
        File.ReadLines("inputs/day01.txt")
        |> Seq.map splitAndConvert
        |> Seq.toList
        |> List.unzip

    printfn "Part 1: %i" (part1 lhs rhs)
    printfn "Part 2: %i" (part2 lhs rhs)
    printfn ""
