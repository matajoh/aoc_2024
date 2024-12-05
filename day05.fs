module Day05

open System
open System.IO

let compare rules a b =
    if Set.contains (a, b) rules then -1
    elif Set.contains (b, a) rules then 1
    else 0

let splitAndConvert (delimiter: char) (s: String) =
    s.Split(delimiter) |> Array.map int |> Array.toList

let rec parseRules rules lines =
    match lines with
    | "" :: tail -> rules, tail
    | line :: tail ->
        let parts = splitAndConvert '|' line
        parseRules (Set.add (parts.[0], parts.[1]) rules) tail
    | [] -> rules, []

let toPages line = line |> splitAndConvert ','

let rec isAscending compare pages =
    match pages with
    | [] -> true
    | _ :: [] -> true
    | a :: b :: _ when compare a b > 0 -> false
    | _ :: tail -> isAscending compare (tail)

let part1 rules updates =
    updates
    |> List.filter (isAscending (compare rules))
    |> List.map (fun pages -> pages.[pages.Length / 2])
    |> List.sum

let part2 rules updates =
    let comparer = compare rules

    updates
    |> List.filter (fun pages -> not (isAscending comparer pages))
    |> List.map (List.sortWith comparer)
    |> List.map (fun pages -> pages.[pages.Length / 2])
    |> List.sum

let run =
    printfn "== Day 05 =="

    let rules, lines =
        File.ReadLines("inputs/day05.txt") |> Seq.toList |> (parseRules Set.empty)

    let updates = lines |> List.map toPages

    printfn "Part 1: %i" (part1 rules updates)
    printfn "Part 2: %i" (part2 rules updates)
    printfn ""
