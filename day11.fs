module Day11

open System
open System.IO

open Extensions
open System.Collections.Generic

let Powers10 = List.powrange 10UL 20

let split x =
    let s = List.binarySearch compare x Powers10
    let i = if s < 0 then ~~~s else s + 1

    if i % 2 = 0 then
        let p = Powers10.[i / 2]
        let l = x / p
        Some(l, x - l * p)
    else
        None


let change s =
    if s = 0UL then
        [ 1UL ]
    else
        match split s with
        | Some(l, r) -> [ l; r ]
        | None -> [ s * 2024UL ]


let rec blink count stones =
    if count = 0 then
        stones
    else
        blink (count - 1) (List.collect change stones)

let rec countStones (cache: Dictionary<uint64 * int, uint64>) count stone =
    let key = stone, count

    if cache.ContainsKey(key) then
        cache.[key]
    elif count = 0 then
        cache.[key] <- 1UL
        1UL
    else
        let n = blink 1 [ stone ] |> List.map (countStones cache (count - 1)) |> List.sum
        cache.[key] <- n
        n

let part1 cache stones =
    stones |> List.map (countStones cache 25) |> List.sum

let part2 cache stones =
    stones |> List.map (countStones cache 75) |> List.sum

let run =
    printfn "== Day 11 =="

    let stones =
        File.ReadAllText("inputs/day11.txt") |> String.split " " |> List.map uint64

    let cache = new Dictionary<uint64 * int, uint64>()

    printfn "Part 1: %d" (part1 cache stones)
    printfn "Part 2: %d" (part2 cache stones)
    printfn ""
