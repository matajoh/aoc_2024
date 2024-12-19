module Day19

open System.IO

open Algorithms
open Extensions
open System.Collections.Generic


let parse lines =
    let root =
        lines
        |> List.head
        |> String.split ", "
        |> List.map (Seq.toList)
        |> PrefixTree.build

    let patterns = lines |> List.skip 2

    root, patterns


let countArrangements root s =
    let cache = new Dictionary<string, bigint>()

    let rec f s =
        if cache.ContainsKey(s) then
            cache.[s]
        else if String.length s = 0 then
            cache.[s] <- bigint 1
            bigint 1
        else
            let count =
                PrefixTree.findPrefixes (s |> Seq.toList) root
                |> List.sumBy (fun p ->
                    let a = String.length p
                    let b = String.length s - 1
                    let sub = s[a..b]
                    f sub)

            cache.[s] <- count
            count

    f s


let part1 arrangements =
    arrangements |> List.map sign |> List.sum


let part2 arrangements = arrangements |> List.sum


let run =
    printfn "== Day 18 =="

    let arrangements =
        File.ReadLines("inputs/day19.txt")
        |> Seq.toList
        |> parse
        |> (fun (r, p) -> p |> List.map (countArrangements r))

    printfn "Part 1: %d" (part1 arrangements)
    printfn "Part 2: %A" (part2 arrangements)
    printfn ""
