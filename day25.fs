module Day25

open System
open System.IO
open System.Text.RegularExpressions

open Extensions


let rec fit (key, lock) =
    match key, lock with
    | [], [] -> true
    | x :: xs, y :: ys when x &&& y = 0 -> fit (xs, ys)
    | _ -> false


let pin x =
    x
    |> List.map (fun c -> if c = '#' then '1' else '0')
    |> List.toArray
    |> String
    |> (fun s -> Convert.ToInt32(s, 2))


let toPins text =
    text
    |> String.split "\n"
    |> List.toGrid
    |> Grid.byColumn
    |> List.map (fun (_, c) -> c |> List.map snd)
    |> List.map pin


let parse text =
    let keys =
        seq {
            for m in Regex.Matches(text, @"[\.]{5}\n(?:[#\.]{5}\n?){6}") do
                yield m.Value
        }
        |> Seq.map toPins
        |> Seq.toList

    let locks =
        seq {
            for m in Regex.Matches(text, @"[#]{5}\n(?:[#\.]{5}\n?){6}") do
                yield m.Value
        }
        |> Seq.map toPins
        |> Seq.toList

    keys, locks


let run =
    printfn "== Day 25 =="

    let keys, locks = File.ReadAllText("inputs/day25.txt") |> parse
    let part1 = List.allPairs keys locks |> List.filter fit |> List.length

    printfn "Part 1: %d" part1
    printfn ""
