module Day22

open Maths
open System.IO
open System.Collections.Generic

let inline mix a b = a ^^^ b
let inline prune a = modulo a 16777216


let evolve s =
    let a = prune (mix s (s <<< 6))
    let b = prune (mix a (a >>> 5))
    prune (mix b (b <<< 11))


let generate (prices: Dictionary<int, int>) sum seed =
    let key a b c d =
        ((a + 10) <<< 15) ||| ((b + 10) <<< 10) ||| ((c + 10) <<< 5) ||| (d + 10)

    let ks = new HashSet<int>()

    let rec f n a b c d s =
        let s' = evolve s

        if n = 2000 then
            sum + (bigint s)
        else
            let p = s % 10
            let diff = (s' % 10) - p

            if n >= 4 then
                let k = key a b c d

                if ks.Add(k) then
                    prices.[k] <- prices.GetValueOrDefault(k, 0) + p

            f (n + 1) b c d diff s'

    f 0 0 0 0 0 seed


let analyze seeds =
    let prices = new Dictionary<int, int>()
    let sum = seeds |> List.fold (generate prices) (bigint 0)
    sum, prices.Values |> Seq.max


let run =
    printfn "== Day 22 =="

    let seeds = File.ReadLines("inputs/day22.txt") |> Seq.map int |> Seq.toList
    let part1, part2 = analyze seeds

    printfn "Part 1: %A" part1
    printfn "Part 2: %d" part2
    printfn ""
