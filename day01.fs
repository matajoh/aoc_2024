module Day01

open System
open System.IO

let part1 (values : string seq) =
    values
    |> Seq.map to_digits
    |> Seq.map calibration_value
    |> Seq.sum 


let part2 (values : string list) =
    values 
    |> Seq.map (replaceText 0)
    |> part1

let run =
    printfn "== Day 01 =="
    
    let values = 
        File.ReadLines("inputs/day01.txt")
        |> Seq.toList
    
    printfn "Part 1: %i" (part1 values)
    printfn "Part 2: %i" (part2 values)
    printfn ""
