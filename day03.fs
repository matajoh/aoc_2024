module Day03

open System.IO
open System.Text.RegularExpressions

type Instruction =
    | Mul of int * int
    | Enable
    | Disable

let parse program =
    seq {
        for m in Regex.Matches(program, @"mul\((\d\d?\d?),(\d\d?\d?)\)|do\(\)|don't\(\)") do
            yield
                match m.Groups.[0].Value with
                | "do()" -> Enable
                | "don't()" -> Disable
                | _ -> Mul(int m.Groups.[1].Value, int m.Groups.[2].Value)
    }
    |> Seq.toList

let part1 program =
    program
    |> List.sumBy (function
        | Mul(a, b) -> a * b
        | _ -> 0)


let rec execute enabled program =
    match program with
    | [] -> 0
    | Enable :: rest -> execute true rest
    | Disable :: rest -> execute false rest
    | Mul(a, b) :: rest when enabled -> a * b + execute enabled rest
    | _ :: rest -> execute enabled rest


let part2 program = execute true program

let run =
    printfn "== Day 03 =="

    let program = File.ReadAllText("inputs/day03.txt") |> parse

    printfn "Part 1: %d" (part1 program)
    printfn "Part 2: %d" (part2 program)
    printfn ""
