module Day07

open System.IO

open Extensions
open System.Numerics

let concat a b =
    ((string a) + (string b)) |> BigInteger.Parse

let apply operators a b ys =
    operators |> List.map (fun o -> (o a b), ys)

let rec findSolution (next: 'a -> 'a -> 'a list -> ('a * 'a list) list) current target =
    match current with
    | (x, []) :: _ when x = target -> true
    | (x, []) :: xs when x <> target -> findSolution next xs target
    | (x, _) :: xs when x > target -> findSolution next xs target
    | (x, y :: ys) :: xs -> findSolution next (List.append (next x y ys) xs) target
    | [] -> false
    | _ -> false

let solutionExists next (t, xs) =
    findSolution next [ (List.head xs, List.tail xs) ] t

let part1 problems =
    problems
    |> List.filter (solutionExists (apply [ (*); (+) ]))
    |> List.map fst
    |> List.sum

let part2 problems =
    problems
    |> List.filter (solutionExists (apply [ concat; (*); (+) ]))
    |> List.map fst
    |> List.sum


let run =
    printfn "== Day 07 =="

    let toProblem list =
        match list with
        | a :: b :: _ -> BigInteger.Parse a, String.split " " b |> List.map BigInteger.Parse
        | _ -> failwith "Invalid input"

    let problems =
        File.ReadLines("inputs/day07.txt")
        |> Seq.map (String.split ": ")
        |> Seq.map toProblem
        |> Seq.toList

    printfn "Part 1: %A" (part1 problems)
    printfn "Part 2: %A" (part2 problems)
    printfn ""
