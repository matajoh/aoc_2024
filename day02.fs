module Day02

open System
open System.IO

type ReportCheck =
    { Ascending: int
      Descending: int
      Unsafe: int }

    static member Zero =
        { Ascending = 0
          Descending = 0
          Unsafe = 0 }

    static member isSafe reportCheck =
        match reportCheck with
        | { Ascending = _
            Descending = 0
            Unsafe = 0 } -> true
        | { Ascending = 0
            Descending = _
            Unsafe = 0 } -> true
        | _ -> false

let splitAndConvert (s: String) =
    let parts = s.Split(" ")
    parts |> Seq.map int |> Seq.toList

let checkLevels reportCheck (a, b) =
    match b - a with
    | x when x > 0 && x <= 3 ->
        { reportCheck with
            Ascending = reportCheck.Ascending + 1 }
    | x when x < 0 && x >= -3 ->
        { reportCheck with
            Descending = reportCheck.Descending + 1 }
    | _ ->
        { reportCheck with
            Unsafe = reportCheck.Unsafe + 1 }

let checkReport report =
    report |> List.pairwise |> List.fold checkLevels ReportCheck.Zero

let checkReports reports =
    reports |> List.map checkReport |> List.filter ReportCheck.isSafe |> List.length

let part1 reports = reports |> checkReports

let rec subLists prefix list =
    match list with
    | [] -> []
    | x :: xs -> (prefix @ xs) :: subLists (prefix @ [ x ]) xs

let part2 reports =
    reports
    |> List.map (subLists [])
    |> List.map checkReports
    |> List.map sign
    |> List.sum

let run =
    printfn "== Day 02 =="

    let reports =
        File.ReadLines("inputs/day02.txt") |> Seq.map splitAndConvert |> Seq.toList

    printfn "Part 1: %i" (part1 reports)
    printfn "Part 2: %i" (part2 reports)
    printfn ""
