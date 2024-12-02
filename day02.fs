module Day02

open System
open System.IO

type ReportCheck =
    { ascending: int
      descending: int
      unsafe: int }

    static member Zero =
        { ascending = 0
          descending = 0
          unsafe = 0 }

    static member isSafe reportCheck =
        match reportCheck with
        | { ascending = _
            descending = 0
            unsafe = 0 } -> true
        | { ascending = 0
            descending = _
            unsafe = 0 } -> true
        | _ -> false

let splitAndConvert (s: String) =
    let parts = s.Split(" ")
    parts |> Seq.map int |> Seq.toList

let checkLevels reportCheck (a, b) =
    match b - a with
    | x when x > 0 && x <= 3 ->
        { reportCheck with
            ascending = reportCheck.ascending + 1 }
    | x when x < 0 && x >= -3 ->
        { reportCheck with
            descending = reportCheck.descending + 1 }
    | _ ->
        { reportCheck with
            unsafe = reportCheck.unsafe + 1 }

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
