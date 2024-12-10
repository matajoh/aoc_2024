module Day10

open System.IO

open Extensions

let neighbors pos =
    let r, c = pos

    seq {
        r - 1, c
        r + 1, c
        r, c - 1
        r, c + 1
    }

let rec followTrail map path a =
    let x = Grid.find a map

    let validPos b =
        match Grid.tryFind b map with
        | Some y when y = x + 1 -> true
        | _ -> false

    if x = 9 then
        Set.singleton (a :: path)
    else
        neighbors a
        |> Seq.filter validPos
        |> Seq.map (followTrail map (a :: path))
        |> Set.unionMany


let part1 trails =
    trails
    |> Seq.map (fun xs -> Set.map List.head xs)
    |> Seq.map Set.count
    |> Seq.sum


let part2 trails =
    trails |> List.map Set.count |> List.sum


let run =
    printfn "== Day 09 =="

    let inline charToInt (c: char) = int c - int '0'

    let map =
        File.ReadLines("inputs/day10.txt")
        |> Seq.toList
        |> List.toGrid
        |> Grid.map (fun _ -> charToInt)

    let trails =
        map
        |> Grid.filter (Set.singleton 0)
        |> Grid.keys
        |> Seq.map (followTrail map [])
        |> Seq.toList

    printfn "Part 1: %d" (part1 trails)
    printfn "Part 2: %d" (part2 trails)
    printfn ""
