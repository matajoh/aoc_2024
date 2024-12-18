module Day18

open System.IO

open Algorithms
open Extensions


type MemorySpace =
    { Rows: int
      Columns: int
      Pixels: Position list
      Corrupted: Set<Position> }

    static member neighbors m p =
        let valid p =
            if p.Row < 0 || p.Column < 0 || p.Row >= m.Rows || p.Column >= m.Columns then
                false
            elif Set.contains p m.Corrupted then
                false
            else
                true

        [ North; East; South; West ] |> Seq.map (Direction.next p) |> Seq.filter valid

    static member atTime t m =
        { m with
            Corrupted = List.take t m.Pixels |> set }

    static member minPath m =
        let goal =
            { Row = m.Rows - 1
              Column = m.Columns - 1 }

        let astar =
            { Distance = Position.distance
              Heuristic = fun p -> Position.distance p goal
              Neighbors = MemorySpace.neighbors m
              Goal = (=) goal }

        match AStar.tryFindMinPath { Row = 0; Column = 0 } astar with
        | Some path -> Some(List.length path - 1)
        | None -> None


let part1 m =
    m |> MemorySpace.atTime 1024 |> MemorySpace.minPath |> Option.get


let rec findLastValid m s e =
    if s + 1 >= e then
        m.Pixels[s]
    else
        let t = (s + e) / 2

        match m |> MemorySpace.atTime t |> MemorySpace.minPath with
        | Some _ -> findLastValid m t e
        | None -> findLastValid m s t


let part2 m =
    let p = findLastValid m 0 (List.length m.Pixels - 1)
    sprintf "%d,%d" p.Column p.Row


let run =
    printfn "== Day 18 =="

    let pixels =
        File.ReadLines("inputs/day18.txt")
        |> Seq.map (String.split ",")
        |> Seq.map (fun s -> { Row = int s.[1]; Column = int s.[0] })
        |> Seq.toList

    let ms =
        { Rows = 71
          Columns = 71
          Pixels = pixels
          Corrupted = Set.empty }

    printfn "Part 1: %d" (part1 ms)
    printfn "Part 2: %s" (part2 ms)
    printfn ""
