module Day23

open System.IO

open Algorithms
open Extensions


let part1 graph =
    graph.Nodes
    |> Array.filter (fun n -> Seq.head n.Name = 't')
    |> Seq.map (fun n -> n.Index)
    |> Seq.collect (Graph.clique3 graph)
    |> set
    |> Set.count


let part2 graph =
    graph
    |> Graph.maximalCliques
    |> Seq.maxBy Set.count
    |> Seq.map (Graph.name graph)
    |> Seq.sort
    |> String.concat ","


let run =
    printfn "== Day 23 =="

    let graph =
        File.ReadLines("inputs/day23.txt")
        |> Seq.map (fun s ->
            let parts = String.split "-" s
            parts.[0], parts.[1])
        |> Graph.fromList

    printfn "Part 1: %d" (part1 graph)
    printfn "Part 2: %s" (part2 graph)
    printfn ""
