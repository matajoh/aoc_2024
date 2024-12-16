module Day16

open System.IO

open Algorithms
open Extensions


type Tile =
    | Wall
    | Start
    | End


type Reindeer =
    { P: Position
      D: Direction }

    static member position r = r.P

    static member distance a b =
        1000 * Direction.numTurns a.D b.D + Position.distance a.P b.P

    static member neighbors maze r =
        let p = Direction.next r.P r.D

        let turns =
            [ { r with D = Direction.turnLeft r.D }
              { r with D = Direction.turnRight r.D } ]

        match Map.tryFind p maze with
        | Some Wall -> turns |> List.toSeq
        | _ -> { r with P = p } :: turns |> List.toSeq


let part1 paths =
    paths
    |> List.head
    |> List.pairwise
    |> List.map (fun (a, b) -> Reindeer.distance a b)
    |> List.sum


let part2 paths =
    paths
    |> List.collect (fun p -> p |> List.map (fun r -> r.P))
    |> List.distinct
    |> List.length


let run =
    printfn "== Day 16 =="

    let toTile c =
        match c with
        | '#' -> Some Wall
        | 'S' -> Some Start
        | 'E' -> Some End
        | _ -> None

    let maze =
        File.ReadLines("inputs/day16.txt")
        |> Seq.toList
        |> List.toGrid
        |> Grid.choose toTile
        |> Grid.table

    let start = Map.findKey (fun _ v -> v = Start) maze
    let goal = Map.findKey (fun _ v -> v = End) maze

    let paths =
        { Distance = Reindeer.distance
          Heuristic = Reindeer.position >> Position.distance goal
          Neighbors = Reindeer.neighbors maze
          Goal = Reindeer.position >> (=) goal }
        |> AStar.FindMinPaths { P = start; D = East }
        |> Seq.toList

    printfn "Part 1: %d" (part1 paths)
    printfn "Part 2: %d" (part2 paths)
    printfn ""
