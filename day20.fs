module Day20

open System.IO

open Algorithms
open Extensions
open System.Collections.Generic


type Tile =
    | Wall
    | Open
    | Start
    | End
    | Path of int * int

    static member fromChar c =
        match c with
        | '#' -> Wall
        | '.' -> Open
        | 'S' -> Start
        | 'E' -> End
        | _ -> failwith "Invalid tile"


let rec findPath path m p =
    match Map.find p m with
    | End -> List.rev (p :: path)
    | _ ->
        let r =
            match path with
            | [] -> { Row = 0; Column = 0 }
            | x :: _ -> x

        Direction.Values
        |> List.map (Direction.next p)
        |> List.filter (fun p' -> p' <> r && Map.find p' m <> Wall)
        |> List.head
        |> findPath (p :: path) m


let rec markPath path tiles =
    let m = new Dictionary<Position, Tile>()

    for k, v in Map.toSeq tiles do
        m.[k] <- v

    for i, p in List.indexed path do
        let j = List.length path - 1 - i
        m.[p] <- Path(i, j)

    m

let findCheats nearestNeighbors n (map: Dictionary<Position, Tile>) path =
    let length = List.length path - 1

    let tryCheat p0 t0 p1 =
        if p0 < p1 then
            match t0, map.[p1] with
            | Path(s0, e0), Path(s1, e1) ->
                let cheat = length - (Position.distance p0 p1) - min s0 s1 - min e0 e1

                if cheat >= 100 then Some cheat else None
            | _ -> failwith "Invalid state"
        else
            None

    let find p0 =
        let t0 = map.[p0]
        p0 |> nearestNeighbors n |> List.choose (tryCheat p0 t0)

    let rec f p c =
        match p with
        | [] -> List.concat c |> List.countBy id
        | x :: xs -> f xs ((find x) :: c)

    f path []


let part1 nearestNeighbors map path =
    findCheats nearestNeighbors 2 map (List.sort path) |> List.sumBy snd


let part2 nearestNeighbors map path =
    findCheats nearestNeighbors 20 map (List.sort path) |> List.sumBy snd


let run =
    printfn "== Day 20 =="

    let tiles =
        File.ReadLines("inputs/day20.txt")
        |> Seq.toList
        |> List.toGrid
        |> Grid.map (fun _ c -> Tile.fromChar c)
        |> Grid.table

    let start = Map.pick (fun k v -> if v = Start then Some k else None) tiles
    let path = findPath [] tiles start
    let map = markPath path tiles

    let kdtree =
        { Distance = Position.distance
          Select = (fun i p -> if i = 0 then p.Row else p.Column)
          MaxDimension = 1
          MaxPerNode = 10 }

    let root = KDTree.build path kdtree

    let nearestNeighbors = KDTree.search kdtree root


    printfn "Part 1: %d" (part1 nearestNeighbors map path)
    printfn "Part 2: %d" (part2 nearestNeighbors map path)
    printfn ""
