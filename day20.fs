module Day20

open System.IO

open Extensions


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


let rec markPath path i m =
    if i = List.length path then
        m
    else
        let j = List.length path - 1 - i
        let p = path.[i]
        m |> Map.add p (Path(i, j)) |> markPath path (i + 1)


let findCheats n map path =
    let length = List.length path - 1

    let tryCheat p0 t0 p1 =
        let d = Position.distance p0 p1

        if d <= n then
            match t0, Map.find p1 map with
            | Path(s0, e0), Path(s1, e1) ->
                let cheat = length - d - min s0 s1 - min e0 e1

                if cheat > 0 then Some cheat else None
            | _ -> failwith "Invalid state"
        else
            None

    let find p0 ps =
        let t0 = Map.find p0 map
        ps |> List.choose (tryCheat p0 t0)

    let rec f p c =
        match p with
        | [] -> List.concat c |> List.countBy id
        | x :: xs -> f xs ((find x xs) :: c)

    f path []


let part1 map path =
    findCheats 2 map (List.sort path)
    |> List.filter (fun (d, _) -> d >= 100)
    |> List.sumBy snd


let part2 map path =
    findCheats 20 map (List.sort path)
    |> List.filter (fun (d, _) -> d >= 100)
    |> List.sumBy snd


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
    let map = markPath path 0 tiles

    printfn "Part 1: %d" (part1 map path)
    printfn "Part 2: %d" (part2 map path)
    printfn ""
