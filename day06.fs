module Day06

open System.IO

open Extensions


type Position = { row: int; column: int }


type Direction =
    | Up
    | Down
    | Left
    | Right


type Guard =
    { position: Position
      direction: Direction
      isOutside: bool }


type Tile =
    | Exit of int
    | Obstruction of int
    | Open of int

    static member compare a b = compare (Tile.toInt a) (Tile.toInt b)

    static member toInt tile =
        match tile with
        | Exit i -> i
        | Obstruction i -> i
        | Open i -> i


type LabMap =
    { rows: Tile list list
      columns: Tile list list
      guard: Guard }

    static member ofList(list: (int * int * char) list) =
        let max2 (max0, max1) (t0, t1, _) =
            (max max0 (t0 + 1)), (max max1 (t1 + 1))

        let rowCount, colCount = list |> List.fold max2 (0, 0)

        let convert select maxVal list =
            let obstructions =
                list
                |> List.map (fun (r, c, x) -> if x = '#' then Some(select (r, c) + 1) else None)
                |> List.choose id
                |> List.sort
                |> List.map Obstruction

            List.append ((Exit 0) :: obstructions) [ Exit(maxVal + 1) ]

        let rows =
            list
            |> List.groupBy (fun (r, _, _) -> r)
            |> List.map (fun (_, row) -> convert snd rowCount row)

        let cols =
            list
            |> List.groupBy (fun (_, c, _) -> c)
            |> List.map (fun (_, col) -> convert fst colCount col)

        let r, c, _ = list |> List.filter (fun (_, _, x) -> x = '^') |> List.head

        { rows = rows
          columns = cols
          guard =
            { position = { row = r + 1; column = c + 1 }
              direction = Up
              isOutside = false } }

    static member moveGuard map =
        let g = map.guard
        let p, d = g.position, g.direction

        let value, list =
            if d = Left || d = Right then
                Open p.column, map.rows.[p.row - 1]
            else
                Open p.row, map.columns.[p.column - 1]

        let i = List.binarySearch Tile.compare value list |> (~~~)

        let t =
            if d = Down || d = Right then
                List.item i list
            else
                List.item (i - 1) list

        let p', d', o =
            match d, t with
            | Up, Obstruction i -> { p with row = i + 1 }, Right, false
            | Up, Exit i -> { p with row = i + 1 }, d, true
            | Right, Obstruction i -> { p with column = i - 1 }, Down, false
            | Right, Exit i -> { p with column = i - 1 }, d, true
            | Down, Obstruction i -> { p with row = i - 1 }, Left, false
            | Down, Exit i -> { p with row = i - 1 }, d, true
            | Left, Obstruction i -> { p with column = i + 1 }, Up, false
            | Left, Exit i -> { p with column = i + 1 }, d, true
            | _ -> failwith "Invalid result from search"

        { map with
            guard =
                { position = p'
                  direction = d'
                  isOutside = o } }

    static member addObstruction m (r, c) =
        let row = m.rows.[r - 1]
        let col = m.columns.[c - 1]
        let ri = List.binarySearch Tile.compare (Open c) row |> (~~~)

        if ri < 0 then
            None
        else
            let ci = List.binarySearch Tile.compare (Open r) col |> (~~~)
            let row' = List.insertAt ri (Obstruction c) row
            let col' = List.insertAt ci (Obstruction r) col

            Some
                { m with
                    rows = List.updateAt (r - 1) row' m.rows
                    columns = List.updateAt (c - 1) col' m.columns }


let rec patrol route visited map =
    if map.guard.isOutside then
        Some((map.guard :: route) |> List.rev)
    elif Set.contains map.guard visited then
        None
    else
        patrol (map.guard :: route) (Set.add map.guard visited) (LabMap.moveGuard map)


let expand (a, b) =
    let p0, d = a.position, a.direction
    let p1 = b.position

    match d, p0, p1 with
    | Up, { row = r1; column = c }, { row = r0 } -> List.arange r0 r1 |> List.map (fun r -> (r, c))
    | Right, { row = r; column = c0 }, { column = c1 } -> List.arange c0 c1 |> List.map (fun c -> (r, c))
    | Down, { row = r0; column = c }, { row = r1 } -> List.arange r0 r1 |> List.map (fun r -> (r, c))
    | Left, { row = r; column = c1 }, { column = c0 } -> List.arange c0 c1 |> List.map (fun c -> (r, c))


let routeTiles map =
    map
    |> patrol [] Set.empty
    |> Option.get
    |> List.pairwise
    |> List.collect expand
    |> set


let part1 map = map |> routeTiles |> Set.count


let part2 map =
    let initPos = map.guard.position.row, map.guard.position.column

    map
    |> routeTiles
    |> Set.remove initPos
    |> Set.toSeq
    |> Seq.map (LabMap.addObstruction map)
    |> Seq.choose id
    |> Seq.map (patrol [] Set.empty)
    |> Seq.filter Option.isNone
    |> Seq.length


let run =
    printfn "== Day 06 =="

    let seqToRow (r, s) =
        s |> Seq.indexed |> Seq.map (fun (c, x) -> r, c, x) |> Seq.toList

    let map =
        File.ReadLines("inputs/day06.txt")
        |> Seq.toList
        |> List.indexed
        |> List.collect seqToRow
        |> LabMap.ofList

    printfn "Part 1: %i" (part1 map)
    printfn "Part 2: %i" (part2 map)
    printfn ""
