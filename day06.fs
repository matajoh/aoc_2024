module Day06

open System.IO

open Extensions


type Position = { Row: int; Column: int }


type Direction =
    | Up
    | Down
    | Left
    | Right


type Guard =
    { Position: Position
      Direction: Direction
      IsOutside: bool }


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
    { Rows: Tile list list
      Columns: Tile list list
      Guard: Guard }

    static member ofGrid grid =
        let convert maxVal list =
            let obstructions =
                list
                |> List.map (fun (i, x) -> if x = '#' then Some(Obstruction(i + 1)) else None)
                |> List.choose id

            List.append ((Exit 0) :: obstructions) [ Exit(maxVal + 1) ]

        let rows = grid |> Grid.byRow |> List.map snd |> List.map (convert grid.Rows)

        let cols = grid |> Grid.byColumn |> List.map snd |> List.map (convert grid.Columns)

        let r, c = Grid.findValue '^' grid |> List.head

        { Rows = rows
          Columns = cols
          Guard =
            { Position = { Row = r + 1; Column = c + 1 }
              Direction = Up
              IsOutside = false } }

    static member moveGuard map =
        let g = map.Guard
        let p, d = g.Position, g.Direction

        let value, list =
            if d = Left || d = Right then
                Open p.Column, map.Rows.[p.Row - 1]
            else
                Open p.Row, map.Columns.[p.Column - 1]

        let i = List.binarySearch Tile.compare value list |> (~~~)

        let t =
            if d = Down || d = Right then
                List.item i list
            else
                List.item (i - 1) list

        let p', d', o =
            match d, t with
            | Up, Obstruction i -> { p with Row = i + 1 }, Right, false
            | Up, Exit i -> { p with Row = i + 1 }, d, true
            | Right, Obstruction i -> { p with Column = i - 1 }, Down, false
            | Right, Exit i -> { p with Column = i - 1 }, d, true
            | Down, Obstruction i -> { p with Row = i - 1 }, Left, false
            | Down, Exit i -> { p with Row = i - 1 }, d, true
            | Left, Obstruction i -> { p with Column = i + 1 }, Up, false
            | Left, Exit i -> { p with Column = i + 1 }, d, true
            | _ -> failwith "Invalid result from search"

        { map with
            Guard =
                { Position = p'
                  Direction = d'
                  IsOutside = o } }

    static member addObstruction m (r, c) =
        let row = m.Rows.[r - 1]
        let col = m.Columns.[c - 1]
        let ri = List.binarySearch Tile.compare (Open c) row |> (~~~)

        if ri < 0 then
            None
        else
            let ci = List.binarySearch Tile.compare (Open r) col |> (~~~)
            let row' = List.insertAt ri (Obstruction c) row
            let col' = List.insertAt ci (Obstruction r) col

            Some
                { m with
                    Rows = List.updateAt (r - 1) row' m.Rows
                    Columns = List.updateAt (c - 1) col' m.Columns }


let rec patrol route visited map =
    if map.Guard.IsOutside then
        Some((map.Guard :: route) |> List.rev)
    elif Set.contains map.Guard visited then
        None
    else
        patrol (map.Guard :: route) (Set.add map.Guard visited) (LabMap.moveGuard map)


let expand (a, b) =
    let p0, d = a.Position, a.Direction
    let p1 = b.Position

    match d, p0, p1 with
    | Up, { Row = r1; Column = c }, { Row = r0 } -> List.arange r0 r1 |> List.map (fun r -> (r, c))
    | Right, { Row = r; Column = c0 }, { Column = c1 } -> List.arange c0 c1 |> List.map (fun c -> (r, c))
    | Down, { Row = r0; Column = c }, { Row = r1 } -> List.arange r0 r1 |> List.map (fun r -> (r, c))
    | Left, { Row = r; Column = c1 }, { Column = c0 } -> List.arange c0 c1 |> List.map (fun c -> (r, c))


let routeTiles map =
    map
    |> patrol [] Set.empty
    |> Option.get
    |> List.pairwise
    |> List.collect expand
    |> set


let part1 map = map |> routeTiles |> Set.count


let part2 map =
    let initPos = map.Guard.Position.Row, map.Guard.Position.Column

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
        File.ReadLines("inputs/day06.txt") |> Seq.toList |> List.toGrid |> LabMap.ofGrid

    printfn "Part 1: %i" (part1 map)
    printfn "Part 2: %i" (part2 map)
    printfn ""
