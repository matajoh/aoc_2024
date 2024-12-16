module Day06

open System.IO

open Extensions


type Guard =
    { P: Position
      D: Direction
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

        let p = Grid.findValue '^' grid |> List.head

        { Rows = rows
          Columns = cols
          Guard =
            { P =
                { Row = p.Row + 1
                  Column = p.Column + 1 }
              D = North
              IsOutside = false } }

    static member moveGuard map =
        let g = map.Guard
        let p, d = g.P, g.D

        let value, list =
            if d = West || d = East then
                Open p.Column, map.Rows.[p.Row - 1]
            else
                Open p.Row, map.Columns.[p.Column - 1]

        let i = List.binarySearch Tile.compare value list |> (~~~)

        let t =
            if d = South || d = East then
                List.item i list
            else
                List.item (i - 1) list

        let p', d', o =
            match d, t with
            | North, Obstruction i -> { p with Row = i + 1 }, East, false
            | North, Exit i -> { p with Row = i + 1 }, d, true
            | East, Obstruction i -> { p with Column = i - 1 }, South, false
            | East, Exit i -> { p with Column = i - 1 }, d, true
            | South, Obstruction i -> { p with Row = i - 1 }, West, false
            | South, Exit i -> { p with Row = i - 1 }, d, true
            | West, Obstruction i -> { p with Column = i + 1 }, North, false
            | West, Exit i -> { p with Column = i + 1 }, d, true
            | _ -> failwith "Invalid result from search"

        { map with
            Guard = { P = p'; D = d'; IsOutside = o } }

    static member addObstruction m p =
        let row = m.Rows.[p.Row - 1]
        let col = m.Columns.[p.Column - 1]
        let ri = List.binarySearch Tile.compare (Open p.Column) row |> (~~~)

        if ri < 0 then
            None
        else
            let ci = List.binarySearch Tile.compare (Open p.Row) col |> (~~~)
            let row' = List.insertAt ri (Obstruction p.Column) row
            let col' = List.insertAt ci (Obstruction p.Row) col

            Some
                { m with
                    Rows = List.updateAt (p.Row - 1) row' m.Rows
                    Columns = List.updateAt (p.Column - 1) col' m.Columns }


let rec patrol route visited map =
    if map.Guard.IsOutside then
        Some((map.Guard :: route) |> List.rev)
    elif Set.contains map.Guard visited then
        None
    else
        patrol (map.Guard :: route) (Set.add map.Guard visited) (LabMap.moveGuard map)


let expand (a, b) =
    let p0, d = a.P, a.D
    let p1 = b.P

    match d, p0, p1 with
    | North, { Row = r1 }, { Row = r0 } -> List.arange r0 r1 |> List.map (fun r -> { p0 with Row = r })
    | East, { Column = c0 }, { Column = c1 } -> List.arange c0 c1 |> List.map (fun c -> { p0 with Column = c })
    | South, { Row = r0 }, { Row = r1 } -> List.arange r0 r1 |> List.map (fun r -> { p0 with Row = r })
    | West, { Column = c1 }, { Column = c0 } -> List.arange c0 c1 |> List.map (fun c -> { p0 with Column = c })


let routeTiles map =
    map
    |> patrol [] Set.empty
    |> Option.get
    |> List.pairwise
    |> List.collect expand
    |> set


let part1 map = map |> routeTiles |> Set.count


let part2 map =
    map
    |> routeTiles
    |> Set.remove map.Guard.P
    |> Set.toSeq
    |> Seq.map (LabMap.addObstruction map)
    |> Seq.choose id
    |> Seq.map (patrol [] Set.empty)
    |> Seq.filter Option.isNone
    |> Seq.length


let run =
    printfn "== Day 06 =="

    let map =
        File.ReadLines("inputs/day06.txt") |> Seq.toList |> List.toGrid |> LabMap.ofGrid

    printfn "Part 1: %i" (part1 map)
    printfn "Part 2: %i" (part2 map)
    printfn ""
