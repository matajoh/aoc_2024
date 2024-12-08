module Extensions

open System
open System.IO

module Array2D =
    let tryGet (r, c) a =
        let rows = Array2D.length1 a
        let cols = Array2D.length2 a

        if r < 0 || r >= rows || c < 0 || c >= cols then
            None
        else
            Some(Array2D.get a r c)

    let rowColumnIndices a =
        seq {
            for r in 0 .. Array2D.length1 a - 1 do
                for c in 0 .. Array2D.length2 a - 1 do
                    yield (r, c)
        }

type grid<'a> when 'a: comparison =
    { table: Map<int * int, 'a>
      rows: int
      columns: int }

module Grid =
    let find key grid = Map.find key grid.table

    let tryFind key grid = Map.tryFind key grid.table

    let map mapping grid =
        { grid with
            table = Map.map mapping grid.table }

    let filterMap (mapping: 'a -> Option<'b>) grid =
        let folder filtered key value =
            match mapping value with
            | Some v -> Map.add key v filtered
            | None -> filtered

        { rows = grid.rows
          columns = grid.columns
          table = Map.fold folder grid.table Map.empty }

    let filter (vals: Set<'a>) grid =
        let f _ x = Set.contains x vals

        { grid with
            table = Map.filter f grid.table }

    let filterOut (vals: Set<'a>) grid =
        let f _ x = not (Set.contains x vals)

        { grid with
            table = Map.filter f grid.table }

    let findValue x grid =
        grid.table
        |> Map.toList
        |> List.filter (fun (_, y) -> y = x)
        |> List.map fst

    let byRow grid =
        grid.table
        |> Map.toList
        |> List.groupBy (fun ((r, _), _) -> r)
        |> List.map (fun (r, xs) -> r, (xs |> List.map (fun ((_, c), x) -> c, x)))

    let byColumn grid =
        grid.table
        |> Map.toList
        |> List.groupBy (fun ((_, c), _) -> c)
        |> List.map (fun (c, xs) -> c, (xs |> List.map (fun ((r, _), x) -> r, x)))

    let create rows columns table = {rows=rows; columns=columns; table=table}


module List =
    let print list =
        List.map
            (fun x ->
                printfn "%A" x
                x)
            list

    let binarySearch compare value list =
        let rec search left right =
            if left = right then
                ~~~left
            else
                let mid = (left + right) / 2
                let test = List.item mid list
                let cmp = compare value test

                if cmp < 0 then
                    search left mid
                elif cmp > 0 then
                    if right = mid + 1 then ~~~right else search mid right
                else
                    mid

        search 0 (List.length list - 1)

    let arange min max =
        seq {
            for i in min..max do
                yield i
        }
        |> Seq.toList

    let toGrid list =
        let rows = List.length list
        let columns = list |> List.map Seq.length |> List.max

        let table =
            list
            |> List.map (Seq.indexed >> Seq.toList)
            |> List.indexed
            |> List.collect (fun (r, xs) -> xs |> List.map (fun (c, x) -> (r, c), x))
            |> Map.ofList

        Grid.create rows columns table


module String =
    let split (separator: String) (string: String) = string.Split(separator) |> Array.toList
