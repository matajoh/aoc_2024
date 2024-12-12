module Extensions

open System

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
    { Table: Map<int * int, 'a>
      Rows: int
      Columns: int }

module Grid =
    let find key grid = Map.find key grid.Table

    let tryFind key grid = Map.tryFind key grid.Table

    let map mapping grid =
        { Rows = grid.Rows
          Columns = grid.Columns
          Table = Map.map mapping grid.Table }

    let filterMap (mapping: 'a -> Option<'b>) grid =
        let folder filtered key value =
            match mapping value with
            | Some v -> Map.add key v filtered
            | None -> filtered

        { Rows = grid.Rows
          Columns = grid.Columns
          Table = Map.fold folder grid.Table Map.empty }

    let filter (vals: Set<'a>) grid =
        let f _ x = Set.contains x vals

        { grid with
            Table = Map.filter f grid.Table }

    let filterOut (vals: Set<'a>) grid =
        let f _ x = not (Set.contains x vals)

        { grid with
            Table = Map.filter f grid.Table }

    let findValue x grid =
        grid.Table |> Map.toList |> List.filter (fun (_, y) -> y = x) |> List.map fst

    let byRow grid =
        grid.Table
        |> Map.toList
        |> List.groupBy (fun ((r, _), _) -> r)
        |> List.map (fun (r, xs) -> r, (xs |> List.map (fun ((_, c), x) -> c, x)))

    let byColumn grid =
        grid.Table
        |> Map.toList
        |> List.groupBy (fun ((_, c), _) -> c)
        |> List.map (fun (c, xs) -> c, (xs |> List.map (fun ((r, _), x) -> r, x)))

    let isInside grid (r, c) =
        not (r < 0 || r >= grid.Rows || c < 0 || c >= grid.Columns)

    let keys grid = Map.keys grid.Table |> Seq.toList

    let create rows columns table =
        { Rows = rows
          Columns = columns
          Table = table }


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

    let powrange x count =
        seq {
            let mutable x' = 1UL

            for _ in 1..count do
                yield x'
                x' <- x' * x
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
