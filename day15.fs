module Day15

open System.IO

open Extensions


type Tile =
    | Wall
    | Box
    | BoxLeft
    | BoxRight


type Position = { Row: int; Column: int }


type Move =
    | Up
    | Right
    | Down
    | Left

    static member fromChar c =
        match c with
        | '^' -> Up
        | '>' -> Right
        | 'v' -> Down
        | '<' -> Left
        | _ -> failwith "Invalid direction"

    static member next pos m =
        match m with
        | Up -> { pos with Row = pos.Row - 1 }
        | Right -> { pos with Column = pos.Column + 1 }
        | Down -> { pos with Row = pos.Row + 1 }
        | Left -> { pos with Column = pos.Column - 1 }


type Robot =
    { Position: Position; Moves: Move list }


type Warehouse =
    { Rows: int
      Columns: int
      Tiles: Map<Position, Tile>
      Robot: Robot }

    static member fromGrid grid moves =
        let tiles =
            grid.Table
            |> Map.toList
            |> List.choose (fun ((r, c), t) ->
                let pos = { Row = r; Column = c }

                match t with
                | '#' -> Some(pos, Wall)
                | 'O' -> Some(pos, Box)
                | _ -> None)
            |> Map.ofList

        let (r, c) = Grid.findValue '@' grid |> List.head

        let robot =
            { Position = { Row = r; Column = c }
              Moves = moves }

        { Rows = grid.Rows
          Columns = grid.Columns
          Tiles = tiles
          Robot = robot }

    static member step warehouse =
        let rec findEmpty boxes p m =
            match Map.tryFind p warehouse.Tiles with
            | Some Wall -> None
            | Some Box -> findEmpty ((p, Box) :: boxes) (Move.next p m) m
            | Some BoxLeft ->
                let l = p
                let r = Move.next l Right

                match m with
                | Right -> findEmpty ((l, BoxLeft) :: (r, BoxRight) :: boxes) (Move.next r m) m
                | Left -> failwith "Invalid state"
                | _ ->
                    let lEmpty = findEmpty ((l, BoxLeft) :: (r, BoxRight) :: boxes) (Move.next l m) m
                    let rEmpty = findEmpty ((l, BoxLeft) :: (r, BoxRight) :: boxes) (Move.next r m) m

                    match lEmpty, rEmpty with
                    | None, _ -> None
                    | _, None -> None
                    | Some lhs, Some rhs -> Some(List.append lhs rhs)
            | Some BoxRight ->
                let r = p
                let l = Move.next r Left

                match m with
                | Left -> findEmpty ((l, BoxLeft) :: (r, BoxRight) :: boxes) (Move.next l m) m
                | Right -> failwith "Invalid state"
                | _ ->
                    let lEmpty = findEmpty ((l, BoxLeft) :: (r, BoxRight) :: boxes) (Move.next l m) m
                    let rEmpty = findEmpty ((l, BoxLeft) :: (r, BoxRight) :: boxes) (Move.next r m) m

                    match lEmpty, rEmpty with
                    | None, _ -> None
                    | _, None -> None
                    | Some lhs, Some rhs -> Some(List.append lhs rhs)
            | None -> Some boxes

        let rec moveBoxes tiles m boxes =
            match boxes with
            | [] -> tiles
            | (p, t) :: xs ->
                let t' = tiles |> Map.remove p |> Map.add (Move.next p m) t
                moveBoxes t' m xs

        let r = warehouse.Robot

        match r.Moves with
        | [] -> None
        | x :: xs ->
            match findEmpty [] (Move.next r.Position x) x with
            | Some boxes ->
                let boxes' =
                    boxes
                    |> List.distinct
                    |> match x with
                       | Up -> List.sortBy (fun (p, _) -> p.Row)
                       | Right -> List.sortByDescending (fun (p, _) -> p.Column)
                       | Down -> List.sortByDescending (fun (p, _) -> p.Row)
                       | Left -> List.sortBy (fun (p, _) -> p.Column)

                let tiles = moveBoxes warehouse.Tiles x boxes'

                let robot =
                    { r with
                        Position = Move.next r.Position x
                        Moves = xs }

                Some
                    { warehouse with
                        Tiles = tiles
                        Robot = robot }
            | None ->
                let robot = { r with Moves = xs }
                Some { warehouse with Robot = robot }

    static member gps warehouse =
        warehouse.Tiles
        |> Map.toList
        |> List.choose (fun (k, v) ->
            if v = Box || v = BoxLeft then
                Some(k.Row * 100 + k.Column)
            else
                None)

    static member expand warehouse =
        let f (p, t) =
            let l = { p with Column = p.Column * 2 }
            let r = { l with Column = l.Column + 1 }

            match t with
            | Wall -> [ (l, Wall); (r, Wall) ]
            | Box -> [ (l, BoxLeft); (r, BoxRight) ]
            | _ -> failwith "invalid state"

        let tiles = warehouse.Tiles |> Map.toList |> List.collect f |> Map.ofList

        let r = warehouse.Robot

        let r' =
            { r with
                Position =
                    { r.Position with
                        Column = r.Position.Column * 2 } }

        { warehouse with
            Tiles = tiles
            Robot = r'
            Columns = warehouse.Columns * 2 }


let rec parseMap mapLines lines =
    match lines with
    | "" :: xs -> List.toGrid (List.rev mapLines), xs
    | x :: xs -> parseMap (x :: mapLines) xs
    | _ -> failwith "Invalid input"


let rec execute warehouse =
    match Warehouse.step warehouse with
    | None -> warehouse
    | Some w -> execute w


let part1 warehouse =
    warehouse |> execute |> Warehouse.gps |> List.sum


let part2 warehouse =
    warehouse |> Warehouse.expand |> execute |> Warehouse.gps |> List.sum


let run =
    printfn "== Day 15 =="

    let grid, lines = File.ReadLines("inputs/day15.txt") |> Seq.toList |> parseMap []
    let moves = lines |> Seq.collect (Seq.map Move.fromChar) |> Seq.toList
    let warehouse = Warehouse.fromGrid grid moves

    printfn "Part 1: %d" (part1 warehouse)
    printfn "Part 2: %d" (part2 warehouse)
    printfn ""
