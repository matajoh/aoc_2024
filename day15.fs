module Day15

open System.IO

open Extensions


type Tile =
    | Wall
    | Box
    | BoxLeft
    | BoxRight


type Direction with
    static member fromChar c =
        match c with
        | '^' -> North
        | '>' -> East
        | 'v' -> South
        | '<' -> West
        | _ -> failwith "Invalid direction"


type Robot = { P: Position; Moves: Direction list }


type Warehouse =
    { G: grid<Tile>
      Robot: Robot }

    static member fromGrid grid moves =
        let toTile c =
            match c with
            | '#' -> Some Wall
            | 'O' -> Some Box
            | _ -> None

        let g = Grid.choose toTile grid
        let pos = Grid.findValue '@' grid |> List.head
        let robot = { P = pos; Moves = moves }

        { G = g; Robot = robot }

    static member step warehouse =
        let rec findEmpty boxes p m =
            match Map.tryFind p warehouse.G.Table with
            | Some Wall -> None
            | Some Box -> findEmpty ((p, Box) :: boxes) (Direction.next p m) m
            | Some BoxLeft ->
                let l = p
                let r = Direction.next l East

                match m with
                | East -> findEmpty ((l, BoxLeft) :: (r, BoxRight) :: boxes) (Direction.next r m) m
                | West -> failwith "Invalid state"
                | _ ->
                    let lEmpty =
                        findEmpty ((l, BoxLeft) :: (r, BoxRight) :: boxes) (Direction.next l m) m

                    let rEmpty =
                        findEmpty ((l, BoxLeft) :: (r, BoxRight) :: boxes) (Direction.next r m) m

                    match lEmpty, rEmpty with
                    | None, _ -> None
                    | _, None -> None
                    | Some lhs, Some rhs -> Some(List.append lhs rhs)
            | Some BoxRight ->
                let r = p
                let l = Direction.next r West

                match m with
                | West -> findEmpty ((l, BoxLeft) :: (r, BoxRight) :: boxes) (Direction.next l m) m
                | East -> failwith "Invalid state"
                | _ ->
                    let lEmpty =
                        findEmpty ((l, BoxLeft) :: (r, BoxRight) :: boxes) (Direction.next l m) m

                    let rEmpty =
                        findEmpty ((l, BoxLeft) :: (r, BoxRight) :: boxes) (Direction.next r m) m

                    match lEmpty, rEmpty with
                    | None, _ -> None
                    | _, None -> None
                    | Some lhs, Some rhs -> Some(List.append lhs rhs)
            | None -> Some boxes

        let rec moveBoxes tiles m boxes =
            match boxes with
            | [] -> tiles
            | (p, t) :: xs ->
                let t' = tiles |> Map.remove p |> Map.add (Direction.next p m) t
                moveBoxes t' m xs

        let r = warehouse.Robot

        match r.Moves with
        | [] -> None
        | x :: xs ->
            match findEmpty [] (Direction.next r.P x) x with
            | Some boxes ->
                let boxes' =
                    boxes
                    |> List.distinct
                    |> match x with
                       | North -> List.sortBy (fun (p, _) -> p.Row)
                       | East -> List.sortByDescending (fun (p, _) -> p.Column)
                       | South -> List.sortByDescending (fun (p, _) -> p.Row)
                       | West -> List.sortBy (fun (p, _) -> p.Column)

                let table = moveBoxes warehouse.G.Table x boxes'

                let robot =
                    { r with
                        P = Direction.next r.P x
                        Moves = xs }

                Some
                    { warehouse with
                        G = { warehouse.G with Table = table }
                        Robot = robot }
            | None ->
                let robot = { r with Moves = xs }
                Some { warehouse with Robot = robot }

    static member gps warehouse =
        warehouse.G.Table
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

        let table = warehouse.G.Table |> Map.toList |> List.collect f |> Map.ofList

        let r = warehouse.Robot

        let r' =
            { r with
                P = { r.P with Column = r.P.Column * 2 } }

        { warehouse with
            G =
                { warehouse.G with
                    Columns = warehouse.G.Columns * 2
                    Table = table }
            Robot = r' }


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
    let moves = lines |> Seq.collect (Seq.map Direction.fromChar) |> Seq.toList
    let warehouse = Warehouse.fromGrid grid moves

    printfn "Part 1: %d" (part1 warehouse)
    printfn "Part 2: %d" (part2 warehouse)
    printfn ""
