module Day12

open System.IO

open Extensions

type Direction =
    | Up
    | Right
    | Down
    | Left

    static member right (r, c) d =
        match d with
        | Up -> (r, c + 1)
        | Right -> (r + 1, c)
        | Down -> (r, c - 1)
        | Left -> (r - 1, c)

    static member next (r, c) d =
        match d with
        | Up -> (r - 1, c)
        | Right -> (r, c + 1)
        | Down -> (r + 1, c)
        | Left -> (r, c - 1)

    static member turnRight d =
        match d with
        | Up -> Right
        | Right -> Down
        | Down -> Left
        | Left -> Up

    static member turnLeft d =
        match d with
        | Up -> Left
        | Left -> Down
        | Down -> Right
        | Right -> Up

    static member neighbors n =
        [ Up; Right; Down; Left ] |> List.map (Direction.next n)

type Plot =
    { Id: int
      Crop: char
      Tiles: Set<int * int> }

    static member area p = Set.count p.Tiles

    static member isOutside p n = not (Set.contains n p.Tiles)

    static member perimeter p =
        let fenceLength x =
            Direction.neighbors x |> List.filter (Plot.isOutside p) |> List.length

        p.Tiles |> Set.toList |> List.map fenceLength |> List.sum

    static member sides p =
        let rec rightHandWalk turns visited d x =
            if Set.contains (d, x) visited then
                turns, visited |> Set.map snd
            else
                let v' = Set.add (d, x) visited
                let r = Direction.right x d
                let n = Direction.next x d

                match Set.contains n p.Tiles, Set.contains r p.Tiles with
                | true, true -> rightHandWalk (turns + 1) v' (Direction.turnLeft d) x
                | false, true -> rightHandWalk turns v' d n
                | _, false ->
                    let d' = Direction.turnRight d
                    rightHandWalk (turns + 1) v' d' (Direction.next x d')

        let edges n =
            [ Up; Down; Left; Right ]
            |> List.map (fun d -> Direction.turnRight d, Direction.next n d)
            |> List.filter ((snd >> (Plot.isOutside p)))

        let f (sides, visited) (d, x) =
            if Set.contains x visited then
                sides, visited
            else
                let turns, border = rightHandWalk 0 Set.empty d x
                sides + turns, Set.union border visited

        p.Tiles |> Set.toList |> List.collect edges |> List.fold f (0, Set.empty) |> fst


let rec findPlots garden visited plots current tiles =
    match plots, current, tiles with
    | _, _, [] -> plots
    | p :: ps, x :: xs, _ ->
        let crop = p.Crop

        let toAdd =
            Direction.neighbors x
            |> List.filter (Grid.isInside garden)
            |> List.filter (fun n -> (Grid.find n garden) = crop)
            |> List.filter (fun n -> not (Set.contains n p.Tiles))

        let p' =
            { p with
                Tiles = (Set.union (set toAdd) p.Tiles) }

        let v' = Set.union (set toAdd) visited
        findPlots garden v' (p' :: ps) (List.append xs toAdd) tiles
    | _, _, y :: ys when Set.contains y visited -> findPlots garden visited plots current ys
    | _, [], y :: ys ->
        let p =
            { Id = List.length plots
              Crop = Grid.find y garden
              Tiles = Set.singleton y }

        findPlots garden (Set.add y visited) (p :: plots) [ y ] ys
    | _ -> failwith "Invalid state"

let part1 plots =
    plots |> List.map (fun p -> (Plot.area p) * (Plot.perimeter p)) |> List.sum

let part2 plots =
    plots |> List.map (fun p -> (Plot.area p) * (Plot.sides p)) |> List.sum

let run =
    printfn "== Day 12 =="

    let garden = File.ReadLines("inputs/day12.txt") |> Seq.toList |> List.toGrid

    let plots = findPlots garden Set.empty [] [] (Grid.keys garden)

    printfn "Part 1: %d" (part1 plots)
    printfn "Part 2: %d" (part2 plots)
    printfn ""
