module Algorithms


type astar<'S> when 'S: comparison =
    { Distance: 'S -> 'S -> int
      Heuristic: 'S -> int
      Neighbors: 'S -> 'S seq
      Goal: 'S -> bool }


type state<'S> when 'S: comparison =
    { Queue: Set<int * 'S>
      GScore: Map<'S, int>
      CameFrom: Map<'S, 'S list> }


module private State =
    let create s =
        { Queue = Set.singleton (0, s)
          GScore = Map.ofList [ s, 0 ]
          CameFrom = Map.empty }

    let addNeighbor x y gs fs s =
        { s with
            Queue = Set.add (fs, y) s.Queue
            GScore = Map.add y gs s.GScore
            CameFrom = Map.add y [ x ] s.CameFrom }

    let addAlternatePath x y s =
        let xs = Map.find y s.CameFrom

        { s with
            CameFrom = Map.add y (x :: xs) s.CameFrom }

    let dequeue state =
        let x = Set.minElement state.Queue

        x,
        { state with
            Queue = Set.remove x state.Queue }

    let gscore x state = Map.find x state.GScore

    let reconstructPaths x s =
        let rec f path y =
            match Map.tryFind y s.CameFrom with
            | Some parents -> parents |> Seq.collect (f (y :: path))
            | None -> seq { y :: path }

        f [] x


module AStar =
    let findMinPaths start astar =
        let addNeighbors x xGS s y =
            let yGS = xGS + astar.Distance x y

            let addNeighbor () =
                let yFS = yGS + astar.Heuristic y
                State.addNeighbor x y yGS yFS s

            match Map.tryFind y s.GScore with
            | None -> addNeighbor ()
            | Some gs when yGS < gs -> addNeighbor ()
            | Some gs when yGS = gs -> State.addAlternatePath x y s
            | _ -> s

        let rec f s =
            if Set.isEmpty s.Queue then
                Seq.empty
            else
                let (_, x), s' = State.dequeue s
                let gs = State.gscore x s

                if astar.Goal x then
                    State.reconstructPaths x s
                else
                    astar.Neighbors x |> Seq.fold (addNeighbors x gs) s' |> f

        State.create start |> f

    let tryFindMinPath start astar = Seq.tryHead (findMinPaths start astar)

    let findMinPath start astar =
        match tryFindMinPath start astar with
        | Some path -> path
        | None -> failwith "Unable to find path to goal"
