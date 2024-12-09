module Algorithms

open System
open System.Collections.Generic

type AStar<'S> when 'S: comparison(distance, heuristic, neighbors, goal) =
    member _.Distance = distance
    member _.Heuristic = heuristic
    member _.Neighbors = neighbors
    member _.Goal = goal

    static member private reconstructPath (cameFrom: Dictionary<'S, 'S>) current =
        let rec f path s =
            if cameFrom.ContainsKey(s) then
                f (s :: path) cameFrom[s]
            else
                path

        f [] current

    member public this.FindMinPath start =
        let queue = new PriorityQueue<'S, int>()
        queue.Enqueue(start, 0)
        let gScore = new Dictionary<'S, int>()
        gScore[start] <- 0
        let cameFrom = new Dictionary<'S, 'S>()

        let rec f (queue: PriorityQueue<'S, int>) (gScore: Dictionary<'S, int>) (cameFrom: Dictionary<'S, 'S>) =
            if queue.Count = 0 then
                failwith "Unable to find a path to the goal"
            else
                let current = queue.Dequeue()

                if this.Goal current then
                    AStar.reconstructPath cameFrom current
                else
                    let gscore = gScore[current]

                    for neighbor in this.Neighbors current do
                        let tentative_gscore = gscore + (this.Distance current neighbor)

                        if tentative_gscore < gScore.GetValueOrDefault(neighbor, Int32.MaxValue) then
                            cameFrom[neighbor] <- current
                            gScore[neighbor] <- tentative_gscore
                            let fScore = tentative_gscore + this.Heuristic neighbor
                            queue.Enqueue(neighbor, fScore)

                    f queue gScore cameFrom

        f queue gScore cameFrom
