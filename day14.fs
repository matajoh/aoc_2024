module Day14

open System.IO
open System.Text.RegularExpressions

open Maths


type Vector =
    { X: int
      Y: int }

    static member addModulo (width, height) a b =
        { X = modulo (a.X + b.X) width
          Y = modulo (a.Y + b.Y) height }

    static member scale s v = { X = s * v.X; Y = s * v.Y }


type Robot =
    { Position: Vector
      Velocity: Vector }

    static member move size steps r =
        let v = Vector.scale steps r.Velocity
        let p = Vector.addModulo size r.Position v
        { r with Position = p }

    static member quadrant (width, height) r =
        let dx = compare r.Position.X (width / 2)
        let dy = compare r.Position.Y (height / 2)

        match dx, dy with
        | -1, -1 -> Some 0
        | -1, 1 -> Some 1
        | 1, -1 -> Some 2
        | 1, 1 -> Some 3
        | _ -> None


let parseRobots text =
    seq {
        for m in Regex.Matches(text, @"p=(\d+),(\d+) v=(\-?\d+),(\-?\d+)") do
            yield
                { Position =
                    { X = int m.Groups.[1].Value
                      Y = int m.Groups.[2].Value }
                  Velocity =
                    { X = int m.Groups.[3].Value
                      Y = int m.Groups.[4].Value } }
    }
    |> Seq.toList


let safetyFactor size robots =
    robots
    |> List.choose (Robot.quadrant size)
    |> List.countBy id
    |> List.fold (fun p q -> p * (snd q)) 1


let part1 size robots =
    robots |> List.map (Robot.move size 100) |> safetyFactor size


let part2 size robots =
    let mutable minScore = (0, safetyFactor size robots)

    for steps in 0..10000 do
        let score = robots |> List.map (Robot.move size steps) |> safetyFactor size

        if score < snd minScore then
            minScore <- (steps, score)

    fst minScore


let run =
    printfn "== Day 14 =="

    let robots = File.ReadAllText("inputs/day14.txt") |> parseRobots
    let size = 101, 103

    printfn "Part 1: %d" (part1 size robots)
    printfn "Part 2: %d" (part2 size robots)
    printfn ""
