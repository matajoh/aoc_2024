module Day13

open System.IO
open System.Text.RegularExpressions

open Maths

type Point = { X: rational; Y: rational }

type Button = { AddX: rational; AddY: rational }

type ClawMachine =
    { A: Button
      B: Button
      Prize: Point }

let parse machines =
    seq {
        for m in
            Regex.Matches(
                machines,
                @"Button A: X\+(\d+), Y\+(\d+)\nButton B: X\+(\d+), Y\+(\d+)\nPrize: X=(\d+), Y=(\d+)"
            ) do
            let a =
                { AddX = rational.parse m.Groups.[1].Value
                  AddY = rational.parse m.Groups.[2].Value }

            let b =
                { AddX = rational.parse m.Groups.[3].Value
                  AddY = rational.parse m.Groups.[4].Value }

            let p =
                { X = rational.parse m.Groups.[5].Value
                  Y = rational.parse m.Groups.[6].Value }

            yield { A = a; B = b; Prize = p }
    }
    |> Seq.toList


let solve m =
    let a = m.A.AddX
    let b = m.B.AddX
    let c = m.A.AddY
    let d = m.B.AddY
    let x = m.Prize.X
    let y = m.Prize.Y
    let det = (a * d) - (b * c)

    if rational.isZero det then
        None
    else
        let numA = ((d * x) - (b * y)) / det
        let numB = ((-c * x) + (a * y)) / det

        match numA, numB with
        | { S = Minus }, _ -> None
        | _, { S = Minus } -> None
        | { Num = na }, { Num = nb } when rational.isWhole numA && rational.isWhole numB -> Some(na, nb)
        | _ -> None


let countTokens (a, b) = a * (bigint 3) + b


let part1 machines =
    machines |> List.choose solve |> List.map countTokens |> List.sum


let part2 machines =
    let diff = rational.fromBigInt (bigint 10000000000000L)

    let add10k p =
        { p with
            X = p.X + diff
            Y = p.Y + diff }

    machines
    |> List.map (fun m -> { m with Prize = add10k m.Prize })
    |> List.choose solve
    |> List.map countTokens
    |> List.sum


let run =
    printfn "== Day 13 =="

    let machines = File.ReadAllText("inputs/day13.txt") |> parse

    printfn "Part 1: %A" (part1 machines)
    printfn "Part 2: %A" (part2 machines)
    printfn ""
