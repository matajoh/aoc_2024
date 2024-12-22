
let run i =
    match i with
    | 1 -> Day01.run
    | 2 -> Day02.run
    | 3 -> Day03.run
    | 4 -> Day04.run
    | 5 -> Day05.run
    | 6 -> Day06.run
    | 7 -> Day07.run
    | 8 -> Day08.run
    | 9 -> Day09.run
    | 10 -> Day10.run
    | 11 -> Day11.run
    | 12 -> Day12.run
    | 13 -> Day13.run
    | 14 -> Day14.run
    | 15 -> Day15.run
    | 16 -> Day16.run
    | 17 -> Day17.run
    | 18 -> Day18.run
    | 19 -> Day19.run
    | 20 -> Day20.run
    | 21 -> Day21.run
    | 22 -> Day22.run
    | _ -> printfn "Day %i not implemented" i

[<EntryPoint>]
let main argv =
    printfn "## Advent of Code 2024 ##"
    printfn ""

    match argv.Length with
    | 0 -> [ 1..25 ] |> Seq.iter (fun i -> run i)
    | _ -> argv |> Seq.map int |> Seq.iter (fun i -> run i)

    0
