module Day24

open System.IO
open System.Text.RegularExpressions
open System.Collections.Generic


type Gate =
    | AND
    | OR
    | XOR
    | Input


let parseGates text =
    let inputs =
        seq {
            for m in Regex.Matches(text, @"([xy]\d{2}): ([01])") do
                let name = m.Groups.[1].Value
                let value = m.Groups.[2].Value
                yield name, (Input, name, value)
        }

    let gates =
        seq {
            for m in Regex.Matches(text, @"([a-z0-9]{3}) ((?:AND|OR|XOR)) ([a-z0-9]{3}) -> ([a-z0-9]{3})") do
                let a = m.Groups.[1].Value
                let b = m.Groups.[3].Value
                let name = m.Groups.[4].Value

                let op =
                    match m.Groups.[2].Value with
                    | "AND" -> AND
                    | "OR" -> OR
                    | "XOR" -> XOR
                    | _ -> failwith "Invalid op"

                if a < b then
                    yield name, (op, a, b)
                else
                    yield name, (op, b, a)
        }

    Seq.append inputs gates |> Map.ofSeq


let getValue gates =
    let values = Dictionary<string, uint64>()

    let rec f name =
        if not (values.ContainsKey(name)) then
            match Map.find name gates with
            | Input, _, value -> values.[name] <- uint64 value
            | AND, a, b -> values.[name] <- (f a) &&& (f b)
            | OR, a, b -> values.[name] <- (f a) ||| (f b)
            | XOR, a, b -> values.[name] <- (f a) ^^^ (f b)

        values.[name]

    f


let outputs gates =
    Map.keys gates |> Seq.filter (fun k -> Seq.head k = 'z') |> Seq.toList


let part1 gates =
    outputs gates
    |> List.sort
    |> List.map (getValue gates)
    |> List.rev
    |> List.fold (fun v b -> (v <<< 1) ||| b) 0UL


let part2 gates =
    let gatesInv = gates |> Map.toList |> List.map (fun (k, v) -> v, k) |> Map.ofList

    let findGate errors (g: Gate) (i0: string) (i1: string) =
        let pick (i: string) k v =
            match k with
            | a, b, c when a = g && (b = i || c = i) -> if b = i then Some(c, v) else Some(b, v)
            | _ -> None

        let k = if i0 < i1 then g, i0, i1 else g, i1, i0

        match Map.tryFind k gatesInv with
        | Some n -> errors, n
        | None ->
            match Map.tryPick (pick i0) gatesInv, Map.tryPick (pick i1) gatesInv with
            | Some(err1, n), _ -> i1 :: err1 :: errors, n
            | None, Some(err0, n) -> i0 :: err0 :: errors, n
            | None, None -> failwith "Unable to proceed"

    let rec f errors carry b =
        let x = sprintf "x%02d" b
        let y = sprintf "y%02d" b
        let z = sprintf "z%02d" b

        if not (Map.containsKey x gates) then
            errors
        elif b = 0 then
            // validate half-adder
            let xor0 = Map.find (XOR, x, y) gatesInv
            let and0 = Map.find (AND, x, y) gatesInv

            if xor0 <> z then
                f (xor0 :: z :: errors) and0 (b + 1)
            else
                f errors and0 (b + 1)
        else
            // validate full-adder
            let xor0 = Map.find (XOR, x, y) gatesInv
            let and0 = Map.find (AND, x, y) gatesInv

            let e0, xor1 = findGate errors XOR xor0 carry
            let e1, and1 = findGate e0 AND xor0 carry
            let e2, or0 = findGate e1 OR and0 and1

            if xor1 <> z then
                f (xor1 :: z :: e2) or0 (b + 1)
            else
                f e2 or0 (b + 1)

    f [] "" 0 |> List.distinct |> List.sort |> String.concat ","


let run =
    printfn "== Day 24 =="

    let gates = File.ReadAllText("inputs/day24.txt") |> parseGates

    printfn "Part 1: %d" (part1 gates)
    printfn "Part 2: %s" (part2 gates)
    printfn ""
