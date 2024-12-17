module Day17

open System
open System.IO
open System.Text.RegularExpressions

open Extensions


type Operator =
    | ADV = 0
    | BXL = 1
    | BST = 2
    | JNZ = 3
    | BXC = 4
    | OUT = 5
    | BDV = 6
    | CDV = 7


type Registers = { A: uint64; B: uint64; C: uint64 }


let rec execute earlyExit program r output ip =
    let combo v =
        match v with
        | 0
        | 1
        | 2
        | 3 -> uint64 v
        | 4 -> r.A
        | 5 -> r.B
        | 6 -> r.C
        | _ -> failwith "reserved"

    if ip >= List.length program - 1 then
        List.rev output
    else
        let op = enum<Operator> (List.item ip program)
        let arg = List.item (ip + 1) program

        match op with
        | Operator.ADV ->
            let n = r.A
            let d = 1UL <<< int (combo arg)
            execute earlyExit program { r with A = n / d } output (ip + 2)
        | Operator.BXL -> execute earlyExit program { r with B = r.B ^^^ uint64 arg } output (ip + 2)
        | Operator.BST -> execute earlyExit program { r with B = (combo arg) % 8UL } output (ip + 2)
        | Operator.JNZ ->
            if r.A = 0UL then
                execute earlyExit program r output (ip + 2)
            else
                execute earlyExit program r output arg
        | Operator.BXC -> execute earlyExit program { r with B = r.B ^^^ r.C } output (ip + 2)
        | Operator.OUT ->
            let o = (combo arg) % 8UL

            if earlyExit then
                [ o ]
            else
                execute earlyExit program r (((combo arg) % 8UL) :: output) (ip + 2)
        | Operator.BDV ->
            let n = r.A
            let d = 1UL <<< int (combo arg)
            execute earlyExit program { r with B = n / d } output (ip + 2)
        | Operator.CDV ->
            let n = r.A
            let d = 1UL <<< int (combo arg)
            execute earlyExit program { r with C = n / d } output (ip + 2)
        | _ -> failwith "Invalid operator"


let parseProgram text =
    let m =
        Regex.Match(text, @"Register A: (\d+)\nRegister B: (\d+)\nRegister C: (\d+)\n\nProgram: ((?:\d,)+\d)")

    let registers =
        { A = uint64 m.Groups.[1].Value
          B = uint64 m.Groups.[2].Value
          C = uint64 m.Groups.[3].Value }

    let program = String.split "," m.Groups.[4].Value |> List.map int
    program, registers


let part1 program registers =
    let outputs = execute false program registers [] 0
    String.Join(",", outputs)


let part2 program =
    let test a =
        execute true program { A = a; B = 0UL; C = 0UL } [] 0 |> List.head

    let rec findA program a =
        match program with
        | [] -> [ a / 8UL ]
        | x :: xs ->
            [ 0UL .. 7UL ]
            |> List.map ((+) a)
            |> List.map (fun a -> a, test a)
            |> List.filter (snd >> (=) (uint64 x))
            |> List.map (fun (a, _) -> a * 8UL)
            |> List.collect (findA xs)

    findA (List.rev program) 0UL |> List.head

let run =
    printfn "== Day 17 =="

    let program, registers = File.ReadAllText("inputs/day17.txt") |> parseProgram

    printfn "Part 1: %s" (part1 program registers)
    printfn "Part 2: %d" (part2 program)
    printfn ""
