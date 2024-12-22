module Day21

open System
open System.IO

open Extensions
open System.Collections.Generic


type Keypad =
    { Labels: Map<char, Position>
      Buttons: Set<Position>
      Empty: Position }

    static member button c k = Map.find c k.Labels

    static member ofGrid g =
        let labels = g.Table |> Map.toList |> List.map (fun (k, v) -> v, k) |> Map.ofList
        let buttons = g.Table |> Map.keys |> set
        let empty = Grid.findValue ' ' g |> List.head

        { Labels = labels
          Buttons = buttons
          Empty = empty }


let Numbers = [ "789"; "456"; "123"; " 0A" ] |> List.toGrid |> Keypad.ofGrid

let Directions = [ " ^A"; "<v>" ] |> List.toGrid |> Keypad.ofGrid


let minMovesAtLevel maxLevel =
    let cache = new Dictionary<int * char * char, bigint>()

    let rec f l (a, b) =
        let key = l, a, b
        let keypad = if l = maxLevel then Numbers else Directions

        if not (cache.ContainsKey(key)) then
            if l = 0 then
                cache.[key] <- bigint 1
            else
                let s = Keypad.button a keypad
                let g = Keypad.button b keypad
                let dr = g.Row - s.Row
                let dc = g.Column - s.Column

                let hmoves = List.replicate (abs dc) (if dc < 0 then '<' else '>')
                let vmoves = List.replicate (abs dr) (if dr < 0 then '^' else 'v')

                let count a b =
                    [ [ 'A' ]; a; b; [ 'A' ] ]
                    |> List.concat
                    |> List.pairwise
                    |> List.sumBy (f (l - 1))

                if { s with Column = g.Column } = keypad.Empty then
                    cache.[key] <- count vmoves hmoves
                elif { s with Row = g.Row } = keypad.Empty then
                    cache.[key] <- count hmoves vmoves
                else
                    let hfirst = count hmoves vmoves
                    let vfirst = count vmoves hmoves
                    cache.[key] <- min hfirst vfirst

        cache.[key]

    f


let minLength minMoves level code =
    ('A' :: Seq.toList code) |> List.pairwise |> List.sumBy (minMoves level)


let complexity levels (code: String) =
    let number = code.[0..2] |> int |> bigint
    let minMoves = minMovesAtLevel levels
    let length = code |> minLength minMoves levels
    number * length


let part1 codes = codes |> List.sumBy (complexity 3)


let part2 codes = codes |> List.sumBy (complexity 26)


let run =
    printfn "== Day 21 =="

    let codes = File.ReadLines("inputs/day21.txt") |> Seq.toList

    printfn "Part 1: %A" (part1 codes)
    printfn "Part 2: %A" (part2 codes)
    printfn ""
