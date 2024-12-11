module Day09

open System.IO
open System.Collections.Generic

let FreeId = -1

type Item =
    { Id: int
      Pos: int
      Size: int }

    static member isFile i = i.Id <> FreeId

    static member isFreeSpace i = i.Id = FreeId

    static member checksum i =
        if i.Id < 0 then
            uint64 0
        else
            let a = uint64 i.Id
            let b = uint64 i.Pos
            let c = uint64 i.Size
            a * (c * b + (c * (c - (uint64 1))) / (uint64 2))

    static member take n i =
        { i with
            Pos = i.Pos + n
            Size = i.Size - n }


let findLastBy pred start (diskMap: LinkedList<Item>) =
    seq {
        let mutable current = diskMap.Last

        while current <> diskMap.First && current.Value.Pos > start do
            if pred current.Value then
                yield current

            current <- current.Previous
    }
    |> Seq.tryHead


let rec cleanDisk (diskMap: LinkedList<Item>) (node: LinkedListNode<Item>) =
    if node = diskMap.Last then
        diskMap
    elif Item.isFile node.Value then
        cleanDisk diskMap node.Next
    elif Item.isFreeSpace diskMap.Last.Value then
        diskMap.RemoveLast()
        cleanDisk diskMap node
    else
        let free = node.Value

        match findLastBy Item.isFile free.Pos diskMap with
        | Some fit ->
            let file = fit.Value

            if file.Size < free.Size then
                diskMap.AddBefore(node, { file with Pos = free.Pos }) |> ignore
                node.Value <- Item.take file.Size free
                fit.Value <- { file with Id = FreeId }
                cleanDisk diskMap node
            elif file.Size > free.Size then
                diskMap.AddBefore(fit, { free with Pos = file.Pos }) |> ignore
                node.Value <- { free with Id = file.Id }
                fit.Value <- Item.take free.Size file
                cleanDisk diskMap node.Next
            else
                node.Value <- { free with Id = file.Id }
                fit.Value <- { file with Id = FreeId }
                cleanDisk diskMap node.Next
        | None -> diskMap


let part1 diskMap =
    cleanDisk diskMap diskMap.First |> Seq.map Item.checksum |> Seq.sum


let rec defragDisk (diskMap: LinkedList<Item>) (node: LinkedListNode<Item>) =
    if node = diskMap.Last then
        diskMap
    elif Item.isFile node.Value then
        defragDisk diskMap node.Next
    elif Item.isFreeSpace diskMap.Last.Value then
        diskMap.RemoveLast()
        defragDisk diskMap node
    else
        let free = node.Value
        let fits i = Item.isFile i && i.Size <= free.Size

        match findLastBy fits free.Pos diskMap with
        | Some fit ->
            let file = fit.Value
            fit.Value <- { file with Id = FreeId }

            if file.Size < free.Size then
                diskMap.AddBefore(node, { file with Pos = free.Pos }) |> ignore
                node.Value <- Item.take file.Size free
                defragDisk diskMap node
            else
                node.Value <- { free with Id = file.Id }
                defragDisk diskMap node.Next
        | None -> defragDisk diskMap node.Next


let part2 diskMap =
    defragDisk diskMap diskMap.First |> Seq.map Item.checksum |> Seq.sum


let run =
    printfn "== Day 09 =="

    let cumSum (sum, _) x = sum + x, (sum, x)

    let diskMap =
        File.ReadAllText("inputs/day09.txt")
        |> Seq.map (fun c -> int (c - '0'))
        |> Seq.scan cumSum (0, (0, 0))
        |> Seq.skip 1
        |> Seq.map snd
        |> Seq.indexed
        |> Seq.map (fun (i, (pos, size)) ->
            if i % 2 = 0 then
                { Id = i / 2; Pos = pos; Size = size }
            else
                { Id = FreeId; Pos = pos; Size = size })
        |> Seq.toList

    printfn "Part 1: %d" (part1 (LinkedList diskMap))
    printfn "Part 2: %d" (part2 (LinkedList diskMap))
    printfn ""
