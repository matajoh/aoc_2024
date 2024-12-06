module Extensions

module Array2D =
    let tryGet (r, c) a =
        let rows = Array2D.length1 a
        let cols = Array2D.length2 a

        if r < 0 || r >= rows || c < 0 || c >= cols then
            None
        else
            Some(Array2D.get a r c)

    let rowColumnIndices a =
        seq {
            for r in 0 .. Array2D.length1 a - 1 do
                for c in 0 .. Array2D.length2 a - 1 do
                    yield (r, c)
        }


module List =
    let print list = List.map (fun x -> printfn "%A" x; x) list

    let binarySearch compare value list =
        let rec search left right =
            if left = right then
                ~~~left
            else
                let mid = (left + right) / 2
                let test = List.item mid list
                let cmp = compare value test
                if cmp < 0 then
                    search left mid
                elif cmp > 0 then
                    if right = mid + 1 then
                        ~~~right
                    else search mid right
                else
                    mid

        search 0 (List.length list - 1)

    let arange min max =
        seq {
            for i in min..max do
                yield i
        } |> Seq.toList
