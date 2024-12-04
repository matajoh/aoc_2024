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