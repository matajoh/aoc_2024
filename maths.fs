module Maths

let rec gcd a b = if b = 0 then a else gcd b (a % b)

let lcm a b = a * (b / gcd a b)

let rec gcdBigInt a b =
    if b = bigint 0 then a else gcdBigInt b (a % b)

let lcmBigInt a b = a * (b / gcdBigInt a b)

type Sign =
    | Plus
    | Minus
    | Zero

    static member from n =
        if n < 0L then Minus
        elif n > 0L then Plus
        else Zero


[<CustomComparison>]
[<CustomEquality>]
type Rational =
    { Num: bigint
      Den: bigint
      S: Sign }

    static member Zero =
        { Num = bigint 0
          Den = bigint 1
          S = Plus }

    static member One =
        { Num = bigint 1
          Den = bigint 1
          S = Plus }

    static member toInt r =
        match r.Den = bigint 1, r.S with
        | true, Plus -> Some(int64 r.Num)
        | true, Minus -> Some -(int64 r.Num)
        | true, Zero -> Some 0L
        | _ -> None

    static member toDecimal r =
        match r.S with
        | Plus -> decimal r.Num / decimal r.Den
        | Minus -> -(decimal r.Num / decimal r.Den)
        | Zero -> 0m

    override this.ToString() =
        if this.Den = bigint 1 then
            if this.S = Minus then
                sprintf "-%A" this.Num
            else
                this.Num.ToString()
        else if this.S = Plus then
            sprintf "%A/%A" this.Num this.Den
        elif this.S = Minus then
            sprintf "-%A/%A" this.Num this.Den
        else
            "0"

    override this.Equals other = compare this (other :?> Rational) = 0

    override this.GetHashCode() =
        let r = (this.Num, this.Den)
        r.GetHashCode()

    static member invert r = { r with Num = r.Den; Den = r.Num }

    static member private simplify r =
        assert (r.Den <> bigint 0)

        if r.Num = bigint 0 then
            { r with Den = bigint 1; S = Zero }
        elif r.Num = bigint 1 || r.Num = bigint 1 then
            r
        else
            match gcdBigInt r.Num r.Den with
            | div when div = (bigint 1) -> r
            | div ->
                { r with
                    Num = r.Num / div
                    Den = r.Den / div }

    static member private den r0 r1 =
        match r0.Den, r1.Den with
        | d0, d1 when d0 = (bigint 1) -> d1
        | d0, d1 when d1 = (bigint 1) -> d0
        | d0, d1 -> lcmBigInt d0 d1

    static member add a b =
        match a.S, b.S with
        | Zero, Zero -> Rational.Zero
        | Zero, _ -> b
        | _, Zero -> a
        | s0, s1 ->
            let den = Rational.den a b
            let an = (den / a.Den) * a.Num
            let bn = (den / b.Den) * b.Num

            if s0 = s1 then
                { Num = an + bn; Den = den; S = s0 } |> Rational.simplify
            elif an > bn then
                { Num = an - bn; Den = den; S = s0 } |> Rational.simplify
            elif an < bn then
                { Num = bn - an; Den = den; S = s1 } |> Rational.simplify
            else
                Rational.Zero

    static member subtract a b = Rational.add a (Rational.negate b)

    static member negate r =
        match r.S with
        | Zero -> r
        | Plus -> { r with S = Minus }
        | Minus -> { r with S = Plus }

    static member multiply a b =
        match a.S, b.S with
        | Zero, _ -> Rational.Zero
        | _, Zero -> Rational.Zero
        | s0, s1 ->
            let lhs = { a with Den = b.Den } |> Rational.simplify
            let rhs = { b with Den = a.Den } |> Rational.simplify
            let sign = if s0 = s1 then Plus else Minus

            { Num = lhs.Num * rhs.Num
              Den = lhs.Den * rhs.Den
              S = sign }
            |> Rational.simplify

    static member divide a b = Rational.multiply a (Rational.invert b)

    static member abs r = { r with S = Plus }

    static member isWhole r = r.Den = bigint 1

    static member (+)(a, b) = Rational.add a b

    static member (-)(a, b) = Rational.subtract a b

    static member (~-) r = Rational.negate r

    static member (*)(a, b) = Rational.multiply a b

    static member (/)(a, b) = Rational.divide a b

    interface System.IComparable with
        member a.CompareTo other =
            let b =
                match other with
                | :? Rational as r -> r
                | :? uint64 as n ->
                    { Num = bigint n
                      Den = bigint 1
                      S = Plus }
                | :? int64 as n ->
                    { Num = bigint (abs n)
                      Den = bigint 1
                      S = Sign.from n }
                | _ -> failwith "Invalid comparison"

            match a.S, b.S with
            | Plus, Minus -> 1
            | Minus, Plus -> -1
            | Plus, Zero -> 1
            | Minus, Zero -> -1
            | Zero, Plus -> -1
            | Zero, Minus -> 1
            | Zero, Zero -> 0
            | _ ->
                let den = Rational.den a b
                let an = (den / a.Den) * a.Num
                let bn = (den / b.Den) * b.Num
                if a.S = Plus then compare an bn else compare bn an

    static member sign r =
        match r.S with
        | Plus -> 1
        | Minus -> -1
        | Zero -> 0

    static member fromBigInt v =
        if v < (bigint 0) then
            { Num = v; Den = bigint 1; S = Minus }
        elif v > (bigint 0) then
            { Num = v; Den = bigint 1; S = Plus }
        else
            Rational.Zero

    static member fromInt(i: int) = Rational.fromBigInt (bigint i)

    static member tryParse(s: string) =
        let v = ref (bigint 0)

        if bigint.TryParse(s, v) then
            Some(Rational.fromBigInt v.Value)
        else
            None

    static member parse s =
        match Rational.tryParse s with
        | Some r -> r
        | _ -> failwith "Invalid integer string"

    static member isZero r = r.S = Zero


type rational = Rational
