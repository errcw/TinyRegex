#light

// TinyRegex: A tiny regular matcher using derivatives in F#
module TinyRegex = 

    type Regex =
        | Empty
        | Char of char
        | Alt of Regex * Regex
        | Seq of Regex * Regex
        | Rep of Regex

    // Consumes the given token and returns the rest
    let (|TK|_|) (token : char) (tokens : char list) = function
        | t :: rest when t = token -> Some(rest)
        | _ -> None

    // Consumes any character and returns the character and the rest
    let (|CH|_|) tokens = function
        | t :: rest -> Some(t, rest)
        | _ -> None

    // A regular expression is:
    // <regex> = <term> '|' <regex>
    //         | <term>
    // <term> = { <factor> }
    // <factor> = <base> [ '*' ]
    // <base> = <char>
    //        | '(' <regex> ')'
    let rec (|Expr|_|) t =
        match t with
        | Term(e1, TK '|' (Expr(e2, rest))) -> Some(Alt(e1, e2), rest)
        | Term(e, rest) -> Some(e, rest)
        | _ -> None

    and (|Term|_|) t =
        match t with
        | Factor(e1, Factor(e2, rest)) -> Some(Seq(e1, e2), rest)
        | Factor(e, rest) -> Some(e, rest)
        | _ -> None

    and (|Factor|_|) t =
        match t with
        | Base(e, TK '*' rest) -> Some(Rep e, rest)
        | Base(e, rest) -> Some(e, rest)
        | _ -> None

    and (|Base|_|) t =
        match t with
        | TK '(' (Expr(e, TK ')' rest)) -> Some(e, rest)
        | CH(ch, rest) -> Some(Char ch, rest)
        | _ -> None


    // Testing
    match Seq.toList "(abc" with
    | Expr(e, []) -> printfn "%A" e
    | _ -> printf "No match"