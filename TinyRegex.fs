#light

// TinyRegex: A tiny regular matcher using derivatives in F#
module TinyRegex = 

    type Regex =
        | Empty
        | Char of char
        | Alt of Regex * Regex
        | Seq of Regex * Regex
        | Rep of Regex

    let (|TK|_|) token tokens =
        match tokens with
        | t :: rest when t = token -> Some(rest)
        | _ -> None

    let (|CH|_|) tokens =
        let special = function
            | '(' | ')' | '*' | '|' -> true
            | _ -> false
        match tokens with
        | t :: rest when not(special t) -> Some(t, rest)
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
        | Factor(e1, Term(e2, rest)) -> Some(Seq(e1, e2), rest)
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

    let parseRegex regex =
        match Seq.toList regex with
        | Expr(e, []) -> e
        | _ -> failwith "Not a valid regex"