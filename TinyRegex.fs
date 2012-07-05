#light

// TinyRegex: A tiny regular matcher using derivatives in F#
module TinyRegex

type Regex =
    | Char of char
    | Alt of Regex * Regex
    | Seq of Regex * Regex
    | Rep of Regex
    | Empty
    | Null

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

let parse re =
    match Seq.toList re with
    | Expr(e, []) -> e
    | _ -> failwith "Not a valid regular expression"


// Simplifying constructors
let seq e1 e2 =
    match e1, e2 with
    | (Null, _) | (_, Null) -> Null
    | (Empty, e) | (e, Empty) -> e
    | _ -> Seq(e1, e2)

let alt e1 e2 =
    match e1, e2 with
    | (Null, e) | (e, Null) -> e
    | _ -> Alt(e1, e2)

let rep e =
    match e with
    | Null -> Empty
    | Empty -> Empty
    | _ -> Rep e

// Returns Empty if the given expression accepts the empty string, otherwise Null
let rec empty = function
    | Empty -> Empty
    | Null -> Null
    | Char c -> Null
    | Alt(e1, e2) -> alt (empty e1) (empty e2)
    | Seq(e1, e2) -> seq (empty e1) (empty e2)
    | Rep e -> Empty

// Returns the derivative of an expression with respect to a character
let rec derivative re c =
    match re with
    | Empty -> Null
    | Null -> Null
    | Char ch when ch = c -> Empty
    | Alt(e1, e2) -> alt (derivative e1 c) (derivative e2 c)
    | Seq(e1, e2) -> alt (seq (derivative e1 c) e2) (seq (empty e1) (derivative e2 c)) 
    | Rep e -> seq (derivative e c) (rep e)
    | _ -> Null

let rec matchesR re str =
    match str with
    | [] -> (empty re) = Empty
    | c :: rest -> matchesR (derivative re c) rest

let matches re str =
    matchesR (parse re) (Seq.toList str)