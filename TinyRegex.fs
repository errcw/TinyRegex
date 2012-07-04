// TinyRegex: A tiny regular matcher using derivatives in F#
module TinyRegex

// A regular expression is:
// <regex> = <term> '|' <regex>
//         | <term>
// <term> = { <factor> }
// <factor> = <base> [ '*' ]
// <base> = <char>
//        | '(' <regex> ')'

type Regex =
    | Empty
    | Null
    | Char of char
    | Alt of Regex * Regex
    | Seq of Regex * Regex
    | Rep of Regex * Regex

type Tokens =
    | Char of char
    | LParen
    | RParen
    | Star
    | Pipe

let tokenize (input : string) =
    input |> Seq.map (function
        | '(' -> LParen
        | ')' -> RParen
        | '*' -> Star
        | '|' -> Pipe
        | c -> Char c)
