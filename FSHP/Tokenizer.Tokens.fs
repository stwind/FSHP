namespace FSHP.Tokenizer

[<AutoOpen>]
module Tokens =

    type HtmlAttr = HtmlAttr of string * string

    type Token = 
        | StartTag of string * HtmlAttr list * bool
        | EndTag of string
        | Character of string
        | DocType of string * string * string * bool
        | Comment of string
        | EOF
