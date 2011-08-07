namespace FSHP

[<AutoOpen>]
module TokenTypes =

    type HtmlAttr = HtmlAttr of string * string

    type Token = 
        | StartTag of char list * HtmlAttr list * bool
        | EndTag of string
        | Character of string
        | DocType of string
        | Comment of string
        | EOF
