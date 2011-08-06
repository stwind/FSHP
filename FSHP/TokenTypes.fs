namespace FSHP

[<AutoOpen>]
module TokenTypes =

    type HtmlAttr = HtmlAttr of string * string

    type Token = 
        | StartTag of string * HtmlAttr list * bool
        | EndTag of string
        | TextNode of string
        | DocType of string
        | Comment of string
