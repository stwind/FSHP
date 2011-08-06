namespace FSHP

open FSHP
open FSHP.TokenTypes
open FSHP.ParserStates
open FSHP.Parser

[<AutoOpen>]
module Tokenizer =

    let tokenize stream =
        let state = { 
            currentPos = 0 
            inputStream = stream 
            tokenStream = []
            charToken = []
        }
        let rec parse stateFunc s =
            match consumeChar s with
            | Some (s0, c) -> 
                match stateFunc s0 c with 
                | Next (next, s1) -> parse next s1
            | _ -> s
        parse dataState state
