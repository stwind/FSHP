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
            currentToken = None
            tokenStream = []
            charToken = []
            parseError = []
        }
        let rec parse stateFunc s =
            match stateFunc s with
            | Next (next, s1) -> parse next s1
            | End s2 -> s2
            //match consumeChar s with
            //| Some (s0, c) -> 
                //match stateFunc s0 c with 
                //| Next (next, s1) -> parse next s1
            //| _ -> s
        parse dataState state
