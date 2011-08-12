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
            tempBuffer = []
        }
        let rec parse stateFunc s =
            match stateFunc s with
            | Next (next, s1) -> parse next s1
            | End s2 -> s2
        parse dataState state
