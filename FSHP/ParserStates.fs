namespace FSHP

open System
open FSHP
open FSHP.TokenTypes

module Parser =
    type ParseError = {
            position : int
            reason : string
        }

    type State = {
            currentPos : int
            inputStream : string
            currentToken : Token option
            tokenStream : Token list
            charToken : char list
            parseError : ParseError list
        }

    type Next<'s> =
        | Next of ('s -> Next<'s>) * 's
        | End of 's

    let consumeChar state =
        let cp, is = state.currentPos, state.inputStream
        let c = if cp < is.Length
                    then is.[cp]
                    else Char.NUL
        { state with currentPos = cp + 1 }, c

    let unconsumeChar state =
        let cp, is = state.currentPos, state.inputStream
        { state with currentPos = cp - 1 }, is.[cp - 1]

    let emitCharToken state c =
        { state with charToken = c :: state.charToken }

    let emitToken state token = 
        { state with tokenStream = token :: state.tokenStream }

    let parseError state reason =
        let err = { position = state.currentPos; reason = reason  }
        { state with parseError = err :: state.parseError }

    let newToken state token = 
        { state with currentToken = token  }

    let isCharInRange range input =
        if List.exists (fun x -> x = input) range
            then Some input
            else None

    let (|Lower|_|) input = isCharInRange ['a'..'z'] input

    let (|Upper|_|) input = isCharInRange ['A'..'Z'] input

    let (|NotCharRef|_|) extra input =
        let allowed = [Char.HT; Char.LF; Char.FF; Char.SP; '<'; '&'; Char.NUL]
                      @ extra
        isCharInRange allowed input

module ParserStates  =

    open Parser

    // 8.2.4.69 Tokenizing character references
    let consumeCharRef state extra =
        match consumeChar state with
        | s1, NotCharRef extra _ -> None
        | s1, '#' -> Some ('#', s1)
        | _ -> None

    // 8.2.4.1 Data State
    // not handling the U+0000 case, as we use it as the EOF
    let rec dataState state =
        match consumeChar state with
        | s, '&' -> Next (charRefInDataState, s)
        | s, '<' -> Next (tagOpenState, s)
        | s, Char.NUL -> emitToken s EOF |> End
        | s, other -> Next (dataState, emitCharToken s other)

    // 8.2.4.2 Character reference in data state
    and charRefInDataState state =
        match consumeCharRef state [] with
        | Some (c, s1) -> Next (dataState, emitCharToken s1 c)
        | _ -> Next (dataState, emitCharToken state '&')

    // 8.2.4.8 Tag open state
    and tagOpenState state =
        match consumeChar state with
        | s, '!' -> Next (dataState, s)
        | s, '/' -> Next (dataState, s)
        | s, Lower c | s, Upper c -> 
            Next (dataState, ([Char.ToLower c], [], false) |> StartTag |> Some |> newToken state)
        | s, '?' -> Next (dataState, parseError state "? in tagOpenState")
        | s, _ -> Next (dataState, s)
