namespace FSHP

open FSHP
open FSHP.TokenTypes

module Parser =
    type State = {
            currentPos : int
            inputStream : string
            tokenStream : Token list
            charToken : char list
        }

    type Next<'s, 'c> = Next of ('s -> 'c -> Next<'s, 'c>) * 's

    let consumeChar state =
        let cp, is = state.currentPos, state.inputStream
        if cp < is.Length
            then Some ({ state with currentPos = cp + 1 }, is.[cp])
            else None

    let unconsumeChar state =
        let cp, is = state.currentPos, state.inputStream
        { state with currentPos = cp - 1 }, is.[cp - 1]

    let emitCharToken state c =
        { state with charToken = c :: state.charToken }

    let (|NotCharRef|_|) extra input =
        let allowed = [Char.HT; Char.LF; Char.FF; Char.SP; '<'; '&'; Char.NUL]
                      @ extra
        if List.exists (fun x -> x = input) allowed
            then Some ()
            else None

module ParserStates  =

    open Parser

    // 8.2.4.69 Tokenizing character references
    let consumeCharRef state extra =
        match consumeChar state with
        | Some (s1, c) ->
            match c with
            | NotCharRef extra -> None
            | '#' -> Some ('#', s1)
            | _ -> None
        | _ -> None

    // 8.2.4.1 Data State
    let rec dataState state = function
            | '&' -> Next (charRefInDataState, state)
            | 'a' -> Next (tagOpenState, state)
            | _ -> Next (dataState, state)

    // 8.2.4.2 Character reference in data state
    and charRefInDataState state _ =
        let s, _ = unconsumeChar state
        match consumeCharRef s [] with
        | Some (c, s1) -> Next (dataState, emitCharToken s1 c)
        | _ -> Next (dataState, emitCharToken s '&')

    // 8.2.4.8 Tag open state
    and tagOpenState state = function
        | '1' -> Next (dataState, state)
        | _ -> Next (dataState, state)
