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
            tempBuffer : char list
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

    let consumeChars count state =
        let rec consume s acc = function
            | 0 -> s, List.rev acc
            | n -> match consumeChar s with
                   | s1, Char.NUL -> consume s1 acc 0
                   | s1, c -> (c :: acc) |> consume s1 <| n - 1
        consume state [] count

    let isFollowedBy (str : string) state =
        let cp, is = state.currentPos, state.inputStream
        is.[cp .. str.Length] = str

    let rollback state =
        match state.currentPos > 0 with
        | true -> { state with currentPos = state.currentPos - 1 }
        | _ -> state

    let reconsumeChar state = state |> rollback |> consumeChar

    let consumeCharUpTo chars state =
        let rec consume s acc =
            match consumeChar s with
            | s, c when List.exists (fun x -> x = c) chars -> rollback s, List.rev acc
            | s, other -> consume s (other :: acc)
        consume state []

    let consumeUntilMatch str state =
        let rec consume (current : string) s0 acc =
            match consumeChar s0 with
            | s, c when c = current.[0] && current.Length <> 1 ->
                consume current.[1..] s (c :: acc)
            | s, c when c = current.[0] && current.Length = 1 || c = Char.NUL ->
                s, c :: acc
            | s, c -> 
                consume str state (c :: acc)
        consume str state []

    let emitCharToken c state =
        { state with charToken = c :: state.charToken }

    let rec emitCharTokens chars state =
        match chars with
        | h :: rest -> state |> emitCharToken h |> emitCharTokens rest
        | [] -> state

    let emitToken token state = 
        { state with tokenStream = token :: state.tokenStream }

    let emitCurrentToken state =
        { emitToken state.currentToken.Value state with currentToken = None }

    let parseError reason state =
        let err = { position = state.currentPos; reason = reason  }
        { state with parseError = err :: state.parseError }

    let newToken token state = 
        { state with currentToken = token  }

    let appendToTokenName c state =
        match state.currentToken with
        | Some (StartTag (name, attrs, selfClosing)) ->  
            (name + string c, attrs, selfClosing) |> StartTag |> Some |> newToken <| state
        | Some (Comment name) ->
            name + string c |> Comment |> Some |> newToken <| state
        | Some (DocType (name, pid, sid, quirk)) ->
            (name + string c, pid, sid, quirk) |> DocType |> Some |> newToken <| state
        | _ -> state

    let appendCharsToTokenName (chars : char list) state =
        let rec append state = function
            | c :: rest -> 
                state |> appendToTokenName c |> append <| rest
            | [] -> state
        append state chars

    let setSelfClosing state =
        match state.currentToken with
        | Some (StartTag (name, attrs, selfClosing)) ->  
            (name, attrs, true) |> StartTag |> Some |> newToken <| state
        | _ -> state

    let setQuirk newVal state =
        match state.currentToken with
        | Some (DocType (name, pid, sid, _)) ->  
            (name, pid, sid, newVal) |> DocType |> Some |> newToken <| state
        | _ -> state

    let setPid newVal state =
        match state.currentToken with
        | Some (DocType (name, pid, sid, quirk)) ->  
            (name, newVal, sid, quirk) |> DocType |> Some |> newToken <| state
        | _ -> state
        
    let appendPid c state =
        match state.currentToken with
        | Some (DocType (name, pid, sid, quirk)) ->  
            (name, pid + string c, sid, quirk) |> DocType |> Some |> newToken <| state
        | _ -> state

    let setSid newVal state =
        match state.currentToken with
        | Some (DocType (name, pid, sid, quirk)) ->  
            (name, pid, newVal, quirk) |> DocType |> Some |> newToken <| state
        | _ -> state

    let appendSid c state =
        match state.currentToken with
        | Some (DocType (name, pid, sid, quirk)) ->  
            (name, pid, sid + string c, quirk) |> DocType |> Some |> newToken <| state
        | _ -> state

    let newAttribute (k, v) state =
        match state.currentToken with
        | Some (StartTag (name, attrs, selfClosing)) ->  
            (name, HtmlAttr (k, v) :: attrs, selfClosing) |> StartTag |> Some |> newToken <| state
        | _ -> state

    let appendToCurrentAttrName (c : char) state =
        match state.currentToken with
        | Some (StartTag (name, attrs, selfClosing)) ->  
            match attrs with
            | [] -> state
            | HtmlAttr (k, v) :: _ -> 
                (name, HtmlAttr (k + string c, v) :: attrs, selfClosing) 
                    |> StartTag |> Some |> newToken <| state
        | _ -> state

    let appendToCurrentAttrValue (c : char) state =
        match state.currentToken with
        | Some (StartTag (name, attrs, selfClosing)) ->  
            match attrs with
            | [] -> state
            | HtmlAttr (k, v) :: _ -> 
                (name, HtmlAttr (k, v + string c) :: attrs, selfClosing) 
                    |> StartTag |> Some |> newToken <| state
        | _ -> state

    let setTempBuffer buf state =
        { state with tempBuffer = buf }

    let appendToTempBuffer c state =
        { state with tempBuffer = c :: state.tempBuffer |> List.rev }

    let isTempBufferMatch str state = 
        String(Array.ofList state.tempBuffer) = str

    let findLastStartTag state =
        let rec find tokens =
            match tokens with
            | [] -> None
            | (StartTag (_, _, _) as h) :: _ -> Some h
            | _ :: rest -> find rest
        find state.tokenStream

    let isCurrentTokenEndTag state =
        match (state.currentToken, findLastStartTag state) with
        | Some (EndTag name0), Some (StartTag (name1, _, _)) -> name0 = name1
        | _ -> false

    let isCharInRange range input =
        if List.exists (fun x -> x = input) range
            then Some input
            else None

    let (|Lower|_|) input = isCharInRange ['a'..'z'] input

    let (|Upper|_|) input = isCharInRange ['A'..'Z'] input

    let (|NotCharRef|_|) extra input =
        let allowed = Char.HT :: Char.LF :: Char.FF :: 
                      Char.SP :: '<' :: '&' :: Char.NUL :: extra
        isCharInRange allowed input
