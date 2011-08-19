namespace FSHP.Tokenizer

open System
open FSHP
open FSHP.Tokenizer.Tokens
open FSHP.Tokenizer.Actions

module States  =

    type StateDescAttribute(name) = 
        inherit Attribute ()
        member this.name = name

    // 8.2.4.69 Tokenizing character references
    [<StateDesc("Tokenizing character references")>]
    let consumeCharRef state extra =
        match consumeChar state with
        | _, NotCharRef extra _ -> None
        | s, '#' -> Some ('#', s)
        | _ -> None

    // 8.2.4.1 Data State
    // not handling the U+0000 case, as we use it as the EOF
    [<StateDesc("Data State")>]
    let rec dataState state =
        match consumeChar state with
        | s, '&' -> 
            Next (charRefInDataState []), s
        | s, '<' -> 
            Next (tagOpenState), s
        | s, Char.NUL -> 
            //emitToken EOF s |> End
            Next (eofState), s
        | s, other -> 
            Next (dataState), s |> emitCharToken other

    // 8.2.4.2 Character reference in data state
    and [<StateDesc("Character reference in data state")>]
        charRefInDataState extra state =
        match consumeCharRef state extra with
        | Some (c, s1) -> 
            Next (dataState), s1 |> emitCharToken c
        | _ -> 
            Next (dataState), state |> emitCharToken '&'

    // 8.4.2.3  RCDATA state
    and [<StateDesc("RCDATA state")>]
        rcDataState state =
        match consumeChar state with
        | s, '&' -> 
            Next (charRefInRCDataState), s
        | s, '<' -> 
            Next (rcDataLTSignState), s
        | s, Char.NUL ->
            Next (eofState), s
        | s, other -> 
            Next (rcDataState), s |> emitCharToken other

    // 8.2.4.4 Character reference in RCDATA state
    and [<StateDesc("Character reference in RCDATA state")>]
        charRefInRCDataState state =
        match consumeCharRef state [] with
        | Some (c, s1) -> 
            Next (rcDataState), s1 |> emitCharToken c
        | _ -> 
            Next (rcDataState), state |> emitCharToken '&'

    // 8.2.4.5 RAWTEXT state
    and [<StateDesc("RAWTEXT state")>]
        rawTextState state =
        match consumeChar state with
        | s, '<' ->
            Next (rawTextLTSignState), s
        | s, Char.NUL ->
            Next (eofState), s
        | s, other -> 
            Next (rawTextState), s |> emitCharToken other

    // 8.2.4.6 Script data state
    and [<StateDesc("Script data state")>]
        scriptDataState state =
        match consumeChar state with
        | s, '<' ->
            Next (scriptDataLTSignState), s
        | s, Char.NUL ->
            Next (eofState), s
        | s, other -> 
            Next (scriptDataState), s |> emitCharToken other

    // 8.2.4.7 PLAINTEXT state
    and [<StateDesc("PLAINTEXT state")>]
        plainTextState state =
        match consumeChar state with
        | s, Char.NUL ->
            Next (eofState), s
        | s, other -> 
            Next (plainTextState), s |> emitCharToken other

    // 8.2.4.8 Tag open state
    and [<StateDesc("Tag open state")>]
        tagOpenState state =
        match consumeChar state with
        | s, '!' -> 
            Next (markupDecOpenState), s
        | s, '/' -> 
            Next (endTagOpenState), s
        | s, Lower c | s, Upper c -> 
            Next (tagNameState), (Char.ToLower c |> string, [], false) |> StartTag |> Some |> newToken <| s
        | s, '?' -> 
            Next (bogusCommentState), state |> parseError "? in tagOpenState"
        | _ -> 
            Next (dataState), state |> parseError "error in tagOpenState" |> emitCharToken '<'

    // 8.2.4.9 End Tag Open State
    and [<StateDesc("End Tag Open State")>]
        endTagOpenState state =
        match consumeChar state with
        | s, Lower c | s, Upper c ->
            Next (tagNameState), Char.ToLower c |> string |> EndTag |> Some |> newToken <| s
        | s, '<' -> 
            Next (dataState), s |> parseError "< in endTagOpenState"
        | s, Char.NUL ->
            Next (dataState), state |> emitCharToken '<' |> emitCharToken '/'
        | s, _ -> 
            Next (bogusCommentState), state |> parseError "error in endTagOpenState"

    // 8.2.4.10 Tag name state
    and [<StateDesc("Tag name state")>]
        tagNameState state =
        match consumeChar state with
        | s, Char.HT | s, Char.LF | s, Char.FF | s, Char.SP ->
            Next (beforeAttrNameState), s
        | s, '/' ->
            Next (selfClosingStartTagState), s
        | s, '>' ->
            Next (dataState), emitCurrentToken s
        | s, Upper c ->
            Next (tagNameState), s |> appendToTokenName (Char.ToLower c)
        | s, Char.NUL ->
            Next (dataState), s |> parseError "EOF in tagNameState"
        | s, c ->
            Next (tagNameState), s |> appendToTokenName c

    // 8.2.4.11 RCDATA less-than sign state
    and [<StateDesc("RCDATA less-than sign state")>]
        rcDataLTSignState state =
        match consumeChar state with
        | s, '/' ->
            Next (rcEndTagOpenState), s |> setTempBuffer []
        | _ -> 
            Next (rcDataState), state |> emitCharToken '<'

    // 8.2.4.12 RCDATA end tag open state
    and [<StateDesc("RCDATA end tag open state")>]
        rcEndTagOpenState state =
        match consumeChar state with
        | s, Lower c | s, Upper c ->
            Next (rcEndTagNameState), s |> newToken (c |> Char.ToLower |> string |> EndTag |> Some)
                                       |> appendToTempBuffer c
        | _ ->
            Next (rcDataState), state |> emitCharTokens ['<';'/']

    // 8.2.4.13 RCDATA end tag name state
    and [<StateDesc("RCDATA end tag name state")>]
        rcEndTagNameState state =
        match consumeChar state with
        | s, Char.HT | s, Char.LF | s, Char.FF | s, Char.SP when isCurrentTokenEndTag s ->
            Next (beforeAttrNameState), s
        | s, '/' when isCurrentTokenEndTag s ->
            Next (selfClosingStartTagState), s
        | s, '>' when isCurrentTokenEndTag s ->
            Next (dataState), s |> emitCurrentToken
        | s, Lower c | s, Upper c ->
            Next (rcEndTagNameState), s |> appendToTokenName (Char.ToLower c) 
                                       |> appendToTempBuffer c
        | _ -> 
            Next (rcDataState), state |> emitCharTokens (['<';'/'] @ state.tempBuffer)

    // 8.2.4.14 RAWTEXT less-than sign state
    and [<StateDesc("RAWTEXT less-than sign state")>]
        rawTextLTSignState state =
        match consumeChar state with
        | s, '/' ->
            Next (rawTextEndTagOpenState), s |> setTempBuffer []
        | _ -> 
            Next (rawTextState), state |> emitCharToken '<'

    // 8.2.4.15 RAWTEXT end tag open state
    and [<StateDesc("RAWTEXT end tag open state")>]
        rawTextEndTagOpenState state =
        match consumeChar state with
        | s, Lower c | s, Upper c ->
            Next (rawTextEndTagNameState), s |> newToken (c |> Char.ToLower |> string |> EndTag |> Some)
                                            |> appendToTempBuffer c
        | _ -> 
            Next (rawTextState), state |> emitCharTokens ['<';'/']

    // 8.2.4.16 RAWTEXT end tag name state
    and [<StateDesc("RAWTEXT end tag name state")>]
        rawTextEndTagNameState state =
        match consumeChar state with
        | s, Char.HT | s, Char.LF | s, Char.FF | s, Char.SP when isCurrentTokenEndTag s ->
            Next (beforeAttrNameState), s
        | s, '/' when isCurrentTokenEndTag s ->
            Next (selfClosingStartTagState), s
        | s, '>' when isCurrentTokenEndTag s ->
            Next (dataState), s |> emitCurrentToken
        | s, Lower c | s, Upper c ->
            Next (rawTextEndTagNameState), s |> appendToTokenName (Char.ToLower c) 
                                            |> appendToTempBuffer c
        | _ -> 
            Next (rawTextState), state |> emitCharTokens (['<';'/'] @ state.tempBuffer)

    // 8.2.4.17 Script data less-than sign state
    and [<StateDesc("Script data less-than sign state")>]
        scriptDataLTSignState state =
        match consumeChar state with
        | s, '/' ->
            Next (scriptDataEndTagOpenState), s |> setTempBuffer []
        | s, '!' ->
            Next (scriptDataEscapeStartState), s |> emitCharTokens ['<';'!']
        | _ ->
            Next (scriptDataState), state |> emitCharToken '<'

    // 8.2.4.18 Script data end tag open state
    and [<StateDesc("Script data end tag open state")>]
        scriptDataEndTagOpenState state =
        match consumeChar state with
        | s, Lower c | s, Upper c ->
            Next (scriptDataEndTagNameState), s |> newToken (c |> Char.ToLower |> string |> EndTag |> Some)
                                               |> appendToTempBuffer c
        | _ ->
            Next (scriptDataState), state |> emitCharTokens ['<';'/']

    // 8.2.4.19 Script data end tag name state
    and [<StateDesc("Script data end tag name state")>]
        scriptDataEndTagNameState state =
        match consumeChar state with
        | s, Char.HT | s, Char.LF | s, Char.FF | s, Char.SP when isCurrentTokenEndTag s ->
            Next (beforeAttrNameState), s
        | s, '/' when isCurrentTokenEndTag s ->
            Next (selfClosingStartTagState), s
        | s, '>' when isCurrentTokenEndTag s ->
            Next (dataState), s |> emitCurrentToken
        | s, Lower c | s, Upper c ->
            Next (scriptDataEndTagNameState), s |> appendToTokenName (Char.ToLower c) 
                                               |> appendToTempBuffer c
        | _ -> 
            Next (scriptDataState), state |> emitCharTokens (['<';'/'] @ state.tempBuffer)

    // 8.2.4.20 Script data escape start state
    and [<StateDesc("Script data escape start state")>]
        scriptDataEscapeStartState state =
        match consumeChar state with
        | s, '-' ->
            Next (scriptDataEscapeStartDashState), s |> emitCharToken '-'
        | _ ->
            Next (scriptDataState), state

    // 8.2.4.21 Script data escape start dash state
    and [<StateDesc("Script data escape start dash state")>]
        scriptDataEscapeStartDashState state =
        match consumeChar state with
        | s, '-' ->
            Next (scriptDataEscapedDashDashState), s |> emitCharToken '-'
        | _ ->
            Next (scriptDataState), state

    // 8.2.4.22 Script data escaped state
    and [<StateDesc("Script data escaped state")>]
        scriptDataEscapedState state =
        match consumeChar state with
        | s, '-' ->
            Next (scriptDataEscapedDashState), s |> emitCharToken '-'
        | s, '<' ->
            Next (scriptDataEscapedLTSignState), s
        | _, Char.NUL ->
            Next (dataState), state |> parseError "EOF in scriptDataEscapedState"
        | s, other ->
            Next (scriptDataEscapedState), s |> emitCharToken other

    // 8.2.4.23 Script data escaped dash state
    and [<StateDesc("Script data escaped dash state")>]
        scriptDataEscapedDashState state =
        match consumeChar state with
        | s, '-' ->
            Next (scriptDataEscapedDashDashState), s |> emitCharToken '-'
        | s, '<' ->
            Next (scriptDataEscapedLTSignState), s
        | _, Char.NUL ->
            Next (dataState), state |> parseError "EOF in scriptDataEscapedDashState"
        | s, other ->
            Next (scriptDataEscapedState), s |> emitCharToken other

    // 8.2.4.24 Script data escaped dash dash state
    and [<StateDesc("Script data escaped dash dash state")>]
        scriptDataEscapedDashDashState state =
        match consumeChar state with
        | s, '-' ->
            Next (scriptDataEscapedDashDashState), s |> emitCharToken '-'
        | s, '<' ->
            Next (scriptDataEscapedLTSignState), s
        | s, '>' ->
            Next (scriptDataState), s |> emitCharToken '>'
        | s, Char.NUL ->
            Next (dataState), state |> parseError "EOF in scriptDataEscapedDashDashState"
        | s, other ->
            Next (scriptDataEscapedState), s |> emitCharToken other

    // 8.2.4.25 Script data escaped less-than sign state
    and [<StateDesc("Script data escaped less-than sign state")>]
        scriptDataEscapedLTSignState state =
        match consumeChar state with
        | s, '/' ->
            Next (scriptDataEscapedEndTagOpenState), s |> setTempBuffer []
        | s, Lower c | s, Upper c ->
            Next (scriptDataDoubleEscapedStartState), s |> setTempBuffer []
                                                       |> appendToTempBuffer (Char.ToLower c)
                                                       |> emitCharTokens ['<';c]
        | _ ->
            Next (scriptDataEscapedState), state |> emitCharToken '<'

    // 8.2.4.26 Script data escaped end tag open state
    and [<StateDesc("Script data escaped end tag open state")>]
        scriptDataEscapedEndTagOpenState state =
        match consumeChar state with
        | s, Lower c | s, Upper c ->
            Next (scriptDataEndTagNameState), s |> newToken (c |> Char.ToLower |> string |> EndTag |> Some)
                                               |> appendToTempBuffer c
        | _ ->
            Next (scriptDataEscapedState), state |> emitCharTokens ['<';'/']

    // 8.2.4.27 Script data escaped end tag name state
    and [<StateDesc("Script data escaped end tag name state")>]
        scriptDataEscapedEndTagNameState state =
        match consumeChar state with
        | s, Char.HT | s, Char.LF | s, Char.FF | s, Char.SP when isCurrentTokenEndTag s ->
            Next (beforeAttrNameState), s
        | s, '/' when isCurrentTokenEndTag s ->
            Next (selfClosingStartTagState), s
        | s, '>' when isCurrentTokenEndTag s ->
            Next (dataState), s |> emitCurrentToken
        | s, Lower c | s, Upper c ->
            Next (scriptDataEscapedEndTagNameState), s |> appendToTokenName (Char.ToLower c) 
                                                      |> appendToTempBuffer c
        | _ -> 
            Next (scriptDataEscapedState), state |> emitCharTokens (['<';'/'] @ state.tempBuffer)

    // 8.2.4.28 Script data double escape start state
    and [<StateDesc("Script data double escape start state")>]
        scriptDataDoubleEscapedStartState state =
        match consumeChar state with
        | s, c when c = Char.HT || c = Char.LF || c = Char.FF ||
                    c = Char.SP || c = '/' || c = '>' ->
            if isTempBufferMatch "script" s
                then Next (scriptDataDoubleEscapedState), s
                else Next (scriptDataEscapedState), s |> emitCharToken c
        | s, Lower c | s, Upper c ->
            Next (scriptDataDoubleEscapedStartState), s |> appendToTempBuffer (Char.ToLower c)
                                                       |> emitCharToken c
        | _ ->
            Next (scriptDataEscapedState), state

    // 8.2.4.29 Script data double escaped state
    and [<StateDesc("Script data double escaped state")>]
        scriptDataDoubleEscapedState state =
        match consumeChar state with
        | s, '-' ->
            Next (scriptDataDoubleEscapedDashState), s |> emitCharToken '-'
        | s, '<' ->
            Next (scriptDataDoubleEscapedLTSignState), s |> emitCharToken '<'
        | s, Char.NUL ->
            Next (dataState), state |> parseError "EOF in scriptDataDoubleEscapedState"
        | s, other ->
            Next (scriptDataDoubleEscapedState), s |> emitCharToken other
            
    // 8.2.4.30 Script data double escaped dash state
    and [<StateDesc("Script data double escaped dash state")>]
        scriptDataDoubleEscapedDashState state =
        match consumeChar state with
        | s, '-' ->
            Next (scriptDataDoubleEscapedDashDashState), s |> emitCharToken '-'
        | s, '<' ->
            Next (scriptDataDoubleEscapedLTSignState), s |> emitCharToken '<'
        | s, Char.NUL ->
            Next (dataState), state |> parseError "EOF in scriptDataDoubleEscapedDashState"
        | s, other ->
            Next (scriptDataDoubleEscapedState), s |> emitCharToken other

    // 8.2.4.31 Script data double escaped dash dash state
    and [<StateDesc("Script data double escaped dash dash state")>]
        scriptDataDoubleEscapedDashDashState state =
        match consumeChar state with
        | s, '-' ->
            Next (scriptDataDoubleEscapedDashDashState), s |> emitCharToken '-'
        | s, '<' ->
            Next (scriptDataEscapedLTSignState), s |> emitCharToken '<'
        | s, '>' ->
            Next (scriptDataState), s |> emitCharToken '>'
        | s, Char.NUL ->
            Next (dataState), state |> parseError "EOF in scriptDataDoubleEscapedDashDashState"
        | s, other ->
            Next (scriptDataDoubleEscapedState), s |> emitCharToken other

    // 8.2.4.32 Script data double escaped less-than sign state
    and [<StateDesc("Script data double escaped less-than sign state")>]
        scriptDataDoubleEscapedLTSignState state =
        match consumeChar state with
        | s, '/' ->
            Next (scriptDataDoubleEscapeEndState), s |> emitCharToken '/'
        | _ ->
            Next (scriptDataDoubleEscapedState), state

    // 8.2.4.33 Script data double escape end state
    and [<StateDesc("Script data double escape end state")>]
        scriptDataDoubleEscapeEndState state =
        match consumeChar state with
        | s, (Char.HT as c) | s, (Char.LF as c) | s, (Char.FF as c)
        | s, (Char.SP as c) | s, ('/' as c) | s, ('>' as c) ->
            if isTempBufferMatch "script" s
                then Next (scriptDataEscapedState), s
                else Next (scriptDataDoubleEscapedState), s |> emitCharToken c
        | s, Lower c | s, Upper c ->
            Next (scriptDataDoubleEscapeEndState), s |> appendToTempBuffer (Char.ToLower c)
                                                    |> emitCharToken c
        | _ ->
            Next (scriptDataDoubleEscapedState), state

    // 8.2.4.34 Script data double escape end state
    and [<StateDesc("Script data double escape end state")>]
    // ignoring the U+0000 case
        beforeAttrNameState state =
        match consumeChar state with
        | s, Char.HT | s, Char.LF | s, Char.FF | s, Char.SP -> beforeAttrNameState s
        | s, '/' ->
            Next (selfClosingStartTagState), s
        | s, '>' ->
            Next (dataState), s |> emitCurrentToken
        | s, Upper c ->
            Next (attrNameState), s |> newAttribute (string (Char.ToLower c), "")
        | s, '"' | s, ''' | s, '<' | s, '=' ->
            Next (attrNameState), s |> parseError "unallowed character in beforeAttrNameState"
        | s, Char.NUL ->
            Next (dataState), state |> parseError "EOF in beforeAttrNameState"
        | s, other ->
            Next (attrNameState), s |> newAttribute (string (Char.ToLower other), "")

    // 8.2.4.35 Attribute name state
    // TODO: additional handler must be implement here, see w3 specs
    and [<StateDesc("Attribute name state")>]
        attrNameState state =
        match consumeChar state with
        | s, Char.HT | s, Char.LF | s, Char.FF | s, Char.SP ->
            Next (afterAttrNameState), s
        | s, '/' ->
            Next (selfClosingStartTagState), s
        | s, '=' ->
            Next (beforeAttrValueState), s
        | s, '>' ->
            Next (dataState), s |> emitCurrentToken
        | s, Upper c ->
            Next (attrNameState), s |> appendToCurrentAttrName c
        | s, c when c = '"' || c = ''' || c = '<' ->
            Next (attrNameState), s |> parseError "unallowed character in attrNameState"
                                   |> appendToCurrentAttrName c
        | s, Char.NUL ->
            Next (dataState), state |> parseError "EOF in beforeAttrNameState"
        | s, other ->
            Next (attrNameState), s |> appendToCurrentAttrName other

    // 8.2.4.36 After attribute name state
    and [<StateDesc("After attribute name state")>]
        afterAttrNameState state =
        match consumeChar state with
        | s, Char.HT | s, Char.LF | s, Char.FF | s, Char.SP -> afterAttrNameState s
        | s, '/' ->
            Next (selfClosingStartTagState), s
        | s, '=' ->
            Next (beforeAttrValueState), s
        | s, '>' ->
            Next (dataState), s |> emitCurrentToken
        | s, Upper c ->
            Next (attrNameState), s |> appendToCurrentAttrName c
        | s, '"' | s, ''' | s, '<' ->
            Next (attrNameState), s |> parseError "unallowed character in afterAttrNameState"
        | s, Char.NUL ->
            Next (dataState), state |> parseError "EOF in afterAttrNameState"
        | s, other ->
            Next (attrNameState), s |> newAttribute (string (Char.ToLower other), "")

    // 8.2.4.37 Before attribute value state
    and [<StateDesc("Before attribute value state")>]
        beforeAttrValueState state =
        match consumeChar state with
        | s, Char.HT | s, Char.LF | s, Char.FF | s, Char.SP -> beforeAttrValueState s
        | s, '"' ->
            Next (attrValueDoubleQuotedState), s
        | s, '&' ->
            Next (attrValueUnquotedState), s
        | s, ''' ->
            Next (attrValueSingleQuotedState), s
        | s, '>' ->
            Next (dataState), s |> emitCurrentToken
        | s, '"' | s, ''' | s, '<' ->
            Next (attrValueUnquotedState), s |> parseError "unallowed character in beforeAttrValueState"
        | s, Char.NUL ->
            Next (dataState), state |> parseError "EOF in beforeAttrValueState"
        | s, other ->
            Next (attrValueUnquotedState), s |> appendToCurrentAttrValue other

    // 8.2.4.38 Attribute value (double-quoted) state
    and [<StateDesc("Attribute value (double-quoted) state")>]
        attrValueDoubleQuotedState state =
        match consumeChar state with
        | s, '"' ->
            Next (afterAttrValueQuotedState), s
        | s, '&' ->
            Next (attrValueUnquotedState), s
        | s, Char.NUL ->
            Next (dataState), state |> parseError "EOF in attrValueDoubleQuotedState"
        | s, other ->
            Next (attrValueUnquotedState), s |> appendToCurrentAttrValue other

    // 8.2.4.39 Attribute value (single-quoted) state
    and [<StateDesc("Attribute value (single-quoted) state")>]
        attrValueSingleQuotedState state =
        match consumeChar state with
        | s, ''' ->
            Next (afterAttrValueQuotedState), s
        | s, '&' ->
            Next (charRefInAttrValueState ['''] attrValueSingleQuotedState), s
        | s, Char.NUL ->
            Next (dataState), state |> parseError "EOF in attrValueSingleQuotedState"
        | s, other ->
            Next (attrValueSingleQuotedState), s |> appendToCurrentAttrValue other

    // 8.2.4.40 Attribute value (unquoted) state
    and [<StateDesc("Attribute value (unquoted) state")>]
        attrValueUnquotedState state =
        match consumeChar state with
        | s, Char.HT | s, Char.LF | s, Char.FF | s, Char.SP ->
            Next (beforeAttrNameState), s
        | s, '&' ->
            Next (charRefInAttrValueState ['>'] attrValueUnquotedState), s
        | s, '>' ->
            Next (dataState), s |> emitCurrentToken
        | s, '"' | s, ''' | s, '<' | s, '=' | s, '`' ->
            Next (attrValueUnquotedState), s |> parseError "unallowed character in attrValueUnquotedState"
        | s, Char.NUL ->
            Next (dataState), s |> parseError "EOF in attrValueUnquotedState"
        | s, other ->
            Next (attrValueUnquotedState), s |> appendToCurrentAttrValue other

    // 8.2.4.41 Attribute value (unquoted) state
    and [<StateDesc("Attribute value (unquoted) state")>]
        charRefInAttrValueState extra back state =
        match consumeCharRef state extra with
        | Some (c, s1) -> 
            Next (back), s1 |> appendToCurrentAttrValue c
        | _ ->
            Next (back), state |> appendToCurrentAttrValue '&'

    // 8.2.4.42 After attribute value (quoted) state
    and [<StateDesc("After attribute value (quoted) state")>]
        afterAttrValueQuotedState state =
        match consumeChar state with
        | s, Char.HT | s, Char.LF | s, Char.FF | s, Char.SP ->
            Next (beforeAttrNameState), s
        | s, '/' ->
            Next (selfClosingStartTagState), s
        | s, '>' ->
            Next (dataState), s |> emitCurrentToken
        | _, Char.NUL ->
            Next (dataState), state |> parseError "EOF in afterAttrValueQuotedState"
        | _ ->
            Next (beforeAttrNameState), state |> parseError "unallowed character in afterAttrValueQuotedState"

    // 8.2.4.43 After attribute value (quoted) state
    and [<StateDesc("After attribute value (quoted) state")>]
        selfClosingStartTagState state =
        match consumeChar state with
        | s, '>' ->
            Next (dataState), s |> setSelfClosing |> emitCurrentToken
        | _, Char.NUL ->
            Next (dataState), state |> parseError "EOF in selfClosingStartTagState"
        | _ ->
            Next (beforeAttrNameState), state |> parseError "unallowed character in selfClosingStartTagState"

    // 8.2.4.44 Bogus comment state
    and [<StateDesc("Bogus comment state")>]
        bogusCommentState state =
        let s, chars = consumeCharUpTo ['<';Char.NUL] state
        let comment = String(Array.ofList chars) |> Comment |> Some |> newToken <| s
        let out = match consumeChar comment with
                  | _, Char.NUL -> comment
                  | s1, _ -> s1
        Next (dataState), out

    // 8.2.4.45 Markup declaration open state
    and [<StateDesc("Markup declaration open state")>]
        markupDecOpenState state =
        match state with
        | s when isFollowedBy "--" s ->
            let s1, _ = consumeChars 2 s
            Next (commentStartState), Comment "" |> Some |> newToken <| s1
        | s when isFollowedBy "DOCTYPE" s ->
            let s1, _ = consumeChars 7 s
            Next (doctypeState), s1
    // TODO got to check html namespace, but we temporarilly omit it here, see w3 specs
        | s when isFollowedBy "[CDATA[" s ->
            let s1, _ = consumeChars 7 s
            Next (cdataSectionState), s1
        | _ ->
            Next (bogusCommentState), state |> parseError "unallowed character in markupDecOpenState"
            
    // 8.2.4.46 Comment start state
    and [<StateDesc("Comment start state")>]
        commentStartState state =
        match consumeChar state with
        | s, '-' ->
            Next (commentStartDashState), s
        | s, '>' ->
            Next (dataState), s |> parseError "> in commentStartState" |> emitCurrentToken
        | s, Char.NUL ->
            Next (dataState), state |> parseError "EOF in commentStartState" |> emitCurrentToken
        | s, other ->
            Next (commentState), s |> appendToTokenName other 

    // 8.2.4.47 Comment start dash state
    and [<StateDesc("Comment start dash state")>]
    // FIXME seems we have trouble ignoring the U+0000 case up till now
        commentStartDashState state =
        match consumeChar state with
        | s, '-' ->
            Next (commentEndState), s
        | s, '>' ->
            Next (dataState), s |> parseError "> in commentStartDashState" |> emitCurrentToken
        | s, Char.NUL ->
            Next (dataState), state |> parseError "EOF in commentStartDashState" |> emitCurrentToken
        | s, other ->
            Next (commentState), s |> appendCharsToTokenName ['-';other]

    // 8.2.4.48 Comment state
    and [<StateDesc("Comment state")>]
        commentState state =
        match consumeChar state with
        | s, '-' ->
            Next (commentEndDashState), s
        | _, Char.NUL ->
            Next (dataState), state |> parseError "EOF in commentState" |> emitCurrentToken
        | s, other ->
            Next (commentState), s |> appendToTokenName other

    // 8.2.4.49 Comment end dash state
    and [<StateDesc("Comment end dash state")>]
        commentEndDashState state =
        match consumeChar state with
        | s, '-' ->
            Next (commentEndState), s
        | s, Char.NUL ->
            Next (dataState), state |> parseError "EOF in commentEndDashState" |> emitCurrentToken
        | s, other ->
            Next (commentState), s |> appendCharsToTokenName ['-';other]

    // 8.2.4.50 Comment end state
    and [<StateDesc("Comment end state")>]
        commentEndState state =
        match consumeChar state with
        | s, '>' ->
            Next (dataState), s |> emitCurrentToken
        | s, '!' ->
            Next (commentEndBangState), s |> parseError "! in commentEndState"
        | s, '-' ->
            Next (commentEndState), s |> appendToTokenName '-'
        | s, Char.NUL ->
            Next (dataState), state |> parseError "EOF in commentEndState" |> emitCurrentToken
        | s, other ->
            Next (commentState), s |> appendCharsToTokenName ['-';other]

    // 8.2.4.51 Comment end bang state
    and [<StateDesc("Comment end bang state")>]
        commentEndBangState state =
        match consumeChar state with
        | s, '-' ->
            Next (commentEndDashState), s |> appendCharsToTokenName ['-';'!']
        | s, '>' ->
            Next (dataState), s |> emitCurrentToken
        | s, Char.NUL ->
            Next (dataState), state |> parseError "EOF in commentEndBangState" |> emitCurrentToken
        | s, other ->
            Next (commentState), s |> appendCharsToTokenName ['-';'!';other]

    // 8.2.4.52 DOCTYPE state
    and [<StateDesc("DOCTYPE state")>]
        doctypeState state =
        match consumeChar state with
        | s, Char.HT | s, Char.LF | s, Char.FF | s, Char.SP ->
            Next (beforeDoctypeNameState), s
        | s, Char.NUL ->
            Next (dataState), state |> newToken (Some (DocType ("", "", "", true)))
                                   |> emitCurrentToken
        | s, other ->
            Next (beforeDoctypeNameState), state |> parseError "unallowed character in doctypeState"

    // 8.2.4.53 Before DOCTYPE name state
    and [<StateDesc("Before DOCTYPE name state")>]
        beforeDoctypeNameState state =
        match consumeChar state with
        | s, Char.HT | s, Char.LF | s, Char.FF | s, Char.SP -> beforeDoctypeNameState s
        | s, Upper c ->
            Next (doctypeNameState), s |> newToken (Some (DocType ((Char.ToLower c |> string), "", "", false)))
        | s, '>' ->
            Next (dataState), s |> newToken (Some (DocType ("", "", "", true))) |> emitCurrentToken
        | _, Char.NUL ->
            Next (dataState), state |> newToken (Some (DocType ("", "", "", true))) |> emitCurrentToken
        | s, other ->
            Next (doctypeNameState), s |> newToken (Some (DocType (string other , "", "", false)))

    // 8.2.4.54 DOCTYPE name state
    and [<StateDesc("DOCTYPE name state")>]
        doctypeNameState state =
        match consumeChar state with
        | s, Char.HT | s, Char.LF | s, Char.FF | s, Char.SP ->
            Next (afterDoctypeNameState), s
        | s, '>' ->
            Next (dataState), s |> emitCurrentToken
        | s, Upper c ->
            Next (doctypeNameState), s |> appendToTokenName (Char.ToLower c)
        | _, Char.NUL ->
            Next (dataState), state |> parseError "EOF in doctypeNameState"
                                   |> setQuirk true |> emitCurrentToken 
        | s, other ->
            Next (doctypeNameState), s |> appendToTokenName other

    // 8.2.4.55 After DOCTYPE name state
    and [<StateDesc("After DOCTYPE name state")>]
        afterDoctypeNameState state =
        match consumeChar state with
        | s, Char.HT | s, Char.LF | s, Char.FF | s, Char.SP -> afterDoctypeNameState s
        | s, '>' ->
            Next (dataState), s |> emitCurrentToken
        | s, Char.NUL ->
            Next (dataState), state |> parseError "EOF in doctypeNameState"
                                   |> setQuirk true |> emitCurrentToken 
        | s, _ when isFollowedBy "PUBLIC" s ->
            let s1, _ = consumeChars 6 s
            Next (afterDoctypePublicKeywordState), s1
        | s, _ when isFollowedBy "SYSTEM" s ->
            let s1, _ = consumeChars 6 s
            Next (afterDoctypeSystemKeywordState), s1
        | s, _ ->
            Next (bogusDoctypeState), s |> parseError "unallowed character in afterDoctypeNameState"

    // 8.2.4.56 After DOCTYPE name state
    and [<StateDesc("After DOCTYPE name state")>]
        afterDoctypePublicKeywordState state =
        match consumeChar state with
        | s, Char.HT | s, Char.LF | s, Char.FF | s, Char.SP ->
            Next (beforeDoctypePublicIdState), s
        | s, '"' ->
            Next (doctypePublicIdDoubleQuotedState), s |> parseError "unallowed character in afterDoctypePublicKeywordState"
                                                      |> setPid ""
        | s, ''' ->
            Next (doctypePublicIdSingleQuotedState), s |> parseError "unallowed character in afterDoctypePublicKeywordState"
                                                      |> setPid ""
        | s, '>' ->
            Next (dataState), s |> parseError "> in afterDoctypePublicKeywordState"
                               |> setQuirk true |> emitCurrentToken
        | _, Char.NUL ->
            Next (dataState), state |> parseError "EOF in afterDoctypePublicKeywordState"
                               |> setQuirk true |> emitCurrentToken
        | s, other ->
            Next (bogusDoctypeState), s |> parseError "unallowed character in afterDoctypePublicKeywordState"
                                       |> setQuirk true

    // 8.2.4.57 Before DOCTYPE public identifier state
    and [<StateDesc("Before DOCTYPE public identifier state")>]
        beforeDoctypePublicIdState state =
        match consumeChar state with
        | s, Char.HT | s, Char.LF | s, Char.FF | s, Char.SP -> beforeDoctypePublicIdState s
        | s, '"' ->
            Next (doctypePublicIdDoubleQuotedState), s |> setPid ""
        | s, ''' ->
            Next (doctypePublicIdSingleQuotedState), s |> setPid ""
        | s, '>' ->
            Next (dataState), s |> parseError "> in beforeDoctypePublicIdState"
                               |> setQuirk true |> emitCurrentToken
        | _, Char.NUL ->
            Next (dataState), state |> parseError "EOF in beforeDoctypePublicIdState"
                                   |> setQuirk true |> emitCurrentToken
        | s, other ->
            Next (bogusDoctypeState), s |> parseError "unallowed character in beforeDoctypePublicIdState"
                                       |> setQuirk true

    // 8.2.4.58 DOCTYPE public identifier (double-quoted) state
    and [<StateDesc("DOCTYPE public identifier (double-quoted) state")>]
        doctypePublicIdDoubleQuotedState state =
        match consumeChar state with
        | s, '"' ->
            Next (afterDoctypePublicIdState), s
        | s, '>' ->
            Next (dataState), s |> parseError "> in doctypePublicIdDoubleQuotedState"
                               |> setQuirk true |> emitCurrentToken
        | _, Char.NUL ->
            Next (dataState), state |> parseError "EOF in doctypePublicIdDoubleQuotedState"
                                   |> setQuirk true |> emitCurrentToken
        | s, other ->
            Next (doctypePublicIdDoubleQuotedState), s |> appendPid other

    // 8.2.4.59 DOCTYPE public identifier (single-quoted) state
    and [<StateDesc("DOCTYPE public identifier (single-quoted) state")>]
        doctypePublicIdSingleQuotedState state =
        match consumeChar state with
        | s, ''' ->
            Next (afterDoctypePublicIdState), s
        | s, '>' ->
            Next (dataState), s |> parseError "> in doctypePublicIdDoubleQuotedState"
                               |> setQuirk true |> emitCurrentToken
        | _, Char.NUL ->
            Next (dataState), state |> parseError "EOF in beforeDoctypePublicIdState"
                                   |> setQuirk true |> emitCurrentToken
        | s, other ->
            Next (doctypePublicIdDoubleQuotedState), s |> appendPid other

    // 8.2.4.60 After DOCTYPE public identifier state
    and [<StateDesc("After DOCTYPE public identifier state")>]
        afterDoctypePublicIdState state =
        match consumeChar state with
        | s, Char.HT | s, Char.LF | s, Char.FF | s, Char.SP ->
            Next (betweenDoctypePublicSystemIdState), s
        | s, '>' ->
            Next (dataState), s |> emitCurrentToken
        | s, '"' ->
            Next (doctypeSystemIdDoubleQuotedState), s |> parseError "unallowed character in afterDoctypePublicIdState"
                                                      |> setSid ""
        | s, ''' ->
            Next (doctypeSystemIdSingleQuotedState), s |> parseError "unallowed character in afterDoctypePublicIdState"
                                                      |> setSid ""
        | _, Char.NUL ->
            Next (dataState), state |> parseError "EOF in afterDoctypePublicIdState"
                                   |> setQuirk true |> emitCurrentToken
        | s, other ->
            Next (bogusDoctypeState), s |> parseError "unallowed character in afterDoctypePublicIdState"
                                       |> setQuirk true

    // 8.2.4.61 Between DOCTYPE public and system identifiers state
    and [<StateDesc("Between DOCTYPE public and system identifiers state")>]
        betweenDoctypePublicSystemIdState state =
        match consumeChar state with
        | s, Char.HT | s, Char.LF | s, Char.FF | s, Char.SP -> betweenDoctypePublicSystemIdState s
        | s, '>' ->
            Next (dataState), s |> emitCurrentToken
        | s, '"' ->
            Next (doctypeSystemIdDoubleQuotedState), s |> setSid ""
        | s, ''' ->
            Next (doctypeSystemIdSingleQuotedState), s |> setSid ""
        | _, Char.NUL ->
            Next (dataState), state |> parseError "EOF in betweenDoctypePublicSystemIdState"
                                   |> setQuirk true |> emitCurrentToken
        | s, other ->
            Next (bogusDoctypeState), s |> parseError "unallowed character in betweenDoctypePublicSystemIdState"
                                       |> setQuirk true

    // 8.2.4.62 Between DOCTYPE public and system identifiers state
    and [<StateDesc("Between DOCTYPE public and system identifiers state")>]
        afterDoctypeSystemKeywordState state =
        match consumeChar state with
        | s, Char.HT | s, Char.LF | s, Char.FF | s, Char.SP ->
            Next (beforeDoctypeSystemIdState), s
        | s, '"' ->
            Next (doctypeSystemIdDoubleQuotedState), s |> parseError "unallowed character in afterDoctypeSystemKeywordState"
                                                      |> setSid ""
        | s, ''' ->
            Next (doctypeSystemIdSingleQuotedState), s |> parseError "unallowed character in afterDoctypeSystemKeywordState"
                                                      |> setSid ""
        | s, '>' ->
            Next (dataState), s |> parseError "> in afterDoctypeSystemKeywordState"
                               |> setQuirk true |> emitCurrentToken
        | _, Char.NUL ->
            Next (dataState), state |> parseError "EOF in afterDoctypeSystemKeywordState"
                                   |> setQuirk true |> emitCurrentToken
        | s, other ->
            Next (bogusDoctypeState), s |> parseError "unallowed character in afterDoctypeSystemKeywordState"
                                       |> setQuirk true

    // 8.2.4.63 Before DOCTYPE system identifier state
    and [<StateDesc("Before DOCTYPE system identifier state")>]
        beforeDoctypeSystemIdState state =
        match consumeChar state with
        | s, Char.HT | s, Char.LF | s, Char.FF | s, Char.SP -> beforeDoctypeSystemIdState s
        | s, '"' ->
            Next (doctypeSystemIdDoubleQuotedState), s |> setSid ""
        | s, ''' ->
            Next (doctypeSystemIdSingleQuotedState), s |> setSid ""
        | s, '>' ->
            Next (dataState), s |> parseError "> in beforeDoctypeSystemIdState"
                               |> setQuirk true |> emitCurrentToken
        | _, Char.NUL ->
            Next (dataState), state |> parseError "EOF in beforeDoctypeSystemIdState"
                                   |> setQuirk true |> emitCurrentToken
        | s, other ->
            Next (bogusDoctypeState), s |> parseError "unallowed character in beforeDoctypeSystemIdState"
                                       |> setQuirk true

    // 8.2.4.64 DOCTYPE system identifier (double-quoted) state
    and [<StateDesc("DOCTYPE system identifier (double-quoted) state")>]
        doctypeSystemIdDoubleQuotedState state = 
        match consumeChar state with
        | s, '"' ->
            Next (afterDoctypeSystemIdState), s
        | s, '>' ->
            Next (dataState), s |> parseError "> in doctypeSystemIdDoubleQuotedState"
                               |> setQuirk true |> emitCurrentToken
        | _, Char.NUL ->
            Next (dataState), state |> parseError "EOF in doctypeSystemIdDoubleQuotedState"
                                   |> setQuirk true |> emitCurrentToken
        | s, other ->
            Next (doctypeSystemIdDoubleQuotedState), s |> appendSid other

    // 8.2.4.65 DOCTYPE system identifier (single-quoted) state
    and [<StateDesc("DOCTYPE system identifier (single-quoted) state")>]
        doctypeSystemIdSingleQuotedState state =
        match consumeChar state with
        | s, ''' ->
            Next (afterDoctypeSystemIdState), s
        | s, '>' ->
            Next (dataState), s |> parseError "> in doctypeSystemIdSingleQuotedState"
                               |> setQuirk true |> emitCurrentToken
        | _, Char.NUL ->
            Next (dataState), state |> parseError "EOF in doctypeSystemIdSingleQuotedState"
                                   |> setQuirk true |> emitCurrentToken
        | s, other ->
            Next (doctypeSystemIdSingleQuotedState), s |> appendSid other

    // 8.2.4.66 After DOCTYPE system identifier state
    and [<StateDesc("After DOCTYPE system identifier state")>]
        afterDoctypeSystemIdState state =
        match consumeChar state with
        | s, Char.HT | s, Char.LF | s, Char.FF | s, Char.SP -> afterDoctypeSystemIdState s
        | s, '>' ->
            Next (dataState), s |> emitCurrentToken
        | _, Char.NUL ->
            Next (dataState), state |> parseError "EOF in afterDoctypeSystemIdState"
                                   |> setQuirk true |> emitCurrentToken
        | s, other ->
            Next (bogusDoctypeState), s |> parseError "unallowed character in afterDoctypeSystemIdState"

    // 8.2.4.67 Bogus DOCTYPE state
    and [<StateDesc("Bogus DOCTYPE state")>]
        bogusDoctypeState state =
        match consumeChar state with
        | s, '>' ->
            Next (dataState), s |> emitCurrentToken
        | _, Char.NUL ->
            Next (dataState), state |> emitCurrentToken
        | s, _ -> bogusDoctypeState s

    // 8.2.4.68 CDATA section state
    and [<StateDesc("CDATA section state")>]
        cdataSectionState state =
        match consumeUntilMatch "]]>" state with
        | s, '>' :: ']' :: ']' :: chars ->
            Next (dataState), s |> emitCharTokens (List.rev chars)
        | _, Char.NUL :: ']' :: ']' :: chars
        | _, Char.NUL :: ']' :: chars
        | _, Char.NUL :: chars ->
            Next (dataState), state |> emitCharTokens (List.rev chars)
        | _ ->
            Next (dataState), state

    and eofState state = End, state
