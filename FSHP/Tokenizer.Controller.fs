namespace FSHP.Tokenizer

open FSHP
open FSHP.Helper
open FSHP.Tokenizer.Tokens
open FSHP.Tokenizer.Actions
open FSHP.Tokenizer.States

[<AutoOpen>]
module Controller =

    let getStateName stateFunc =
        let t = stateFunc.GetType().ToString()
        t |> reMatch (regex "(?<=\+)(\w+)(?=@)")

    let charJustConsumed state =
        let cp, is = state.currentPos, state.inputStream
        if cp < is.Length
            then is.[cp - 1]
            else Char.NUL

    let liftDbg stateFunc state =
        let name, charConsumed = getStateName stateFunc, charJustConsumed state 
        let text = "entering '" + name.Value + "', " + "character to be consumed '" + string charConsumed + "'"
        do text |> printfn "%A"
        stateFunc state

    let toState stream = 
        { 
            currentPos = 0 
            inputStream = stream 
            currentToken = None
            tokenStream = []
            charToken = []
            parseError = []
            tempBuffer = []
        }

    let bindM m f = fun s -> let m', s' = m s in f m' s'

    let returnM m = fun s -> m, s

    type TokenizerBuilder() =
        member this.Bind (m, f) = bindM m f
        member this.Return m = returnM m
        member this.ReturnFrom m = m

    let tokenizer = new TokenizerBuilder()

    let tokenize stream =
        let state = toState stream
        let rec loop next =
            tokenizer {
                    let! n = next
                    match n with
                    | Next n1 -> return! loop n1
                    | End -> return n
                }
        loop dataState state |> snd
