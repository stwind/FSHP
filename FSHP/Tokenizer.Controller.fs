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

    let liftDbg stateFunc state =
        do stateFunc |> getStateName |> printfn "%A"
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

    //type M<'n, 's> = M of ('s -> 'n * 's)

    //let runState (M f) state = f state

    //type TokenizerBuilder() =
        //member this.Bind (m, f) =
            //M (fun s -> let next, s' = runState m s in runState (f next) s')
        //member this.Return x = 
            //M (fun s -> x s)
        //member this.ReturnFrom x = x

    //let tokenizer = new TokenizerBuilder ()

    //let tokenize stream =
        //let state = toState stream
        //let rec loop next s =
            //tokenizer { return! loop next s }

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
            | Next (next, s1) -> parse (liftDbg next) s1
            | End s2 -> s2
        parse dataState state
