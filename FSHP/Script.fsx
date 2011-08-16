// This file is a script that can be executed with the F# Interactive.  
// It can be used to explore and test the library project.
// Note that script files will not be part of the project build.

#load "CharData.fs"
#load "Helper.fs"
#load "Tokenizer.Tokens.fs"
#load "Tokenizer.Actions.fs"
#load "Tokenizer.States.fs"
#load "Tokenizer.Controller.fs"

open FSHP.Tokenizer
open FSHP.Tokenizer.States

tokenize "<html>hello</html>" |> printfn "%A"
