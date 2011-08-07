// This file is a script that can be executed with the F# Interactive.  
// It can be used to explore and test the library project.
// Note that script files will not be part of the project build.

#load "CharData.fs"
#load "TokenTypes.fs"
#load "ParserStates.fs"
#load "Tokenizer.fs"

open FSHP.Tokenizer

tokenize "a1&<ga1d" |> printfn "%A"
