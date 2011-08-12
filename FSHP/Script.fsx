﻿// This file is a script that can be executed with the F# Interactive.  
// It can be used to explore and test the library project.
// Note that script files will not be part of the project build.

#load "CharData.fs"
#load "TokenTypes.fs"
#load "Parser.fs"
#load "ParserStates.fs"
#load "Tokenizer.fs"

open FSHP.Tokenizer

tokenize "<html>hello</html>" |> printfn "%A"
