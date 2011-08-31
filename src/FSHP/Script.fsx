// This file is a script that can be executed with the F# Interactive.  
// It can be used to explore and test the library project.
// Note that script files will not be part of the project build.

#load "CharData.fs"
#load "Helper.fs"
#load "Dom.Events.fs"
#load "Dom.Nodes.fs"
//#load "Tokenizer.Tokens.fs"
//#load "Tokenizer.Actions.fs"
//#load "Tokenizer.States.fs"
//#load "Tokenizer.Controller.fs"

//open FSHP.Tokenizer
//open FSHP.Tokenizer.States

open FSHP.Dom.Nodes
let document = new Document()
let comment = document.CreateComment "comment elem"
let docElem = document.CreateElement "doc elem"
document.AppendChild comment |> ignore
document.AppendChild (document.CreateElement "doc elem") |> ignore
document.ChildNodes.Length |> printfn "%A"

//tokenize "<html>hello<h1>fuck</h1></html>" |> printfn "%A"
