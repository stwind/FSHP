namespace FSHP

open System
open System.Text.RegularExpressions

module Helper =
    
    let regex s = new Regex(s)

    let (=~) s (re:Regex) = re.IsMatch(s)

    let (<>~) s (re:Regex) = not (s =~ re)

    let reMatches (re:Regex) s = re.Matches(s)

    let reMatch (re:Regex) s = re.Match(s)

    let (===) = fun x y -> Object.ReferenceEquals(x, y)

    let listFirst defaultVal lst = match lst with | [] -> defaultVal | h :: _ -> h

    let cliDefaultArg (str:string) defaultVal = if str = "" then defaultVal else str

    let insertBefore newItem refItem compare items =
        let rec loop l tail =
            match l with
            | [] -> newItem :: tail |> List.rev
            | hd :: tl as lst when compare hd refItem -> 
                (List.rev tail) @ (newItem :: lst)
            | hd :: tl -> hd :: tail |> loop tl
        loop items []

    let removeItem item compare items =
        let rec loop l tail =
            match l with
            | [] -> List.rev tail
            | hd :: tl when compare hd item ->
                (List.rev tail) @ tl
            | hd :: tl -> hd :: tail |> loop tl
        loop items []
