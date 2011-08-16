namespace FSHP

open System
open System.Text.RegularExpressions

module Helper =
    
    let regex s = new Regex(s)

    let (=~) s (re:Regex) = re.IsMatch(s)

    let (<>~) s (re:Regex) = not (s =~ re)

    let reMatches (re:Regex) s = re.Matches(s)

    let reMatch (re:Regex) s = re.Match(s)