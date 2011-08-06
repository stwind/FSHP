namespace FSHP

module Helper =

    open System

    let toHexDigit n =
        if n < 10 then char (n + 0x30) else char (n + 0x62)
