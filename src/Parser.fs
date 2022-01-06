namespace RayTracer.Challenge

open System

module Parser =
    
    let tryParseInt (s: string) =
        let success, value = Int32.TryParse(s)
        if success then Some value
        else None

    let tryParseFloat (s: string) =
        let success, value = Double.TryParse(s)
        if success then Some value
        else None
