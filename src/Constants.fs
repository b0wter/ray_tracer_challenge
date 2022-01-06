namespace RayTracer.Challenge

open System.Globalization
open Microsoft.FSharp.Core

module Constants =
    
    [<Literal>]
    let floatEpsilon = 0.00001

    let inverseFloatEpsilon = System.Math.Round(1.0/floatEpsilon)
    
