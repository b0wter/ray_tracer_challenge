namespace RayTracer.Specflow

open System.Collections.Generic
open RayTracer.Challenge
open FsUnit.Xunit

module CanvasDriver =

    type CanvasDriver () =
        let mutable canvas : Canvas.Canvas<Color.Color> option = None
        
        member public __.SetCanvas(c) =
            do canvas <- Some c
            
        member public __.GetCanvas() =
            match canvas with
            | Some c -> c
            | None -> failwith "The canvas has not yet been set and cannot be retrieved."