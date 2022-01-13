namespace RayTracer.Specflow

open System.Collections.Generic
open RayTracer.Challenge
open FsUnit.Xunit

module Driver =

    type Driver() =
        let tuples : Dictionary<string, Tuple.Tuple> = Dictionary<string, Tuple.Tuple>()
        let colors : Dictionary<string, Color.Color> = Dictionary<string, Color.Color>()
        let matrices : Dictionary<string, Matrices.Matrix> = Dictionary<string, Matrices.Matrix>()
        let mutable canvas : Canvas.Canvas<Color.Color> option = None
        let mutable ppm : string option = None
        
        member public __.SetTuple(key, tuple) =
            do tuples.Add(key, tuple)
            
        member public __.SetColor(key, tuple) =
            do colors.Add(key, tuple)
            
        member public __.SetMatrix(key, matrix) =
            do matrices.Add(key, matrix)
            
        member public __.GetTuple(key) =
            if tuples.ContainsKey(key) then
                Some tuples.[key]
            else
                None
                
        member public __.ForceTuple(key) =
            tuples.[key]
                
        member public __.GetColor(key: string) : Color.Color option =
            if colors.ContainsKey(key) then
                Some colors.[key]
            else
                None
                
        member public __.GetMatrix(key: string) : Matrices.Matrix option =
            if matrices.ContainsKey(key) then
                Some matrices.[key]
            else
                None

        member public __.ForceMatrix (key: string) =
            matrices.[key]
            
        member public __.ShouldEqual(t, x, y, z, w) =
            match __.GetTuple t with
            | None -> failwithf "tuple not set %s" t
            | Some t ->
                t.X |> should equal x
                t.Y |> should equal y
                t.Z |> should equal z
                t.W |> should equal w
                
        member public __.SetCanvas(c) =
            do canvas <- Some c
            
        member public __.GetCanvas() =
            match canvas with
            | Some c -> c
            | None -> failwith "The canvas has not yet been set and cannot be retrieved."
            
        member public __.SetPPM (s: string) =
            do ppm <- Some s
            
        member public __.GetPPM () =
            match ppm with
            | Some p -> p
            | None -> failwith "The ppm data has not yet been set and cannot be retrieved."
