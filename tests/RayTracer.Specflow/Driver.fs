namespace RayTracer.Specflow

open System.Collections.Generic
open RayTracer.Challenge
open FsUnit.Xunit

module Driver =

    type Driver() =
        let tuples : Dictionary<string, Tuple.Tuple> = Dictionary<string, Tuple.Tuple>()
        let colors : Dictionary<string, Color.Color> = Dictionary<string, Color.Color>()
        let mutable canvas : Canvas.Canvas<Color.Color> option = None
        
        member public __.SetTuple(key, tuple) =
            do tuples.Add(key, tuple)
            
        member public __.SetColor(key, tuple) =
            do colors.Add(key, tuple)
            
        member public __.GetTuple(key) =
            if tuples.ContainsKey(key) then
                Some tuples.[key]
            else
                None
                
        member public __.GetColor(key: string) : Color.Color option =
            if colors.ContainsKey(key) then
                Some colors.[key]
            else
                None
            
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
