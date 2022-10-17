namespace RayTracer.Challenge

open System

module Tuple =
    
    [<CustomEquality; NoComparison>]
    type Tuple =
        {
        X : float
        Y : float
        Z : float
        W : float
        }
        member this.Magnitude : float =
            Math.Sqrt(this.X * this.X + this.Y * this.Y + this.Z * this.Z)
            
        static member Create (x, y, z, w) =
            {
                X = x
                Y = y
                Z = z
                W = w
            }
        
        static member (~-) (t: Tuple) =
            {
                t with
                    X = -t.X
                    Y = -t.Y
                    Z = -t.Z
                    W = -t.W
            }
            
        static member (%) (a: Tuple, b: Tuple) =
            let diff : Tuple = a - b
            let m = diff.Magnitude
            m < 0.00001
            
        static member ($) (a: Tuple, b: Tuple) =
            {
                X = (a.Y * b.Z - a.Z * b.Y)
                Y = (a.Z * b.X - a.X * b.Z)
                Z = (a.X * b.Y - a.Y * b.X)
                W = 0.0
            }
            
        static member (-) (a: Tuple, b: Tuple) =
            (*
                Point = 0.0w    Vector = 1.0w
                ----------------------------[is]----[should]---
                Point - Point   -> Vector   (0)     (0)
                Point - Vector  -> Point    (1)     (1)
                Vector - Vector -> Vector   (0)     (0)
                Vector - Point  -> Error! Makes no sense.
            *)
            let newW = a.W - b.W
            if newW = -1 then
                failwith "cannot subtract a point from a vector"
            else
                Tuple.Create (a.X - b.X, a.Y - b.Y, a.Z - b.Z, newW)
            
        static member (+) (a: Tuple, b: Tuple) =
            (*
                Point = 0.0w    Vector = 1.0w
                -----------------------------
                Point + Point   -> Error! Makes no sense.
                Point + Vector  -> Point  (1) [1]
                Vector + Vector -> Vector (0) [0]
                Vector + Point  -> Point  (1) [1]
            *)
            //let newW = Math.Min(a.W + b.W, 1.0)
            Tuple.Create (a.X + b.X, a.Y + b.Y, a.Z + b.Z, a.W + b.W)
            
        static member (*) (t: Tuple, f: float) =
            {
                t with
                    X = t.X * f
                    Y = t.Y * f
                    Z = t.Z * f
                    W = t.W * f
            }
            
        static member (*) (f: float, t: Tuple) =
            {
                t with
                    X = t.X * f
                    Y = t.Y * f
                    Z = t.Z * f
                    W = t.W * f
            }
            
        static member (/) (t: Tuple, f: float) =
            {
                t with
                    X = t.X / f
                    Y = t.Y / f
                    Z = t.Z / f
                    W = t.W / f
            }
            
        override this.Equals other =
            match other with
            | :? Tuple as t ->
                Math.Abs(t.X - this.X) < Constants.floatEpsilon &&
                Math.Abs(t.Y - this.Y) < Constants.floatEpsilon &&
                Math.Abs(t.Z - this.Z) < Constants.floatEpsilon &&
                Math.Abs(t.W - this.W) < Constants.floatEpsilon
            | _ -> false
                
            
    let create (x, y, z, w) =
        Tuple.Create (x, y, z, w)

    let createVector (x, y, z) =
        create (x, y, z, 0.0)
        
    let createPoint (x, y, z) =
        create (x, y, z, 1.0)
        
    let fromList list =
        match list with
        | [ x; y; z; w ] ->
            create (x, y, z, w)
        | _ ->
            failwithf "Can only create a tuple from a list with 4 elements. The given list had %i elements" list.Length
        
    let origin =
        createPoint (0.0, 0.0, 0.0)
        
    let isPoint t =
        t.W = 1.0
        
    let isVector t =
        t.W = 0.0
        
    let subtract (a: Tuple) (b: Tuple) =
        a - b
        
    let add a b =
        a + b
        
    let multiply (f: float) (t: Tuple) =
        t * f
        
    let divide (f: float) (t: Tuple) =
        t / f
        
    let negate (t: Tuple) =
        -t

    let magnitude (t: Tuple) =
        t.Magnitude
        
    let normalize t =
        let m = 1.0 / (t |> magnitude)
        t * m
        
    let cross a b =
        {
            X = (a.Y * b.Z - a.Z * b.Y)
            Y = (a.Z * b.X - a.X * b.Z)
            Z = (a.X * b.Y - a.Y * b.X)
            W = 0.0
        }
        
    let dot a b =
        a.X * b.X +
        a.Y * b.Y +
        a.Z * b.Z +
        a.W * b.W
        