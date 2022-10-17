namespace RayTracer.Challenge

open System

module Rays =
    
    type Ray = {
        Origin: Tuple.Tuple
        Direction: Tuple.Tuple
    }
    
    let create (origin, direction) =
        {
            Origin = origin
            Direction = direction
        }
        
    let positionAfter t r =
        r.Origin + t * r.Direction
        
        
    let transform (m: Matrices.Matrix) (r: Ray) =
        {
            r with
                Origin = m * r.Origin
                Direction = m * r.Direction
        }