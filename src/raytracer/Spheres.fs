namespace RayTracer.Challenge

module Spheres =
    type Sphere = {
        Transformation: Matrices.Matrix
    }

    let create () =
        {
            Transformation = Matrices.identity4
        }
        
    let withTransform (m: Matrices.Matrix) (s: Sphere) =
        { s with Transformation = m }

    let normalAt (s: Sphere, p: Tuple.Tuple) =
        let objectPoint = (Matrices.inverse s.Transformation) * p
        let objectNormal = Tuple.normalize (objectPoint - Tuple.zeroPoint)
        Tuple.normalize { (Matrices.transpose(Matrices.inverse(s.Transformation)) * objectNormal) with W = 0.0 }