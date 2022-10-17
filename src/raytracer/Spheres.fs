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
