namespace RayTracer.Challenge

open System

module Intersections =
    
    type IntersectionTarget =
        | Sphere of Spheres.Sphere
    
    type Intersection = {
        Ray: Rays.Ray
        T: float
        Target: IntersectionTarget
    }
    
    let intersectionTargetTransform =
        function
        | Sphere s -> s.Transformation
   
    type Intersections = Intersection list
    
    type Hit = Intersection option
    
    let hit (is: Intersections) : Hit =
        let sorted = is |> List.sortBy (fun i -> i.T)
        sorted |> List.tryFind (fun s -> s.T >= 0)
    
    let intersectionsWith (target: IntersectionTarget) (ray: Rays.Ray) : Intersections =
        // Instead of transforming the sphere for the intersection test
        // we use an inverse of the transform on the ray because that makes
        // the computation easier
        let ray =
            target
            |> intersectionTargetTransform
            |> Matrices.inverse
            |> (fun matrix -> Rays.transform matrix ray)
        
        let sphereToRay = ray.Origin - Tuple.createPoint (0, 0, 0)
        
        let a = Tuple.dot ray.Direction ray.Direction
        let b = 2.0 * (Tuple.dot ray.Direction sphereToRay)
        let c = (Tuple.dot sphereToRay sphereToRay) - 1.0
        
        let discriminant = b * b - 4.0 * a * c
        
        if discriminant < 0 then []
        else
            [
                {
                    Ray = ray
                    T = (-b - Math.Sqrt(discriminant)) / (2.0 * a)
                    Target = target
                }
                {
                    Ray = ray
                    T = (-b + Math.Sqrt(discriminant)) / (2.0 * a)
                    Target = target
                }
            ] |> List.sortBy (fun i -> i.T)
