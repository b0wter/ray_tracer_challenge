namespace Raytracer.Extras

open System
open RayTracer.Challenge

module Chapter5Raycast =
    let private edgeLength = 1024
    
    (*
                                                   |
        /|\                                        |
         |                                         |
         |     (camera)          (sphere)          |(canvas)
         Z                                         |
         |                                         |
         |                                         |
        -+-                                    
           |-- X -->
    *)
    
    let writeRaycastToFile (filename: string) =
        let canvas = Canvas.create edgeLength edgeLength Color.blue
        let canvasDistance = 100.0
        let sphere = Spheres.create ()
        let camera = Tuple.createPoint (-5.0, 0.0, 0.0)
        
        for y in 0..edgeLength - 1 do
            for z in 0..edgeLength - 1 do
                let ray = Rays.create (camera, Tuple.createVector(1, y, z))
                let intersections = Intersections.intersectionsWith (Intersections.Sphere sphere) ray
                if intersections |> Intersections.hit |> Option.isSome then
                    Canvas.setPixel z y Color.red canvas
                else
                    Canvas.setPixel z y Color.black canvas
                
        let ppm = canvas |> PPM.fromCanvas
        do System.IO.File.WriteAllText(filename, ppm)
            
                
