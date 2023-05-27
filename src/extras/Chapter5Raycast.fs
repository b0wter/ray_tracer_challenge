namespace Raytracer.Extras

open System
open System.Threading.Tasks
open RayTracer.Challenge

module Chapter5Raycast =
    let private edgeLength = 500
    
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
    
    let writeRaycastToFile (degreeOfParallelization: int) (filename: string) =
        task {
            let canvas = Canvas.create edgeLength edgeLength Color.blue
            let canvasDistance = 10.0
            let sphere = Spheres.create ()
            let camera = Tuple.createPoint (0.0, 0.0, -5.0)
            let offset = -edgeLength / 2
            
            let drawRange (startX: int) (endX: int) =
                for x in startX..endX - 1 do
                    for y in 0..edgeLength - 1 do
                        let ray = Rays.create (camera, Tuple.createVector(offset + x |> float, offset + y |> float, edgeLength * 2 |> float))
                        let intersections = Intersections.intersectionsWith (Intersections.Sphere sphere) ray
                        if intersections |> Intersections.hit |> Option.isSome then
                            Canvas.setPixel x y Color.red canvas
                        else
                            Canvas.setPixel x y Color.black canvas
                            
            let drawRangeTask startY endY =
                Task.Run(fun () -> drawRange startY endY)
                            
            let numberOfTasks = degreeOfParallelization
            let taskRange = edgeLength / numberOfTasks
            let tasks =
                [0..numberOfTasks - 1]
                |> List.map (fun i ->
                    let start = i * taskRange
                    let ``end`` = start + taskRange
                    drawRangeTask start  ``end``)
                
            let! _ = Task.WhenAll tasks
            
            let ppm = canvas |> PPM.fromCanvas
            do System.IO.File.WriteAllText(filename, ppm)
        }
