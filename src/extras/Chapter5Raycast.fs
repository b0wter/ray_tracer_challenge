namespace Raytracer.Extras

open System
open System.Threading.Tasks
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
    
    let writeRaycastToFile (degreeOfParallelization: int) (filename: string) =
        task {
            let canvas = Canvas.create edgeLength edgeLength Color.blue
            let canvasDistance = 100.0
            let sphere =
                Spheres.create ()
                |> Spheres.withTransform (Matrices.translation (0.0, 513.0, 512.0))
            let camera = Tuple.createPoint (-2.0, 512.0, 512.0)
            
            let drawRange (startY: int) (endY: int) =
                for y in startY..endY - 1 do
                    for z in 0..edgeLength - 1 do
                        let ray = Rays.create (camera, Tuple.createVector(canvasDistance, y, z))
                        let intersections = Intersections.intersectionsWith (Intersections.Sphere sphere) ray
                        try
                            if intersections |> Intersections.hit |> Option.isSome then
                                Canvas.setPixel z y Color.red canvas
                            else
                                Canvas.setPixel z y Color.black canvas
                        with
                        | _ ->
                            printfn "%i - %i" z y
                            
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
