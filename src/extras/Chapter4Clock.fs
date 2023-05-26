namespace Raytracer.Extras

open System
open RayTracer.Challenge

module Chapter4Clock =
    let private edgeLength = 128
    
    let writeClockRenderToFile (filename: string) =
        let radiansPerHour = Math.PI / 6.0
        let point = Tuple.createPoint (0.0, 0.0, (edgeLength |> float) * 0.4)
        let translationMatrix = Matrices.translation (((edgeLength |> float) / 2.0), 0.0, ((edgeLength |> float) / 2.0))
        let canvas = Canvas.create 128 128 Color.black
        
        let points =
            [0.0..11.0]
            |> List.map ((*) radiansPerHour)
            |> List.map Matrices.rotationY
            |> List.map (fun rotationMatrix -> translationMatrix * rotationMatrix * point)
            
        do points |> List.iter (fun point ->
            canvas |> Canvas.setPixel (point.X |> int) (point.Z |> int) Color.white)
        
        let ppm = canvas |> PPM.fromCanvas
        do System.IO.File.WriteAllText(filename, ppm)
