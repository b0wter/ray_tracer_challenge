namespace Raytracer.Extras

open RayTracer.Challenge

module Chapter2Canvas =
    
    let writePredefinedToFile (filename: string) =
        let result = Chapter1Projectile.runPredefined ()
        let positions = result |> List.map (fun r -> r.Position.X |> int, r.Position.Y |> int)
        let minX = positions |> List.minBy (fun (x, _) -> x) |> fst
        let maxX = positions |> List.maxBy (fun (x, _) -> x) |> fst
        let minY = positions |> List.minBy (fun (_, y) -> y) |> snd
        let maxY = positions |> List.maxBy (fun (_, y) -> y) |> snd
        let offsetX = 0 - minX
        let offsetY = 0 - minY
        printfn "Min/Max X: %i / %i | Min/Max Y: %i / %i | Offset X/Y: %i / %i" minX maxX minY maxY offsetX offsetY
        let width = maxX - minX + 1
        let height = maxY - minY + 1
        let canvas = Canvas.create width height Color.black
        printfn "Created canvas %ix%i" width height
        do positions |> List.iter (fun p ->
            let x = ((p |> fst) + offsetX)
            let y = (height - 1) - ((p |> snd) + offsetY)
            //let y = (p |> snd) + offsetY 
            do printfn "Writing color to %i - %i" x y
            canvas |> Canvas.setPixel x y Color.white)
        let ppm = canvas |> PPM.fromCanvas
        do System.IO.File.WriteAllText(filename, ppm)
        

