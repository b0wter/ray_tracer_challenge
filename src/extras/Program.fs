open System.Diagnostics
open Raytracer.Extras

//let chapter1Result = Chapter1Projectile.runPredefined ()
//let chapter2Result = Chapter2Canvas.writePredefinedToFile "result.ppm"

let watch = Stopwatch()
watch.Start()
let chapter7Result = (Chapter5Raycast.writeRaycastToFile 4 "result.ppm").GetAwaiter().GetResult()
watch.Stop()
printfn "%i" watch.ElapsedMilliseconds

