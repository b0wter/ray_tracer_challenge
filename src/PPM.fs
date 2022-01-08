namespace RayTracer.Challenge

open System

module PPM =
    let header width height =
        [ "P3"
          $"%i{width} %i{height}"
          "255"
        ]
        

    let color (c: Color.Color) : string =
        let r = (255.0 * c.R) |> int
        let g = (255.0 * c.G) |> int
        let b = (255.0 * c.B) |> int
        $"%i{r} %i{g} %i{b}"
        
        
    let body (points: Color.Color[,]) =
        let createRow (row: Color.Color []) : string =
            row |> Array.fold (fun acc next -> acc + " " + (next |> color)) String.Empty
        points
        |> Array2D.asRows
        |> List.map createRow
    
    
    let fromCanvas (canvas: Canvas.Canvas<Color.Color>) =
        let width = canvas.Points |> Array2D.width
        let height = canvas.Points |> Array2D.height
        let asLines = (header width height) @ (body canvas.Points)
        String.Join(Environment.NewLine, asLines)
    
    