namespace RayTracer.Challenge

open System

module PPM =
    let header width height =
        [ "P3"
          $"%i{width} %i{height}"
          "255"
        ]


    let trim (s: string) =
        if s |> String.IsNullOrEmpty then s
        else s.Trim ()


    let comp (f: float) =
        Math.Round(255.0 * (Math.Max(0, Math.Min(1.0, f))), 0) |> int |> string

    
    let color (c: Color.Color) : string =
        let r = Math.Round(255.0 * (Math.Max(0, Math.Min(1.0, c.R))), 0) |> int
        let g = Math.Round(255.0 * (Math.Max(0, Math.Min(1.0, c.G))), 0) |> int
        let b = Math.Round(255.0 * (Math.Max(0, Math.Min(1.0, c.B))), 0) |> int
        $"%i{r} %i{g} %i{b}"
        
        
    let body (points: Color.Color[,]) =
        let createRow (row: Color.Color []) : string =
            let colorComponents = row |> Seq.collect (fun c -> seq {c.R; c.G; c.B})
            let previousRows = System.Collections.Generic.List<string>(20)
            let lastRow =
                colorComponents
                |> Seq.fold (fun (acc: string) next ->
                        let color = next |> comp
                        let acc =
                            if (acc.Length + 1 + color.Length > 70) then
                                previousRows.Add(acc)
                                String.Empty
                            else acc
                        let prefix =
                            if acc.Length = 0 then String.Empty
                            else " "
                        acc + prefix + color) String.Empty
            previousRows.Add(lastRow)
            String.Join(Environment.NewLine, previousRows)

        points
        |> Array2D.asRows
        |> List.map createRow
    
    
    let fromCanvas (canvas: Canvas.Canvas<Color.Color>) =
        let width = canvas.Points |> Array2D.width
        let height = canvas.Points |> Array2D.height
        let asLines = (header width height) @ (body canvas.Points)
        String.Join(Environment.NewLine, asLines) + Environment.NewLine
    
    