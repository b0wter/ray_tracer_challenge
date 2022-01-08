namespace RayTracer.Specflow

open System
open TechTalk.SpecFlow
open FsUnit.Xunit
open RayTracer.Challenge

module CanvasSteps =
    
    [<Binding>]
    type CanvasSteps(driver: Driver.Driver) =
        let _driver = driver
        
        let [<Given>] ``c ← canvas\((\d*), (\d*)\)`` (width, height) =
            let c = Canvas.create width height Color.black
            _driver.SetCanvas c

        let [<Then>] ``c\.width = (\d*)`` (width: int) =
            let canvas = _driver.GetCanvas()
            canvas.Width |> should equal width
            canvas.Points |> Array2D.row 1 |> Array.length |> should equal width

        let [<Then>] ``c\.height = (\d*)`` (height: int) =
            let canvas = _driver.GetCanvas()
            canvas.Height |> should equal height
            canvas.Points |> Array2D.col 1 |> Array.length |> should equal height
            
        let [<Then>] ``every pixel of c is color\((.*), (.*), (.*)\)`` (r: float, g: float, b: float) =
            let color = Color.createSafe (r, g, b)
            let canvas = _driver.GetCanvas()
            canvas |> Canvas.forall ((=) color)
            
        let [<When>] ``write_pixel\(c, (\d+), (\d+), (\w+)\)`` (x: int, y: int, c1: string) =
            match c1 |> _driver.GetColor with
            | Some c ->
                let canvas = _driver.GetCanvas()
                do canvas |> Canvas.setPixel x y c
            | None ->
                failwithf "Color %s has not been set." c1

        let [<Then>] ``pixel_at\(c, (\d+), (\d+)\) = (\w+)`` (x: int, y: int, c1: string) =
            match c1 |> driver.GetColor with
            | Some c ->
                let canvas = _driver.GetCanvas ()
                let is = canvas |> Canvas.getPixel x y
                is |> should equal c
            | None ->
                failwithf "Color %s has not been set." c1
                
        let [<When>] ``ppm ← canvas_to_ppm\(c\)`` () =
            let canvas = _driver.GetCanvas ()
            let ppm = canvas |> PPM.fromCanvas
            _driver.SetPPM ppm
            
        let [<Then>] ``lines (\d)-(\d) of ppm are`` (from: int, ``to``: int, s: string) =
            let expectedLines = s.Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries)
            let allLines =
                (_driver.GetCanvas () |> PPM.fromCanvas).Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries)
            let actualLines =
                allLines
                |> Array.skip (from - 1)
                |> Array.take (``to`` - from + 1)
            actualLines |> should equal expectedLines
            
        let [<Then>] ``ppm ends with a newline character`` () =
            let ppm = _driver.GetPPM ()
            let asLines = ppm.Split(Environment.NewLine)
            asLines |> Array.last |> should be EmptyString
        
        let [<When>] ``every pixel of c is set to color\(1, 0.8, 0.6\)`` () =
            let canvas = _driver.GetCanvas ()
            let color = Color.create (1.0, 0.8, 0.6)
            canvas.Points |> Array2D.iteri (fun i j _ -> canvas.Points[i,j] <- color)