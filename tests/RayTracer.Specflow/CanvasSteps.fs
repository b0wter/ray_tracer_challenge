namespace RayTracer.Specflow

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
