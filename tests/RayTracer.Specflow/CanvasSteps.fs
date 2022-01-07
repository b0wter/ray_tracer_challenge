namespace RayTracer.Specflow

open TechTalk.SpecFlow
open FsUnit.Xunit
open RayTracer.Challenge

module CanvasSteps =
    
    [<Binding>]
    type CanvasSteps(driver: CanvasDriver.CanvasDriver) =
        let _driver = driver
        
        let [<Given>] ``c ← canvas\((\d*), (\d*)\)`` (width, height) =
            let c = Canvas.create width height Color.black
            _driver.SetCanvas c

        let [<Then>] ``c\.width = (\d*)`` (width: int) =
            let canvas = _driver.GetCanvas()
            canvas.Width |> should equal width

        let [<Then>] ``c\.height = (\d*)`` (height: int) =
            let canvas = _driver.GetCanvas()
            canvas.Height |> should equal height
            
        let [<Then>] ``every pixel of c is color\((.*), (.*), (.*)\)`` (r: float, g: float, b: float) =
            let color = Color.createSafe (r, g, b)
            let canvas = _driver.GetCanvas()
            canvas |> Canvas.forall ((=) color)
            
        let 
            
