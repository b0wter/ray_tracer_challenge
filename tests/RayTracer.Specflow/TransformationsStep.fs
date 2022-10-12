namespace RayTracer.Specflow

open RayTracer.Challenge
open TechTalk.SpecFlow
open FsUnit.Xunit

module TransformationsStep =

    [<Binding>]
    type TransformationsSteps(driver: Driver.Driver) =
        let _driver = driver
        
        let matrixFromTable (table: Table) : Matrices.Matrix =
            let width = table.Header.Count
            let height = table.RowCount
            let matrix = Matrices.create height width 0.0
            do table.Rows |> Seq.iteri (fun i row ->
                    row.Values |> Seq.iteri (fun j cell ->
                            let f = System.Double.Parse(cell)
                            matrix |> Matrices.set i j f
                        )
                )
            matrix
   
        let [<Given>] ``(.*) ← translation\((-?\d), (-?\d), (-?\d)\)`` (name: string, x: int, y: int, z: int) =
            let transform = Matrices.translation (x, y, z)
            do _driver.SetMatrix (name, transform)

        let [<Then>] ``([a-zA-Z]*) \* ([a-zA-Z]*) = ([a-zA-Z]*)`` (name1: string, name2: string, name3: string) =
            let arg1 = Input.tryFromDriver _driver name1
            let arg2 = Input.tryFromDriver _driver name2
            let arg3 = Input.tryFromDriver _driver name3
            
            let multiplicationResult = Input.multiply arg1 arg2
    
            multiplicationResult |> should equal arg3

        let [<Given>] ``(.*) ← scaling\((-?\d), (-?\d), (-?\d)\)`` (name: string, x: int, y: int, z: int) =
            let transform = Matrices.scaling (x, y, z)
            _driver.SetMatrix (name, transform)
        
        let [<Then>] ``([a-zA-Z]*) \* ([a-zA-Z]*) = point\((-?\d*), (-?\d*), (-?\d*)\)`` (name1: string, name2: string, x: int, y: int, z: int) =
            let arg1 = Input.tryFromDriver _driver name1
            let arg2 = Input.tryFromDriver _driver name2
            let expected = Tuple.createPoint (x, y, z) |> Input.Tuple
            
            let multiplicationResult = Input.multiply arg1 arg2
            
            multiplicationResult |> should equal expected
            
        let [<Then>] ``([a-zA-Z]*) \* ([a-zA-Z]*) = vector\((-?\d*), (-?\d*), (-?\d*)\)`` (name1: string, name2: string, x: int, y: int, z: int) =
            let arg1 = Input.tryFromDriver _driver name1
            let arg2 = Input.tryFromDriver _driver name2
            let expected = Tuple.createVector (x, y, z) |> Input.Tuple
            
            let multiplicationResult = Input.multiply arg1 arg2
            
            multiplicationResult |> should equal expected
