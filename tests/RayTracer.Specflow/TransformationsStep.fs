namespace RayTracer.Specflow

open System
open RayTracer.Challenge
open TechTalk.SpecFlow
open FsUnit.Xunit

module TransformationsStep =

    [<Binding>]
    type TransformationsSteps(driver: Driver.Driver) =
        let _driver = driver
        
        let [<Given>] ``(.*) ← translation\((-?\d*), (-?\d*), (-?\d*)\)`` (name: string, x: int, y: int, z: int) =
            let transform = Matrices.translation (x, y, z)
            do _driver.SetMatrix (name, transform)

        //let [<Then>] ``([a-zA-Z]*) \* ([a-zA-Z]*) = ([a-zA-Z]*)`` (name1: string, name2: string, name3: string) =
        let [<Then>] ``(\w*) \* (\w*) = (\w*)`` (name1: string, name2: string, name3: string) =
            let arg1 = Input.forceFromDriver _driver name1
            let arg2 = Input.forceFromDriver _driver name2
            let arg3 = Input.forceFromDriver _driver name3
            
            let multiplicationResult = Input.multiply arg1 arg2
    
            multiplicationResult |> should equal arg3

        let [<Given>] ``(.*) ← scaling\((-?\d), (-?\d), (-?\d)\)`` (name: string, x: int, y: int, z: int) =
            let transform = Matrices.scaling (x, y, z)
            _driver.SetMatrix (name, transform)
        
        let [<Then>] ``(\w*) = point\((-?\d*), (-?\d*), (-?\d*)\)`` (name: string, x: int, y: int, z: int) =
            let arg = Input.forceFromDriver _driver name
            let expected = Tuple.createPoint (x, y, z) |> Input.Tuple
            
            arg |> should equal expected
            
        let [<Then>] ``(\w*) \* (\w*) = vector\((-?\d*), (-?\d*), (-?\d*)\)`` (name1: string, name2: string, x: int, y: int, z: int) =
            let arg1 = Input.forceFromDriver _driver name1
            let arg2 = Input.forceFromDriver _driver name2
            let expected = Tuple.createVector (x, y, z) |> Input.Tuple
            
            let multiplicationResult = Input.multiply arg1 arg2
            
            multiplicationResult |> should equal expected
            
        let [<Then>] ``(\w*) \* (\w*) = point\((-?\d*), (-?\d*), (-?\d*)\)`` (name1: string, name2: string, x: int, y: int, z: int) =
            let arg1 = Input.forceFromDriver _driver name1
            let arg2 = Input.forceFromDriver _driver name2
            let expected = Tuple.createPoint (x, y, z) |> Input.Tuple
            
            let multiplicationResult = Input.multiply arg1 arg2
            
            multiplicationResult |> should equal expected

        let [<Given>] ``(.*) ← rotation_x\(π / 4\)`` (name: string) =
            let matrix = Matrices.rotationX (Math.PI / 4.0)
            do _driver.SetMatrix(name, matrix)
            
        let [<Given>] ``(.*) ← rotation_x\(π / 2\)`` (name: string) =
            let matrix = Matrices.rotationX (Math.PI / 2.0)
            do _driver.SetMatrix(name, matrix)
            
        let [<Then>] ``(\w*) \* (\w*) = point\(0, √2/2, √2/2\)`` (name1: string, name2: string) =
            let arg1 = Input.forceFromDriver _driver name1
            let arg2 = Input.forceFromDriver _driver name2
            let expected = Tuple.createPoint (0, Math.Sqrt(2.0)/2.0, Math.Sqrt(2.0)/2.0) |> Input.Tuple
            
            let multiplicationResult = Input.multiply arg1 arg2
            
            multiplicationResult |> should equal expected

        let [<Given>] ``(\w*) ← rotation_y\(π / (\d)\)`` (name: string, f: float) =
            let matrix = Matrices.rotationY (Math.PI / f)
            do _driver.SetMatrix (name, matrix)
            
        let [<Then>] ``(\w*) \* (\w*) = point\(√2/2, 0, √2/2\)`` (name1: string, name2: string) =
            let arg1 = Input.forceFromDriver _driver name1
            let arg2 = Input.forceFromDriver _driver name2
            let expected = Tuple.createPoint (Math.Sqrt(2.0)/2.0, 0, Math.Sqrt(2.0)/2.0) |> Input.Tuple
            
            let multiplicationResult = Input.multiply arg1 arg2
            
            multiplicationResult |> should equal expected

        let [<Given>] ``(\w*) ← rotation_z\(π / (\d)\)`` (name: string, f: float) =
            let matrix = Matrices.rotationZ (Math.PI / f)
            do _driver.SetMatrix (name, matrix)
            
        let [<Then>] ``(\w*) \* (\w*) = point\(-√2/2, √2/2, 0\)`` (name1: string, name2: string) =
            let arg1 = Input.forceFromDriver _driver name1
            let arg2 = Input.forceFromDriver _driver name2
            let expected = Tuple.createPoint (-Math.Sqrt(2.0)/2.0, Math.Sqrt(2.0)/2.0, 0) |> Input.Tuple
            
            let multiplicationResult = Input.multiply arg1 arg2
            
            multiplicationResult |> should equal expected

        let [<Then>] ``(\w*) \* (\w*) = point\(0, √2/2, -√2/2\)`` (name1: string, name2: string) =
            let arg1 = Input.forceFromDriver _driver name1
            let arg2 = Input.forceFromDriver _driver name2
            let expected = Tuple.createPoint (0, Math.Sqrt(2.0)/2.0, -Math.Sqrt(2.0)/2.0) |> Input.Tuple
            
            let multiplicationResult = Input.multiply arg1 arg2
            
            multiplicationResult |> should equal expected

        let [<Given>] ``(\w*) ← shearing\((\d*), (\d*), (\d*), (\d*), (\d*), (\d*)\)`` (name: string, a: float, b: float, c: float, d: float, e: float, f: float) =
            let shearing = Matrices.shearing (a, b, c, d, e, f)
            do _driver.SetMatrix (name, shearing)
            
        let [<When>] ``(\w*) ← (\w*) \* (\w*)`` (name1: string, name2: string, name3: string) =
            let arg2 = Input.forceFromDriver _driver name2
            let arg3 = Input.forceFromDriver _driver name3
            
            let multiplicationResult = Input.multiply arg2 arg3
            let tuple =
                match multiplicationResult with
                | Input.Tuple t -> t
                | _ -> failwith "Input needs to be a tuple at this point"
            
            do _driver.SetTuple (name1, tuple)
            
        let [<When>] ``T ← C \* B \* A`` () =
            let c = Input.forceFromDriver _driver "C"
            let b = Input.forceFromDriver _driver "B"
            let a = Input.forceFromDriver _driver "A"
            
            let multiplicationResult =
                match Input.multiply (Input.multiply c b) a with
                | Input.Matrix m -> m
                | _ -> failwith "Input needs to be a matrix at this point"
            
            do _driver.SetMatrix("T", multiplicationResult)
            
        let [<When>] ``t ← view_transform\(from, to, up\)`` () =
            ()