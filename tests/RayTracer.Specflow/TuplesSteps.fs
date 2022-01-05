namespace RayTracer.Specflow

open TechTalk.SpecFlow
open FsUnit.Xunit
open RayTracer.Challenge

module TuplesSteps =
    
    [<Binding>]
    type TupleSteps(driver: TuplesDriver.TuplesDriver) =
        let _driver = driver
            
        let [<Given>] ``(.*) ← tuple\((.*), (.*), (.*), (.*)\)`` (t: string, x, y, z, w) =
            _driver.SetTuple(t, Tuple.create (x, y, z, w))

        let [<Then>] ``(.*)\.x = (.*)`` (t: string, x: float) =
            match _driver.GetTuple t with
            | None -> failwithf "no tuple set %s" t
            | Some t -> t.X |> should equal x

        let [<Then>] ``(.*)\.y = (.*)`` (t: string, y: float) =
            match _driver.GetTuple t with
            | None -> failwithf "no tuple set %s" t
            | Some t -> t.Y |> should equal y

        let [<Then>] ``(.*)\.z = (.*)`` (t: string, z: float) =
            match _driver.GetTuple t with
            | None -> failwithf "no tuple set %s" t
            | Some t -> t.Z |> should equal z

        let [<Then>] ``(.*)\.w = (.*)`` (t: string, w: float) =
            match _driver.GetTuple t with
            | None -> failwithf "no tuple set %s" t
            | Some t -> t.W |> should equal w
            
        let [<Then>] ``(.*) is a point`` (t: string) =
            match _driver.GetTuple t with
            | None -> failwithf "no tuple set %s" t
            | Some t -> t |> Tuple.isPoint |> should be True
            
        let [<Then>] ``(.*) is not a point`` (t: string) =
            match _driver.GetTuple t with
            | None -> failwithf "no tuple set %s" t
            | Some t -> t |> Tuple.isPoint |> should be False
            
        let [<Then>] ``(.*) is a vector`` (t: string) =
            match _driver.GetTuple t with
            | None -> failwithf "no tuple set %s" t
            | Some t -> t |> Tuple.isVector |> should be True
        let [<Then>] ``(.*) is not a vector`` (t: string) =
            match _driver.GetTuple t with
            | None -> failwithf "no tuple set %s" t
            | Some t -> t |> Tuple.isVector |> should be False
            
        let [<Given>] ``(.*) ← point\((.*), (.*), (.*)\)`` (t: string, x, y, z) =
            _driver.SetTuple(t, Tuple.createPoint (x, y, z))
            
        let [<Given>] ``(.*) ← vector\((.*), (.*), (.*)\)`` (t: string, x, y, z) =
            _driver.SetTuple(t, Tuple.createVector (x, y, z))
            
        let [<Given>] ``(.*) ← color\((.*), (.*), (.*)\)`` (t: string, r, g, b) =
            _driver.SetColor(t, Color.create (r, g, b))            

        let [<Then>] ``(.*) \+ (.*) = tuple\((.*), (.*), (.*), (.*)\)`` (t1: string, t2: string, x: float, y: float, z: float, w: float) =
            let first = _driver.GetTuple t1
            let second = _driver.GetTuple t2
            match first, second with
            | Some f, Some s ->
                let result = f + s
                result.X |> should equal x
                result.Y |> should equal y
                result.Z |> should equal z
                result.W |> should equal w
            | None, _ ->
                failwithf "no tuple set %s" t1
            | _, None ->
                failwithf "no tuple set %s" t2
    
        let [<Then>] ``(.*) - (.*) = (vector|point)\((.*), (.*), (.*)\)`` (t1: string, t2: string, tuple: string, x: float, y: float, z: float) =
            let first = _driver.GetTuple t1
            let second = _driver.GetTuple t2
            
            let expectedW =
                if tuple = "vector" then 0.0
                else if tuple = "point" then 1.0
                else (failwithf "Unexpected tuple variant: %s" tuple)
                
            match first, second with
            | Some f, Some s ->
                let result = Tuple.subtract f s
                result.X |> should equal x
                result.Y |> should equal y
                result.Z |> should equal z
                result.W |> should equal expectedW
            | None, _ ->
                failwithf "no tuple set %s" t1
            | _, None ->
                failwithf "no tuple set %s" t2
                
        let [<Then>] ``([a-z,A-Z,0-9]+) = tuple\((.*), (.*), (.*), (.*)\)`` (t: string, x: float, y: float, z: float, w: float) =
            _driver.ShouldEqual (t, x, y, z, w)
            
        let [<Then>] ``-([a-z,A-Z,0-9]+) = tuple\((.*), (.*), (.*), (.*)\)`` (t: string, x: float, y: float, z: float, w: float) =
            let negated = Tuple.create (x, y, z, w)
            let negatedNegated = negated |> Tuple.negate
            _driver.ShouldEqual (t, negatedNegated.X, negatedNegated.Y, negatedNegated.Z, negatedNegated.W)
            
        let [<Then>] ``(.*) (\*|\/) (.*) = tuple\((.*), (.*), (.*), (.*)\)`` (t1: string, operation: string, scalar: float, x: float, y: float, z: float, w: float) =
            match t1 |> _driver.GetTuple with
            | Some a ->
                let result =
                    match operation with
                    | "*" -> a * scalar
                    | "/" -> a / scalar
                    | _ -> failwithf "Unknown tuple operation: %s" operation
                result.X |> should equal x
                result.Y |> should equal y
                result.Z |> should equal z
                result.W |> should equal w
            | None ->
                failwithf "no tuple set %s" t1
        
        let [<Then>] ``magnitude\(([a-z,A-Z,0-9]+)\) = √14`` (t: string) =
            match t |> _driver.GetTuple with
            | Some a ->
                let result = a |> Tuple.magnitude
                let expected = System.Math.Sqrt(14)
                result |> should equal expected
            | None ->
                failwithf "no tuple set %s" t
        
        let [<Then>] ``magnitude\(([a-z,A-Z,0-9]+)\) = (\d)`` (t: string, value: float) =
            match t |> _driver.GetTuple with
            | Some a ->
                let result = a |> Tuple.magnitude
                result |> should equal value
            | None ->
                failwithf "no tuple set %s" t
        
        let [<Then>] ``normalize\(([a-z,A-Z,0-9]+)\) = vector\((.*), (.*), (.*)\)`` (t: string, x: float, y: float, z: float) =
            match t |> _driver.GetTuple with
            | Some a ->
                let result = a |> Tuple.normalize
                let expected = Tuple.createVector (x, y, z)
                result |> should equal expected
            | None ->
                failwithf "no tuple set %s" t

        let [<Then>][<When>] ``norm <- normalize\((.*)\)`` (t: string) =
            match t |> _driver.GetTuple with
            | Some a ->
                let normalized = a |> Tuple.normalize
                _driver.SetTuple ("norm", normalized)
            | None ->
                failwithf "no tuple set %s" t
                
        let [<Then>] ``normalize\(v\) = approximately vector\(0.26726, 0.53452, 0.80178\)`` () =
            match "v" |> _driver.GetTuple with
            | Some a ->
                let normalized = a |> Tuple.normalize
                let expected = Tuple.createVector(0.26726, 0.53452, 0.80178)
                (normalized % expected) |> should be True
            | None ->
                failwithf "no tuple set %s" "v"

        let [<When>] ``norm ← normalize\(v\)`` () =
            ``norm <- normalize\((.*)\)`` "v"
        
        let [<Then>] ``dot\(a, b\) = 20`` () =
            match "a" |> _driver.GetTuple, "b" |> _driver.GetTuple with
            | Some a, Some b ->
                let dot = a |> Tuple.dot b
                dot |> should equal 20.0
            | None, _ | _, None ->
                failwithf "no tuple set %s" "'a' or 'b'"
                
                
        let [<Then>] ``cross\((.*), (.*)\) = vector\((-?\d), (-?\d), (-?\d)\)`` (t1: string, t2: string, x: int, y: int, z: int) =
            match t1 |> _driver.GetTuple, t2 |> _driver.GetTuple with
            | Some a, Some b ->
                let cross = a $ b
                cross |> should equal (Tuple.createVector (x, y, z))
            | None, _ | _, None ->
                failwithf "no tuple set %s" "'a' or 'b'"
            
        // Explicit component reference because that makes it easier to pattern match the specflow steps.
        let [<Then>] ``(.*)\.red = (-?\d+\.\d+)`` (c: string, value: float) =
            match c |> _driver.GetColor with
            | Some c ->
                c.R |> should equal value
            | None ->
                failwithf "no color set %s" c
                
        // Explicit component reference because that makes it easier to pattern match the specflow steps.
        let [<Then>] ``(.*)\.green = (-?\d+\.\d+)`` (c: string, value: float) =
            match c |> _driver.GetColor with
            | Some c ->
                c.G |> should equal value
            | None ->
                failwithf "no color set %s" c
                
        // Explicit component reference because that makes it easier to pattern match the specflow steps.
        let [<Then>] ``(.*)\.blue = (-?\d+\.\d+)`` (c: string, value: float) =
            match c |> _driver.GetColor with
            | Some c ->
                c.B |> should equal value
            | None ->
                failwithf "no color set %s" c
                
                
            