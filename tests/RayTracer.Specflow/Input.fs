namespace RayTracer.Specflow

open RayTracer.Challenge

module Input =

    type Input =
        | Tuple of Tuple.Tuple
        | Matrix of Matrices.Matrix
        
    let forceFromDriver (driver: Driver.Driver) (name: string) : Input =
        if name = "identity_matrix" then Matrices.identity 4 |> Input.Matrix
        else
            let argAsTuple = driver.GetTuple name
            let argAsMatrix = driver.GetMatrix name
            (argAsTuple
             |> Option.map Input.Tuple
             |> Option.orElse (argAsMatrix |> Option.map Input.Matrix)).Value
        
    let multiply input1 input2 =
        match input1, input2 with
        | Tuple _, Tuple _ -> failwith "Cannot multiply two tuples"
        | Matrix m1, Tuple t2 -> m1 * t2 |> Input.Tuple
        | Tuple _, Matrix _ -> failwith "Cannot multiply a tuple with a matrix"
        | Matrix m1, Matrix m2 -> m1 * m2 |> Input.Matrix
        
    let forceMatrix (input: Input) =
        match input with
        | Matrix m -> m
        | _ -> failwith "Input is not if type matrix"