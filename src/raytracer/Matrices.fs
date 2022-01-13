namespace RayTracer.Challenge

open System
open RayTracer.Challenge

module Matrices =
    
    [<CustomEquality; NoComparison>]
    type Matrix =
        {
            Cells: float [,]
            Width: int
            Height: int
        }
        member this.Item
            with get(i,j) =
                this.Cells.[i,j]
            and set(i,j) v =
                this.Cells[i,j] <- v
    
        member this.Col i =
            this.Cells |> Array2D.col i
            
        member this.Row i =
            this.Cells |> Array2D.row i
            
        static member (*) (a: Matrix, b: Matrix) =
            if a.Width  <> 4 ||
               a.Height <> 4 ||
               b.Width  <> 4 ||
               b.Height <> 4 then
               failwith "Can only multiply 4x4 matrices."
            else
                let cells =
                    Array2D.init 4 4 (fun i j ->
                        let r = a.Row i 
                        let c = b.Col j
                        (r,c)
                        ||> Array.map2 (fun a b -> a * b)
                        |> Array.sum
                    )
                {
                    Width = 4
                    Height = 4
                    Cells = cells
                }
            
        static member (*) (a: Matrix, b: Tuple.Tuple) : Tuple.Tuple =
            if a.Width <> 4 || a.Height <> 4 then
               failwith "Can only multiply 4x4 matrices."
            else
                let b = [| b.X; b.Y; b.Z; b.W |]
                let cells =
                    let rows = a.Cells |> Array2D.asRows
                    rows
                    |> List.map (fun row ->
                        (b, row)
                        ||> Array.map2 (*)
                        |> Array.sum
                        )
                    
                Tuple.fromList cells
                
        static member (/) (m: Matrix, f: float) : Matrix =
            { m with Cells = Array2D.init m.Height m.Width (fun r c -> m.[r,c] / f) }
            
        override this.Equals other =
            let comp (f: float) (g: float) : bool =
                if f = 0 && g = 0 then true
                else
                    Math.Abs(f/g - 1.0) <= Constants.floatEpsilon
    
            match other with
            | :? Matrix as m ->
                let flatA = this.Cells |> Array2D.flatten
                let flatB = m.Cells |> Array2D.flatten
                Seq.forall2 comp flatA flatB
            | x ->
                failwithf "Can only perform equality checks for a matrix and another matrix, but given a %s." (x.GetType().FullName)
                
                
    /// <summary>
    /// Creates a new matrix with the given initializer.
    /// </summary>
    let init d1 d2 (f: int -> int -> float) =
        {
            Cells = Array2D.init d1 d2 f //Array.init (d1 * d2) (fun i -> f (i/d2) (i % d1))
            Width = d2
            Height = d1
        }
    
    /// <summary>
    /// Creates a new matrix with the same value for each cell.
    /// </summary>
    let create d1 d2 v =
        {
            Cells = Array2D.create d1 d2 v
            Width = d2
            Height = d1
        }
        
    let isSquare m =
        m.Width = m.Height
    
    /// <summary>
    /// Creates an identity matrix for the given dimension.
    /// </summary>
    let identity d =
        init d d (fun i j -> if i = j then 1 else 0)
        
    /// <summary>
    /// 4x4 identity matrix
    /// </summary>
    let identity4 =
        identity 4
        
    /// <summary>
    /// Creates a new square matrix with the same value for each cell.
    /// </summary>
    let createSquare d v =
        create d d v
        //Array2D.create d d
        
    /// <summary>
    /// Creates a new square matrix with the given initializer.
    /// </summary>
    let initSquare d =
        Array2D.init d d
        
    /// <summary>
    /// Gets a value from a matrix.
    /// </summary>
    let get i j (m: Matrix) =
        m.[i, j]
        
    /// <summary>
    /// Sets a value in a matrix.
    /// </summary>
    /// <remarks>
    /// Operation is performed in place!
    /// </remarks>
    let set i j v (m: Matrix) =
        m.[i, j] <- v
        
    let col i (m: Matrix) =
        m.Col i
        
    let row i (m: Matrix) =
        m.Row i

    /// <summary>
    /// Transposes the given matrix
    /// </summary>
    let transpose (m: Matrix) =
        if m |> isSquare then
            init m.Height m.Width (fun i j -> m.[j,i])
        else
            failwith "Cannot transpose a non-square matrix."
        
    /// <summary>
    /// Returns a new matrix where the given row and col have been removed.
    /// </summary>
    /// <remarks>
    /// Zero-based, creates new matrix
    /// </remarks>
    let subMatrix row col matrix =
        let cells =
            Array2D.init
                (matrix.Height - 1) (matrix.Width - 1)
                (fun i j ->
                    let i = if i >= row then i + 1 else i
                    let j = if j >= col then j + 1 else j
                    matrix.[i,j]
                )
        {
            Cells = cells
            Width = matrix.Width - 1
            Height = matrix.Height - 1
        }
        
    /// <summary>
    /// Computes the determinant for a 2x2 or 3x3 matrix.
    /// </summary>
    let rec determinant (m: Matrix) =
        let cofactor m r c =
            let sign = if (r + c) % 2 = 0 then 1.0 else -1.0
            m
            |> subMatrix r c
            |> determinant
            |> (*) sign
            
        if m.Width <> m.Height then
            failwith "Cannot compute determinant for non-square matrix."
        else if m.Width = 2 then
            m.Cells.[0,0] * m.Cells[1,1] - m.Cells[1,0] * m.Cells.[0,1]
        else if m.Width = 3 then
            m.Cells.[0,0] * m.Cells[1,1] * m.Cells[2,2] +
            m.Cells.[0,1] * m.Cells[1,2] * m.Cells[2,0] +
            m.Cells.[0,2] * m.Cells[1,0] * m.Cells[2,1] -
            m.Cells.[2,0] * m.Cells[1,1] * m.Cells[0,2] -
            m.Cells.[2,1] * m.Cells[1,2] * m.Cells[0,0] -
            m.Cells.[2,2] * m.Cells[1,0] * m.Cells[0,1]
        else if m.Width = 4 then
            let cofactor0 = cofactor m 0 0
            let cofactor1 = cofactor m 0 1
            let cofactor2 = cofactor m 0 2
            let cofactor3 = cofactor m 0 3
            m.[0,0] * cofactor0 + m.[0,1] * cofactor1 + m.[0,2] * cofactor2 + m.[0,3] * cofactor3
        else
            failwithf "Can only compute determinant for 2x2 or 3x3 matrices, given %ix%i" m.Height m.Width

    
    let minor (row: int) (col: int) (m: Matrix) =
        if row >= m.Height then
            failwithf "Index out of bounds while computing a minor. row %i, but height is %i" row m.Height
        else if row < 0 then
            failwith "Matrix index cannot be negative (row)."
        else if col >= m.Width then
            failwithf "Index out of bounds while computing a minor. col %i, but width is %i" col m.Width
        else if col < 0 then
            failwith "Matrix index cannot be negative (col)."
        else
            m
            |> subMatrix row col
            |> determinant
            

    let cofactor (row: int) (col: int) (m: Matrix) =
        if m.Width <> m.Height then
            failwithf "Can only compute the cofactor of a square matrix, given %ix%i" m.Height m.Width
        else if m.Width > 4 then
            failwithf "Can only compute the minor of 4x4 matrices or smaller. Given %i" m.Width
        else
            let sign = if (row + col) % 2 = 0 then 1.0 else -1.0
            m
            |> minor row col
            |> (*)sign
            
    
    let isInvertible m =
        (m |> determinant) <> 0


    let inverse m =
        if m |> isInvertible then
            let det = m |> determinant
            init m.Height m.Width (fun r c ->
                m |> cofactor r c
            )
            |> transpose
            |> fun m -> m / det
        else
            failwith "Matrix is not invertible."