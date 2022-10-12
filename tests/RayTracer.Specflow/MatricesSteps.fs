namespace RayTracer.Specflow

open RayTracer.Challenge
open TechTalk.SpecFlow
open FsUnit.Xunit

module MatricesSteps =
    
    [<Binding>]
    type MatricesSteps(driver: Driver.Driver) =
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
        
        let [<Given>] ``the following (\d)x(\d) matrix (.*):`` (d1: int, d2: int, name: string, table: Table) =
            let matrix = matrixFromTable table
            do matrix.Height |> should equal d1
            do matrix.Height |> should equal (matrix.Cells |> Array2D.length1)
            do matrix.Width |> should equal d2
            do matrix.Width |> should equal (matrix.Cells |> Array2D.length2)
            do _driver.SetMatrix(name, matrix)
            
        let [<Given>] ``the following matrix (.*):`` (name: string, table: Table) =
            let matrix = matrixFromTable table
            do _driver.SetMatrix(name, matrix)
            
        let [<Then>] ``(.*)\[(\d),(\d)\] = (-?\d+\.?\d?)`` (m: string, i: int, j: int, value: float) =
            let matrix =  m |> _driver.ForceMatrix
            do matrix[i,j] |> should equal value
            
        let [<Then>] ``(.) = (.)`` (m1: string, m2: string) =
            let matrix1 = m1 |> _driver.GetMatrix
            let matrix2 = m2 |> _driver.GetMatrix
            matrix1 |> should equal matrix2

        let [<Then>] ``(.) != (.)`` (m1: string, m2: string) =
            let matrix1 = m1 |> _driver.GetMatrix
            let matrix2 = m2 |> _driver.GetMatrix
            matrix1 |> should not' (equal matrix2)

        let [<Then>] ``(.) \* (.) is the following 4x4 matrix:`` (m1: string, m2: string, table: Table) =
            let matrix1 = m1 |> _driver.ForceMatrix
            let matrix2 = m2 |> _driver.ForceMatrix
            let matrix3 = table |> matrixFromTable
            let result = matrix1 * matrix2
            result |> should equal matrix3
            
        let [<Then>] ``(.) \* (.) = tuple\((.*), (.*), (.*), (.*)\)`` (m: string, t: string, x: float, y: float, z: float, w: float) =
            let matrix = m |> _driver.ForceMatrix
            let tuple = t |> _driver.ForceTuple
            let expected = Tuple.create (x, y, z, w)
            (matrix * tuple) |> should equal expected
            
        let [<Then>] ``(.) \* identity_matrix = (.)`` (m1: string, m2: string) =
            do m1 |> should equal m2
            let matrix = m1 |> _driver.ForceMatrix
            do matrix.Width |> should equal matrix.Height
            let identity = Matrices.identity matrix.Height
            matrix * identity |> should equal matrix

        let [<Then>] ``identity_matrix \* (.*) = (.*)`` (t1: string, t2: string) =
            do t1 |> should equal t2
            let tuple = t1 |> _driver.ForceTuple
            let identity = Matrices.identity4
            identity * tuple |> should equal tuple
            
        let [<Then>] ``transpose\((.)\) is the following matrix:`` (m: string, table: Table) =
            let expected = table |> matrixFromTable
            let matrix = m |> _driver.ForceMatrix
            let transposed = matrix |> Matrices.transpose
            transposed |> should equal expected
            
        let [<Given>] ``(.) ← transpose\(identity_matrix\)`` (m: string) =
            let transposedIdentity = Matrices.identity4 |> Matrices.transpose
            _driver.SetMatrix(m, transposedIdentity)
            
        let [<Then>] ``(.) = identity_matrix`` (m: string) =
            let matrix = m |> _driver.ForceMatrix
            matrix |> should equal Matrices.identity4
            
        let [<Then>] ``determinant\((.*)\) = (.*)`` (m: string, expected: float) =
            let matrix = m |> _driver.ForceMatrix
            let d = matrix |> Matrices.determinant
            d |> should equal expected
            
        let [<Then>] ``submatrix\((.*), (\d*), (\d*)\) is the following (\d)x(\d) matrix:`` (m: string, row: int, col: int, d1: int, d2: int, table: Table) =
            do d1 |> should equal d2
            let expected = matrixFromTable table
            let matrix = m |> _driver.ForceMatrix
            let subMatrix = matrix |> Matrices.subMatrix row col
            subMatrix |> should equal expected
            
        let [<Then>] ``(.) ← submatrix\((.), (.*), (.*)\)`` (m1: string, m2: string, row: int, col: int) =
            let matrix = m2 |> _driver.ForceMatrix
            let sub = matrix |> Matrices.subMatrix row col
            do _driver.SetMatrix (m1, sub)
           
        let [<Then>] ``minor\((.), (\d), (\d)\) = (.*)`` (m: string, row: int, col: int, result: float) =
            let matrix = m |> _driver.ForceMatrix
            let minor = matrix |> Matrices.minor row col
            minor |> should equal result
            
        let [<Then>] ``cofactor\((.), (\d), (\d)\) = (.*)`` (m: string, row: int, col: int, expected: float) =
            let matrix = m |> _driver.ForceMatrix
            let cofactor = matrix |> Matrices.cofactor row col
            cofactor |> should equal expected
            
        let [<Then>] ``(.*) is not invertible`` (m: string) =
            let matrix = m |> _driver.ForceMatrix
            matrix |> Matrices.isInvertible |> should be False
            
            
        let [<Then>] ``(.*) is invertible`` (m: string) =
            let matrix = m |> _driver.ForceMatrix
            matrix |> Matrices.isInvertible |> should be True
            
            

        let [<Given>] ``(.*) ← inverse\((.*)\)`` (name1: string, name2: string) =
            let argument = _driver.ForceMatrix name2
            let inverse = argument |> Matrices.inverse 
            do _driver.SetMatrix (name1, inverse)
                
        
        let [<Then>] ``(.)\[(\d),(\d)\] = -160/532`` (m: string, row: int, col: int) =
            let matrix = m |> _driver.ForceMatrix
            matrix[row, col] |> should equal (-160.0/532.0)
                
        
        let [<Then>] ``(.)\[(\d),(\d)\] = 105/532`` (m: string, row: int, col: int) =
            let matrix = m |> _driver.ForceMatrix
            matrix[row, col] |> should equal (105.0/532.0)


        let [<Then>] ``(.) is the following (\d)x(\d) matrix:`` (m: string, d1: int, d2: int, table: Table) =
            let expected = table |> matrixFromTable
            do expected.Height |> should equal d1
            do expected.Width |> should equal d2
            let matrix = _driver.ForceMatrix m
            let rounded = Matrices.init matrix.Height matrix.Width (fun r c -> System.Math.Round(matrix[r,c], 5))
            rounded |> should equal expected


        let [<Then>] ``inverse\((.)\) is the following (\d)x(\d) matrix:`` (m: string, d1: int, d2: int, table: Table) =
            let expected = table |> matrixFromTable
            do expected.Height |> should equal d1
            do expected.Width |> should equal d2
            let matrix =
                _driver.ForceMatrix m
                |> Matrices.inverse
            let rounded = Matrices.init matrix.Height matrix.Width (fun r c -> System.Math.Round(matrix[r,c], 5))
            rounded |> should equal expected
            
            
        let [<Given>] ``C ← A \* B`` () =
            let a = "A" |> _driver.ForceMatrix
            let b = "B" |> _driver.ForceMatrix
            do _driver.SetMatrix("C", a * b) 
        
        
        let [<Then>] ``(\w) \* inverse\((\w)\) = (\w)`` (m1: string, m2: string, m3: string) =
           let matrix1 = m1 |> _driver.ForceMatrix
           let inverse = (m2 |> _driver.ForceMatrix) |> Matrices.inverse
           let expected = (m3 |> _driver.ForceMatrix)
           let product = matrix1 * inverse
           //Array2D.iter2 (fun a b -> a |> should equal b) product.Cells expected.Cells
           product |> should equal expected
           //(matrix1 * inverse) |> should equal expected
