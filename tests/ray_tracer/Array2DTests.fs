module Array2DTests

    open RayTracer.Challenge
    open FsUnit.Xunit
    open Xunit

    let array =
        let grid = Array2D.create 5 3 "..."
        grid[0,0] <- "0,0"
        grid[0,1] <- "1,0"
        grid[0,2] <- "2,0"
        grid[0,3] <- "3,0"
        grid[0,4] <- "4,0"
        
        grid[1,0] <- "0,1"
        grid[1,1] <- "1,1"
        grid[1,2] <- "2,1"
        grid[1,3] <- "3,1"
        grid[1,4] <- "4,1"
        
        grid[2,0] <- "0,2"
        grid[2,1] <- "1,2"
        grid[2,2] <- "2,2"
        grid[2,3] <- "3,2"
        grid[2,4] <- "4,2"
        grid
        
    let row0 = [ "0,0";"1,0";"2,0";"3,0";"4,0" ]
    let row1 = [ "0,1";"1,1";"2,1";"3,1";"4,1" ]
    let row2 = [ "0,2";"1,2";"2,2";"3,2";"4,2" ]
    let rows = [row0; row1; row2 ]
    
    let col0 = [ "0,0";"0,1";"0,2" ]
    let col1 = [ "1,0";"1,1";"1,2" ]
    let col2 = [ "2,0";"2,1";"2,2" ]
    let col3 = [ "3,0";"3,1";"3,2" ]
    let col4 = [ "4,0";"4,1";"4,2" ]
    let cols = [ col0; col1; col2; col3; col4 ]
    
    type Row() =
        [<Theory>]
        [<InlineData(0)>]
        [<InlineData(1)>]
        [<InlineData(2)>]
        let ``Getting the rows from a 2D array returns correct rows`` (index: int) =
            let row = array |> Array2D.row index |> List.ofArray
            let expected = rows |> List.skip index |> List.head
            row |> should equal expected
            
    type Col() =
        [<Theory>]
        [<InlineData(0)>]
        [<InlineData(1)>]
        [<InlineData(2)>]
        [<InlineData(3)>]
        [<InlineData(4)>]
        let ``Getting the cols from a 2D array returns first cols`` (index: int) =
            let col = array |> Array2D.col index |> List.ofArray
            let expected = cols |> List.skip index |> List.head
            col |> should equal expected

    type Flatten() =
        [<Fact>]
        let ``Flattening a two-dimensional array returns grid as concatenated rows`` () =
            let expected = (row0 @ row1 @ row2) 
            let flattened = array |> Array2D.flatten |> List.ofSeq
            flattened |> should equal expected
            
    type Create() =
        [<Fact>]
        let ``Creating an array with width 10 and height 20 returns an array of these dimensions`` () =
            let array = Array2D.create 20 10 0
            // Array2D uses (y, x) coordinates instead of (x, y) coordinates.
            array |> Array2D.width |> should equal 20
            array |> Array2D.length2 |> should equal 20
            array |> Array2D.height |> should equal 10
            array |> Array2D.length1 |> should equal 10

    type ForAll() =
        [<Fact>]
        let ``ForAll should return true of all elements of the given array fulfil the predicate`` () =
            let array = Array2D.create 20 10 3
            (array |> Array2D.forall ((=) 3)) |> should be True
            
        [<Fact>]
        let ``ForAll should return false if any elements of the given array does not fulfil the predicate`` () =
            let array = Array2D.create 20 10 3
            do array[1,1] <- 2
            (array |> Array2D.forall ((=) 3)) |> should be False

    type Fold() =
        [<Fact>]
        let ``Folding over a 2D array should be done row-wise`` () =
            let mutable counter = -1
            let array = Array2D.init 20 10 (fun _ _ ->
                    do counter <- counter + 1
                    counter
                )
            let folder =
                fun previous next ->
                    if (previous + 1) = next then next
                    else failwithf "The next element in the folding sequence does not match the rule. Is %i should be %i" next (previous + 1)
            (array |> Array2D.fold folder -1) |> should be (greaterThan 0)