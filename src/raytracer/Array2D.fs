namespace RayTracer.Challenge

open System

/// <summary>
/// This module contains additionals functions to interact with an Array2D.
/// </summary>
module Array2D =
    
    (*
        Hint: Array2D uses (y, x) coordinates instead of (x, y) coordinates.
        =====
    *)
    
    let height = Array2D.length1 // see hint!
    
    let width = Array2D.length2 // see hint!
    
    /// <summary>
    /// Selects the i-th row from the array.
    /// </summary>
    /// <remarks>
    /// Zero-based
    /// </remarks>
    let row i (array: 'a [,]) =
        array.[i,*]
        
    /// <summary>
    /// Selects the i-th column from the array.
    /// </summary>
    /// <remarks>
    /// Zero-based
    /// </remarks>
    let col i (array: 'a [,]) =
        array.[*,i]
        
    /// <summary>
    /// Returns the array as a list of columns.
    /// </summary>
    /// <remarks>
    /// Zero-based
    /// </remarks>
    let asCols (array: 'a [,]) =
        let cols = array |> width
        [0..cols-1] |> List.map (fun c -> col c array)
        
    /// <summary>
    /// Returns the array as a list of rows.
    /// </summary>
    /// <remarks>
    /// Zero-based
    /// </remarks>
    let asRows (array: 'a [,]) =
        let rows = array |> height
        [0..rows-1] |> List.map (fun r -> row r array)

    /// <summary>
    /// Converts an Array2D into a one-dimensional sequence of elements.
    /// Sequence is concatenated rows.
    /// </summary>
    let flatten (a:'a[,]) =
        a |> Seq.cast<'a>
        //a |> asRows |> Seq.concat
    
    /// <summary>
    /// Applies a folding operation over a two dimensional array. Fold order is first row from left to right
    /// then the next row.
    /// </summary>
    let fold (folder: 'acc -> 'value -> 'acc) (initialValue: 'acc) (array: 'value[,]) : 'acc =
        let elements = array |> flatten
        elements |> Seq.fold folder initialValue

    /// <summary>
    /// Checks whether a predicate is true for all cells of a two dimensional array.
    /// </summary>
    let forall (predicate: 'a -> bool) (array: 'a[,]) =
        array |> flatten |> Seq.forall predicate
        
    /// <summary>
    /// Creates a two-dimensional array with the given width, height and initial value.
    /// </summary>
    let create width height value =
        Array2D.create height width value
        
    /// <summary>
    /// Creates a two-dimensional array with the given width, height and its values initialized with the given initializer.
    /// </summary>
    let init width height initializer =
        Array2D.init height width initializer
        
        
    let iter2 action (a1: 'a[,]) (a2: 'a[,]) =
        let flatA1 = a1 |> flatten
        let flatA2 = a2 |> flatten
        Seq.iter2 action flatA1 flatA2
