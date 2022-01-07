namespace RayTracer.Challenge

open System

/// <summary>
/// This module contains additionals functions to interact with an Array2D.
/// </summary>
module Array2D =
    
    let row i (array: 'a [,]) =
        array.[i,*]
        
    let col i (array: 'a [,]) =
        array.[*,i]
        
    let asCols (array: 'a [,]) =
        let cols = array |> Array2D.length1
        [0..cols-1] |> List.map (fun c -> col c array)
        
    let asRows (array: 'a [,]) =
        let rows = array |> Array2D.length2
        [0..rows-1] |> List.map (fun r -> row r array)

    /// <summary>
    /// Converts an Array2D into a one-dimensional sequence of elements.
    /// Sequence is concatenated rows.
    /// </summary>
    let flatten (A:'a[,]) =
        A |> Seq.cast<'a>
    
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