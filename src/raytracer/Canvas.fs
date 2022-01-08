namespace RayTracer.Challenge

open System

module Canvas =
    
    type Canvas<'a> =
        {
            Width: int
            Height: int
            Points: 'a[,]
        }
        
    /// <summary>
    /// Checks whether the given coordinates are inside the bounds of the given 2d array.
    /// </summary>
    let checkBounds x y (canvas: _[,]) =
        let width = canvas |> Array2D.length1
        let height = canvas |> Array2D.length2
        (x >= 0 && x < width) && (y >= 0 && y < height)
        
    /// <summary>
    /// Sets a pixel on a canvas.
    /// </summary>
    /// <remarks>
    /// Operation is performed in place.
    /// </remarks>
    let setPixel x y value canvas =
        do canvas.Points.[y,x] <- value
        
    /// <summary>
    /// Retrieves the value of a pixel on the canvas.
    /// </summary>
    let getPixel x y canvas =
        canvas.Points.[y,x]

    /// <summary>
    /// Creates a canvas where every position is initialized with the given value.
    /// </summary>
    let create width height value =
        {
            Width = width
            Height = height
            Points = Array2D.create width height value
        }
        
    let forall (predicate: 'a -> bool) (canvas: Canvas<'a>) : bool =
        canvas.Points |> Array2D.forall predicate