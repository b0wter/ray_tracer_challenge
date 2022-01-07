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
    let setPixel (canvas: 'a[,]) x y value =
        do canvas.[x,y] <- value

    /// <summary>
    /// Creates a canvas where every position is initialized with the given value.
    /// </summary>
    let create width height value =
        {
            Width = width
            Height = height
            // Watch out! Array2D uses the y,x coordinates but the ray tracer uses x,y.
            Points = Array2D.create height width value
        }
        
    let forall (predicate: 'a -> bool) (canvas: Canvas<'a>) : bool =
        canvas.Points |> Array2D.forall predicate