namespace RayTracer.Challenge

open System

module Color =
    
    [<CustomEquality; NoComparison>]
    type Color =
        {
        R: float
        G: float
        B: float
        }
        override this.Equals other =
            match other with
            | :? Color as c ->
                Math.Abs(c.R - this.R) < Constants.floatEpsilon &&
                Math.Abs(c.G - this.G) < Constants.floatEpsilon &&
                Math.Abs(c.B - this.B) < Constants.floatEpsilon
            | _ -> false
    
        static member (+) (a: Color, b: Color) : Color =
            {
                R = a.R + b.R
                G = a.G + b.G
                B = a.B + b.B
            }
            
        static member (-) (a: Color, b: Color) : Color =
            {
                R = a.R - b.R
                G = a.G - b.G
                B = a.B - b.B
            }
            
        static member (*) (a: Color, b: Color) : Color =
            {
                R = a.R * b.R
                G = a.G * b.G
                B = a.B * b.B
            }
            
        static member (*) (a: Color, f: float) : Color =
            {
                R = a.R * f
                G = a.G * f
                B = a.B * f
            }
            
        static member (*) (f: float, a: Color) : Color =
            {
                R = a.R * f
                G = a.G * f
                B = a.B * f
            }
            
    /// <summary>
    /// Creates a new color instance and makes sure that every component is at max 1.0.
    /// </summary>
    let createSafe (r, g, b) =
        if r > 1.0 || g > 1.0 || b > 1.0 then failwith "cannot create color with a component greater than 1.0"
        else
            {
                R = r
                G = g
                B = b
            }
            
    /// <summary>
    /// Creates a new color instance without checking the components.
    /// </summary>
    let create (r, g, b) =
        {
            R = r
            G = g
            B = b
        }
        
    let black =
        create(0.0, 0.0, 0.0)
        
    let white =
        create(1.0, 1.0, 1.0)
