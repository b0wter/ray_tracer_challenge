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
    
        override this.GetHashCode () =
            ( (this.R * Constants.inverseFloatEpsilon |> int).ToString()
            + (this.G * Constants.inverseFloatEpsilon |> int).ToString()
            + (this.B * Constants.inverseFloatEpsilon |> int).ToString()).GetHashCode()
            
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
            
    let create (r, g, b) =
        {
            R = r
            G = g
            B = b
        }
