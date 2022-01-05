namespace RayTracer.Challenge

open System

module Color =
    
    type Color =
        {
        R: float
        G: float
        B: float
        }
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
            
    let create (r, g, b) =
        {
            R = r
            G = g
            B = b
        }
