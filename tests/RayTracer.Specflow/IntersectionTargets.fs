namespace RayTracer.Specflow

open RayTracer.Challenge.Intersections

module IntersectionTargets =
    
    let tryFromDriver (driver: Driver.Driver) (name: string) : IntersectionTarget =
        driver.ForceSphere name |> IntersectionTarget.Sphere

