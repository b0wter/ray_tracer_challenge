namespace RayTracer.Specflow

open TechTalk.SpecFlow
open RayTracer.Challenge
open FsUnit.Xunit

module RaysSteps =
    
    [<Binding>]
    type RaysSteps(driver: Driver.Driver) =
        
        let [<When>] ``(\w*) ← ray\((\w*), (\w*)\)`` (name1: string, name2: string, name3: string) =
            let origin = driver.ForceTuple name2
            let direction = driver.ForceTuple name3
            let ray = Rays.create (origin, direction)
            do driver.SetRay (name1, ray)

        let [<Then>] ``(\w*).origin = (\w*)`` (name1: string, name2: string) =
            let ray = driver.ForceRay name1
            let tuple = driver.ForceTuple name2
            ray.Origin |> should equal tuple
            
        let [<Then>] ``(\w*).direction = (\w*)`` (name1: string, name2: string) =
            let ray = driver.ForceRay name1
            let tuple = driver.ForceTuple name2
            ray.Direction |> should equal tuple

        let [<Given>] ``(\w*) ← ray\(point\((.*), (.*), (.*)\), vector\((.*), (.*), (.*)\)\)`` (name: string, originX: float, originY: float, originZ: float, directionX: float, directionY: float, directionZ: float) =
            let origin = Tuple.createPoint (originX, originY, originZ)
            let direction = Tuple.createVector (directionX, directionY, directionZ)
            let ray = Rays.create (origin, direction)
            do driver.SetRay (name, ray)
            
        let [<Then>] ``position\((\w*), (.*)\) = point\((.*), (.*), (.*)\)`` (name: string, t: float, x: float, y: float, z: float) =
            let ray = driver.ForceRay name
            let expected = Tuple.createPoint (x, y, z)
            ray |> Rays.positionAfter t |> should equal expected
            
        let [<When>] ``r2 ← transform\(r, m\)`` () =
            let ray = driver.ForceRay "r"
            let m = Input.forceFromDriver driver "m" |> Input.forceMatrix
            let transformed = Rays.transform m ray
            do driver.SetRay ("r2", transformed)
        
        let [<Then>] ``r2.origin = point\((\w*), (\w*), (\w*)\)`` (x: int, y: int, z: int) =
            let ray = driver.ForceRay "r2"
            ray.Origin |> should equal (Tuple.createPoint (x, y, z))
        
        let [<Then>] ``r2.direction = vector\((\w*), (\w*), (\w*)\)`` (x: int, y: int, z: int) =
            let ray = driver.ForceRay "r2"
            ray.Direction |> should equal (Tuple.createVector (x, y, z))
