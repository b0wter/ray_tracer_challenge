namespace RayTracer.Specflow

open RayTracer.Challenge
open TechTalk.SpecFlow
open FsUnit.Xunit
module Expect = Expecto.Flip.Expect

module IntersectionsSteps =
    
    [<Binding>]
    type IntersectionSteps(driver: Driver.Driver) =
        
        let [<Given>] ``(\w*) ← sphere\(\)`` (name: string) =
            let sphere = Spheres.create ()
            do driver.SetSphere (name, sphere)
            
        let [<When; Given>] ``(\w*) ← intersection\((.*), (\w*)\)`` (name1: string, t: float, name2: string) =
            let sphere = IntersectionTargets.tryFromDriver driver name2
            let intersection = {
                Intersections.Intersection.Ray = Rays.create (Tuple.origin, Tuple.origin)
                Intersections.Intersection.T = t
                Intersections.Intersection.Target = sphere
            }
            do driver.SetObject (name1, intersection)

        let [<Then>] ``i.t = (.*)`` (expected: float) =
            let intersection = driver.ForceObject<Intersections.Intersection> "i"
            intersection.T |> should equal expected
            
        let [<Then>] ``i.object = s`` () =
            let intersection = driver.ForceObject<Intersections.Intersection> "i"
            let target = match intersection.Target with Intersections.IntersectionTarget.Sphere sphere -> sphere
            let expected = driver.ForceSphere "s"
            target |> should equal expected

        let [<When>] ``(\w*) ← intersect\((\w*), (\w*)\)`` (name1: string, name2: string, name3: string) =
            let target = IntersectionTargets.tryFromDriver driver name2
            let ray = driver.ForceRay name3
            let intersection = ray |> Intersections.intersectionsWith target
            do driver.SetObject (name1, intersection)
        
        let [<When; Given>] ``(\w*) ← intersections\((\w*), (\w*)\)`` (name1: string, name2: string, name3: string) =
            let i1 = driver.ForceObject<Intersections.Intersection> name2
            let i2 = driver.ForceObject<Intersections.Intersection> name3
            driver.SetObject (name1, [i1; i2])

        let [<Given>] ``(\w*) ← intersections\((\w*), (\w*), (\w*), (\w*)\)`` (name1: string, name2: string, name3: string, name4: string, name5: string) =
            let i1 = driver.ForceObject<Intersections.Intersection> name2
            let i2 = driver.ForceObject<Intersections.Intersection> name3
            let i3 = driver.ForceObject<Intersections.Intersection> name4
            let i4 = driver.ForceObject<Intersections.Intersection> name5
            do driver.SetObject (name1, [i1; i2; i3; i4])
        
        let [<When>] ``(\w*) ← hit\((\w*)\)`` (name1: string, name2: string) =
            let intersections = driver.ForceObject<Intersections.Intersections> name2
            let hit = intersections |> Intersections.hit
            do driver.SetObject (name1, hit)   
        
        let [<Then>] ``(\w*).count = (\d*)`` (name: string, hits: int) =
            let intersections = driver.ForceObject<Intersections.Intersection list> name
            intersections |> should haveLength hits
        
        let [<Then>] ``xs\[(\d*)\].t = (\d*)`` (index: int, t: float) =
            let intersections = driver.ForceObject<Intersections.Intersection list> "xs"
            (intersections |> List.skip index |> List.head).T |> should equal t
            
        let [<Then>] ``xs\[(\d)\] = (.*)`` (index: int, value: float) =
            let intersections = driver.ForceObject<Intersections.Intersection list> "xs"
            let element = intersections |> List.skip index |> List.head
            element.T |> should equal value
            
        let [<Then>] ``xs\[(\d)\].object = s`` (index: int) =
            let intersections = driver.ForceObject<Intersections.Intersection list> "xs"
            let element = intersections |> List.skip index |> List.head
            let expected = driver.ForceSphere "s"
            match element.Target with
            | Intersections.IntersectionTarget.Sphere s ->
                s |> should equal expected

        let [<Then>] ``i = i4`` () =
            let hit = driver.ForceObject<Intersections.Hit> "i"
            let intersection = driver.ForceObject<Intersections.Intersection> "i4"
            hit.Value |> should equal intersection
            
        let [<Then>] ``i is nothing`` () =
            let hit = driver.ForceObject<Intersections.Hit> "i"
            hit |> Expect.isNone "Expected no intersection but found one"
        
        let [<Then>] ``i = i1`` () =
            let hit = driver.ForceObject<Intersections.Hit> "i"
            let intersection = driver.ForceObject<Intersections.Intersection> "i1"
            hit.Value |> should equal intersection
        
        let [<Then>] ``i = i2`` () =
            let hit = driver.ForceObject<Intersections.Hit> "i"
            let intersection = driver.ForceObject<Intersections.Intersection> "i2"
            hit.Value |> should equal intersection
