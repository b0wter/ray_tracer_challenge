namespace RayTracer.Specflow

open System
open RayTracer.Challenge
open TechTalk.SpecFlow
open FsUnit.Xunit

module SpheresSteps =
    
    [<Binding>]
    type SphereSteps(driver: Driver.Driver) =
        
        let [<Then>] ``s.transform = identity_matrix`` () =
            let sphere = driver.ForceSphere "s"
            sphere.Transformation |> should equal Matrices.identity4
            
        let [<When>] ``set_transform\(s, t\)`` () =
            let originalSphere = driver.ForceSphere "s"
            let transform = driver.ForceMatrix "t"
            let updatedSphere = originalSphere |> Spheres.withTransform transform
            do driver.UpdateSphere ("s", updatedSphere)
            
        let [<When>] ``set_transform\(s, translation\(5, 0, 0\)\)`` () =
            let translation = Matrices.translation (5, 0, 0)
            do driver.SetMatrix ("s", translation)
            let updatedSphere =
                driver.ForceSphere "s"
                |> Spheres.withTransform translation
            do driver.UpdateSphere ("s", updatedSphere)
            
        let [<When>] ``set_transform\(s, scaling\((.*), (.*), (.*)\)\)`` (x: int, y: int, z: int) =
            let originalSphere = driver.ForceSphere "s"
            let transform = Matrices.scaling (x, y, z)
            let updatedSphere = originalSphere |> Spheres.withTransform transform
            do driver.UpdateSphere ("s", updatedSphere)
            
        let [<Then>] ``s\.transform = t`` () =
            let transform = driver.ForceMatrix "t"
            let sphere = driver.ForceSphere "s"
            sphere.Transformation |> should equal transform
            
        let [<When>] ``n ← normal_at\(s, point\(√3\/3, √3\/3, √3\/3\)\)`` () =
            let sphere = driver.ForceSphere "s"
            let sqrt3By3 = System.Math.Sqrt(3.0) / 3.0
            let normal = Spheres.normalAt (sphere, Tuple.createPoint(sqrt3By3, sqrt3By3, sqrt3By3))
            do driver.SetTuple ("n", normal)
            
        let [<Then>] ``n = vector\(√3\/3, √3\/3, √3\/3\)`` () =
            let normal = driver.ForceTuple "n"
            let sqrt3By3 = System.Math.Sqrt(3.0) / 3.0
            let expected = Tuple.createVector (sqrt3By3, sqrt3By3, sqrt3By3)
            normal |> should equal expected

        let [<When>] ``n ← normal_at\(s, point\(1, 0, 0\)\)`` () =
            let sphere = driver.ForceSphere "s"
            let normal = Spheres.normalAt (sphere, Tuple.createPoint(1, 0, 0))
            do driver.SetTuple ("n", normal)
            
        let [<Then>] ``n = vector\(1, 0, 0\)`` () =
            let normal = driver.ForceTuple "n"
            let expected = Tuple.createVector (1, 0, 0)
            normal |> should equal expected

        let [<When>] ``n ← normal_at\(s, point\(0, 1, 0\)\)`` () =
            let sphere = driver.ForceSphere "s"
            let normal = Spheres.normalAt (sphere, Tuple.createPoint(0, 1, 0))
            do driver.SetTuple ("n", normal)
            
        let [<Then>] ``n = vector\(0, 1, 0\)`` () =
            let normal = driver.ForceTuple "n"
            let expected = Tuple.createVector (0, 1, 0)
            normal |> should equal expected

        let [<When>] ``n ← normal_at\(s, point\(0, 0, 1\)\)`` () =
            let sphere = driver.ForceSphere "s"
            let normal = Spheres.normalAt (sphere, Tuple.createPoint(0, 0, 1))
            do driver.SetTuple ("n", normal)
            
        let [<Then>] ``n = vector\(0, 0, 1\)`` () =
            let normal = driver.ForceTuple "n"
            let expected = Tuple.createVector (0, 0, 1)
            normal |> should equal expected
            
        let [<Then>] ``n = normalize\(n\)`` () =
            let normal = driver.ForceTuple "n"
            normal |> Tuple.magnitude |> should equal 1.0
            
        let [<Given>] ``set_transform\(s, translation\(0, 1, 0\)\)`` () =
            let sphere = driver.ForceSphere "s"
            let transform = Matrices.translation (0, 1, 0)
            let updatedSphere = sphere |> Spheres.withTransform transform
            do driver.UpdateSphere ("s", updatedSphere)
            
        let [<When>] ``n ← normal_at\(s, point\(0, 1.70711, -0.70711\)\)`` () =
            let sphere = driver.ForceSphere "s"
            let point = Tuple.createPoint (0, 1.70711, -0.70711)
            let normal = Spheres.normalAt (sphere, point)
            do driver.SetTuple ("n", normal)
            
        let [<Then>] ``n = vector\(0, 0.70711, -0.70711\)`` () =
            let normal = driver.ForceTuple "n"
            let expected = Tuple.createVector (0, 0.70711, -0.70711)
            normal |> should equal expected
            
        let [<Given>] ``m ← scaling\(1, 0.5, 1\) \* rotation_z\(π/5\)`` () =
            let scaling = Matrices.scaling (1, 0.5, 1)
            let rotation = Matrices.rotationZ (Math.PI / 5.0)
            do driver.SetMatrix ("m", (scaling * rotation))
            
        let [<Given>] ``set_transform\(s, m\)`` () =
            let sphere = driver.ForceSphere "s"
            let matrix = driver.ForceMatrix "m"
            let updatedSphere = sphere |> Spheres.withTransform matrix
            do driver.UpdateSphere ("s", updatedSphere)
            
        let [<When>] ``n ← normal_at\(s, point\(0, √2\/2, -√2\/2\)\)`` () =
            let sphere = driver.ForceSphere "s"
            let point = Tuple.createPoint (-1.0, Math.Sqrt(2.0) / 2.0, -Math.Sqrt(2.0) / 2.0)
            let normal = Spheres.normalAt (sphere, point)
            do driver.SetTuple ("n", normal)
            
        let [<Then>] ``n = vector\(0, 0.97014, -0.24254\)`` () =
            let normal = driver.ForceTuple "n"
            let expected = Tuple.createVector (0.0, 0.97014, -0.24254)
            normal |> should equal expected