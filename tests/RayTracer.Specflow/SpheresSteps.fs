namespace RayTracer.Specflow

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