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
            do driver.SetSphere ("s", updatedSphere)
            