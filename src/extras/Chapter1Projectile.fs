namespace Raytracer.Extras

open RayTracer.Challenge

module Chapter1Projectile =
    
    type Environment =
        {
            Gravity : Tuple.Tuple
            Wind: Tuple.Tuple
            /// <summary>
            /// Timestep size in seconds.
            /// </summary>
            TimeStepSize: float
        }
        
    type Projectile =
        {
            Position: Tuple.Tuple
            Velocity: Tuple.Tuple
        }
    
    let tick env projectile =
        {
            projectile with
                Position = projectile.Position + projectile.Velocity * env.TimeStepSize
                Velocity = projectile.Velocity + (env.Gravity + env.Wind) * env.TimeStepSize
        }
        
    let run env projectile =
        let tick = tick env
        let rec step previousSteps projectile =
            let previousSteps = projectile :: previousSteps
            if projectile.Position.Y < 0 then
                previousSteps |> List.rev
            else
                let updatedProjectile = projectile |> tick
                step previousSteps updatedProjectile
        step [] projectile

    let runPredefined () =
        let env =
            {
                Gravity = Tuple.createVector (0.0, -9.81, 0.0)
                Wind = Tuple.createVector (-1.0, 0.0, 0.0)
                TimeStepSize = 0.25
            }
        let projectile =
            {
                Position = Tuple.createPoint (0.0, 0.0, 0.0)
                Velocity = Tuple.createVector (20.0, 40.0, 0.0)
            }
        run env projectile