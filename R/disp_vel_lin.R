## A function to ...
disp_vel_lin <-  function(Vx0=10, t=10, b=0.1, m=0.1){
        #' Function to calculate displaement and velocity of a projectile when air friction is introduced
        X <- Vx0*m/b*(1-exp(-t*b/m))
        Vx <- Vx0*exp(-t*b/m)
        return(c(X, Vx))
}