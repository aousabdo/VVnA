#' Function to calculate displaement and velocity of a projectile when air friction is introduced
#' 
#' This function takes in the initial velocity, time of flight, 
#' drag coefficient, and mass of object and returns the distance 
#' distance travlled and speed at a given time t
#' @param Vx0 initial velocity in the horizontal direction
#' @param t Time of flight in seconds
#' @param b drag coefficient
#' @param m mass of object in kg
#' @return X Displacement in the horizontal direction
#' @return Vx speed in the horizontal direction at a given time t
#' @export
disp_vel_lin <-  function(Vx0=10, t=10, b=0.1, m=0.1){
        X <- Vx0*m/b*(1-exp(-t*b/m))
        Vx <- Vx0*exp(-t*b/m)
        return(c(X, Vx))
}
