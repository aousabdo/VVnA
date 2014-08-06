#' Function to calculate displaement and velocity of a projectile in vacuum 
#' 
#' This function takes in the initial velocity, time of flight, 
#' and returns the distance travlled and speed at a given time t
#' @param v0 initial velocity in m/s
#' @param y0 initial height in m
#' @param theta0 initial angle in degrees
#' @param t Time of flight in seconds
#' @return x_t Displacement in the horizontal direction as a function of time (in meters)
#' @return vx_t speed in the horizontal direction as a function of time (in m/s units)
#' @return y_t Displacement in the vertical direction as a function of time (in meters)
#' @return vy_t speed in the vertical direction as a function of time (in m/s units)
#' @return y_x Displacement in the vertical direction as a function of horizontal displacement (in meters)
#' @export
Projectile <-  function(y0, v0, theta0, t){
        if(!is.numeric(y0)) stop('y0 must be numeric')
        if(!is.numeric(v0)) stop('v0 must be numeric')
        if(!is.numeric(theta0)) stop('theta0 must be numeric')
        if(!is.numeric(t)) stop('t must be numeric')
        
        ## calculate horizontal and vertical components of the initial velocity
        vx0 <- v0*cos(theta0*pi/180)
        vy0 <- v0*sin(theta0*pi/180)    
    
        ## acceleration of gravity
        g <- 9.8
        
        ## calculate displacement and velocity in the horizontal direction as a function of time
        x_t  <- vx0*t
        vx_t <- vx0 
        
        ## calculate displacement and velocity in the vertical direction as a function of time
        y_t  <- y0 + vy0*t - 0.5*g*t^2
        vy_t <- vy0 - g*t
                
        ## calculate vertical distance as a function of horizontal displacement
        y_x <- tan(theta0*pi/180)*x_t -(g/(2*vx0^2)*x_t^2)
        
        return(c(x_t, vx_t, y_t, vy_t, y_x))
}