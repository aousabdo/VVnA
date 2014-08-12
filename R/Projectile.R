#' projectile
#' 
#' Function to calculate displaement and velocity of a projectile in vacuum 
#' 
#' This function takes in the initial velocity, time of flight, 
#' and returns the distance travlled and speed at a given time t
#' @param v0 initial velocity in m/s
#' @param y0 initial height in m
#' @param theta0 initial angle in degrees
#' @param t Time of flight in seconds
#' @return x Displacement in the horizontal direction as a function of time (in meters)
#' @return vx speed in the horizontal direction as a function of time (in m/s units)
#' @return y Displacement in the vertical direction as a function of time (in meters)
#' @return vy speed in the vertical direction as a function of time (in m/s units)
#' @return y_x Displacement in the vertical direction as a function of horizontal displacement (in meters)
#' @export
projectile <-  function(t, y0, v0, theta0){
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
        x  <- vx0*t
        vx <- vx0 
        
        ## calculate displacement and velocity in the vertical direction as a function of time
        y  <- y0 + vy0*t - 0.5*g*t^2
        vy <- vy0 - g*t
                
        ## calculate vertical distance as a function of horizontal displacement
        y_x <- y0 + tan(theta0*pi/180)*x -(g/(2*vx0^2)*x^2)
        
        ret = list(x = x, vx = vx, y = y, vy = vy, y_x = y_x)
        
        return(ret)
}