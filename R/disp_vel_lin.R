#' Function to calculate displaement and velocity of a projectile when air friction is introduced
#' This function only treats the linear case of velocity dependence. 
#' 
#' This function takes in the initial velocity, time of flight, 
#' drag coefficient, and mass of object and returns the distance 
#' travlled and speed at a given time t
#' @param v0 initial velocity in m/s
#' @param y0 initial height in m
#' @param theta0 initial angle in degrees
#' @param t Time of flight in seconds
#' @param b drag coefficient in Newtons.seconds/meters
#' @param m mass of object in kg
#' @return x Displacement in the horizontal direction in meters
#' @return vx speed in the horizontal direction at a given time t (in m/s units)
#' @return y Displacement in the vertical direction in meters
#' @return vy speed in the vertical direction at a given time t (in m/s units)
#' @export
disp_vel_lin <-  function(y0, v0, theta0, t, b, m){
        if(!is.numeric(y0)) stop('y0 must be numeric')
        if(!is.numeric(v0)) stop('v0 must be numeric')
        if(!is.numeric(theta0)) stop('theta0 must be numeric')
        if(!is.numeric(t)) stop('t must be numeric')
        if(!is.numeric(b)) stop('b must be numeric')
        if(!is.numeric(m)) stop('m must be numeric')
        
        ## calculte tau
        tau <- m/b
        ## calculate horizontal and vertical components of the initial velocity
        vx0 <- v0*cos(theta0*pi/180)
        vy0 <- v0*sin(theta0*pi/180)
        
        ## terminal velocity 
        g <- 9.8 ## acceleration of gravity in Newton/meter
        vter <- (g*tau)
        
        ## calculate displacement and velocity in the horizontal direction as a function of time
        x  <- vx0*tau*(1-exp(-t/tau))
        vx <- vx0*exp(-t/tau)

        ## calculate displacement and velocity in the vertical direction as a function of time
        y  <- y0 + vter*t + tau*(vy0-vter)*(1-exp(-t/tau))
        vy <- vy0*exp(-t/tau) + vter*(1-exp(-t/tau))
        
        return(c(x, vx, y, vy))
}
