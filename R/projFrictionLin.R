#' Function to calculate displacement and velocity of a projectile when air friction is introduced.
#' This function only treats the linear case of velocity dependence. 
#' 
#' This function takes in the initial velocity, time of flight, 
#' drag coefficient, and mass of object and returns the distance 
#' travlled and speed at a given time t
#' @author Aous Abdo
#' @param v0 initial velocity in m/s
#' @param y0 initial height in m
#' @param theta0 initial angle in degrees
#' @param t Time of flight in seconds
#' @param b drag coefficient in Newtons.seconds/meters
#' @param m mass of object in kg
#' @return x Displacement in the horizontal direction as a function of time (in meters)
#' @return vx speed in the horizontal direction as a function of time (in m/s units)
#' @return y Displacement in the vertical direction as a function of time (in meters)
#' @return vy speed in the vertical direction as a function of time (in m/s units)
#' @return y_x Displacement in the vertical direction as a function of horizontal displacement (in meters)
#' @examples
#' 
#' projFrictoinLin(t = seq(1,5,0.1), y0 = 10, v0 = 50, theta0 = 30, b = 0.5, m = 5)
#' 
#' ## Example of how to call both projectile functions
#' ## and make a projectile path plot 
#' t      <- seq(0,5,0.1)
#' v0     <- 50 
#' theta0 <- 30
#' b      <- 0.9
#' m      <- 5
#'  
#' vacuum   <- Projectile(t, y0 = 10, v0 = v0, theta0 = theta0)
#' friction <- projFrictoinLin(t, y0 = 10, v0 = v0, theta0 = theta0, b = b, m = m)
#' 
#' x  <- vacuum$x
#' y  <- vacuum$y
#' xf <- friction$x
#' yf <- friction$y_x
#' 
#' plot(x, y, col="blue", pch=19, ylim=c(min(y, yf), max(y, yf)), 
#'      xlab = "Horizontal Distance Travelled (m)", 
#'      ylab = "Vertical Distance Travelled (m)")
#' points(xf, yf, col="red", pch=21)
#' legend(x="topleft", legend = c("No Friction", "Friction"), 
#'     col = c("blue", "red"), pch = c(19, 21))
#' 
#' @export
projFrictoinLin <-  function(t, y0, v0, theta0, b, m){
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
        y  <- y0 - vter*t + tau*(vy0 + vter)*(1-exp(-t/tau))
        vy <- vy0*exp(-t/tau) + vter*(1-exp(-t/tau))
        
        ## calculate vertical distance as a function of horizontal displacement
        y_x <- y0 + ((vy0 + vter)/vx0) * x + vter*tau*log(1-(x/(vx0*tau)))

        ret = list(x = x, vx = vx, y = y, vy = vy, y_x = y_x)
        
        return(ret)
}