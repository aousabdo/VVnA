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
#' @return x_t Displacement in the horizontal direction as a function of time (in meters)
#' @return vx_t speed in the horizontal direction as a function of time (in m/s units)
#' @return y_t Displacement in the vertical direction as a function of time (in meters)
#' @return vy_t speed in the vertical direction as a function of time (in m/s units)
#' @return y_x Displacement in the vertical direction as a function of horizontal displacement (in meters)
#' @examples
#' 
#' projFrictoinLin(y0 = 10, v0 = 50, theta0 = 30, t = 12, b = 0.5, m = 5)
#' 
#'  ## Example of how to call both projectile functions
#'  ## and make a projectile path plot 
#'  t      <- seq(0,5,0.1)
#'  v0     <- 50 
#'  theta0 <- 30
#'  b      <- 0.9
#'  m      <- 5
#'  x_t  <- vector(mode="numeric", length=length(t))
#'  y_t  <- vector(mode="numeric", length=length(t))
#'  xf_t <- vector(mode="numeric", length=length(t))
#'  yf_t <- vector(mode="numeric", length=length(t))
#'  for (i in 1:length(t)){
#'         x_t[i]  <- Projectile(y0 = 10, v0 = v0, 
#'         theta0 = theta0, t = t[i])[1]    
#'         y_t[i]  <- Projectile(y0 = 10, v0 = v0, 
#'         theta0 = theta0, t = t[i])[5] 
#'         xf_t[i] <- projFrictoinLin(y0 = 10, v0 = v0, 
#'         theta0 = theta0, t = t[i], b = b, m = m)[1]    
#'         yf_t[i] <- projFrictoinLin(y0 = 10, v0 = v0, 
#'         theta0 = theta0, t = t[i], b = b, m = m)[5]
#' }
#' 
#' plot(x_t, y_t, col="blue", pch=19, ylim=c(min(y_t, yf_t), max(y_t, yf_t)), 
#'      xlab = "Horizontal Distance Travelled (m)", 
#'      ylab = "Vertical Distance Travelled (m)")
#' points(xf_t, yf_t, col="red", pch=21)
#' legend(x="topleft", legend = c("No Friction", "Friction"), 
#'     col = c("blue", "red"), pch = c(19, 21))
#' @export
projFrictoinLin <-  function(y0, v0, theta0, t, b, m){
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
        x_t  <- vx0*tau*(1-exp(-t/tau))
        vx_t <- vx0*exp(-t/tau)

        ## calculate displacement and velocity in the vertical direction as a function of time
        y_t  <- y0 - vter*t + tau*(vy0 + vter)*(1-exp(-t/tau))
        vy_t <- vy0*exp(-t/tau) + vter*(1-exp(-t/tau))
        
        ## calculate vertical distance as a function of horizontal displacement
        y_x <- y0 + ((vy0 + vter)/vx0) * x_t + vter*tau*log(1-(x_t/(vx0*tau)))
        
        return(c(x_t, vx_t, y_t, vy_t, y_x))
}