Friction <- function(t = seq(0,5,0.1), y0= 10, v0 = 50, theta0 = 30, b = 0.9,  m = 5, legend=TRUE){
        ## get projectile parameters for air friction case
        friction <- projFrictoinLin(t = t, y0 = y0, v0 = v0, theta0 = theta0, b = b, m = m)
        
        x <- friction$x; y <- friction$y_x
        
        ## make plot
        par(mfrow=c(2,2), mar = c(3,3,0.5,1), oma=c(2,2,2,2), mgp=c(2.2,1,0))
        plot(t, friction$x, col="blue", pch=19, ylab="Horiz. Disp (m)")
        plot(t, friction$vx, col="blue", pch=19, ylab="Horiz. Spd")
        plot(t, friction$y, col="red", pch=19, ylab="Vert. Disp (m)")
        plot(t, friction$vy, col="red", pch=19, ylab="Vert. spd (m)")
        title("Projectile in Friction", outer=TRUE)
}
Friction()