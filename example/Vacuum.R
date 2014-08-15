Vacuum <- function(t = seq(0,5,0.1), y0= 10, v0 = 50, theta0 = 30, legend=TRUE){
        ## get projectile parameters for vacuum
        vacuum   <- projectile(t = t, y0 = y0,  v0 = v0, theta0 = theta0)
        
        x  <- vacuum$x; y  <- vacuum$y
        
        ## make plot
        par(mfrow=c(2,2), mar = c(3,3,0.5,1), oma=c(2,2,2,2), mgp=c(2.2,1,0))
        plot(t, vacuum$x, col="blue", pch=19, ylab="Horiz. Disp (m)")
        plot(t, rep(vacuum$vx,length(t)), col="blue", pch=19, ylab="Horiz. Spd")
        plot(t, vacuum$y, col="red", pch=19, ylab="Vert. Disp (m)")
        plot(t, vacuum$vy, col="red", pch=19, ylab="Vert. spd (m)")
        title("Projectile in Vacuum", outer=TRUE)
}
Vacuum()