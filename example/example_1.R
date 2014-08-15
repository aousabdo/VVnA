example_1 <- function(t = seq(0,50,0.1), y0= 10, v0 = 500, theta0 = 30, b = 0.9,  m = 5, legend=TRUE){
        ## get projectile parameters for vacuum and air friction cases
        vacuum   <- projectile(t = t, y0 = y0,  v0 = v0, theta0 = theta0)
        friction <- projFrictoinLin(t = t, y0 = y0, v0 = v0, theta0 = theta0, b = b, m = m)
        
        x  <- vacuum$x; y  <- vacuum$y; xf <- friction$x; yf <- friction$y_x
        
        ## make plot
        plot(x, y, col="blue", pch=19, ylim=c(min(y, yf), max(y, yf)),
             xlab = "Horizontal Distance Travelled (m)",
             ylab = "Vertical Distance Travelled (m)", 
             main = "Projectile Motion")
        points(xf, yf, col="red", pch=21)
        if(legend){
                legend(x="topleft", legend = c("No Air Friction", "With Air Friction"),
                       col = c("blue", "red"), pch = c(19, 21))}
}
example_1()