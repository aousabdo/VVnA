example_1 <- function(t = seq(0,5,0.1), v0 = 50, theta0 = 30, b = 0.9,  m = 5){
        
        vacuum   <- Projectile(t = seq(0,5,0.1), y0 = 10, v0 = 50, theta0 = 30)
        friction <- projFrictoinLin(t = seq(0,5,0.1), y0 = 10, v0 = 50, theta0 = 30, b = 0.5, m = 5)
        
        x  <- vacuum$x; y  <- vacuum$y; xf <- friction$x; yf <- friction$y_x
        
        plot(x, y, col="blue", pch=19, ylim=c(min(y, yf), max(y, yf)),
             xlab = "Horizontal Distance Travelled (m)",
             ylab = "Vertical Distance Travelled (m)")
        points(xf, yf, col="red", pch=21)
        legend(x="topleft", legend = c("No Friction", "Friction"),
               col = c("blue", "red"), pch = c(19, 21))
}
