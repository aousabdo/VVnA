line3D <- function(x0=0, y0=0, z0=0, vx0=1, vy0=1, vz0=1, t){
        x <- x0 + vx0 * t
        y <- y0 + vy0 * t
        z <- z0 + vz0 * t
        return(list(x=x,y=y,z=z))
}

print(line3D(t = seq(1,10,.1)))