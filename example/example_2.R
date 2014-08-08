t      <- seq(0,5,1)
v0     <- 50
y0     <- 0
theta0 <- 30
b      <- 0.5
m      <- 5

truth <- projFrictoinLin(t = t, y0 = y0, v0 = v0, theta0 = theta0, b = b, m = m)

n <- 10
df <- data.frame(y0=jit(y0,n), v0=jit(v0,n), theta0=jit(theta0,n),b=jit(b,n),m=jit(m,n))

MC <- list()

for(i in 1:n){
        MC[[length(List)+1]] <- with(df, projFrictoinLin(t = t, y0 = y0[i], 
                                                         v0 = v0[i], theta0 = theta0[i], 
                                                         b = b[i], m = m[i]))
}