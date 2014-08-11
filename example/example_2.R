## initialize parameters
t      <- seq(0,5,0.1)
v0     <- 50
y0     <- 0
theta0 <- 30
b      <- 0.5
m      <- 5

## We will assume that truth to be the projectile motion as initialized
## by the values above
truth <- projFrictoinLin(t = t, y0 = y0, v0 = v0, theta0 = theta0, b = b, m = m)

## Simulate n number of runs while slightley changing each input parameter
Sim <- function(n=5, amount=1, seed=123){
        #function to simulate projectile motion
        set.seed(seed)
        df <- data.frame(y0=jit(y0,n, amount), v0=jit(v0,n, amount), 
                         theta0=jit(theta0,n, amount),b = jit(b,n, amount),
                         m=jit(m,n, amount))       
        ## List of projectile data for MC runs
        MC <- list()     
        for(i in 1:n){
                MC[[i]] <- with(df, projFrictoinLin(t = t, y0 = y0[i], 
                                                    v0 = v0[i], 
                                                    theta0 = theta0[i], 
                                                    b = b[i], m = m[i]))
        }
        # return list
        return(MC)
}

MC <- Sim()

makePlot <- function(simList, par=2){
        if(par==1){
                parplot="x"
                yLab = "Horizontal Displacement (m)"
        }
        if(par==2){
                parplot="y"
                yLab="Vertical Displacement (m)"
        }
        Min <- min(simList[[1]][[parplot]])
        print(Min)
        Max <- max(simList[[1]][[parplot]])
        for(i in 1:n){
                Min <- min(Min, simList[[i]][[parplot]])
                Max <- max(Max, simList[[i]][[parplot]])
        }
        
        plot(t, truth[[parplot]], ylim=c(Min, Max), pch=19, col="blue",
             ylab=yLab, xlab="Time (s)")
        for (i in 1:n){
                points(t, simList[[i]][[parplot]], col=i, pch=5)
        }
        legend(x="topleft", legend = c("Truth", "MC"), pch=c(19, 5), col=c("blue"))
}
makePlot(MC, par = 2)