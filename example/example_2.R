## initialize parameters
t      <- seq(0,5,0.1)
v0     <- 50
y0     <- 0
theta0 <- 30
b      <- 0.5
m      <- 5

## We will assume the truth to be the projectile motion as initialized
## by the values above
set.seed(pi)
truth <- projFrictoinLin(t = t, y0 = y0, v0 = v0, theta0 = theta0, b = b, m = m)

## Simulate n number of runs while slightley changing each input parameter
Sim <- function(t = seq(0,5,0.1), y0 = 0, v0 = 50, 
                theta0 = 30, b = 0.5, m = 5, 
                n = 5, amount=1, seed=123){
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
                                                    b = abs(b[i]), m = abs(m[i])))
        }
        # return list
        return(MC)
}

makePlot <- function(simList=NULL, par=2, N){
        ## function to plot projectile parameter as a function to time
        if(par==1){
                parplot="x"
                yLab = "Horizontal Displacement (m)"
        }
        else if(par==2){
                parplot="y"
                yLab="Vertical Displacement (m)"
        }
        if(par==3){
                parplot="vx"
                yLab = "Horizontal Velocity (m)"
        }
        else if(par==4){
                parplot="vy"
                yLab="Vertical Velocity (m)"
        }
        Min <- min(simList[[1]][[parplot]])
        Max <- max(simList[[1]][[parplot]])
        for(i in 1:N){
                Min <- min(Min, simList[[i]][[parplot]])
                Max <- max(Max, simList[[i]][[parplot]])
        }
        
        plot(t, truth[[parplot]], ylim=c(Min, Max), pch=19, col="blue",
             ylab=yLab, xlab="Time (s)")
        for (i in 1:N){
                points(t, simList[[i]][[parplot]], col=i, pch=5)
        }
        legend(x="topleft", legend = c("Truth", "MC"), pch=c(19, 5),
               col=c("blue", 1))
}

## Make some plots
## prepare the gird layout
par(mfcol=c(2,2), mar = c(3,3,0.5,1), oma=c(2,2,2,2), mgp=c(2.2,1,0))

## we'll simulate 5 runs
N <- 5

makePlot(simList = Sim(n = N, amount = 1, seed = 123), par = 1, N = N)
makePlot(simList = Sim(n = N, amount = 1, seed = 123), par = 2, N = N)
makePlot(simList = Sim(n = N, amount = 1, seed = 123), par = 3, N = N)
makePlot(simList = Sim(n = N, amount = 1, seed = 123), par = 4, N = N)