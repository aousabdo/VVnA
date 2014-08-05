b <- 0.1
m <- 0.1
t <- seq(0,5,0.5)
Vx0 <- 11

disp_vel <-  function(Vx0=10, t=10, b=0.1, m=0.1){
        #' Function to calculate displaement and velocity of a projectile when air friction is introduced
        X <- Vx0*m/b*(1-exp(-t*b/m))
        Vx <- Vx0*exp(-t*b/m)
        return(c(X, Vx))
}

Vx <- array(data = NA, dim=length(t))
X <- array(data = NA, dim=length(t))
for (i in 1:length(t)){
        X[i]  <- disp_vel(t=t[i])[1]
        Vx[i] <- disp_vel(t=t[i])[2]
}
        
plot(t, Vx, col="blue", type="o", ylim=c(0, max(Vx,X)))
points(t,X, col="red")
lines(t,X, col="red")


#' Find column name corresponding to a particular functional
#'
#' The original data set contains very long column headers. This function
#' does a keyword search over the headers to find those column headers that
#' match a particular keyword, e.g., mean, median, etc.
#' @param x The data we are querying (data.frame)
#' @param v The keyword we are searching for (character)
#' @param ignorecase Should case be ignored (logical)
#' @return A vector of column names matching the keyword
#' @export
findvar <- function(x,v, ignorecase=TRUE) {
        if(!is.character(v)) stop('name must be character')
        if(!is.data.frame(x)) stop('x must be a data.frame')
        v <- grep(v,names(x),value=T, ignore.case=ignorecase)
        if(length(v)==0) v <- NA
        return(v)
}