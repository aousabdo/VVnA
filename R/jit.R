#' jit
#' 
#' simple function to jitter a vector by the specified amount
#' after repeating it n numbers
#' 
#' @param x a numerical Vector
#' @param n an integer representing how many times to repeat the jittering 
#' for each entry in x. If n is a non-integer numeric, it will coerced
#' into an integer
#' @param amount same as in \link{jitter} when using unifrom values
#' and is equal to the standard deviation when using normal values
#' @param method randomizing method: "norm" or "uniform"
#' @examples jit(x = 40, n = 10, method = "norm", amount=3)
#' jit(x=10, n=5, method="uniform")
#' @export
#' 
jit <- function(x, n, amount=NULL, method="norm"){
        if(!is.numeric(x)) stop('x must be numeric')
        if(!is.numeric(n)) stop('n must be numeric')
        if(method=="uniform"){
                if(!is.null(amount)){if(!is.numeric(amount)) stop('amount must be numeric')}
                rand <-  jitter(rep(x, n), amount = amount)
        }
        else if(method=="norm"){
                if(is.null(amount)) stop('amount must be numeric')
                rand <- rnorm(n, mean=x, sd=amount)
        }
        return(rand)
}