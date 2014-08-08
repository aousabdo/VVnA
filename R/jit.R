#' jit
#' 
#' simple function to jitter a vector by the specified amount
#' after repeating it n numbers
#' 
#' @param x a numerical Vector
#' @param n an integer representing how many times to repeat the jittering 
#' for each entry in x. If n is a non-integer numeric, it will coerced
#' into an integer
#' @param amount same as in \link{jitter}
#' @examples jit(1:5, 2, 3)
#' @export
#' 
jit <- function(x, n, amount=NULL){
        if(!is.numeric(x)) stop('x must be numeric')
        if(!is.numeric(n)) stop('n must be numeric')
        if(!is.null(amount)){if(!is.numeric(amount)) stop('amount must be numeric')}
        jitter(rep(x, n), amount = amount)
}