#' jit
#' 
#' simple function to jitter a vector by the specified amount
#' after repeating it n numbers
jit <- function(x, n, amount=NULL){
        jitter(rep(x, n), amount = amount)
}