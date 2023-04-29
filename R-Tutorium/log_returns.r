log_function <- function(x){
  help1  <- x[-1]
  help2  <- x[-length(x)]
  log    <- log(help1)-log(help2)
}