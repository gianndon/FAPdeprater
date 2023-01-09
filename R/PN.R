#' Pn Function
#'
#' Returns the probabilities of having n customers in the system
#' @param n number of effective customers/objects
#' @param lambdas vector of dependent arrival rates !IMPORTANT! MUST HAVE LENGTH N!
#' @param mus vector of dependent service rates !IMPORTANT! MUST HAVE LENGTH N!
#'
#' @return Probability
#' @export
#'
#' @examples PN(n=4, lambdas=rep(20, 4), mus=c(30, 32.72, 34.48, 37.39))
PN <- function(n, lambdas, mus){

  # Attention! lambdas are lambda-1
  pn_temp <- c(1, rep(0, n))

  # Temporäre pn
  for (i in 1:n){
    pn_temp[i+1] <- pn_temp[i]*lambdas[i]/mus[i]
  }
  summe_pn_temp <- sum(pn_temp)

  # Pn
  pn <- pn_temp/summe_pn_temp
  test <- sum(pn)

  # Überprüfung ob pn = 1 ist
  if (test == 1){

    # Generate names
    n <- noquote(paste0('P', c(0:n)))
    names(pn) <- n

    # return Value
    return(pn)
  }
  else{
    print("Attention! Something went wrong, sum of Pn is not equal 1! Check for mistakes!", quote=FALSE)
  }
}
