#' Expected Values
#'
#' Returns the expected value depending on the entry for type
#' @param c number of servers
#' @param n number of customers
#' @param pn vector of probabilities of having n customers in the system
#' @param lambda arrival rate
#' @param type character, type in E_Nq, E_N, E_Wq, E_W for the desired expected value
#'
#' @return Expected Values either of E_Nq, E_N, E_Wq, E_W
#' @export
#'
#' @examples EV(c=1, n=4, pn=c(0.410, 0.274, 0.167, 0.097, 0.052), lambda=20, type="E_Nq")
EV <- function(c, n, pn, lambda, type=""){

  # Berechnung E_N
  N <- c(0:n)
  en <- sum(N*pn)

  # Berechnung E_Nq
  # Definition range und leerer Vektor
  N2 <- c(1:n)
  s <- rep(0, n)

  # For iteration um E_Nq zu berechnen
  for (i in 1:n){
    if (i <= c){
      s[i] <- 0
    }
    else{
      s[i] <- N2[i-1]*pn[i+1]
    }
  }
  enq <- sum(s)

  # Berechnung lambda hat
  lambda_hat <- lambda*(1-pn[(n+1)])
  names(lambda_hat) <- ""


  # Rückgabe der Erwartungswerte
  if (type == "E_N"){
    return(en)
  }
  else if (type == "E_Nq"){
    return(enq)
  }
  else if (type == "E_W"){
    return(en/lambda_hat)
  }
  else if (type == "E_Wq"){
    return(enq/lambda_hat)
  }
}
