theta_h <- function(h,theta_s,theta_r,alpha,n){

  m = 1 - 1/n

  # Se <- rep(NA,length(h))
  #
  # Se[h>= 0] <- 1
  # Se[h < 0] <- (1 + (alpha*abs(h)**n))**(-m)

  Se <- (1 + (alpha*abs(h))**n)**(-m)

  theta <- (theta_s - theta_r)*Se + theta_r

  return(theta)

}
