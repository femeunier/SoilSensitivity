K_h <- function(h,K_stheta_s,theta_r,alpha,n,L){

 m <- 1 - 1/n
 theta <- theta_h(h,theta_s,theta_r,alpha,n)
 Se <- Se(theta,theta_s,theta_r)

 K <- K_s*(Se**L)*(1 - (1- Se**(1/m))**m)**2

 return(K)
}
