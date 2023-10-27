
# -----------------------------------------------------
# individual model (treatment covariate) with Weibull
# -------------------------------------------------------
ind_weib <- function(){
  for(i in 1:n){
    Y[i,1:3]  ~  dmulti(P[i,from[i],1:3],1)
    
    #transition probabilites
    P[i,1,1]     <- exp(-(G[i,1,2] * t[i]^(A) + G[i,1,3] * t[i]^(A)))
    P[i,1,2]     <- S[i]*exp(-(G[i,2,3])*t[i]^(A))*(1-exp(-((G[i,1,2] * t[i]^(A) + G[i,1,3] * t[i]^(A) - G[i,2,3] * t[i]^(A)))))
    P[i,1,3]     <- 1 - P[i,1,1] - P[i,1,2]
    P[i,2,1]     <- 0
    P[i,2,2]     <- exp(-(G[i,2,3])*t[i]^(A))
    P[i,2,3]     <- 1-P[i,2,2]
    S[i]         <- (G[i,1,2])/(G[i,1,2]+G[i,1,3]-G[i,2,3])
    
    #cumulative hazard function (without the t) 
    G[i,1,2]    <-  exp(-(A)*(log(R12) + inprod(Z_1[i, 1:Npar], beta12[1:Npar])))
    G[i,2,3]    <-  exp(-(A)*(log(R23) + inprod(Z_1[i, 1:Npar], beta23[1:Npar])))
    G[i,1,3]    <-  exp(-(A)*(log(R13) + inprod(Z_1[i, 1:Npar], beta13[1:Npar])))

  }
  
  for (j in 1:n){
    t[j] ~ dweib(ifelse(from[j] == 1, A, A), ifelse(from[j] == 1, G[j,1,2], G[j,2,3]));T(t.cen.lim[j,1], t.cen.lim[j,2])
  }
  
  #priors
  
  R12 ~ dnorm(0, 0.0001);T(0,)
  R23 ~ dnorm(0, 0.0001);T(0,)
  R13 ~ dnorm(0, 0.0001);T(0,)
  
  for (m in 1:Npar) {
    beta12[m] ~ dnorm(0.0, 0.001) 
    beta13[m] ~ dnorm(0.0, 0.001)
    beta23[m] ~ dnorm(0.0, 0.001)
  }
  
  A ~ dexp(2)
 
}






