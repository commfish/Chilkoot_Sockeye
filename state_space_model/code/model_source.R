#State Space Model Function with autocorrelation
mod=function(){
  
  #PRIORS
  lnalpha ~ dnorm(0,1.0E-6)%_%T(0,10) #uninformative
  beta ~ dnorm(0,1.0E-6)%_%T(0,10)  #uninformative, normal distribution, constrained to be >0
  phi ~ dnorm(0,1.0E-6)%_%T(-0.98,0.98) #AR(1) model so phi IS included and does not = zero. uninformative btwn -1 & 1
  resid.red.0 ~ dnorm(0,tau.red)
  sigma.white ~ dunif(0,10)
  
  
  for(y in 1:n) {lnRS[y] ~ dnorm(mean2.lnRS[y],tau.white) }  #Is this a prior or not????
  
  mean2.lnRS[1] <- mean1.lnRS[1] + phi * resid.red.0  
  for (y in 2:n) { mean2.lnRS[y] <- mean1.lnRS[y] + phi * resid.red[y-1] }   #AR1
  
  for(y in 1:n) {  mean1.lnRS[y] <- lnalpha - beta * S[y]  } #This is the Ricker model
  for(y in 1:n) {  resid.red[y]     <- lnRS[y] - mean1.lnRS[y]  }
  for(y in 1:n) {  resid.white[y] <- lnRS[y] - mean2.lnRS[y]  }
  
  
  
  alpha <- exp(lnalpha) #exponentiate to solve for alpha
  sigma.red <- 1 / sqrt(tau.red)
  tau.white <- 1 / sigma.white / sigma.white
  tau.red <- tau.white * (1-phi*phi)
  
  #sigma.white<-1/sqrt(tau.white)
  #sigma<-sigma.red
  
  lnalpha.c <- lnalpha + (sigma.red * sigma.red / 2)  #adjust for calculating means of R.msy, S.msy etc.
  #for the AR model
  #lnalpha.c <- lnalpha
  
  S.max <- 1 / beta
  S.eq <- S.max * lnalpha.c 
  
  S.msy <- S.eq * (0.5 - 0.07*lnalpha.c)  #Hilborn approximation to calculate Smsy
  U.msy <- lnalpha.c * (0.5 - 0.07*lnalpha.c)  #Hilborn approximation of U.msy
  R.msy <- S.msy * exp(lnalpha.c - beta * S.msy) #Xinxian's calculation of R.msy
  MSY <- step(R.msy-S.msy)*(R.msy-S.msy) #if R.msy < S.msy then MSY=0.
}


