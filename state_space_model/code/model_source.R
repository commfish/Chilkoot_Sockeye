#State Space Model Function

# this function gets sourced from this file, and then 
# written in JAGS format using write.model() 
# This happens in the main script

#Definitions:
#Terminal Run = US Harvest + Abover Border Run
#Above Border Run = Esc + Canadian Harvest + Test Fishery Harvest
#Terminal Run = Esc + Canadian Harvest + Test Fishery Harvest + US Harvest
#Esc = Above Border Run estimate -  Canadian Harvest

mod=function(){
  for (y in (A+a.min):(Y+A-1)) {
    log.R[y] ~ dnorm(log.R.mean2[y],tau.R)
    R[y] <- exp(log.R[y])
    log.R.mean1[y] <- log(S[y-a.max]) + lnalpha - beta * S[y-a.max] 
    log.resid[y] <- log(R[y])-log.R.mean1[y]
  }
  log.R.mean2[A+a.min] <- log.R.mean1[A+a.min] + phi * log.resid.0 
  for (y in (A+a.min+1):(Y+A-1)) {
    log.R.mean2[y] <- log.R.mean1[y] + phi * log.resid[y-1]
  }
#PRIORS
  lnalpha ~ dnorm(0,1.0E-6)%_%T(1.0E-6,)
#normal distribution with mean 0 and large variance; constrained to be >0 since more biologically conservative (pg. 406)
  beta ~ dnorm(0,1.0E-6)%_%T(1.0E-6,)   #uninformative; normal distrib; constrained to be >0 *          
  phi ~ dnorm(0,1.0E-6)%_%T(-1,1)#uninformative; btw -1 and 1
  mean.log.RO ~ dnorm(0,1.0E-6)#uninformative (initial returns); mean of initial returns
  tau.RO ~ dgamma(0.001,0.001)#uninformative (initial returns); variance of initial returns
  log.resid.0 ~ dnorm(0,tau.red)#model residual with AR(1) process error
  
  tau.R ~ dgamma(0.001,0.001) 
  sigma.R <- 1 / sqrt(tau.R) 
  alpha <- exp(lnalpha)
  sigma.RO <- 1 / sqrt(tau.RO)#uninformative; informative (not shown) prior had large effect on parameter itself 
#and initial R values, but not on key model quantities
  tau.red <- tau.R * (1-phi*phi)
  lnalpha.c <- lnalpha + (sigma.R * sigma.R / 2 / (1-phi*phi) ) 
  #lnalpha ~ dnorm(0,1.0E-6)%_%T(0,) #alt
  #sigma.R ~dunif(0,100) #alt
  #tau.red <- sigma.R*sigma.R * (1-phi*phi) #alt
  
#THE FIRST SEVERAL COHORTS ORIGINATE FROM UNMONITORED SPAWNING EVENTS
#DRAW THESE RETURNS FROM A COMMON LOGNORMAL DISTRIBUTION 
  R.O<-exp(mean.log.RO)
  for (y in 1:a.max) { 
    log.R[y] ~ dnorm(mean.log.RO,tau.RO) 
    R[y] <- exp(log.R[y]) 
  }
#REFERENCE POINTS (WITH CORRECTION FOR LOGNORMAL SKEWNESS)
  S.max <- 1 / beta 
  alpha.c <- min(exp(lnalpha.c),1.0E4)
  S.eq.c <- lnalpha.c * S.max #Eq.21
  U.msy.c <- lnalpha.c * (0.5-0.07*lnalpha.c) #Hilborn approximation (Hilborn 1985)
  S.msy.c <- S.eq.c *(0.5-0.07*lnalpha.c)   #Hilborn approximation (Hilborn 1985)
  S.msy.c.80 <- S.msy.c *0.80
  positive.lna.c <- step(lnalpha.c)
  lnalpha.c.nonneg <- lnalpha.c * positive.lna.c
  S.eq.c2 <- lnalpha.c.nonneg * S.max 
  peterman.approx.c <- (0.5 - 0.65*pow(lnalpha.c.nonneg,1.27) / (8.7 +pow(lnalpha.c.nonneg,1.27)))
  U.msy.c2 <- lnalpha.c.nonneg * peterman.approx.c 
  S.msy.c2 <- U.msy.c2 / beta  #peterman approx. eq. #14 in Fleishman et al. 2013
  U.max.c2 <- 1 - 1 / exp(lnalpha.c.nonneg) 
  S.msy.c2.80 <- S.msy.c2 *0.80
#GENERATE Y+A-1 = 42 MATURITY SCHEDULES, ONE PER BROOD YEAR USING THE DIRICHLET DISTRIB. (Eq.4-6)
# "pi" (central tendency of "p"), and "D.scale" (dispersion of "p")
  D.scale ~ dunif(0,1)#uninformative
  D.sum <- 1 / (D.scale * D.scale)
  pi.2p ~ dbeta(1,1)#uninformative; https://stats.stackexchange.com/questions/297901/choosing-between-uninformative-beta-priors/298176
  pi.1 ~ dbeta(1,1)#uninformative; Eq.6
  pi[1] <- pi.1
  pi[2] <- pi.2p * (1 - pi[1])
  pi[3] <- 1 - pi[1] - pi[2]
  
  
  for (a in 1:A) {
    gamma[a] <- D.sum * pi[a]
    for (y in 1:(Y+A-1)) {                                                    
      g[y,a] ~ dgamma(gamma[a],0.001)
      p[y,a] <- g[y,a]/sum(g[y,])
    }
  }
  
  #CALCULATE THE NUMBERS AT AGE MATRIX (Number returning to spawn at age in year y); Eq.3
  #Product of the total return from brood year y-a and the prop. mature from cohort y-a returning at age a; 
  for(a in 1:A){
    for(y in a:(Y+(a-1))){
      N.ya[y-(a-1),(A+1-a)]<-p[y,(A+1-a)]*R[y]
    }
  }
  
  #MULTINOMIAL SCALE SAMPLING ON TOTAL ANNUAL RETURN N; Eq.13
  for (y in 1:Y) {
    N[y] <- sum(N.ya[y,1:A])
    for (a in 1:A) {
      q[y,a] <- N.ya[y,a] / N[y]
    }
  } 
  for (t in 1:Y){  
    x[t,1:A] ~ dmulti(q[t,],n.a[t])
  }  
  
  
#INRIVER RUN
    #hbelow (wild only)
  for (y in 1:Y) {
    mu.hbelow_wild[y] ~ dbeta(1,1)
    h.below_wild[y] <- mu.hbelow_wild[y] * N[y]
    log.hb_wild[y] <- log(h.below_wild[y])
    tau.log.hb[y] <- 1 / log(cv.hb[y]*cv.hb[y] + 1)
    hbelow_wild[y] ~ dlnorm(log.hb_wild[y],tau.log.hb[y])
     
    #habove (wild only)
    mu.habove_wild[y] ~ dbeta(1,1)
    h.above_wild[y] <- mu.habove_wild[y] * N[y]
    log.ha_wild[y] <- log(h.above_wild[y])
    tau.log.ha[y] <- 1 / log(cv.ha[y]*cv.ha[y] + 1)
    habove_wild[y] ~ dlnorm(log.ha_wild[y],tau.log.ha[y])
    inriver.run[y] <- max(N[y] - h.below_wild[y] + h.above_wild[y], 1) # inriver run (mr)= total inriver run - harvest below the border (wild) + harvest above border (wild)  
   
    #hbelow (wild + enh)
    mu.hbelow[y] ~ dbeta(1,1)
    h.below[y] <- mu.hbelow[y] * N[y]
    log.hb[y] <- log(h.below[y])
    #tau.log.hb[y] <- 1 / log(cv.hb[y]*cv.hb[y] + 1)
    hbelow[y] ~ dlnorm(log.hb[y],tau.log.hb[y])
    
#IN SOME YEARS, DIRECT ESTIMATES OF INRIVER RUN ABUNDANCE THROUGH MARK-RECAPTURE ESTIMATES
    log.ir[y] <- log(inriver.run[y])             
    tau.log.ir[y] <- 1 / log(cv.ir[y]*cv.ir[y] + 1)     
    ir[y] ~ dlnorm(log.ir[y],tau.log.ir[y])  # mark-recapture data        
    
    #habove (wild + enh)
    mu.habove[y] ~ dbeta(1,1)                                  
    h.above[y] <- mu.habove[y] * inriver.run[y]     
    log.ha[y] <- log(h.above[y])     
    #tau.log.ha[y] <- 1 / log(cv.ha[y]*cv.ha[y] + 1)     
    habove[y] ~ dlnorm(log.ha[y],tau.log.ha[y])     
    mu[y] <- (h.below[y] + h.above[y]) / N[y]     
    S[y] <- max(inriver.run[y] - h.above[y], 1) #escapement = inriver - total harvest above (wild + enh)
    log.S[y] <- log(S[y]) 
  }
  }