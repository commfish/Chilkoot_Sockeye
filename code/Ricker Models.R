#Original code was from Steve Fleischman and Xinxian Zhang. code was modified
#and converted to JAGS by Sara Miller on 28 June 2017 and modified and
#maintained by Rich Brenner....lots of input from Ben Williams! Thanks Ben.

#load----
library(arm)
library(lmtest)
library(rjags)
library(R2OpenBUGS)  #Needed for the write.model function
library(gdata)
library(tidyverse)
library(gls) #Needed to calculate Lambert's W



#data----
brood <- read.csv("data/Chilkoot_Sock.csv", header = TRUE)
brood


# cleanup data
brood %>% 
  dplyr::select(S=Spawn, R=Recruit) %>% 
  mutate(lnRS = log(R/S), n()) -> sr

n <- nrow(sr) #calculates the number of years of data.
sr.data <- list(n = n, S = sr$S, lnRS = sr$lnRS)


#sr <- brood[,2:3] #retrieve spawner recruit data from rows 2-3 of the brood dataset.
#colnames(sr) <- c("S","R") #rename the column names
#sr <- na.omit(sr)  #remove missing rows with missing values(na)
#sr$lnRS<-log(sr$R/sr$S) #add one column called lnRS
#n <- nrow(sr) #calculates the number of years of data.
#sr.data<-list(n=n,S=sr$S, lnRS=sr$lnRS)


#analysis----
#check autocorrelation
mylm <- lm(lnRS ~ S, sr)
dwtest(mylm)  #Durbin Watson test for autocorrelation of "disturbances"
pacf(residuals(mylm)) 


##Ricker model for stock-recruitment analysis ##################################################
#Created by Steve Fleischman  ##################################################################
#First, Ricker WITHOUT autocorrelation #########################################################
Ricker=function(){
  
  lnalpha ~ dnorm(0,1.0E-6)%_%T(0,10) 
  beta ~ dnorm(0,1.0E-6)%_%T(0,10)               
  phi <- 0                #this model does not account for autocorrelation so phi is not used (thus, phi =0)
  sigma.white ~ dunif(0,10)
  resid.red.0 ~ dnorm(0,tau.red)
  
  
#OLD MODEL
  for(y in 1:n) {lnRS[y] ~ dnorm(mean2.lnRS[y],tau.white) }
  
  mean2.lnRS[1]  <- mean1.lnRS[1] + phi * resid.red.0  
  for (y in 2:n) { mean2.lnRS[y] <- mean1.lnRS[y] + phi * resid.red[y-1] }  #NO autocorrelation model
  
  for(y in 1:n) {  mean1.lnRS[y] <- lnalpha - beta * S[y]  }
  for(y in 1:n) {  resid.red[y]  <- lnRS[y] - mean1.lnRS[y]  }
  for(y in 1:n) {  resid.white[y] <- lnRS[y] - mean2.lnRS[y]  } 
  
  
  
  tau.white <- 1 / sigma.white / sigma.white        
  tau.red <- tau.white * (1-phi*phi)
  sigma.red <- 1 / sqrt(tau.red) 
  
  lnalpha.c <- lnalpha + (sigma.red * sigma.red / 2)# lnalpha.c is for the AR model.
                                              #adjust for calculating means of R.msy, S.msy etc., but should =
                                              #same as alpha when phi = 0 (non-AR model).

  alpha <- exp(lnalpha)  #exponentiate to solve for alpha
  S.max <- 1 / beta
  S.eq <- S.max * lnalpha.c 
  
  #S.msy <- S.eq * (0.5 - 0.07*lnalpha.c) #Hilborn approximation of Smsy...could use Scheuerell solution too....
  #U.msy <- lnalpha.c * (0.5 - 0.07*lnalpha.c)
  #R.msy <- S.msy * exp(lnalpha.c - beta * S.msy)  #Solves for recruits at Smsy
  #MSY <- step(R.msy-S.msy)*(R.msy-S.msy) #if R.msy< S.msy then MSY=0.
  #step(x) = 1 if x>=0; otherwise =0 if x<0
  
  
}
#write the non-AR model to a text file to be called by WinBUGS
model_file_loc=paste("code/Chilkoot_Sockeye.txt", sep="")
write.model(Ricker, paste("code/Chilkoot_Sockeye.txt", sep=""))


#############################################################################
#Next, Ricker JAGS model WITH autocorrelation: AR(1)  ###################################
AR=function(){
  
  #PRIORS
  lnalpha ~ dnorm(0,1.0E-6)%_%T(0,10) #uninformative
  beta ~ dnorm(0,1.0E-6)%_%T(0,10)  #uninformative, normal distribution, constrained to be >0
  phi ~ dnorm(0,1.0E-6)%_%T(-0.98,0.98) #AR(1) model so phi IS included and does not = zero. uninformative btwn -1 & 1
  resid.red.0 ~ dnorm(0,tau.red)
  sigma.white ~ dunif(0,10)
  

  
  #OLD MODEL
  #for(y in 1:n) {lnRS[y] ~ dnorm(mean2.lnRS[y],tau.white) }
  #mean2.lnRS[1] <- mean1.lnRS[1] + phi * resid.red.0  
  #for (y in 2:n) { mean2.lnRS[y] <- mean1.lnRS[y] + phi * resid.red[y-1] }   #AR1
  #for(y in 1:n) {  mean1.lnRS[y] <- lnalpha - beta * S[y]  } #This is the Ricker model
  #for(y in 1:n) {  resid.red[y]     <- lnRS[y] - mean1.lnRS[y]  }
  #for(y in 1:n) {  resid.white[y] <- lnRS[y] - mean2.lnRS[y]  }
  
 
  #NEW MODEL
  for(y in 1:n) {lnRS[y] ~ dnorm(mean2.lnRS[y],tau.white) }
  
  #mean2.lnRS[1] <- mean1.lnRS[1] + phi * resid.red.0  

  #for(y in 1:n) {  mean1.lnRS[y] <- lnalpha - beta * S[y]  } #This is the Ricker model
  
  #for(y in 1:n) {  resid.red[y]     <- lnRS[y] - mean1.lnRS[y]  }
  
  #for(y in 1:n) {  resid.white[y] <- lnRS[y] - mean2.lnRS[y]  }
  
  #for (y in 2:n) { mean2.lnRS[y] <- mean1.lnRS[y] + phi * resid.red[y-1] }   #AR1
  
  
  
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
  
 # S.msy <- S.eq * (0.5 - 0.07*lnalpha.c)  #Hilborn approximation to calculate Smsy
  #U.msy <- lnalpha.c * (0.5 - 0.07*lnalpha.c)  #Hilborn approximation of U.msy
  #R.msy <- S.msy * exp(lnalpha.c - beta * S.msy) #Xinxian's calculation of R.msy
  #MSY <- step(R.msy-S.msy)*(R.msy-S.msy) #if R.msy < S.msy then MSY=0.
  #step(x) = 1 if x>=0; otherwise =0 if x<0
  
}

#write the AR model to a text file to be called by WinBUGS or JAGS
model_file_loc=paste("code/Chilkoot_Sockeye_AR.txt", sep="")
write.model(AR, paste("code/Chilkoot_Sockeye_AR.txt", sep=""))



#######################################################################################
#######################################################################################
#NOW BACK TO R CODE
#RUN the Ricker model that does NOT have autocorrelation
#Results
#1000000 iterations, 3 chains, 10000 burn-in period, thin by 100

inits1 <- list(lnalpha=1.5, beta=0.0005, sigma.white=0.7, resid.red.0= 0)
inits2 <- list(lnalpha=2.0, beta=0.0010, sigma.white=0.5, resid.red.0=-1)
inits3 <- list(lnalpha=2.5, beta=0.0020, sigma.white=0.3, resid.red.0= 1)
inits <- list(inits1, inits2, inits3)


parameters <- c("lnalpha","beta", "sigma.red","S.msy","MSY", "lnalpha.c", "alpha", "S.max", "S.eq","U.msy", "sigma.white",
                "resid.red.0")
ptm = proc.time()
jmod <- jags.model(file='code/Chilkoot_Sockeye.txt', data=sr.data, n.chains=3, inits=inits, n.adapt=1000) 
x <- update(jmod, n.iter=100000, by=100, progress.bar='text', DIC=T, n.burnin=10000) 
post <- coda.samples(jmod, parameters, n.iter=100000, thin=100, n.burnin=10000)
post.samp <- post


#Numerical summary of each parameter (mean, median, quantiles)
summary <- summary(post)                     
stats <- summary$statistics;  colnames(stats)
quants <- summary$quantiles;  colnames(quants)
statsquants <- cbind(stats,quants) 
statsquants <- statsquants[,c(1,2,4,5,7,9)] #select columns of interest
write.csv(statsquants, file= paste("results/Ricker.csv") )    

#Density and time series plots
post.samp <- post
nvars <- dim(post.samp[[1]])[2]
nsamps <- dim(post.samp[[1]])[1]
int <- 25
pdf("figures/Ricker_profiles.pdf",height=6, width=8)


for(j in seq(1,nvars,int)){
  par(mfrow=c(5,4),mai=c(0.3,0.3,0.2,0.2))
  for(i in 0:(int-1)){
    mindat=min(c(post.samp[[1]][,i+j],post.samp[[1]][,i+j]))
    maxdat=max(c(post.samp[[1]][,i+j],post.samp[[1]][,i+j]))
    plot(density(post.samp[[1]][,i+j]),col='blue',main=colnames(post.samp[[1]])[i+j],xlim=c(mindat,maxdat))
    lines(density(post.samp[[2]][,i+j]),col='red')
    lines(density(post.samp[[3]][,i+j]),col='green')
    
    plot(as.numeric(post.samp[[1]][,i+j]),col='blue',main=colnames(post.samp[[1]])[i+j],ylim=c(mindat,maxdat),type='l')
    lines(as.numeric(post.samp[[2]][,i+j]),col='red')
    lines(density(post.samp[[3]][,i+j]),col='green')
  }}
dev.off()


#Gelman
gel <- as.data.frame(gelman.diag(post, multivariate=F)[[1]])
poor.threshold=1.10#values less than 1.2 are generally considered converged
poor <- gel[gel[,1] > poor.threshold, ]
write.csv(poor, file= paste("results/Ricker_Gelman.csv") )    

#DIC
dic.pD  <-dic.samples(jmod,n.iter=100000, thin=100,"pD",  n.burnin=10000)
dic.popt<-dic.samples(jmod,n.iter=100000, thin=100,"popt",n.burnin=10000)
dev1 <- sum(dic.pD[[1]])
pD   <- sum(dic.pD[[2]])
dic.pD <- dev1 + pD
dic.pD.summary <- data.frame(dev1, pD, dic.pD)
write.csv(dic.pD.summary, file=paste("results/Ricker_DIC.csv") ) 


#Create coda samples for horsetail plots and probability plots
post2 <- coda.samples(jmod, c("lnalpha", "beta", "lnalpha.c"), n.iter=100000, thin=10,n.burnin=10000) 
x <- as.array(post2)
x <- data.frame(x)
coda <- x[,1:3]
coda <- rename.vars(coda, from=c("beta.1","lnalpha.1","lnalpha.c.1"), to=c("beta","lnalpha", "lnalpha.c"))
write.csv(coda, file= paste("results/Ricker_coda.csv") ,row.names=FALSE)    # writes csv file


###########################################################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##########################################################################################################
#RUN Ricker model WITH AR(1):  
#1000000 iterations, 3 chains, 10000 burn-in period, thin by 1000
inits1 <- list(lnalpha=1.5, beta=0.0005, phi= 0.3, sigma.white=0.7, resid.red.0= 0)
inits2 <- list(lnalpha=2.0, beta=0.0010, phi=-0.1, sigma.white=0.5, resid.red.0=-1)
inits3 <- list(lnalpha=2.5, beta=0.0020, phi= 0.2, sigma.white=0.3, resid.red.0= 1)
inits <- list(inits1, inits2, inits3)

#parameters<-c("lnalpha.c","beta", "sigma.red","S.msy", "MSY", "I90" )
parameters <- c("lnalpha.c","beta", "sigma.red","S.msy", "MSY", "phi", "S.max", "S.eq", "S.msy", "U.msy", "R.msy","lnalpha", "alpha",
                "sigma.white","resid.red.0")
jmod <- jags.model(file='code/Chilkoot_Sockeye_AR.txt', data=sr.data, n.chains=3, inits=inits, n.adapt=10000) 
x <- update(jmod, n.iter=1000000, by=1000, progress.bar='text', DIC=T, n.burnin=10000) 
post <- coda.samples(jmod, parameters, n.iter=1000000, thin=1000, n.burnin=10000) #iterations kept=(iterations/thin)*chains
post.samp <- post

#Numerical summary of each parameter (mean, median, quantiles)
summary <- summary(post)                     
stats <- summary$statistics;  colnames(stats)
quants <- summary$quantiles;  colnames(quants)
statsquants <- cbind(stats,quants)
statsquants <- statsquants[,c(1,2,4,5,7,9)] #select statquant columns of interest: Mean, SD, Time-series SE,...
data.frame(statsquants)%>% #converts statquants to a dataframe
  tibble::rownames_to_column() %>%  #preserves the row names (otherwise, tibble  drops these)
  mutate(CV = SD/Mean) %>% #calculates the CV
  rename(Item=rowname, "Time series SE" = Time.series.SE, "2.5%"=X2.5.,
         "50%" = X50., "97.5%" = X97.5.)-> statsquants #converting to a tibble messed upcolumn names, need
statsquants
write.csv(statsquants, file= paste("results/Ricker_AR.csv") ) 

#Density and time series plots
post.samp <- post
nvars <- dim(post.samp[[1]])[2]
nsamps <- dim(post.samp[[1]])[1]
int <- 25
pdf("figures/Ricker_AR_profiles.pdf",height=6, width=8)
for(j in seq(1,nvars,int)){
  par(mfrow=c(5,4),mai=c(0.3,0.3,0.2,0.2))
  for(i in 0:(int-1)){
    mindat=min(c(post.samp[[1]][,i+j],post.samp[[1]][,i+j]))
    maxdat=max(c(post.samp[[1]][,i+j],post.samp[[1]][,i+j]))
    plot(density(post.samp[[1]][,i+j]),col='blue',main=colnames(post.samp[[1]])[i+j],xlim=c(mindat,maxdat))
    lines(density(post.samp[[2]][,i+j]),col='red')
    lines(density(post.samp[[3]][,i+j]),col='green')
    
    plot(as.numeric(post.samp[[1]][,i+j]),col='blue',main=colnames(post.samp[[1]])[i+j],ylim=c(mindat,maxdat),type='l')
    lines(as.numeric(post.samp[[2]][,i+j]),col='red')
    lines(density(post.samp[[3]][,i+j]),col='green')
  }}
dev.off()

#Gelman
gel <- as.data.frame(gelman.diag(post, multivariate=F)[[1]])
poor.threshold=1.10 #values less than 1.2 are generally considered converged
poor <- gel[gel[,1] > poor.threshold, ]
write.csv(poor, file= paste("results/Ricker_AR_Gelman.csv") )    

#DIC
dic.pD  <-dic.samples(jmod,n.iter=100000, thin=100,"pD",  n.burnin=10000)
dic.popt<-dic.samples(jmod,n.iter=100000, thin=100,"popt",n.burnin=10000)
dev1 <- sum(dic.pD[[1]])
pD   <- sum(dic.pD[[2]])
dic.pD <- dev1 + pD
dic.pD.summary <- data.frame(dev1, pD, dic.pD)
write.csv(dic.pD.summary, file=paste("results/Ricker_AR_DIC.csv") ) 

#Create coda samples for horsetail plots and probability plots for the AR model
post2 <- coda.samples(jmod, c("lnalpha", "beta", "lnalpha.c"), n.iter=100000, thin=10,n.burnin=10000) 
x <- as.array(post2)
x <- data.frame(x)
#coda <- x[,1:3] 
#new stuff from Sara M below this.....
coda1 <- x[,1:3]
coda2 <- x[,4:6]
coda3 <- x[,7:9]
coda1<- rename.vars(coda1, from=c("beta.1","lnalpha.1","lnalpha.c.1"), to=c("beta","lnalpha", "lnalpha.c"))
coda2<- rename.vars(coda2, from=c("beta.2","lnalpha.2","lnalpha.c.2"), to=c("beta","lnalpha", "lnalpha.c"))
coda3<- rename.vars(coda3, from=c("beta.3","lnalpha.3","lnalpha.c.3"), to=c("beta","lnalpha", "lnalpha.c"))
coda<-rbind(coda1,coda2,coda3)
coda$S.max <- 1 / coda$beta 
coda$S.eq.c <- coda$lnalpha.c * coda$S.max #Eq.21
coda$U.msy_Hilborn <- coda$lnalpha.c * (0.5-0.07*coda$lnalpha.c)
coda$S.msy_Hilborn <- coda$S.eq.c *(0.5-0.07*coda$lnalpha.c) #hilborn
coda$Smsy_lambert <- (1-lambert_W0(exp(1-coda$lnalpha.c)))/coda$beta 
coda$Umsy_lambert <- (1-lambert_W0(exp(1-coda$lnalpha.c))) 
coda<-as.data.frame(coda)
summary<-summary(coda) 
q1<-apply(coda,2,quantile,probs=c(0,0.025,0.5,0.975,1))#percentiles
write.csv(q1, file= paste("results/Model 3/Model3_quantiles_lambert.csv") )    
write.csv(summary, file= paste("results/Model 3/Model3_lambert.csv") ) 
write.csv(coda, file= paste("results/Model 3/Model3_coda_lambert.csv") )





#coda <- rename.vars(coda, from=c("beta.1","lnalpha.1","lnalpha.c.1"), to=c("beta","lnalpha", "lnalpha.c"))
#write.csv(coda, file= paste("results/Ricker_AR_coda.csv") ,row.names=FALSE) # writes csv file

