#Chilkoot sockeye state space model
#authors: Rich Brenner & Sara E Miller 
#contact: 
#Last edited: September 2019
#must download program JAGS for this script to work

# warning: some of these packages mask commands, so need to specify the package when calling the fn
# runjags::extract vs. tidyr::extract
# coda::traceplot vs. R2jags::traceplot
####BE SURE TO CHECK THAT THE model_data.R FILE AND ASSOCIATED BROOD TABLE ARE UPDATED BEFORE PROCEEDING####

# load libraries----
library(coda)
library(tidyverse)
library(R2jags)
library(rjags)
library(runjags)
library(R2OpenBUGS)
library(mcmcplots)
library(gsl)
library(stringr)
library(arm)
library(lmtest)
library(gdata)
library(cowplot)

# STEP 1: CHOOSE SETTINGS----

# if test runs then do sensitivity tests with explore, and final run with full
# "explore" version takes ~10min with the current settings.
out.label <-  "rjags_Full_BaseCase" 
package.use <- "rjags"  #"rjags"  or "R2jags"
jags.settings <- "full"  # "test" or "explore" or full" 

# source the model file (this reads in a function called "mod")
# then write the model to a text file to be called by JAGS if using rjags version
# if used R2Jags, can just use the "mod" object directly
# if you get a dmulti error, then the age comps are not whole numbers
source("state_space_model/code/model_source.R") 
print(mod)
write.model(mod, model_file_loc)
model_file_loc=paste("state_space_model/code/","Chilkoot_sockeye.txt", sep="") # where to write the model file

# define the parameters (nodes) of interest (pars to be tracked in the MCMC)
parameters <- c("lnalpha","beta", "sigma.red","S.msy","MSY", "lnalpha.c", "alpha", "S.max", "S.eq","U.msy", "sigma.white",
                "resid.red.0", "phi")

# create output folder for model results
out.path <- paste0("state_space_model/output/", out.label)
if(!exists(out.path)){dir.create(out.path)}

if(jags.settings == "test"){
  n.adapt.use <- 100 ; n.iter.use <- 500;  n.burnin.use <- 100;   thin.use = 10
  by.use <- 10 # this is just for the progress bar
}

if(jags.settings == "explore"){
  n.adapt.use <- 10000 ; n.iter.use <- 10000; n.burnin.use <- 30000 ;   thin.use = 10
  by.use <- 100 # this is just for the progress bar
}

if(jags.settings == "full"){
  n.adapt.use <- 10000  ; n.iter.use <- 4000000    #1,000,000 per chain; 3 chains; thin by 1000
  n.burnin.use <- 2000000  # consider increasing this?
  thin.use = 1000; by.use <- 1000 # this is just for the progress bar 
}

# STEP 2: READ IN DATA, MODEL, AND INITIAL VALUES----
######MAKE SURE THAT brood DATA and PATH ARE UPDATED!!!!#######
# generates the object "dat"
source("state_space_model/code/model_data.R")

# generate initial values
source("state_space_model/code/model_inits.R")

# STEP 3: RUN THE MODEL AND PROCESS THE OUTPUT----
# 2 options: rjags or R2jags

# This step does the actual MCMC sampling. All subsequent steps
# should just extract from "post" without rerunning the model

# start the timer
# R2jags
start.jags <- proc.time()
if(package.use == "R2jags"){ # new version
  r2jags.out <- R2jags::jags(data = dat , inits = inits, 
                             parameters.to.save = parameters, model.file = mod,
                             n.chains = 3, 
                             n.iter = n.iter.use + n.burnin.use ,  
                             # NOTE: R2jags uses n.iter for the TOTAL Samples, and the first n.burnin are discarded)
                             # rjags below does the n.burnin samples first, then n.iter samples to keep
                             n.burnin = n.burnin.use, 
                             n.thin = thin.use, DIC = T)
  end.jags <- proc.time()   # store time for MCMC
  mcmc.samples <- r2jags.out$BUGSoutput$sims.matrix
  mcmc.summary <- r2jags.out$BUGSoutput$summary 
  
  # these are the same as the ones produced below
  write.csv(mcmc.samples[,c("beta","lnalpha","lnalpha.c")], file= paste0(out.path,"/coda.csv") ,row.names=FALSE)    # writes csv file
  write.csv(mcmc.summary, file= paste0(out.path,"/statsquants.csv"))    
  
  # this one is the same as coda.csv, except with all params (~40MB) 
  # - > not tracked in github
  write.csv(mcmc.samples, file= paste0(out.path,"/coda_allpars.csv") ,row.names=FALSE)    # writes csv file
  
  conv.pars <- c("lnalpha","beta", "sigma.red","S.msy","MSY", "lnalpha.c", "alpha", "S.max", "S.eq","U.msy", "sigma.white",
                 "resid.red.0", "phi")
  
  conv.details <- checkConvergence(mcmc.out = r2jags.out, vars.check = conv.pars)
  
  write.csv(conv.details,file=paste0(out.path,"/ConvergenceDetails_R2Jags.csv"), row.names=FALSE)
  
  # for now, call it converged if gelman rubin and geweke are below critical values 
  # for all the conv.pars (the acf handling is finicky)
  # Note: converged = NOT flagged
  conv.check <- !conv.details$Flag[conv.details$Check == "all.gelman.rubin"] & 
    !conv.details$Flag[conv.details$Check == "all.geweke"]
  
  if(conv.check){print("The R2jags model run DID CONVERGE for all the key variables!")}
  if(!conv.check){print("The R2jags model run DID NOT CONVERGE for all the key variables!")}
}

#rjags
if(package.use == "rjags"){
  parameters <- c("lnalpha","beta", "sigma.red","S.msy","MSY", "lnalpha.c", "alpha", "S.max", "S.eq","U.msy", "sigma.white",
                  "resid.red.0", "phi")
  jmod <- rjags::jags.model(file='state_space_model/code/Chilkoot_sockeye.txt', data=dat, n.chains=3, inits=inits, n.adapt=n.adapt.use) 
  stats::update(jmod, n.iter=n.iter.use, by=by.use, progress.bar='text', DIC=T, n.burnin=n.burnin.use) # this modifies the original object, function returns NULL
  post <- rjags::coda.samples(jmod, parameters, n.iter=n.iter.use, thin=thin.use, n.burnin=n.burnin.use)
  
  end.jags <- proc.time()   # store time for MCMC
  post.arr <- as.array(post) # convert to an accessible obj
  
  source("state_space_model/code/2_GENERATE_OUTPUTS.R")
}
end.output  <- proc.time() 

print("Jags took")
print(end.jags - start.jags)
print("Output Processing took")
print(end.output - end.jags)



