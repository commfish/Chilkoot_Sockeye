#Taku sockeye state space model
#authors: Sara E Miller & Gottfried Pestal
#contact: sara.miller@alaska.gov; 907-465-4245
#Last edited: July 2019
#must download program JAGS for this script to work

# warning: some of these packages mask commands, so need to specify the package when calling the fn
# runjags::extract vs. tidyr::extract
# coda::traceplot vs. R2jags::traceplot
library(coda)
library(tidyverse)
library(R2jags)
library(rjags)
library(runjags)
library(R2OpenBUGS)
library(mcmcplots)
library(gsl)
library(tibble)
library(dplyr)
library(tidyr)
library(stringr)

# STEP 1: CHOOSE SETTINGS----

# if test runs then do sensitivity tests with explore, and final run with full
# "explore" version takes ~10min with the current settings.

out.label <-  "rjags_Explore_BaseCase" #"R2Jags_Explore_BaseCase" or #"rjags_Explore_BaseCase" # label to be used for the output folder (and for scenario comparisons)
package.use <- "rjags"  #"rjags"  or "R2jags"
jags.settings <- "explore"  # "test" or "explore" or full" 
sensitivity.analysis <- 0 #0 1 is yes and 0 is no
model_file_loc=paste("state_space_model/code/","Taku_sockeye.txt", sep="") # where to write the model file

# load custom functions
source('state_space_model/code/functions.R')
source("state_space_model/code/MCMC_CustomFunctions.R")

# define the parameters (nodes) of interest (pars to be tracked in the MCMC)
parameters <- c('S.eq.c','S.msy.c','U.msy.c','alpha','beta',
             'lnalpha','lnalpha.c','phi','sigma.R','log.resid.0', 'mean.log.RO',
             'S','R','N','log.resid','mu.hbelow','pi','h.below','N.ya',
             'p','q', 'S.max','D.sum','D.scale','sigma.RO',
             'S.eq.c2', 'U.msy.c2', 'S.msy.c2', 'U.max.c2',
             'mu.habove', 'h.above', 'inriver.run','S.msy.c.80','S.msy.c2.80')

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

if(jags.settings == "large"){
  lg.scalar <- 5
  n.adapt.use <- 10000 ; n.iter.use <- 10000  ; 
  n.burnin.use <- 30000  * lg.scalar  ;   thin.use = 10  
  by.use <- 100    # this is just for the progress bar
}

if(jags.settings == "full"){
  n.adapt.use <- 10000  ; n.iter.use <- 1000000    #1,000,000 per chain; 3 chains; thin by 1000
  n.burnin.use <- 10000  # consider increasing this?
  thin.use = 1000; by.use <- 1000 # this is just for the progress bar 
}

# STEP 2: READ IN DATA, MODEL, AND INITIAL VALUES----
# generates the object "dat"
source("state_space_model/code/model_data.R")

# source the model file (this reads in a function called "mod")
# then write the model to a text file to be called by JAGS if using rjags version
# if used R2Jags, can just use the "mod" object directly
# if you get a dmulti error, then the age comps are not whole numbers
source("state_space_model/code/model_source.R") 
print(mod)
write.model(mod, model_file_loc)

# generate initial values
source("state_space_model/code/model_inits.R")

# STEP 3: RUN THE MODEL AND PROCESS THE OUTPUT----
# 2 optionss: rjags or R2jags

# This step does the actual MCMC sampling. All subsequent steps
# should just extract from "post" without rerunning the model

# start the timer
# R2jags
start.jags <- proc.time()
if(package.use == "R2jags" & sensitivity.analysis == 0){ # new version
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
  
# this only works for any single-value parameters
# parameters with annual values would need to be tested individually (E.g. S[1], S[2], etc)
#     -> build that later  
conv.pars <- c('S.eq.c','S.msy.c','U.msy.c','alpha','beta',
                  'lnalpha','lnalpha.c','phi','sigma.R',
                  'S.eq.c2', 'U.msy.c2', 'S.msy.c2', 'U.max.c2')
  
conv.details <- checkConvergence(mcmc.out = r2jags.out, vars.check = conv.pars)

write.csv(conv.details,file=paste0(out.path,"/ConvergenceDetails_R2Jags.csv"), row.names=FALSE)

# for now, call it converged if gelman rubin and geweke are below critical values 
# for all the conv.pars (the acf handling is finicky)
# Note: converged = NOT flagged
conv.check <- !conv.details$Flag[conv.details$Check == "all.gelman.rubin"] & 
                   !conv.details$Flag[conv.details$Check == "all.geweke"]

if(conv.check){print("The R2jags model run DID CONVERGE for all the key variables!")}
if(!conv.check){print("The R2jags model run DID NOT CONVERGE for all the key variables!")}

# run the script that generates all the outputs 
source("state_space_model/code/2a_GENERATE_OUTPUTS.R")
}

#rjags
if(package.use == "rjags" & sensitivity.analysis == 0){
  parameters <- c('S.eq.c','S.msy.c','U.msy.c','alpha','beta',
                  'lnalpha','lnalpha.c','phi','sigma.R','log.resid.0', 'mean.log.RO',
                  'S','R','N','log.resid','mu.hbelow','pi','h.below','N.ya',
                  'p','q', 'S.max','D.sum','D.scale','sigma.RO',
                  'S.eq.c2', 'U.msy.c2', 'S.msy.c2', 'U.max.c2',
                  'mu.habove', 'h.above', 'inriver.run','S.msy.c.80','S.msy.c2.80')
  jmod <- rjags::jags.model(file='state_space_model/code/Taku_sockeye.txt', data=dat, n.chains=3, inits=inits, n.adapt=n.adapt.use) 
  stats::update(jmod, n.iter=n.iter.use, by=by.use, progress.bar='text', DIC=T, n.burnin=n.burnin.use) # this modifies the original object, function returns NULL
  post <- rjags::coda.samples(jmod, parameters, n.iter=n.iter.use, thin=thin.use, n.burnin=n.burnin.use)

end.jags <- proc.time()   # store time for MCMC
# post = original output from rjags::coda.samples
# post.arr = reformatted version of post
post.arr <- as.array(post) # convert to an accessible obj

# run the script that generates all the outputs 
source("state_space_model/code/2b_GENERATE_OUTPUTS.R")
}

# sensitivity analysis using rjags
# uniform prior
mu_vec <- c(1.0E-06) 
sigma_vec <- c(1.0) 
dist_name<-c("dunif")
out.path2 <- paste0("state_space_model/output/", out.label, "/", "sensitivity_beta")
if(!exists(out.path2)){dir.create(out.path2)}
for(i in 1:length(mu_vec)){
  for(j in 1:length(sigma_vec)){
    create_txt(mu = mu_vec[i], sigma = sigma_vec[j], dist = dist_name)
    sensdir <- file.path(out.path2, paste0("beta_mu",mu_vec[i],"_sigma",sigma_vec[j],"_dist_",dist_name))
    dir.create(sensdir, showWarnings = FALSE)
    files_old <- paste0("state_space_model/code/Taku_sockeye_beta_mu",mu_vec[i],"_sigma",sigma_vec[j],"_dist_",dist_name, ".txt")
    files_new <- paste0(out.path2,"/beta_mu",mu_vec[i],"_sigma",sigma_vec[j],"_dist_",dist_name, "/Taku_sockeye_beta_mu",mu_vec[i],"_sigma",sigma_vec[j],"_dist_",dist_name, ".txt")
    file.copy(from = files_old, to = files_new, overwrite = TRUE)}}

# run sensitivity beta one
for(i in 1:length(mu_vec)){
  for(j in 1:length(sigma_vec)){ 
  if(package.use == "rjags" & sensitivity.analysis == 1){
    parameters <- c('S.eq.c','S.msy.c','U.msy.c','alpha','beta',
                    'lnalpha','lnalpha.c','phi','sigma.R','log.resid.0', 'mean.log.RO',
                    'S','R','N','log.resid','mu.hbelow','pi','h.below','N.ya',
                    'p','q', 'S.max','D.sum','D.scale','sigma.RO',
                    'S.eq.c2', 'U.msy.c2', 'S.msy.c2', 'U.max.c2',
                    'mu.habove', 'h.above', 'inriver.run','S.msy.c.80','S.msy.c2.80')
    
    jmod <- rjags::jags.model(file=paste0(out.path2,"/beta_mu",mu_vec[i],"_sigma",sigma_vec[j], "_dist_",dist_name,"/Taku_sockeye_beta_mu",mu_vec[i],"_sigma",sigma_vec[j], "_dist_",dist_name, ".txt"), 
                              data=dat, n.chains=3, inits=inits, n.adapt=n.adapt.use) 
    stats::update(jmod, n.iter=n.iter.use, by=by.use, progress.bar='text', DIC=T, n.burnin=n.burnin.use) 
    post <- rjags::coda.samples(jmod, parameters, n.iter=n.iter.use, thin=thin.use, n.burnin=n.burnin.use)
    post.arr <- as.array(post)
    source("state_space_model/code/2c_GENERATE_OUTPUTS.R")
    end.jags <- proc.time()     
  } } }

# normal prior
mu_vec <- c(0) 
sigma_vec <- c(1.0E-06) 
dist_name<-c("dnorm")
out.path2 <- paste0("state_space_model/output/", out.label, "/", "sensitivity_beta")
if(!exists(out.path2)){dir.create(out.path2)}
for(i in 1:length(mu_vec)){
  for(j in 1:length(sigma_vec)){
    create_txt(mu = mu_vec[i], sigma = sigma_vec[j], dist = dist_name)
    sensdir <- file.path(out.path2, paste0("beta_mu",mu_vec[i],"_sigma",sigma_vec[j],"_dist_",dist_name))
    dir.create(sensdir, showWarnings = FALSE)
    files_old <- paste0("state_space_model/code/Taku_sockeye_beta_mu",mu_vec[i],"_sigma",sigma_vec[j],"_dist_",dist_name, ".txt")
    files_new <- paste0(out.path2,"/beta_mu",mu_vec[i],"_sigma",sigma_vec[j],"_dist_",dist_name, "/Taku_sockeye_beta_mu",mu_vec[i],"_sigma",sigma_vec[j],"_dist_",dist_name, ".txt")
    file.copy(from = files_old, to = files_new, overwrite = TRUE)}}

# run sensitivity beta two
for(i in 1:length(mu_vec)){
  for(j in 1:length(sigma_vec)){ 
    if(package.use == "rjags" & sensitivity.analysis == 1){
      parameters <- c('S.eq.c','S.msy.c','U.msy.c','alpha','beta',
                      'lnalpha','lnalpha.c','phi','sigma.R','log.resid.0', 'mean.log.RO',
                      'S','R','N','log.resid','mu.hbelow','pi','h.below','N.ya',
                      'p','q', 'S.max','D.sum','D.scale','sigma.RO',
                      'S.eq.c2', 'U.msy.c2', 'S.msy.c2', 'U.max.c2',
                      'mu.habove', 'h.above', 'inriver.run','S.msy.c.80','S.msy.c2.80')
      
      jmod <- rjags::jags.model(file=paste0(out.path2,"/beta_mu",mu_vec[i],"_sigma",sigma_vec[j], "_dist_",dist_name,"/Taku_sockeye_beta_mu",mu_vec[i],"_sigma",sigma_vec[j], "_dist_",dist_name, ".txt"), 
                                data=dat, n.chains=3, inits=inits, n.adapt=n.adapt.use) 
      stats::update(jmod, n.iter=n.iter.use, by=by.use, progress.bar='text', DIC=T, n.burnin=n.burnin.use) 
      post <- rjags::coda.samples(jmod, parameters, n.iter=n.iter.use, thin=thin.use, n.burnin=n.burnin.use)
      post.arr <- as.array(post)
      source("state_space_model/code/2c_GENERATE_OUTPUTS.R")
      end.jags <- proc.time()     
    } } }

end.output  <- proc.time() 

print("Jags took")
print(end.jags - start.jags)
print("Output Processing took")
print(end.output - end.jags)

# STEP 4: SENSITIVITY RESULTS----
#sensitivity results for beta using rjags
if(package.use == "rjags" & sensitivity.analysis == 1){
source('state_space_model/code/3_SENSITIVITY_RESULTS.R')}



###################################
# TEMPORARY COMPARISON PIECE
# Uses the last stored results from both versions
# from R2jags section, use "mcmc.samples "
# from rjags section, use "post.arr"


pdf(paste0("state_space_model/output/ComparisonOfPosteriors.pdf"), onefile= TRUE,height=8.5,width=11)

par(mfrow=c(2,2))
for(par.plot in conv.pars){

plot(density(mcmc.samples[,par.plot]),xlab=par.plot,axes=FALSE,
     main = paste('Distribution of',par.plot),col="red",lwd=2)
lines(density(as.vector(post.arr[,par.plot,])),col="darkblue",lwd=1, lty=1)
axis(1)


legend("topleft",legend = c("R2jags", "rjags"),lty=c(1,1),lwd=c(2,1),
       col=c("red","darkblue"),bty="n")

}

dev.off()





 








