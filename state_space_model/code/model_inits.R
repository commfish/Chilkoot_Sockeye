# this code gets sourced from this file, 
# and creates the "inits" object, which is the used in
# the main script

# inital Values 
inits1 <- list(lnalpha=1.5, beta=0.0005, phi= 0.3, sigma.white=0.7, resid.red.0= 0)
inits2 <- list(lnalpha=2.0, beta=0.0010, phi=-0.1, sigma.white=0.5, resid.red.0=-1)
inits3 <- list(lnalpha=2.5, beta=0.0020, phi= 0.2, sigma.white=0.3, resid.red.0= 1)
inits <- list(inits1, inits2, inits3)
