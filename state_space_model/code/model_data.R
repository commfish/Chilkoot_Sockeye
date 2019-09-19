#Original code was from Steve Fleischman and Xinxian Zhang. code was modified
#and converted to JAGS by Sara Miller on 28 June 2017 and modified and
#maintained by Rich Brenner....lots of input from Ben Williams! Thanks Ben.

library(tidyverse)

# data----
brood <- read.csv("state_space_model/data/Chilkoot_Sock.csv", header = TRUE)
brood

# cleanup data
brood %>% 
  dplyr::select(S=spawn, R=recruit) %>% 
  mutate(lnRS = log(R/S), n()) -> sr

n <- nrow(sr) #calculates the number of years of data.
dat <- list(n = n, S = sr$S, lnRS = sr$lnRS)
