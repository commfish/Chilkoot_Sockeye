# this code gets sourced from this file, 
# and creates the "dat" object, which is then used in
# the main script
#library(tidyverse)
#library(dplyr)
#library(tidyr)

rawdat<-as.data.frame(read.csv("state_space_model/data/test.csv",header=T))
nyrs<-as.numeric(length(rawdat$year))
fyr<-min(rawdat$year)
lyr<-max(rawdat$year)
nages<-3
a.min<-4
a.max<-6
A<-3

#pull in escapement estimates (with dropout included) from dervied dataset
rawdat1<-as.data.frame(read.csv("DATA/DerivedData/SummaryOfEstimates.csv",header=T))
rawdat1 %>%
  filter(est.use ==TRUE) %>%  # GP change to handle new input file
  mutate(cv=sd/mean) %>%
  dplyr::select(year, mean, sd, cv)-> rawdat1

df<-data.frame(1986,  '', '',0.9)
names(df)<-c("year", "mean", "sd", "cv")
rawdat1 <- rbind(rawdat1, df)
rawdat1$year<-as.numeric(rawdat1$year)
rawdat1 %>%
  arrange(year) -> rawdat1
rawdat1 %>%
  dplyr::select(year, mean, cv) %>%
  mutate(ir=mean,
         cv.ir=cv) %>%
  dplyr::select(year, ir, cv.ir) -> mark_recap
write.csv(mark_recap, file= paste("state_space_model/data/ir.csv"))  #this is needed for figs code

#data clean----
year <- as.numeric(as.character(rawdat$year))
ir <- as.numeric(as.character(rawdat1$mean))
cv.ir <- as.numeric(as.character(rawdat1$cv)) 
x <- c(NA,NA,NA,NA)
ir<-c(x, ir)
x <- c(0.9,0.9,0.9,0.9)
cv.ir<-c(x,cv.ir)

hbelow_wild <- as.numeric(as.character(rawdat$hbelow_wild)) #total wild US harvest below FW
hbelow_enh <- as.numeric(as.character(rawdat$hbelow_enh)) #total enh US harvest below FW#hbelow_enh <- as.numeric(as.character(rawdat$hbelow_enh)) #total enh US harvest below FW
hbelow <- as.numeric(as.character(rawdat$hbelow)) #total enh US harvest below FW#hbelow_enh <- as.numeric(as.character(rawdat$hbelow_enh)) #total enh US harvest below FW
habove_wild<- as.numeric(as.character(rawdat$habove_wild)) #total wild CND harvest above the FW
habove_enh<- as.numeric(as.character(rawdat$habove_enh)) #total enh CND harvest above the FW
habove<- as.numeric(as.character(rawdat$habove)) #total CND harvest above the FW
habove_enh<- as.numeric(as.character(rawdat$habove_enh)) #total enh CND harvest above the FW
habove<- as.numeric(as.character(rawdat$habove)) #total CND harvest above the FW
cv.ha <- as.numeric(as.character(rawdat$cv.ha))#cv of total CND harve
cv.hb <- as.numeric(as.character(rawdat$cv.hb))#cv of total CND harve
#rawdat[is.na(rawdat)] <- 0
#rawdat %>%
#  group_by(year) %>%
#  mutate(hbelow_wild = as.numeric(sum(hbelow_UScomm_seine_wild, hbelow_US_gillnet_wild, hbelow_US_personal_wild, na.rm=TRUE)), #total wild US harvest below F
#        hbelow_enh = as.numeric(sum(hbelow_UScomm_seine_enh, hbelow_US_gillnet_enh, hbelow_US_personal_enh, na.rm=TRUE)), #total US harvest below FW
#        hbelow = as.numeric(sum(hbelow_wild,hbelow_enh, na.rm=TRUE)),
#        cv.hb = as.numeric(as.character(cv.hb)), #cv of total US harvest
#        habove_wild = as.numeric(sum(habove_CND_commercial_wild, habove_CND_aborg_wild, habove_CND_test_wild, na.rm=TRUE)), #total wild US harvest below F
#        habove_enh = as.numeric(sum(habove_CND_commercial_enh, habove_CND_aborg_enh, habove_CND_test_enh, na.rm=TRUE)), #total US harvest below FW
#        habove = as.numeric(sum(habove_wild, habove_enh, na.rm=TRUE)),
#        cv.ha = as.numeric(as.character(cv.ha))) -> rawdat # cv of total CND harvest
x<-as.matrix(rawdat[,substr(colnames(rawdat), 1,1)=="x"]) # age comp count data matrix ; these need to be integers
colnames(x)<-NULL
n.a<-rowSums(x) # age comp sample sizes
#write.csv(rawdat, file= paste("state_space_model/data/check.csv")) 

#dat=list(Y = nyrs, A=nages, a.min=a.min, a.max=a.max,
#         x=x, ir=ir, cv.ir=cv.ir, hbelow=rawdat$hbelow,cv.hb=rawdat$cv.hb,
#         habove=rawdat$habove, habove_wild=rawdat$habove_wild,hbelow_wild=rawdat$hbelow_wild, 
#         cv.ha=rawdat$cv.ha, n.a=n.a)
dat=list(Y = nyrs, A=nages, a.min=a.min, a.max=a.max,
         x=x, ir=ir, cv.ir=cv.ir, hbelow=hbelow,cv.hb=cv.hb,
         habove=habove, habove_wild=habove_wild,hbelow_wild=hbelow_wild, 
         cv.ha=cv.ha, n.a=n.a)