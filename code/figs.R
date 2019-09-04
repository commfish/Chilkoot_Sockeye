# R CODE TO CREATE PROFILES DATA AND FIGURES (Horsetail plots, age comp, maturity at age,
# point estimate plots, yield plots)
# created by Ben Williams (Ben.Williams@alaska.gov);Nov 3, 2016; 2017-7-4
# Changes made by Sara Miller (Sara.Miller@alaska.gov); April 2017

LowerB <- 38000 #lower bound of recommended escapement goal range
UpperB <- 86000 #upper bound of recommended escapement goal range
SMSY <- 53974 #Lambert W from AR_quantiles_lambert
lnalpha.c <- 2.16969947667027
lnalpha<- 2.17
beta <- 0.0000130955503090423

#load----
library(tidyverse)
library(reshape2)
library(extrafont)
library(grid)
library(plyr)
library(gsl)
library(scales)
windowsFonts(Times=windowsFont("Times New Roman"))
theme_set(theme_sleek())
source('state_space_model/code/functions.r')

if(!dir.exists(file.path("state_space_model", "output", "rjags_Explore_Basecase", "processed"))){dir.create(file.path("state_space_model", "output", "rjags_Explore_Basecase", "processed"))}

# data----
parameters <- read.csv("state_space_model/data/parameters.csv") #Load Data File (make sure this file is updated)
coda <- read.csv("state_space_model/output/rjags_Explore_Basecase/coda.csv") 

# data clean----
# profile parameters
coda %>% 
  mutate(S.eq.c = lnalpha.c/beta, 
                S.msy.c = (1-lambert_W0(exp(1-lnalpha.c)))/beta, #Lambert W
                R.msy.c = S.msy.c*exp(lnalpha.c-beta*S.msy.c), 
                MSY.c = R.msy.c-S.msy.c, 
                Rmax = exp(lnalpha)*(1/beta)*exp(-1)) -> coda
# analysis----
# create function for probability profiles and figures
profile(i=10, z=500, xa.start=0, xa.end=700,lnalpha.c, beta) #can change i,z, xa.start, xa.end
QM <- read.csv("state_space_model/output/rjags_Explore_BaseCase/processed/QM.csv")
CI <- read.csv("state_space_model/output/rjags_Explore_BaseCase/processed/CI.csv")
num <- nrow(QM)
QM %>%
  dplyr::select(c(escapement)) -> x
coda %>%
  dplyr::select(c(lnalpha.c, beta)) %>%
  filter(row_number()==1:50) %>%
  slice(rep(1:n(), each = num)) -> x1
dataset<-cbind(x, x1) #lnalpha.c, beta, and S
dataset %>%
  mutate(recruitment = escapement*exp(lnalpha.c-beta*escapement),
         variable = rep(1:50,each=num)) -> dataset

QM %>%
  dplyr::select(c(escapement)) %>%
  mutate (lnalpha.c = lnalpha.c,
          beta = beta,
          recruitment = escapement*exp(lnalpha.c-beta*escapement),
          variable = 51)-> x2

dataset<-rbind(dataset, x2)

read.csv('state_space_model/data/parameters.csv') %>%
  mutate(escapement = spawn,
          lnalpha.c = NA,
          beta = NA,
          recruitment =recruit,
          variable = 52) %>%
  dplyr::select(c(escapement, lnalpha.c, beta, recruitment, variable)) %>%
  rbind(., dataset) %>%
  arrange(variable, escapement) -> dataset

ggplot(data=dataset, aes(x=escapement, y=recruitment, group=variable))+
    geom_line(data=subset(dataset,dataset$variable<52),linetype="solid", size=0.5, color="grey80")+
    scale_y_continuous(labels = comma,breaks = seq(0, 1500000, 50000), limits = c(0, 1500000))+
    scale_x_continuous(labels = comma,breaks = seq(0, 350000, 50000), limits = c(0, 350000))+ylab("Recruits (R)")+xlab("Spawners (S)")+
    geom_line(data=dataset, aes(x=escapement, y=escapement, group=1),linetype="solid", size=1)+#replacement line
    geom_line(data=subset(dataset,dataset$variable==51),colour = "black", lty=2, size=2)+
    geom_point(data=subset(dataset,dataset$variable==52),colour = "black", pch=16, size=1)+
    geom_text(size=3, data=dataset, aes(x=Escapement1, y=Recruitment, group=52, label=Year,family="Times", 
                                         hjust = -0.1, vjust= -0.4))

  png(file='figures/Horsetail_Plot.png', res=200, width=6, height=4, units ="in")  
  grid.newpage()
  pushViewport(viewport(layout=grid.layout(1,1)))
  vplayout<-function(x,y) viewport (layout.pos.row=x, layout.pos.col=y)
  print(Fig,vp=vplayout(1,1:1)) 
  dev.off()
#Alternative horsetail plot
  dataset <- subset(dataset,variable == 52) 
  dataset['Escapement1']<-dataset$Escapement
  dataset<-subset(dataset, select = -c(lnalpha.c, beta, Escapement))
  dataset['Escapement']<-'NA'
  CI['Year']<-'NA'
  CI['variable']<-51
  CI['Recruitment']<-'NA'
  CI['Escapement1']<-'NA'
  dataset['Median']<-'NA'
  dataset['q95']<-'NA'
  dataset['q90']<-'NA'
  dataset['q10']<-'NA'
  dataset['q5']<-'NA'
  dataset1<-rbind(dataset, CI)
  dataset1$Escapement<-as.numeric(dataset1$Escapement)
  dataset1$Median<-as.numeric(dataset1$Median)
  dataset1$q5<-as.numeric(dataset1$q5)
  dataset1$q95<-as.numeric(dataset1$q95)
  dataset1$q10<-as.numeric(dataset1$q10)
  dataset1$q90<-as.numeric(dataset1$q90)
  dataset1$Recruitment<-as.numeric(dataset1$Recruitment)
  dataset1$Escapement1<-as.numeric(dataset1$Escapement1)
  
  Fig1<-ggplot(data=dataset1, aes(x=Escapement, y=Median, group=variable))+geom_line(size=2, lty=2, group=51)+
    geom_ribbon(aes(ymin = q5, ymax = q95, group=51), alpha=.15)+
    geom_ribbon(aes(ymin = q10, ymax = q90, group=51), alpha=.15)+
    xlab('Spawners (S)')+
    ylab('Recruits (R)')+
    scale_x_continuous(labels = comma,breaks = seq(0, 350000, 50000), limits = c(0,350000))+
    scale_y_continuous(labels = comma,breaks = seq(0, 650000, 100000), limits = c(0,650000))+
    geom_line(aes(x=Escapement, y=Escapement, group=51),linetype="solid", size=1)+
    geom_text(size=3, data=dataset1, aes(x=Escapement1, y=Recruitment, group=52, label=Year,family="Times", 
                                         hjust = -0.1, vjust= -0.4))
  Fig1<-Fig1+
    geom_point(data=dataset1, aes(x=Escapement1, y=Recruitment, group=52),pch=16, size=1)
 

  png(file='figures/Horsetail_Plot_Reconfig.png', res=200, width=6, height=4, units ="in")  
  grid.newpage()
  pushViewport(viewport(layout=grid.layout(1,1)))
  vplayout<-function(x,y) viewport (layout.pos.row=x, layout.pos.col=y)
  print(Fig1,vp=vplayout(1,1:1)) 
  dev.off()
  
 