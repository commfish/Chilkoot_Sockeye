# THIS SCRIPT IS RUN AFTER 1_RUN_MODELS.R and 2b_GENERATE_OUTPUTS.r
# i and z act as ways to change range of escapement based on stock size

# input values below based on stats output
LowerB <- 55000  #lower bound of recommended escapement goal range
UpperB <- 62000 #upper bound of recommended escapement goal range
SMSY <- 43408  #Lambert W from lambert file
UMSY <- 0.70  #median from staquants file
SMAX <- 62237  #median from staquants file
SEQ <- 118250 #median from staquants file
lnalpha.c <-  1.91940 #median from staquants file
beta <-1.61E-05  #median from staquants file

# load----
library(tidyverse)
library(cowplot)
library(ggplot2)
library(FNGr)
library(scales)
library(gsl)
library(FField)
library(scales)
library(dplyr)

windowsFonts(Times=windowsFont("Times New Roman"))
theme_set(theme_sleek())
source('state_space_model/code/functions.r')

if(!dir.exists(file.path("state_space_model", "output", "rjags_Explore_Basecase", "processed"))){dir.create(file.path("state_space_model", "output", "rjags_Explore_Basecase", "processed"))}

# data----
# loadfonts(device="win") #only need to do this once; takes awhile to run!
coda <- read.csv("state_space_model/output/rjags_Explore_Basecase/coda.csv") 
coda  %>%
  mutate(S.eq.c = lnalpha.c/beta, 
                S.msy.c = (1-lambert_W0(exp(1-lnalpha.c)))/beta, #Lambert W
					      R.msy.c = S.msy.c*exp(lnalpha.c-beta*S.msy.c), 
					      MSY.c = R.msy.c-S.msy.c, 
					      Rmax = exp(lnalpha)*(1/beta)*exp(-1)) -> coda

# analysis----
# create function for probability profiles and figures
profile(i=10, z=500, xa.start=0, xa.end=700,lnalpha.c, beta) #can change i,z, xa.start, xa.end

# escapement, returns, run abundance, and residuals by year
parameters <- read.csv("state_space_model/output/rjags_Explore_BaseCase/parameters.csv") 
Taku_sockeye<- read.csv("state_space_model/data/Taku_sockeye.csv") 
ir<- read.csv("state_space_model/data/ir.csv") 
parameters %>%
  mutate (inriver.run97.5. = as.numeric(inriver.run97.5.),
  inriver.run50. = as.numeric(inriver.run50.),
  year = as.numeric(year),
  S97.5. =as.numeric(S97.5.),
  S50. =as.numeric(S50.)) -> parameters
maxY<-max(parameters$inriver.run97.5., na.rm=TRUE)*1.5
data <- merge(parameters, Taku_sockeye, by=c("year"), all=TRUE)
data <- merge(data, ir, by=c("year"), all=TRUE)
xaxis = tickr(data, year, 4)

# inriver run 
ggplot(data, aes(x=year, y=(inriver.run50.))) +
  geom_line(size=0.75)+ geom_point(size=2)+ylab("Inriver Run")+xlab("Year") +
  geom_ribbon(aes(ymin=(inriver.run2.5.), ymax=(inriver.run97.5.)), alpha=0.20) +
  scale_y_continuous(labels = comma,breaks = seq(0, 250000, 50000), limits = c(0, 250000)) +
  scale_x_continuous(breaks = xaxis$breaks, labels = xaxis$labels) +
  theme(legend.position = "none") + geom_point(aes(x=year, y=ir), pch=8, size=3,colour="grey40")
ggsave("state_space_model/output/rjags_Explore_BaseCase/processed/inriver_run.png", dpi=600, width=8, height=5, units='in')

# escapement
maxY<-max(parameters$S97.5., na.rm=TRUE)*1.5
ggplot(parameters, aes(x=year, y=(S50.))) +
  geom_line(size=0.75)+ geom_point (size=2)+ylab("Escapement (S)") + xlab("Year") +
  geom_ribbon(aes(ymin=(parameters$S2.5.), ymax=(parameters$S97.5.)), alpha=0.20) +
  scale_y_continuous(labels = comma,breaks = seq(0, 200000, 50000), limits = c(0, 200000)) +
  scale_x_continuous(breaks = xaxis$breaks, labels = xaxis$labels) +
  theme(legend.position = "none") +
  geom_line(aes(y=SMSY), colour="grey40", size=1, linetype=2) -> plot1

# returns
maxY<-max(parameters$R97.5., na.rm=TRUE)*1.5
ggplot(parameters, aes(x=year, y=(R50.))) + geom_line(size=0.75) + 
  geom_point(size=2)+ylab("Recruitment (R)") + xlab("Year") +
  geom_ribbon(aes(ymin=R2.5., ymax=R97.5.), alpha=0.20) +
  scale_y_continuous(labels = comma,breaks = seq(0, 500000, 100000), limits = c(0, 500000)) +
  scale_x_continuous(breaks = xaxis$breaks, labels = xaxis$labels) +
  theme(legend.position = "none") -> plot2

# total run abundance                                                                                                                                
maxY<-max(parameters$N97.5., na.rm=TRUE)*1.5
ggplot(parameters, aes(x=year, y=N50.))+geom_line(size=0.75) + 
  geom_point (size=2)+ylab("Total Run Abundance (N)") + xlab("Year") +
  geom_ribbon(aes(ymin=N2.5., ymax=N97.5.), alpha=0.20) +
  scale_y_continuous(labels = comma,breaks = seq(0, 400000, 100000), limits = c(0, 400000)) +
  scale_x_continuous(breaks = xaxis$breaks, labels = xaxis$labels) +
  theme(legend.position = "none") -> plot3  

# Ricker productivity residuals
maxY<-max(parameters$log.resid97.5., na.rm=TRUE)+0.5
minY<-min(parameters$log.resid2.5., na.rm=TRUE)-0.5
ggplot(parameters, aes(x=year, y=log.resid50.))+geom_line(size=0.75)+geom_point (size=2) + 
  ylab("Productivity Residuals")+xlab("Year") +
  geom_ribbon(aes(ymin=parameters$log.resid2.5., ymax=parameters$log.resid97.5.), alpha=0.20) +
  geom_line(aes(y=0), colour="black", size=0.5) +
  scale_y_continuous(breaks = seq(-1, 1, 0.25), limits = c(-1, 1)) +
  scale_x_continuous(breaks = xaxis$breaks, labels = xaxis$labels) +
  theme(legend.position = "none") -> plot4


png(file='state_space_model/output/rjags_Explore_BaseCase/processed/point_estimates.png', res=500, width=8, height=9, units ="in") 
plot_grid(plot1, plot2, plot3, plot4, labels = c("A", "B", "C", "D"), ncol = 1, align="v",hjust=-7,
          vjust=2, label_size=14)
dev.off()

# harvest rate
ggplot(parameters, aes(x=year, y=mu.hbelow50.))+geom_line(size=0.75)+geom_point (size=2)+ ylab("Harvest Rate (below border)") +
  xlab("Year") +
  geom_ribbon(aes(ymin=mu.hbelow2.5., ymax=mu.hbelow97.5.), alpha=0.15) +
  coord_cartesian(ylim=c(0,1)) +
  scale_x_continuous(breaks = xaxis$breaks, labels = xaxis$labels) +
  geom_line(aes(y=UMSY), colour="grey70", size=1, linetype=2) + theme(legend.position = "none") -> plot1

ggplot(parameters, aes(x=year, y=mu.habove50.))+geom_line(size=0.75)+geom_point (size=2)+ ylab("Harvest Rate (above border)") +
  xlab("Year") +
  geom_ribbon(aes(ymin=mu.habove2.5., ymax=mu.habove97.5.), alpha=0.15) +
  coord_cartesian(ylim=c(0,1)) +
  scale_x_continuous(breaks = xaxis$breaks, labels = xaxis$labels) +
  geom_line(aes(y=UMSY), colour="grey70", size=1, linetype=2) + theme(legend.position = "none") -> plot2

png(file='state_space_model/output/rjags_Explore_BaseCase/processed/harvest_rates.png', res=500, width=8, height=9, units ="in") 
plot_grid(plot1, plot2, labels = c("A", "B"), ncol = 1, align="v",hjust=-7,
          vjust=2, label_size=14)
dev.off()


# mean age at maturity (p), age composition (q), total run by age proportions (Nya)
p_q_Nya<- read.csv("state_space_model/output/rjags_Explore_BaseCase/p_q_Nya.csv") 
p_q_Nya %>%
  mutate(age_comp = as.numeric(age_comp),
          year = as.numeric(year),
          Age = factor(age, ordered = TRUE, 
                              levels = c( "Ages 2-4", "Age 5", "Ages 6-8"),
                              labels = c("Ages 2-4", "Age 5", "Ages 6-8"))) %>%
  mutate(age_comp = ifelse(Age == 'Ages 2-4', NA, age_comp))-> data

ggplot(data,aes(x=year, y=p, fill=as.factor(Age))) +
  geom_area(position=position_stack(reverse=TRUE)) + scale_fill_grey(start=0.1, end=0.8) +
  theme(legend.title=element_blank()) +
  ylab("Age-at-Maturity Proportions") + xlab("Year") +
  scale_x_continuous(breaks = xaxis$breaks, labels = xaxis$labels, limits = c(1974, 2020)) -> plot1  

ggplot(data,aes(x=year, y=q, fill=as.factor(Age))) +
  geom_area(position=position_stack(reverse=FALSE)) +
  scale_fill_grey(start=0.1, end=0.8) +
  ylab("Age Composition Proportions") + xlab("Year") +
  theme(legend.title=element_blank()) + geom_point(aes(x=year, y=age_comp), position='stack') +
  scale_x_continuous(breaks = xaxis$breaks, labels = xaxis$labels, limits = c(1974, 2020)) -> plot2

ggplot(data,aes(x=year, y=Nya, fill=as.factor(Age))) +
  geom_area(position=position_stack(reverse=TRUE)) + scale_fill_grey(start=0.1, end=0.8) +
  ylab("Total Run by Age")+xlab("Year") +
  guides(fill = guide_legend(reverse=FALSE)) + 
  theme(legend.title=element_blank()) +
  scale_y_continuous(labels = comma,breaks = seq(0, 350000, 50000), limits = c(0, 350000)) +
  scale_x_continuous(breaks = xaxis$breaks, labels = xaxis$labels, limits = c(1974, 2020)) -> plot3
cowplot::plot_grid(plot1, plot2, plot3,  align = "v", nrow = 3, ncol=1) 
ggsave("state_space_model/output/rjags_Explore_BaseCase/processed/proportions.png", dpi = 500, height = 11, width = 8, units = "in")

# horesetail plots
QM <- read.csv("state_space_model/output/rjags_Explore_BaseCase/processed/QM.csv")
CI<- read.csv("state_space_model/output/rjags_Explore_BaseCase/processed/CI.csv")
coda <- read.csv("state_space_model/output/rjags_Explore_BaseCase/coda.csv") 
parameters <- read.csv("state_space_model/output/rjags_Explore_BaseCase/parameters.csv") 
num <- nrow(QM)
QM %>%
  dplyr::select(c(Escapement)) -> x
coda %>%
  dplyr::select(c(lnalpha.c, beta)) %>%
  filter(row_number()==1:50) %>%
  slice(rep(1:n(), each = num)) -> x1
dataset<-cbind(x, x1) #lnalpha.c, beta, and S
dataset %>%
    mutate(Recruitment = Escapement*exp(lnalpha.c-beta*Escapement),
           variable = rep(1:50,each=num)) -> dataset

QM %>%
  dplyr::select(c(Escapement)) %>%
  mutate (lnalpha.c = lnalpha.c,
          beta = beta,
          Recruitment = Escapement*exp(lnalpha.c-beta*Escapement),
          variable = 51)-> x2

dataset<-rbind(dataset, x2)

dataset %>%
  mutate(year = 'NA',
         R2.5. = 0,
         R97.5. = 0,
         S2.5. = 0,
         S97.5. = 0) -> dataset

parameters %>%
  filter (year %in% c(1980:2014)) %>%
  dplyr::select(c(year, S50., R50., R2.5., R97.5., S2.5., S97.5.)) %>%
  mutate(lnalpha.c = 'NA',
         beta = 'NA',
         Escapement = S50.,
         Recruitment = R50.,
         variable = 52) %>%
  dplyr::select(year, lnalpha.c, beta, Escapement, Recruitment, variable, R2.5., R97.5., S2.5., S97.5.) -> x3
dataset<-rbind(dataset, x3)

ggplot(data=dataset, aes(x=Escapement, y=Recruitment, group=variable)) +
  geom_line(data=subset(dataset,dataset$Variable<52),linetype="solid", size=0.5, color="grey80") +
  scale_y_continuous(labels = comma,breaks = seq(0, 350000, 50000), limits = c(0, 350000)) +
  scale_x_continuous(labels = comma,breaks = seq(0, 350000, 50000), limits = c(0, 350000)) +
  ylab("Recruits (R)")+xlab("Spawners (S)") +
  geom_line(data=dataset, aes(x=Escapement, y=Escapement, group=1),linetype="solid", size=1) +#replacement line
  geom_line(data=subset(dataset,variable==51),colour = "black", lty=2, size=2) +
  geom_text(data=subset(dataset,variable==52), aes(x=Escapement, y=Recruitment, label=year,family="Times")) +
  geom_errorbar(aes(ymax = R97.5., ymin=R2.5.), width=0.20,linetype = 2, colour="grey50") +
  geom_errorbarh(aes(xmax = S97.5., xmin=S2.5.), height=0.20,linetype = 2,colour="grey50")
ggsave("state_space_model/output/rjags_Explore_BaseCase/processed/horsetail.png", dpi = 500, height = 6, width = 8, units = "in")

dataset %>%
  filter (variable %in% c(52)) %>%
  mutate(Escapement1 = Escapement) %>%
  dplyr::select(-c(lnalpha.c, beta, Escapement)) %>%
  mutate(Escapement = 'NA',
         Median = 'NA',
         q95 = 'NA',
         q90 ='NA',
         q10 ='NA',
         q5 = 'NA') -> dataset
CI %>%
  mutate(year = 'NA',
         R2.5.= 'NA',
         R97.5. = 'NA',
         S2.5. ='NA',
         S97.5. ='NA',
         variable = 51,
         Recruitment = 'NA',
         Escapement1 ='NA') -> CI


dataset1<-rbind(dataset, CI)  
dataset1 %>%
  mutate_if(is.character, as.numeric) -> dataset1

x.fact <- 100/max(dataset1$Escapement1) 
y.fact <- 100/max(dataset1$Recruitment)
coords <- FFieldPtRep(coords = cbind(dataset1$Escapement1 * x.fact, dataset1$Recruitment * y.fact), rep.fact = 40)
x.t <- coords$x/x.fact 
y.t <- coords$y/y.fact

ggplot(data=dataset1, aes(x=Escapement, y=Median, group=variable)) + 
  geom_line(size=1, lty=2, group=51) +
  geom_ribbon(aes(ymin = q5, ymax = q95, group=51), alpha=.08) +
  geom_ribbon(aes(ymin = q10, ymax = q90, group=51), alpha=.08) +
  xlab('Spawners (S)') +
  ylab('Recruits (R)') +
  scale_y_continuous(labels = comma,breaks = seq(0, 350000, 50000), limits = c(0, 350000)) +
  scale_x_continuous(labels = comma,breaks = seq(0, 350000, 50000), limits = c(0, 350000)) +
  geom_line(aes(x=Escapement, y=Escapement, group=51),linetype="solid", size=1) +
  geom_point(data=dataset1, aes(x=x.t, y=y.t, group=52),pch=16, size=1) +
  geom_errorbar(data=dataset1, aes(x=Escapement1, ymax=R97.5., ymin=R2.5., group=52), width=0.2,linetype = 1, colour="grey70") +
  geom_point(data=dataset1, aes(x=Escapement1, y=Recruitment, group=52),pch=16, size=1) +
  geom_errorbarh(data=dataset1, aes(x=Escapement1, y=Recruitment, xmax=S97.5., xmin=S2.5.),na.rm=T,  linetype = 1, colour="grey70") +
  geom_text(size=3, data=dataset1, aes(x=Escapement1, y=Recruitment, group=52, label=year,family="Times", 
                                     hjust = -0.1, vjust= -0.4)) 
ggsave("state_space_model/output/rjags_Explore_BaseCase/processed/horsetail2.png", dpi = 500, height = 6, width = 8, units = "in")

# escapement estimates with reference line
parameters <- read.csv("state_space_model/output/rjags_Explore_BaseCase/parameters.csv") 
parameters %>%
  filter (year %in% c(1980:2018)) -> parameters
ggplot(parameters, aes(x=year, y=S50.)) + 
  geom_line(size=0.75) + geom_point (size=2) + ylab("Escapement (S)") + xlab("Year") +
  scale_y_continuous(labels = comma,breaks = seq(0, 150000, 25000), limits = c(0, 150000)) + 
  scale_x_continuous(breaks = xaxis$breaks, labels = xaxis$labels, limits = c(1974, 2020)) +                    
  theme(legend.position = "topright") +
  geom_line(aes(y=SMAX), colour="grey40", size=1, linetype=2) +
  geom_line(aes(y=SMSY), colour="grey40", size=1, linetype=3) +
  geom_errorbar(aes(ymin=S2.5., ymax=S97.5.),size=0.5, linetype = "solid", colour="grey40", width=0.02) +
  geom_line(aes(y=SEQ), colour="grey50", size=1, linetype=1) +
  annotate("rect", xmin = min(parameters$year), xmax = max(parameters$year)+2, ymin = LowerB, ymax = UpperB,
         alpha = .3)
ggsave("state_space_model/output/rjags_Explore_BaseCase/processed/historical_esc.png", dpi = 500, height = 6, width = 8, units = "in")


