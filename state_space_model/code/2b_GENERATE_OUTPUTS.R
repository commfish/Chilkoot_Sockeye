# THIS SCRIPT IS SOURCED FROM INSIDE 1_RUN_MODEL.R
#(if package.use = "rjags") 

# - post.samp and post 
      # these are the mcmc.list objects created by coda.samples
      # you can access individual variables like this: post[,"var.name"]
# - post.arr 
# - coda
# create coda file
#Gelman statistic
#Brooks and Gelman (1997) have suggested, if Rc<1.2 for all model parameters, 
#one can be fairly confident that convergence has been reached. Otherwise, longer chains or other means for improving the convergence may be needed
#Brooks, S. P., and A. Gelman. 1997. General Methods for Monitoring Convergence of Iterative Simulations. 
#Journal of Computational and Graphical Statistics 7: 434–455.

# Numerical summary of each parameter (mean, median, quantiles of posteriers)----
summary<-summary(post)  
stats<-summary$statistics;  colnames(stats)
quants<-summary$quantiles;  colnames(quants)
statsquants <- cbind(stats,quants) 
statsquants %>% 
  as.data.frame() %>% 
  dplyr::select(Mean, SD, 'Time-series SE', '2.5%', '50%', '97.5%') %>%
  rename(time_series_se = 'Time-series SE') %>%
  rownames_to_column('variable') %>%
  mutate (mc_error = time_series_se/SD,
  converge = ifelse(mc_error < 0.05, "true", "false")) %>%
  write.csv(., file= paste0(out.path,"/statsquants.csv"))    

# Gelman Diagnostics----
gel <- as.data.frame(gelman.diag(post, multivariate=F)[[1]])
poor.threshold = 1.2 #values less than 1.2 are generally considered converged
gel %>%
  rownames_to_column('variable') %>%
  rename(point_estimate = "Point est.") %>%
  mutate (converge = ifelse(point_estimate < poor.threshold, "true", "false")) %>%
  write.csv(., file= paste0(out.path,"/gelman.csv") )   

# Geweke Diagnostics----
#Examine convergence of the Markov chains using the Geweke's convergence diagnostic
#a convergence diagnostic for Markov chains based on a test for equality 
#of the means of the ﬁrst and last part of a Markov chain (by default the ﬁrst 10% and the last 50%).
#As for a cut-off you can compare to the standard normal critical values z α/2 where α=0.05
#null hypothesis is rejected if Z large
# Geweke diagnostic
l <- geweke.diag(post)
df <- data.frame(matrix(unlist(l), nrow=length(l), byrow=T),stringsAsFactors=FALSE)
df <- t(df) %>%
  as.data.frame() %>%
  rownames_to_column('variable1') -> df
colnames(df) <- c("variable1", "chain1", "chain2", "chain3")
names <-read.csv(file = paste0(out.path,"/statsquants.csv"))
names  %>%
  dplyr::select(variable) %>% 
  add_row(variable = "frac1") %>% #0.1 and 0.5 default
  add_row(variable = "frac2")  -> names
x <- cbind(names, df)
x  %>% 
  dplyr::select(-variable1) %>%
  write.csv(., paste0(out.path,"/geweke.csv")) 

# SMSY 15th and 65th percentile method----
parameters=c('S.msy.c')
x <- post.arr[,parameters,]
S.MSY<-quantile(x, probs=c(0,0.15,0.50,0.65,1))
S.MSY <- data.frame(S.MSY )
write.csv(S.MSY, file= paste0(out.path,"/percentile_method.csv"))
png(paste0(out.path,"/S.MSY.png"), res=600, height=4.5, width=8, units="in")
plot(post[,parameters]) 
dev.off()

# p_q_Nya.csv file----
rawdat<-as.data.frame(read.csv("state_space_model/data/Taku_sockeye.csv",header=T) ) #get age_comp data
rawdat  %>% select (c(year, x4, x5, x6)) %>%
  mutate(age4 = x4/(x4+x5+x6),
         age5 = x5/(x4+x5+x6),
         age6 = x6/(x4+x5+x6)) %>%
  dplyr::select (c(year, age4, age5, age6)) %>%
  gather(key="age", value="age_comp", -year) %>%
  mutate(age = ifelse(age == 'age4', "Ages 2-4",
                      ifelse(age== 'age5', "Age 5","Ages 6-8"))) -> age_data
summary <- summary(post)# extract p
stats <- summary$statistics;  colnames(stats)
stats <- stats[,c(1)] #select columns of interest
data.frame(stats) %>% 
  rownames_to_column('variable') -> stats
stats %>% 
  filter(str_detect(variable, "p")) %>% 
  filter(!str_detect(variable, "alpha")) %>%
  filter(!str_detect(variable, "lnalpha")) %>% 
  filter(!str_detect(variable, "lnalpha.c")) %>%
  filter(!str_detect(variable, "phi")) %>%
  filter(!str_detect(variable, "pi")) %>%
  rename_at(vars(starts_with("stats")), 
            funs(str_replace(., "stats", "p"))) %>%
  dplyr::select(-c(variable))-> df
new.df <- data.frame(
  year = c(1974:2014, 1974:2014, 1974:2014),
  age = rep(c("Ages 2-4", "Age 5", "Ages 6-8"), each = 41))
p_data <- cbind(df, new.df)

stats %>% 
  filter(str_detect(variable, "q")) %>% 
  filter(!str_detect(variable, "S.eq.c")) %>%
  filter(!str_detect(variable, " S.eq.c2")) %>%
  rename_at(vars(starts_with("stats")), 
            funs(str_replace(., "stats", "q"))) %>%
  dplyr::select(-c(variable))-> df
new.df <- data.frame(
  year = c(1980:2018, 1980:2018, 1980:2018),
  age = rep(c("Ages 2-4", "Age 5", "Ages 6-8"), each = 39))
q_data <- cbind(df, new.df)

stats %>% 
  filter(str_detect(variable, "N.ya")) %>% 
  rename_at(vars(starts_with("stats")), 
            funs(str_replace(., "stats", "Nya"))) %>%
  dplyr::select(-c(variable))-> df
Nya_data <- cbind(df, new.df)
step1 <- merge(age_data, p_data, by=c("year", "age"), all=TRUE)
step2 <- merge(step1, q_data, by=c("year", "age"), all=TRUE)
p_q_Nya <- merge(step2, Nya_data, by=c("year", "age"), all=TRUE)
p_q_Nya <- p_q_Nya[order(p_q_Nya$age, p_q_Nya$year),]
p_q_Nya %>% mutate(age = as.character(age))
write.csv(p_q_Nya, file= paste0(out.path,"/p_q_Nya.csv")) 

# parameters file----
data.frame(quants) %>% 
  rownames_to_column('variable') -> quants
quants %>%
  filter(str_detect(variable, "h.above")) %>% 
  rename_at(vars(starts_with("X")), 
            funs(str_replace(., "X", "h.above"))) %>%
  dplyr::select(-c(variable))-> df
new.df <- data.frame(
  year = rep(c(1980:2018),1))
df1 <- cbind(df, new.df)

quants %>%
  filter(str_detect(variable, "h.below")) %>% 
  rename_at(vars(starts_with("X")), 
            funs(str_replace(., "X", "h.below"))) %>%
  dplyr::select(-c(variable))-> df
df2 <- cbind(df, new.df)

quants %>%
  filter(str_detect(variable, "mu.hbelow")) %>% 
  rename_at(vars(starts_with("X")), 
            funs(str_replace(., "X", "mu.hbelow"))) %>%
  dplyr::select(-c(variable))-> df
df3 <- cbind(df, new.df)

quants %>%
  filter(str_detect(variable, "mu.habove")) %>% 
  rename_at(vars(starts_with("X")), 
            funs(str_replace(., "X", "mu.habove"))) %>%
  dplyr::select(-c(variable))-> df
df4 <- cbind(df, new.df)

quants %>%
  filter(str_detect(variable, "inriver.run")) %>% 
  rename_at(vars(starts_with("X")), 
            funs(str_replace(., "X", "inriver.run"))) %>%
  dplyr::select(-c(variable))-> df
df5 <- cbind(df, new.df)

quants %>%
  filter(str_detect(variable, "S")) %>% 
  filter(!str_detect(variable, "S.eq.c")) %>%
  filter(!str_detect(variable, " S.eq.c2")) %>%
  filter(!str_detect(variable, "S.max")) %>%
  filter(!str_detect(variable, "S.msy.c")) %>%
  filter(!str_detect(variable, "S.msy.c2")) %>%
  rename_at(vars(starts_with("X")), 
            funs(str_replace(., "X", "S"))) %>%
  dplyr::select(-c(variable))-> df
df6 <- cbind(df, new.df)

quants %>%
  filter(str_detect(variable, "N")) %>% 
  filter(!str_detect(variable, "N.ya")) %>%
  rename_at(vars(starts_with("X")), 
            funs(str_replace(., "X", "N"))) %>%
  dplyr::select(-c(variable))-> df
df7 <- cbind(df, new.df)

quants %>%
  filter(str_detect(variable, "log.resid")) %>% 
  filter(!str_detect(variable, "log.resid.0")) %>%
  rename_at(vars(starts_with("X")), 
            funs(str_replace(., "X", "log.resid"))) %>%
  dplyr::select(-c(variable))-> df
new.df <- data.frame(
  year = rep(c(1980:2014),1))
df8 <- cbind(df, new.df)

quants %>%
  filter(str_detect(variable, "R")) %>%
  filter(!str_detect(variable, "mean.log.RO")) %>%
  filter(!str_detect(variable, "sigma.R")) %>%
  filter(!str_detect(variable, "sigma.R0")) %>%
  rename_at(vars(starts_with("X")), 
            funs(str_replace(., "X", "R"))) %>%
  dplyr::select(-c(variable))-> df
new.df <- data.frame(
  year = rep(c(1974:2014),1))
df9 <- cbind(df, new.df)

step1 <- merge(df1, df2, by=c("year"), all=TRUE)
step2 <- merge(step1, df3, by=c("year"), all=TRUE)
step3 <- merge(step2, df4, by=c("year"), all=TRUE)
step4 <- merge(step3, df5, by=c("year"), all=TRUE)
step5 <- merge(step4, df6, by=c("year"), all=TRUE)
step6 <- merge(step5, df7, by=c("year"), all=TRUE)
step7 <- merge(step6, df8, by=c("year"), all=TRUE)
step8 <- merge(step7, df9, by=c("year"), all=TRUE)
write.csv(step8, file= paste0(out.path,"/parameters.csv")) 

# lambert calc----
parameters=c("lnalpha", "beta", "lnalpha.c")
x <- as.data.frame(post.arr[,parameters,])
coda1 <- x[,1:3]
coda2 <- x[,4:6]
coda3 <- x[,7:9]
coda1 %>% 
  rename(beta = beta.1,
         lnalpha = lnalpha.1,
         lnalpha.c = lnalpha.c.1)-> coda1
coda2 %>% 
  rename(beta = beta.2,
         lnalpha = lnalpha.2,
         lnalpha.c = lnalpha.c.2) -> coda2
coda3 %>% 
  rename(beta = beta.3,
         lnalpha = lnalpha.3,
         lnalpha.c = lnalpha.c.3) -> coda3
coda<-rbind(coda1,coda2,coda3)
coda %>% 
  mutate(Smsy_lambert = (1-lambert_W0(exp(1-lnalpha.c)))/beta,
         Umsy_lambert = (1-lambert_W0(exp(1-lnalpha.c))),
         Smsy_lambert80 = Smsy_lambert *0.80)  %>%
  as.data.frame() %>%
  dplyr::select(Smsy_lambert,Umsy_lambert, Smsy_lambert80) -> coda
coda %>% 
  apply(., 2, sd) %>%
  as.data.frame()%>%
  t()%>%
  as.data.frame()%>%
  rownames_to_column('variable') %>%
  mutate(variable = ifelse(variable == '.', "sd", "sd"))-> sd
coda %>% 
  apply(., 2, mean) %>%
  as.data.frame() %>%
  t()%>%
  as.data.frame() %>%
  rownames_to_column('variable') %>%
  mutate(variable = ifelse(variable == '.', "mean", "mean"))-> mean
rbind(sd,mean) -> x1
q1<-apply(coda,2,quantile,probs=c(0,0.025,0.5,0.975,1))
q1 %>%
  as.data.frame() %>%
  rownames_to_column('variable') -> x2
rbind(x1,x2) -> x3
x3 %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column('variable') %>%
  rename(Mean = 'V2',
         SD ='V1',
         perc_0 = 'V3',
         perc_2.5 = 'V4',
         perc_50 = 'V5',
         perc_97.5 = 'V6',
         perc_100 = 'V7') %>%
  mutate (time_series_se = NaN,
          converge = NaN,
          mc_error = NaN) %>%
  dplyr::select(variable, Mean, SD, time_series_se, perc_2.5, perc_50, perc_97.5, mc_error, converge) %>%
  .[-1,] %>%
write.csv(., file= paste0(out.path,"/quantiles_lambert.csv"))    

# create coda file----
parameters=c("lnalpha", "beta", "lnalpha.c")
x <- post.arr[,parameters,]
x <- data.frame(x)
coda1 <- x[,1:3]
coda2 <- x[,4:6]
coda3 <- x[,7:9]
coda1 %>% 
  rename(beta = beta.1,
         lnalpha = lnalpha.1,
         lnalpha.c = lnalpha.c.1)-> coda1
coda2 %>% 
  rename(beta = beta.2,
         lnalpha = lnalpha.2,
         lnalpha.c = lnalpha.c.2) -> coda2
coda3 %>% 
  rename(beta = beta.3,
         lnalpha = lnalpha.3,
         lnalpha.c = lnalpha.c.3) -> coda3
coda<-rbind(coda1,coda2,coda3)
write.csv(coda, file= paste0(out.path,"/coda.csv") ,row.names=FALSE)   

#combine statsquants and lambert datafile----
stats<-as.data.frame(read.csv(file=paste0(out.path, "/statsquants.csv"),header=T))
stats %>%
rename(perc_2.5 = 'X2.5.',
       perc_50 = 'X50.',
       perc_97.5 = 'X97.5.') -> stats
lambert<-as.data.frame(read.csv(file=paste0(out.path, "/quantiles_lambert.csv"),header=T))
rbind(stats, lambert)-> x
write.csv(x, file= paste0(out.path,"/stats.csv") ,row.names=FALSE)  

#trace and density plots----
parameters <- c('S.eq.c','S.msy.c','U.msy.c','alpha','beta',
                'lnalpha','lnalpha.c','phi','S.max','D.sum','D.scale', 'pi','sigma.RO','sigma.R',
                'log.resid.0', 'mean.log.RO')
pdf("state_space_model/output/rjags_Explore_BaseCase/density1.pdf",height=6, width=8)
denplot(post, parms = c(parameters))
dev.off()
pdf("state_space_model/output/rjags_Explore_BaseCase/trace1.pdf",height=10, width=8,onefile=F)
traplot(post, parms = c(parameters))
dev.off()
parameters <- c('S')
pdf("state_space_model/output/rjags_Explore_BaseCase/density2.pdf",height=10, width=8,onefile=F)
denplot(post, parms = c(parameters))
dev.off()
pdf("state_space_model/output/rjags_Explore_BaseCase/trace2.pdf",height=10, width=8,onefile=F)
traplot(post, parms = c(parameters))
dev.off()
parameters <- c('R')
pdf("state_space_model/output/rjags_Explore_BaseCase/density3.pdf",height=10, width=8,onefile=F)
denplot(post, parms = c(parameters))
dev.off()
pdf("state_space_model/output/rjags_Explore_BaseCase/trace3.pdf",height=10, width=8,onefile=F)
traplot(post, parms = c(parameters))
dev.off()
parameters <- c('N')
pdf("state_space_model/output/rjags_Explore_BaseCase/density4.pdf",height=10, width=8,onefile=F)
denplot(post, parms = c(parameters))
dev.off()
pdf("state_space_model/output/rjags_Explore_BaseCase/trace4.pdf",height=10, width=8,onefile=F)
traplot(post, parms = c(parameters))
dev.off()
parameters <- c('p')
pdf("state_space_model/output/rjags_Explore_BaseCase/density5.pdf",height=10, width=8,onefile=F)
denplot(post, parms = c(parameters))
dev.off()
pdf("state_space_model/output/rjags_Explore_BaseCase/trace5.pdf",height=10, width=8,onefile=F)
traplot(post, parms = c(parameters))
dev.off()
parameters <- c('q')
pdf("state_space_model/output/rjags_Explore_BaseCase/density6.pdf",height=10, width=8,onefile=F)
denplot(post, parms = c(parameters))
dev.off()
pdf("state_space_model/output/rjags_Explore_BaseCase/trace6.pdf",height=10, width=8,onefile=F)
traplot(post, parms = c(parameters))
dev.off()

# autocorrelation plots----
windows(record=T)
pdf("state_space_model/output/rjags_Explore_BaseCase/autocorr.pdf",height=6, width=8,onefile=T,useDingbats=F)
autocorr.plot(post, lag.max=5)
dev.off()
dev.off()
autocorr.summary<-autocorr.diag(post)
autocorr.summary<-data.frame(autocorr.summary)
write.csv(autocorr.summary, file= paste0(out.path,"/autocorr.csv")) 

#density and time series plots----
post.samp <- post
nvars <- dim(post.samp[[1]])[2]
nsamps <- dim(post.samp[[1]])[1]
int <- 25
pdf("state_space_model/output/rjags_Explore_BaseCase/profiles.pdf",height=6, width=8)
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
dev.off()