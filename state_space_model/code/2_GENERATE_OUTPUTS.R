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
  dplyr::rename(time_series_se = 'Time-series SE') %>%
  rownames_to_column('variable') %>%
  mutate (mc_error = time_series_se/SD,
          converge = ifelse(mc_error < 0.05, "true", "false")) %>%
  dplyr::rename(perc_2.5 = '2.5%',
                perc_50 = '50%',
                perc_97.5 = '97.5%') %>%
  dplyr::select(variable, Mean, SD, time_series_se, perc_2.5, perc_50, perc_97.5, mc_error, converge) -> statsquants

# lambert calc----
parameters=c("lnalpha", "beta", "lnalpha.c")
x <- as.data.frame(post.arr[,parameters,])
coda1 <- x[,1:3]
coda2 <- x[,4:6]
coda3 <- x[,7:9]
coda1 %>% 
  dplyr::rename(beta = beta.1,
                lnalpha = lnalpha.1,
                lnalpha.c = lnalpha.c.1)-> coda1
coda2 %>% 
  dplyr::rename(beta = beta.2,
                lnalpha = lnalpha.2,
                lnalpha.c = lnalpha.c.2) -> coda2
coda3 %>% 
  dplyr::rename(beta = beta.3,
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
  dplyr::rename(Mean = 'V2',
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
  .[-1,] -> quantiles_lambert
rbind(statsquants, quantiles_lambert) %>%
write.csv(., file= paste0(out.path,"/stats.csv") ,row.names=FALSE)

# lambert density plot----
#may have to run this twice??
parameters=c("lnalpha", "beta", "lnalpha.c")
x <- as.data.frame(post.arr[,parameters,])
coda1 <- x[,1:3]
coda2 <- x[,4:6]
coda3 <- x[,7:9]
coda1 %>% 
  dplyr::rename(beta = beta.1,
                lnalpha = lnalpha.1,
                lnalpha.c = lnalpha.c.1) -> coda1
coda2 %>% 
  dplyr::rename(beta = beta.2,
                lnalpha = lnalpha.2,
                lnalpha.c = lnalpha.c.2) -> coda2
coda3 %>% 
  dplyr::rename(beta = beta.3,
                lnalpha = lnalpha.3,
                lnalpha.c = lnalpha.c.3) -> coda3
coda <- rbind(coda1, coda2, coda3)
coda %>% 
  mutate(Smsy = (1-lambert_W0(exp(1-lnalpha.c)))/beta,
         Umsy = (1-lambert_W0(exp(1-lnalpha.c))))  %>%
  as.data.frame() -> coda
coda %>% 
  dplyr::select(Smsy) -> Smsy
coda %>% 
  dplyr::select(Umsy) -> Umsy

#denplot(Smsy, style="plain",  col="gray30", xlim=)
options(scipen=999)
ggplot(Smsy, aes(x=Smsy, fill=Smsy, color = Smsy)) +
  geom_density(fill ="#999999", alpha=0.5)+
  scale_color_manual(values=c("#999999"))+
  scale_fill_manual(values=c("#999999"))+
  geom_vline(xintercept = 43857,linetype = "longdash" ) +
  labs(x="Smsy",y="Density") + theme_set(theme_bw(base_size=14,base_family=
                                                    'Arial') +
                                           theme(panel.grid.major = element_blank(),
                                                 panel.grid.minor = element_blank())) + theme(legend.position="none") + 
  scale_x_continuous(labels = scales::comma, breaks = seq(0, 150000, 25000), limits = c(0, 150000)) -> plot1

ggplot(Umsy, aes(x=Umsy, fill=Umsy)) +
  geom_density(fill ="#999999", alpha=0.5)+
  scale_color_manual(values=c("#999999")) +
  scale_fill_manual(values=c("#999999")) + geom_vline(xintercept = 0.75,linetype = "longdash" ) +
  labs(x="Umsy",y="Density") + theme_set(theme_bw(base_size=14,base_family=
                                                    'Arial')+
                                           theme(panel.grid.major = element_blank(),
                                                 panel.grid.minor = element_blank())) + theme(legend.position="none") + 
  scale_x_continuous(breaks = seq(0, 1, 0.25), limits = c(0, 1)) -> plot2
png(file= paste0(out.path,"/density",".png"),res=500, width=8, height=9, units ="in")
plot_grid(plot1, plot2,  labels = c("A", "B", "C"), ncol = 1, align="v",hjust=-8,
          vjust=2, label_size=14)
dev.off()

# 90th and 95th percentiles of output quants----
parameters = c('MSY')
x <- post.arr[,parameters,]
MSY <- quantile(x, probs=c(0, 0.025, 0.05, 0.5, 0.95, 0.975, 1))
MSY <- data.frame(MSY)

parameters = c('S.eq')
x <- post.arr[,parameters,]
S.eq <- quantile(x, probs=c(0, 0.025, 0.05, 0.5, 0.95, 0.975, 1))
S.eq <- data.frame(S.eq)

parameters = c('S.max')
x <- post.arr[,parameters,]
S.max <- quantile(x, probs=c(0, 0.025, 0.05, 0.5, 0.95, 0.975, 1))
S.max <- data.frame(S.max)

parameters = c('S.msy')
x <- post.arr[,parameters,]
S.msy <- quantile(x, probs=c(0, 0.025, 0.05, 0.5, 0.95, 0.975, 1))
S.msy  <- data.frame(S.msy)

parameters = c('U.msy')
x <- post.arr[,parameters,]
U.msy <- quantile(x, probs=c(0, 0.025, 0.05, 0.5, 0.95, 0.975, 1))
U.msy <- data.frame(U.msy)

parameters = c('alpha')
x <- post.arr[,parameters,]
alpha <- quantile(x, probs=c(0, 0.025, 0.05, 0.5, 0.95, 0.975, 1))
alpha <- data.frame(alpha)

parameters = c('beta')
x <- post.arr[,parameters,]
beta <-quantile(x, probs=c(0,0.025, 0.05, 0.5, 0.95, 0.975, 1))
beta <- data.frame(beta)

parameters = c('lnalpha')
x <- post.arr[,parameters,]
lnalpha <- quantile(x, probs=c(0, 0.025, 0.05, 0.5, 0.95, 0.975, 1))
lnalpha <- data.frame(lnalpha)

parameters = c('lnalpha.c')
x <- post.arr[,parameters,]
lnalpha.c <- quantile(x, probs=c(0, 0.025, 0.05, 0.5, 0.95, 0.975, 1))
lnalpha.c <- data.frame(lnalpha.c)

parameters = c('resid.red.0')
x <- post.arr[,parameters,]
resid.red.0 <- quantile(x, probs=c(0, 0.025, 0.05, 0.5, 0.95, 0.975, 1))
resid.red.0 <- data.frame(resid.red.0)

parameters = c('sigma.red')
x <- post.arr[,parameters,]
sigma.red <- quantile(x, probs=c(0,0.025, 0.05, 0.5, 0.95, 0.975, 1))
sigma.red <- data.frame(sigma.red)

parameters = c('sigma.white')
x <- post.arr[,parameters,]
sigma.white <- quantile(x, probs=c(0, 0.025, 0.05, 0.5, 0.95, 0.975, 1))
sigma.white <- data.frame(sigma.white)

step1 <- cbind(MSY, S.eq)
step2 <- cbind(step1, S.max)
step3 <- cbind(step2, S.msy)
step4 <- cbind(step3, U.msy)
step5 <- cbind(step4, alpha)
step6 <- cbind(step5, beta)
step7 <- cbind(step6, lnalpha)
step8 <- cbind(step7, lnalpha.c)
step9 <- cbind(step8, resid.red.0)
step10 <- cbind(step9,  sigma.red)
step11 <- cbind(step10,  sigma.white)
step11 %>% 
  t()%>%
  write.csv(., file= paste0(out.path,"/percentiles.csv")) 

# Gelman Diagnostics----
gel <- as.data.frame(gelman.diag(post, multivariate=F)[[1]])
poor.threshold = 1.2 #values less than 1.2 are generally considered converged
gel %>%
  rownames_to_column('variable') %>%
  dplyr::rename(point_estimate = "Point est.") %>%
  mutate (converge = ifelse(point_estimate < poor.threshold, "true", "false")) %>%
  write.csv(., file= paste0(out.path,"/gelman.csv"))   

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
statsquants %>%
  dplyr::select(variable) %>% 
  add_row(variable = "frac1") %>% #0.1 and 0.5 default
  add_row(variable = "frac2") %>%
cbind(., df)%>% 
  dplyr::select(-variable1) %>%
  write.csv(., paste0(out.path,"/geweke.csv")) 

pdf(file= paste0(out.path,"/geweke",".pdf"), height=10, width=8,onefile=T)
geweke.plot(post)
dev.off()

# create coda file----
parameters=c("lnalpha", "beta", "lnalpha.c")
x <- post.arr[,parameters,]
x <- data.frame(x)
coda1 <- x[,1:3]
coda2 <- x[,4:6]
coda3 <- x[,7:9]
coda1 %>% 
  dplyr::rename(beta = beta.1,
                lnalpha = lnalpha.1,
                lnalpha.c = lnalpha.c.1)-> coda1
coda2 %>% 
  dplyr::rename(beta = beta.2,
                lnalpha = lnalpha.2,
                lnalpha.c = lnalpha.c.2) -> coda2
coda3 %>% 
  dplyr::rename(beta = beta.3,
                lnalpha = lnalpha.3,
                lnalpha.c = lnalpha.c.3) -> coda3
coda<-rbind(coda1,coda2,coda3)
write.csv(coda, file= paste0(out.path,"/coda.csv") ,row.names=FALSE)   

#trace and density plots----
parameters <- c("lnalpha","beta", "sigma.red","S.msy","MSY", "lnalpha.c", "alpha", "S.max", "S.eq","U.msy", "sigma.white",
                "resid.red.0")
png(file= paste0(out.path,"/density_plot",".png"), res=500, height=6, width=8, units ="in")
denplot(post, parms = c(parameters), style="plain", greek=TRUE, col="gray30", collapse =T)
dev.off()

pdf(file= paste0(out.path,"/trace_plot",".pdf"), height=6, width=8)
traplot(post, parms = c(parameters))
dev.off()

# autocorrelation plots----
windows(record=T)
pdf(file= paste0(out.path,"/autocorrelation",".pdf"), height=6, width=8,onefile=T,useDingbats=F)
autocorr.plot(post, lag.max=5)
dev.off()
autocorr.summary<-autocorr.diag(post)
autocorr.summary<-data.frame(autocorr.summary)
write.csv(autocorr.summary, file= paste0(out.path,"/autocorr.csv")) 
