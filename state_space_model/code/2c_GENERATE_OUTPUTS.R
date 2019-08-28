# THIS SCRIPT IS SOURCED FROM INSIDE 1_RUN_MODEL.R
#(if package.use = "rjags") 

# - post.samp and post 
      # these are the mcmc.list objects created by coda.samples
      # you can access individual variables like this: post[,"var.name"]
# - post.arr 
# Numerical summary of each parameter (mean, median, quantiles of posteriers)----
summary<-summary(post)  
stats<-summary$statistics;  colnames(stats)
quants<-summary$quantiles;  colnames(quants)
statsquants <- cbind(stats,quants) 
statsquants %>% 
  as.data.frame() %>% 
  dplyr::select(Mean, SD, 'Time-series SE', '2.5%', '50%', '97.5%') %>%
  rename(time_series_se = "Time-series SE") %>%
  rownames_to_column('variable') %>%
  mutate (mc_error = time_series_se/SD,
          converge = ifelse(mc_error < 0.05, "true", "false"),
          mu = "mu",
          mu_val = mu_vec[i],
          sigma = "sigma",
          sigma_val = sigma_vec[j],
          model1 = paste(mu, mu_val, sep="_"),
          model2 = paste(sigma, sigma_val, sep = "_"),
          model = paste (model1, model2,sep = "_" )) %>%
  dplyr::select(-c(model1, model2, mu, sigma)) %>%
  write.csv(., file= paste0(out.path2,"/beta_mu",mu_vec[i],"_sigma",sigma_vec[j],"_dist_",dist_name,"/statsquants.csv")) 

# Gelman Diagnostics----
gel <- as.data.frame(gelman.diag(post, multivariate=F)[[1]])
poor.threshold = 1.2 #values less than 1.2 are generally considered converged
gel %>%
  rownames_to_column('variable') %>%
  rename(point_estimate = "Point est.") %>%
  mutate (converge = ifelse(point_estimate < poor.threshold, "true", "false")) %>%
  filter(variable == "S.eq.c" | variable == "S.msy.c" |variable == "U.msy.c" | variable == "alpha" | 
           variable == "beta" | variable == "lnalpha" |variable == "lnalpha.c" | variable == "phi" |variable == "U.max.c2"|
           variable == "sigma.R" | variable == "S.eq.c2" |variable == "U.msy.c2" | variable == "S.msy.c2" ) %>% 
  mutate (mu = "mu",
          mu_val = mu_vec[i],
          sigma = "sigma",
          sigma_val = sigma_vec[j],
          model1 = paste(mu, mu_val, sep="_"),
          model2 = paste(sigma, sigma_val, sep = "_"),
          model = paste (model1, model2,sep = "_" )) %>%
  dplyr::select(-c(model1, model2, mu, sigma)) %>%
  write.csv(., paste0(out.path2,"/beta_mu",mu_vec[i],"_sigma",sigma_vec[j],"_dist_",dist_name,"/gelman.csv")) 

#trace and density plots----
conv.pars <- c('S.eq.c','S.msy.c','U.msy.c','alpha','beta',
               'lnalpha','lnalpha.c','phi','sigma.R',
               'S.eq.c2', 'U.msy.c2', 'S.msy.c2', 'U.max.c2')
pdf(file=paste0(out.path2,"/beta_mu",mu_vec[i],"_sigma",sigma_vec[j],"_dist_",dist_name,"/density1.pdf"),height=6, width=8)
denplot(post, parms = c(conv.pars))
dev.off()
pdf(file=paste0(out.path2,"/beta_mu",mu_vec[i],"_sigma",sigma_vec[j],"_dist_",dist_name,"/trace1.pdf"),height=10, width=8,onefile=F)
traplot(post, parms = c(conv.pars))
dev.off()

#lambert calc----
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
  mutate (mu = "mu",
          mu_val = mu_vec[i],
          sigma = "sigma",
          sigma_val = sigma_vec[j],
          model1 = paste(mu, mu_val, sep="_"),
          model2 = paste(sigma, sigma_val, sep = "_"),
          model = paste (model1, model2,sep = "_" )) %>%
  dplyr::select(-c(model1, model2, mu, sigma)) %>%
  write.csv(., file= paste0(out.path2,"/beta_mu",mu_vec[i],"_sigma",sigma_vec[j],"_dist_",dist_name,"/quantiles_lambert.csv"))    

#combine statsquants and lambert datafile----
stats<-as.data.frame(read.csv(file=paste0(out.path2,"/beta_mu",mu_vec[i],"_sigma",sigma_vec[j], "_dist_",dist_name,"/statsquants.csv"),header=T))
stats %>%
  rename(perc_2.5 = 'X2.5.',
         perc_50 = 'X50.',
         perc_97.5 = 'X97.5.') -> stats
lambert<-as.data.frame(read.csv(file=paste0(out.path2,"/beta_mu",mu_vec[i],"_sigma",sigma_vec[j], "_dist_",dist_name,"/quantiles_lambert.csv"),header=T))
rbind(stats, lambert) -> x
write.csv(x, file= paste0(out.path2,"/beta_mu",mu_vec[i],"_sigma",sigma_vec[j],"_dist_",dist_name,"/stats.csv") ,row.names=FALSE)  


