profile <-function(i,z,xa.start, xa.end,lnalpha.c, beta){ 
  xa = seq(xa.start, xa.end, by=i) 
  x =(xa+i)*z
  # empty dataframes
  dat <- data.frame(S0=rep(1, length(coda[,1])))
  dat1 <- data.frame(S0=rep(0, length(coda[,1])))
  dat2 <- data.frame(S0=rep(0, length(coda[,1])))
  dat3 <- data.frame(S0=rep(1, length(coda[,1])))
  dat4 <- data.frame(S0=rep(0, length(coda[,1])))
  dat5 <- data.frame(S0=rep(0, length(coda[,1])))
  dat6 <- data.frame(S0=rep(1, length(coda[,1])))
  dat7 <- data.frame(S0=rep(0, length(coda[,1])))
  dat8 <- data.frame(S0=rep(0, length(coda[,1])))
  dat9 <- data.frame(S0=rep(0, length(coda[,1])))
  dat10 <- data.frame(S0=rep(0, length(coda[,1])))
  for (i in 1:length(xa)){
    dat[,i+1] = ifelse((x[i] * exp(coda$lnalpha.c-coda$beta*x[i])-x[i])>(0.7*coda$MSY.c), 0, ifelse(dat[,i]==0, 0,1))
    dat1[,i+1] = ifelse((x[i] * exp(coda$lnalpha.c-coda$beta*x[i])-x[i])>(0.7*coda$MSY.c), 1,0)
    dat2[,i+1] = ifelse((x[i] * exp(coda$lnalpha.c-coda$beta*x[i]))>(0.7*coda$Rmax), 1,0)
    dat3[,i+1] = ifelse((x[i] * exp(coda$lnalpha.c-coda$beta*x[i])-x[i])>(0.8*coda$MSY.c), 0, ifelse(dat3[,i]==0, 0,1))
    dat4[,i+1] = ifelse((x[i] * exp(coda$lnalpha.c-coda$beta*x[i])-x[i])>(0.8*coda$MSY.c), 1,0)
    dat5[,i+1] = ifelse((x[i] * exp(coda$lnalpha.c-coda$beta*x[i]))>(0.8*coda$Rmax), 1,0)
    dat6[,i+1] = ifelse((x[i] * exp(coda$lnalpha.c-coda$beta*x[i])-x[i])>(0.9*coda$MSY.c), 0, ifelse(dat6[,i]==0, 0,1))
    dat7[,i+1] = ifelse((x[i] * exp(coda$lnalpha.c-coda$beta*x[i])-x[i])>(0.9*coda$MSY.c), 1,0)
    dat8[,i+1] = ifelse((x[i] * exp(coda$lnalpha.c-coda$beta*x[i]))>(0.9*coda$Rmax), 1,0)
    dat9[,i+1] = x[i]*exp(coda$lnalpha.c-coda$beta*x[i])-x[i] #expected yield
    dat10[,i+1] = x[i]*exp(coda$lnalpha.c-coda$beta*x[i]) # CI around S
  }
  # Overfishing estimate ----
  f.over <- function(x){
    x %>% 
      dplyr::filter(complete.cases(.)) %>% 
      dplyr::summarise_all(funs(mean)) %>% 
      tidyr::gather() %>% 
      dplyr::select(value)
  }
  
  of_0.7 <- f.over(dat)
  of_0.8 <- f.over(dat3)
  of_0.9 <- f.over(dat6)
  
  # Optimal yield estimate ----
  oy_0.7 <- f.over(dat1)
  oy_0.8 <- f.over(dat4)
  oy_0.9 <- f.over(dat7)
  
  # Optimal recruitment ----
  or_0.7 <- f.over(dat2)
  or_0.8 <- f.over(dat5)
  or_0.9 <- f.over(dat8)
  
  #Bind dataframes together
  Y <- cbind(of_0.7,oy_0.7,or_0.7,of_0.8,oy_0.8,or_0.8,of_0.9,oy_0.9,or_0.9, c(0, x))
  names(Y) <- c('of_0.7','oy_0.7','or_0.7','of_0.8','oy_0.8','or_0.8','of_0.9','oy_0.9',
                'or_0.9','escapement')
  
  #Quantiles and Medians ----
  dat9 %>%
    summarise_all(funs(median = median, 
                       q95=quantile(., 0.95, na.rm=T), 
                       q90=quantile(., 0.90, na.rm=T),
                       q10=quantile(., 0.10, na.rm=T),
                       q5=quantile(., 0.05, na.rm=T))) -> mq
  names(mq) <- c(rep(('Median'),length(x)+1), 
                 rep(('q95'),length(x)+1), 
                 rep(('q90'),length(x)+1), 
                 rep(('q10'),length(x)+1), 
                 rep(('q5'),length(x)+1))
  
  qm <- data.frame(measure = names(mq), value = as.numeric(mq[1,]), escapement=rep(c(0,x), length(unique(names(mq)))))
  qm <- spread(qm, measure, value)
  qm <- qm[c("q95", "q90", "Median","q10", "q5", "escapement")]
  Y <- Y[c("oy_0.9", "oy_0.8", "or_0.9","or_0.8", "of_0.9", "of_0.8", "oy_0.7","or_0.7","of_0.7","escapement")]
  write.csv(qm,("state_space_model/output/rjags_Full_BaseCase/processed/QM.csv"), row.names=FALSE)
  write.csv(Y,("state_space_model/output/rjags_Full_BaseCase/processed/Y.csv"), row.names=FALSE)
  
  #confidence intervals ----
  dat10 %>%
    summarise_all(funs(median = median, 
                       q95=quantile(., 0.95, na.rm=T), 
                       q90=quantile(., 0.90, na.rm=T),
                       q10=quantile(., 0.10, na.rm=T),
                       q5=quantile(., 0.05, na.rm=T))) -> mq
  names(mq) <- c(rep(('Median'),length(x)+1), 
                 rep(('q95'),length(x)+1), 
                 rep(('q90'),length(x)+1), 
                 rep(('q10'),length(x)+1), 
                 rep(('q5'),length(x)+1))
  
  CI <- data.frame(measure = names(mq), value = as.numeric(mq[1,]), escapement=rep(c(0,x), length(unique(names(mq)))))
  CI <- spread(CI, measure, value)
  CI <- CI[c("q95", "q90", "Median","q10", "q5", "escapement")]
  write.csv(CI,("state_space_model/output/rjags_Full_BaseCase/processed/CI.csv"), row.names=FALSE)
  
  #create probability profile plots (0.7, 0.8, 0.9, 0.8 & 0.9)
  Y %>% 
    dplyr::select(escapement, oy_0.7, of_0.7,or_0.7) %>% 
    gather(key="variable", value="value", -escapement) %>% 
    ggplot(aes(escapement/1000, value, lty=variable))+geom_line()+
    xlab('escapement (1,000)')+ylab('Probability')+
    theme(legend.justification=c(1,0), legend.position=c(1,.5), 
          legend.key = element_blank(),legend.title=element_blank())
  ggsave("state_space_model/output/rjags_Full_BaseCase/processed/0.7.AR.png", dpi=200, width=8, height=5, units='in')
  
  Y %>% 
    dplyr::select(escapement, oy_0.8, of_0.8, or_0.8) %>%
    gather(key="variable", value="value", -escapement) %>% 
    ggplot(aes(escapement/1000, value, lty=variable))+geom_line()+
    xlab('escapement (1,000)')+ylab('Probability')+
    theme(legend.justification=c(1,0), legend.position=c(1,.5), 
          legend.key = element_blank(),legend.title=element_blank())
  ggsave("state_space_model/output/rjags_Full_BaseCase/processed/0.8.AR.png", dpi=200, width=8, height=5, units='in')
  
  Y %>% 
    dplyr::select(escapement, oy_0.9, of_0.9, or_0.9) %>% 
    gather(key="variable", value="value", -escapement) %>% 
    ggplot(aes(escapement/1000, value, lty=variable))+geom_line()+
    xlab('escapement (1,000)')+ylab('Probability')+
    theme(legend.justification=c(1,0), legend.position=c(1,.5), 
          legend.key = element_blank(),legend.title=element_blank())
  ggsave("state_space_model/output/rjags_Full_BaseCase/processed/0.9.AR.png", dpi=200, width=8, height=5, units='in')
  
  read.csv("state_space_model/output/rjags_Full_BaseCase/processed/Y.csv") -> Y
  Y %>% 
    dplyr::select(escapement, oy_0.9, oy_0.8) %>% 
    gather(key="variable", value="value", -escapement) %>% 
    mutate(sra = "Yield Profile",
           max_pct =ifelse(grepl("oy_0.8",variable), 
                           0.8,0.9))-> my1
  
  Y %>% 
    dplyr::select(escapement, of_0.9, of_0.8) %>% 
    gather(key="variable", value="value", -escapement) %>% 
    mutate(sra = "Overfishing Profile",
           max_pct =ifelse(grepl("of_0.8",variable), 
                           0.8,0.9))-> my2
  
  Y %>% 
    dplyr::select(escapement, or_0.9, or_0.8) %>% 
    gather(key="variable", value="value", -escapement) %>% 
    mutate(sra = "Recruitment Profile",
           max_pct =ifelse(grepl("or_0.8",variable), 
                           0.8,0.9))-> my3
  
  my4<-rbind(my1, my2, my3)
  my4 %>%
    dplyr::select(escapement, variable, value, sra, max_pct) %>%
    mutate(escapement = as.numeric(escapement),
           Probability = as.numeric(value),
           max_pct = as.factor(max_pct)) -> my4
  ggplot(my4, aes(x = escapement, y = Probability, linetype = max_pct)) + 
    geom_rect(aes(xmin = LowerB, xmax = UpperB, ymin = 0, ymax = 1),
              inherit.aes = FALSE, fill = "grey80", alpha = 0.3) +
    geom_line()+xlab('escapement (S)')+
    scale_x_continuous(labels = comma, breaks = seq(0, 200000, 25000), limits = c(0, 200000))+
    scale_linetype_discrete(name = "Percent of Max.")+
    facet_grid(sra ~ .) +geom_vline(xintercept=SMSY, lwd=1.25)+
    theme(legend.position="bottom")
  options(scipen=99999)
  ggsave("state_space_model/output/rjags_Full_BaseCase/processed/0.8_0.9.png", dpi=200, dev='png', width=7, height=6, units='in')
  
  
  ggplot(qm, aes(escapement, Median))+geom_line(size=1)+
    geom_ribbon(aes(ymin = q5, ymax = q95), alpha=.15)+
    geom_ribbon(aes(ymin = q10, ymax = q90), alpha=.15)+ xlab('escapement (S)')+
    ylab('Expected Yield')+scale_y_continuous(labels = comma)+
    scale_x_continuous(labels = comma,breaks = seq(0, 200000, 25000), limits = c(0,200000))+
    scale_y_continuous(labels = comma,breaks = seq(-200000, 200000, 25000), limits = c(-200000,200000))+
    geom_vline(xintercept = LowerB,linetype = "longdash" )+geom_vline(xintercept = UpperB ,linetype = "longdash")
  ggsave("state_space_model/output/rjags_Full_BaseCase/processed/expected_sustained_yield.png", dpi=200, width=8, height=5, units='in')}


