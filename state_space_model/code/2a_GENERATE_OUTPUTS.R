# THIS SCRIPT IS SOURCED FROM INSIDE 1_RUN_MODEL.R
# (if package.use = "R2jags")

# uses  objects mcmc.samples and mcmc.summary created in the earlier script
# also  r2jags.out$BUGSoutput


# NOTE: For now, the year ranges etc are hardwired here -> change to look up from inputs.
spn.yrs <- 1980:2018

# Read in the latest version of all the subroutines



source("state_space_model/code/WSPMetricsModule_SUB_SlopeCalcFunctions.R")  
source("state_space_model/code/WSPMetricsModule_calcPercChange.R")
source("state_space_model/code/WSPMetricsModule_SmoothSeries.R")
source("state_space_model/code/WSPMetricsModule_SUB_RelAbdBM.R")
source("state_space_model/code/PlottingFunctions.R")



sgen <- apply(mcmc.samples[,c("alpha","beta")],MARGIN=1, 
      FUN = function(X){sgen.out <- calcRickerSgen(X[1], X[2]); return(sgen.out)})
mcmc.samples <- cbind(mcmc.samples,S.gen=sgen)
head(mcmc.samples[,"S.gen"])


quants.plot <- c("2.5%","25%","50%","75%","97.5%")


spn.pattern <- "(S\\[.*?\\])"
spn.matches <- grepl(spn.pattern, dimnames(mcmc.summary)[[1]])
spn.plot.vars <- dimnames(mcmc.summary)[[1]][spn.matches]





pdf(paste0(out.path,"/StateSpaceModel_Diagnostics.pdf"),width=11,height=8.5)


# Page 1: time series of spn and benchmark estimates


plot(1:5,1:5,type="n",xlab= "Year", ylab= "Adult Sockeye (> x cm?)",
     xlim=c(min(spn.yrs),max(spn.yrs)+20),ylim=c(0,130000),bty="n",axes=FALSE)
axis(2)
axis(1,at = pretty(spn.yrs)  )

abline(v=1984,col="lightgrey",lty=1)
lines(spn.yrs,mcmc.summary[spn.plot.vars,"mean"],lwd=2,col="darkblue")
lines(spn.yrs,mcmc.summary[spn.plot.vars,"25%"],lwd=1,col="darkblue",lty=2)
lines(spn.yrs,mcmc.summary[spn.plot.vars,"75%"],lwd=1,col="darkblue",lty=2)

legend("top",legend=c("Mean","25%-75%"),lty=c(1,2),bty="n")

var.plot <- "S.msy.c"
at.plot <- 2022
abline(h=mcmc.summary[var.plot,"50%"],col="green",lty=2)
box.add(mcmc.summary[var.plot,quants.plot],at = at.plot,width = 1)
text(at.plot,mcmc.summary[var.plot,"2.5%"],labels = var.plot,adj=c(0.5,1))

# almost identical to previous
#var.plot <- "S.msy.c2"
#at.plot <- 2026
#abline(h=mcmc.summary[var.plot,"50%"],col="green",lty=2)
#box.add(mcmc.summary[var.plot,quants.plot],at = at.plot,width = 1)
#text(at.plot,mcmc.summary[var.plot,"2.5%"],labels = var.plot,adj=c(0.5,1))

var.plot <- "S.eq.c"
at.plot <- 2030
abline(h=mcmc.summary[var.plot,"50%"],col="green",lty=2)
box.add(mcmc.summary[var.plot,quants.plot],at = at.plot,width = 1)
text(at.plot,mcmc.summary[var.plot,"2.5%"],labels = var.plot,adj=c(0.5,1))


var.plot <- "S.max"
at.plot <- 2026
#abline(h=mcmc.summary[var.plot,"50%"],col="green",lty=2)
box.add(mcmc.summary[var.plot,quants.plot],at = at.plot,width = 1)
text(at.plot,mcmc.summary[var.plot,"2.5%"],labels = var.plot,adj=c(0.5,1))


# Sgen is not yet part of the summary
at.plot <- 2020
sgen.quants <- quantile(mcmc.samples[,"S.gen"],probs=c(0.025,0.25,0.5,0.75,0.975))
abline(h=sgen.quants[3],col="red",lwd=2,lty=2)
box.add(sgen.quants,at = at.plot,width = 1)
text(at.plot,sgen.quants[1],labels = "S.gen",adj=c(0.5,1))

title(main = "Pattern in Modelled *True* Spawner Abundance and Biol. BM")






##############
# Page 2: time series of  inriver run vs. MR estimate



var.pattern <- "(inriver.run\\[.*?\\])"
var.matches <- grepl(var.pattern, dimnames(mcmc.summary)[[1]])
var.plot.vars <- dimnames(mcmc.summary)[[1]][var.matches]
plot(1:5,1:5,type="n",xlab= "Year", ylab= "Adult Sockeye (> x cm?)",
     xlim=c(min(spn.yrs),max(spn.yrs)),ylim=c(0,150000),bty="n",axes=FALSE)
axis(2)
axis(1,at = pretty(spn.yrs)  )
abline(v=1984,col="lightgrey",lty=1)


# input (MR est)
points(spn.yrs,dat$ir,pch=19,col="red",cex=0.8)
ir.ci <- dat$ir*dat$cv.ir*2
segments(spn.yrs, dat$ir - ir.ci, spn.yrs, dat$ir + ir.ci,col="red")

# modelled "true" values
lines(spn.yrs,mcmc.summary[var.plot.vars,"mean"],lwd=2,col="darkblue",lty=1)
lines(spn.yrs,mcmc.summary[var.plot.vars,"25%"],lwd=1,col="darkblue",lty=2)
lines(spn.yrs,mcmc.summary[var.plot.vars,"75%"],lwd=1,col="darkblue",lty=2)

legend("bottomright",legend=c("Modelled Mean","Modelled 25%-75%","MR Est +- 2SD"),
       lty=c(1,2,1),col=c("darkblue","darkblue","red"), pch=c(NA, NA,19),bty="n")


title(main = "Pattern in Modelled In-River Run and Mark-Recapture Estimates")



##########################################################
#   R/S

var.pattern <- "(R\\[.*?\\])"
var.matches <- grepl(var.pattern, dimnames(mcmc.summary)[[1]])
var.plot.vars <- dimnames(mcmc.summary)[[1]][var.matches]
r.mean <- mcmc.summary[var.plot.vars,"mean"]
r.lower <- mcmc.summary[var.plot.vars,"25%"]
r.upper <- mcmc.summary[var.plot.vars,"75%"]

var.pattern <- "(S\\[.*?\\])"
var.matches <- grepl(var.pattern, dimnames(mcmc.summary)[[1]])
var.plot.vars <- dimnames(mcmc.summary)[[1]][var.matches]
s.mean <- mcmc.summary[var.plot.vars,"mean"]
s.lower <- mcmc.summary[var.plot.vars,"25%"]
s.upper <- mcmc.summary[var.plot.vars,"75%"]


plot(spn.yrs[1:35],r.mean[7:41]/s.mean[1:35],
      xlab="Year",ylab = "Raw R/S (Modelled)",
     bty="n",type="o", pch=19,ylim=c(0,6))

title( main= "Modelled R/S")


# ricker fit
ln.a.par <- mcmc.summary["lnalpha.c","mean"]
b.par <- mcmc.summary["beta","mean"]

ricker.spn <- c(1,seq(500,100000,by=500))
ricker.logrps <- ln.a.par - b.par * ricker.spn 
ricker.rec <-  exp(log(ricker.spn) + ricker.logrps )


# log r/s vs s



plot(s.mean[1:35],log(r.mean[7:41]/s.mean[1:35]),
     xlab="Spn",ylab = "log (R/S) (Modelled)",
     bty="n",type="p", pch=19,ylim=c(0,3),xlim=c(0,100000))

segments(s.mean[1:35],log(r.lower[7:41]/s.lower[1:35]),s.mean[1:35],log(r.upper[7:41]/s.upper[1:35]),col="lightblue")
segments(s.lower[1:35],log(r.mean[7:41]/s.mean[1:35]),s.upper[1:35],log(r.mean[7:41]/s.mean[1:35]),col="lightblue")


points(s.mean[1:35],log(r.mean[7:41]/s.mean[1:35]),col="darkblue", pch=19)

text(0,0 , labels = "Note: vertical whiskers are preliminary approximations!", 
     col="red", adj=0)

lines(ricker.spn,ricker.logrps,lwd=2,col="red")


##########################################################
#   R vs S
options(scipen=10^5)
plot(s.mean[1:35],r.mean[7:41],bty="n",
     xlab = "Modelled Spn", ylab= "Modelled Rec",
     ylim= c(0,max(r.upper)),xlim= c(0,max(s.upper)),
     col="darkblue", pch=19
     )



segments(s.mean[1:35],r.lower[7:41],s.mean[1:35],r.upper[7:41],col="lightblue")
segments(s.lower[1:35],r.mean[7:41],s.upper[1:35],r.mean[7:41],col="lightblue")

points(s.mean[1:35],r.mean[7:41],col="darkblue", pch=19)

lines(ricker.spn,ricker.rec,col="red",lwd=2)



title(main= "Modelled R, S, and Ricker Fit")



##############
# Harvest below



var.pattern <- "(h.below\\[.*?\\])"
var.matches <- grepl(var.pattern, dimnames(mcmc.summary)[[1]])
var.plot.vars <- dimnames(mcmc.summary)[[1]][var.matches]
plot(1:5,1:5,type="n",xlab= "Year", ylab= "Adult Sockeye (> x cm?)",
     xlim=c(min(spn.yrs),max(spn.yrs)),ylim=c(0,200000),bty="n",axes=FALSE)
axis(2)
axis(1,at = pretty(spn.yrs)  )
abline(v=1984,col="lightgrey",lty=1)


# input (hbelow)
points(spn.yrs,dat$hbelow,pch=19,col="red",cex=0.8)
hbelow.ci <- dat$hbelow*dat$cv.hb*2
segments(spn.yrs, dat$hbelow - hbelow.ci, spn.yrs, dat$hbelow + hbelow.ci,col="red")


# modelled "true" values
lines(spn.yrs,mcmc.summary[var.plot.vars,"mean"],lwd=2,col="darkblue",lty=1)
lines(spn.yrs,mcmc.summary[var.plot.vars,"25%"],lwd=1,col="darkblue",lty=2)
lines(spn.yrs,mcmc.summary[var.plot.vars,"75%"],lwd=1,col="darkblue",lty=2)

legend("bottomright",legend=c("Modelled Mean","Modelled 25%-75%","Est +- 2SD"),
       lty=c(1,2,1),col=c("darkblue","darkblue","red"), pch=c(NA, NA,19),bty="n")


title(main = "Pattern in Modelled and Estimated Harvest Below")






##############
# Harvest  above



var.pattern <- "(h.above\\[.*?\\])"
var.matches <- grepl(var.pattern, dimnames(mcmc.summary)[[1]])
var.plot.vars <- dimnames(mcmc.summary)[[1]][var.matches]
plot(1:5,1:5,type="n",xlab= "Year", ylab= "Adult Sockeye (> x cm?)",
     xlim=c(min(spn.yrs),max(spn.yrs)),ylim=c(0,60000),bty="n",axes=FALSE)
axis(2)
axis(1,at = pretty(spn.yrs)  )
abline(v=1984,col="lightgrey",lty=1)


# input (habove)
points(spn.yrs,dat$habove,pch=19,col="red",cex=0.8)
habove.ci <- dat$habove*dat$cv.ha*2
segments(spn.yrs, dat$habove - hbelow.ci, spn.yrs, dat$habove + habove.ci,col="red")


# modelled "true" values
lines(spn.yrs,mcmc.summary[var.plot.vars,"mean"],lwd=2,col="darkblue",lty=1)
lines(spn.yrs,mcmc.summary[var.plot.vars,"25%"],lwd=1,col="darkblue",lty=2)
lines(spn.yrs,mcmc.summary[var.plot.vars,"75%"],lwd=1,col="darkblue",lty=2)

legend("bottomright",legend=c("Modelled Mean","Modelled 25%-75%","Est +- 2SD"),
       lty=c(1,2,1),col=c("darkblue","darkblue","red"), pch=c(NA, NA,19),bty="n")


title(main = "Pattern in Modelled and Estimated Harvest Above")



dev.off()




