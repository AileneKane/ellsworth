#Simulating plot resurvey data to figure out how many plots we need to resurvey 
#Started November 8, 2019 by ailene ettinger
#ailene.ettinger@tnc.org
# housekeeping
#helpful website for simulatig: https://aosmith.rbind.io/2018/04/23/simulate-simulate-part-2/#simulate-simulate-dance-to-the-music
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# libraries
library(dplyr)
library(lme4)
library(scales)
#set working directory
setwd("~/GitHub/ellsworth")

#1. read in tree dat, plot data, and treatment data
treed<-read.csv("data/LIVETREES_CLEAN_04282008.csv", header=TRUE)
plotd<-read.csv("data/PLOT_CLEAN_11062008.csv", header=TRUE)
trtd<-read.csv("data/Ellsworth_stands_treatment_data.csv", header=TRUE)
#2. put the data together and standardize some columns for model fitting
source("analyses/source/prep_data.R")

#3. fit models of pre-treatment data to get estimates of block-level and plot-level varianace

#4. set expected effect sizes for simulating data
dens.b = 1#effect of pre-density 
age.b = -.5#age effect (older stands have lower post-density?)
trt.b = -1 #trt is amount removed (0,3 x predens)
sigma.bl = 1#change to estimated block-level sigma from 2006 data
sigma = .01#change to estimated sigma from 2006 data using print(VarCorr(study),comp="Variance")
b0 = 0 # change to something else?

#use the effect sizes and predictors plus error to generate the y variable
#betas<-c(dens.b,age.b)


#ypred<- as.matrix(x)%*%betas + rnorm(nrow(x),0,sigma)
#plot(x$predens,ypred)
#alldatmod<-lm(ypred~x$predens+x$AGE_BH_2006)
#coef(alldatmod)

block = x$block
nblock = length(unique(x$block))
nplot = 28
blockeff = rep(rnorm(nblock, 0, sigma.bl), each = nplot)
ploteff = rnorm(nblock*nplot, 0, sigma)

(ypred = b0 + dens.b*x$predens.z + age.b*x$age2006.z + trt.b*x$trt.z+ blockeff + ploteff)

lmer(ypred ~ predens.z + age2006.z + trt.z+ (1|block), data=x )


#now lets write a for loop that uses different sample sizes to figure out how many plots are needed to correctly recover the effects
fulldat<-cbind(ypred,x)
nplots<-rep(c(5,10,15,20,25), times=10)

allplots<-c()
for(i in 1:length(nplots)){
  subsdatc<-sample_n(fulldat[fulldat$block=="C",], nplots[i])
  subsdatn<-sample_n(fulldat[fulldat$block=="N",], nplots[i])
  subsdats<-sample_n(fulldat[fulldat$block=="S",], nplots[i])
  subsdat<-sbind(subsdatc,subsdatn,subsdats)
  fit<-lmer(ypred ~ predens.z + age2006.z + trt.z+ (1|block), data=subsdat)
  fit.sum<-c(nplots[i],fixef(fit),confint(fit)[3,],confint(fit)[4,],confint(fit)[5,],confint(fit)[6,])
  allplots<-rbind(allplots,fit.sum)
}
allplots<-as.data.frame(allplots)
colnames(allplots)<-c("n","int","predens.b","age.b","trt.b","int.lc","int.uc","predens.b.lc","predens.b.uc","age.b.lc","age.b.uc","trt.b.lc","trt.b.uc")

pdf(paste("analyses/figures/plotnums_var",sigma,".pdf",sep=""),height=5,width=8)
quartz()
par(mfrow=c(1,3))
plot(allplots$n,allplots$predens.b,main="pre-density",ylim=c(0,2))
for(i in 1:dim(allplots)[1]){
  arrows(allplots$n[i],allplots$predens.b.lc[i],allplots$n[i],allplots$predens.b.uc[i],length=0.1,code = 0, angle = 90, lwd=3,col= alpha("gray",0.1))
}
abline(h=dens.b, lwd=2, col="red")
plot(allplots$n,allplots$age.b,main="age",ylim=c(-1,0))
for(i in 1:dim(allplots)[1]){
  arrows(allplots$n[i],allplots$age.b.lc[i],allplots$n[i],allplots$age.b.uc[i],length=0.1,code = 0, angle = 90, lwd=3,col= alpha("gray",0.1))
}
abline(h=age.b, lwd=2, col="red")
plot(allplots$n,allplots$trt.b,main="trt",ylim=c(-1.5,0))
for(i in 1:dim(allplots)[1]){
  arrows(allplots$n[i],allplots$trt.b.lc[i],allplots$n[i],allplots$trt.b.uc[i],length=0.1,code = 0, angle = 90, lwd=3,col= alpha("gray",0.1))
}
abline(h=trt.b, lwd=2, col="red")

dev.off()


###not used
twolevel_fun = function(nstand = 5, nplot = 4, mu = 10, sigma_s = 2, sigma = 1) {
  standeff = rep( rnorm(nstand, 0, sigma_s), each = nplot)
  stand = rep(LETTERS[1:nstand], each = nplot)
  ploteff = rnorm(nstand*nplot, 0, sigma)
  resp = mu + standeff + ploteff
  dat = data.frame(stand, resp)
  lmer(resp ~ 1 + (1|stand), data = dat)
}
twolevel_fun()


nstand = 5
nplot = 4
b0 = -1
b1 = .005
b2 = .1
sds = 2
sd = 1

set.seed(16)
stand = rep(LETTERS[1:nstand], each = nplot)
standeff = rep( rnorm(nstand, 0, sds), each = nplot)
ploteff = rnorm(nstand*nplot, 0, sd)

( elevation = rep( runif(nstand, 1000, 1500), each = nplot) )
( slope = runif(nstand*nplot, 2, 75) )
( resp2 = b0 + b1*elevation + b2*slope + standeff + ploteff )

lmer(resp2 ~ elevation + slope + (1|stand) )

