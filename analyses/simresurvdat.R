#Simulating plot resurvey data to figure out how many plots we need to resurvey 
#Started November 8, 2019 by ailene ettinger
#ailene.ettinger@tnc.org
# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# libraries
library(dplyr)

#set working directory
setwd("~/GitHub/ellsworth")

#read in tree data and plot data
treed<-read.csv("data/LIVETREES_CLEAN_04282008.csv", header=TRUE)
treed<-treed[,1:21]
plotd<-read.csv("data/PLOT_CLEAN_11062008.csv", header=TRUE)

#Summarize number of trees in each plot to get pre-treatment tree density
dens<-aggregate(treed$TAG,by=list(treed$SITE_ID,treed$BASIN,treed$PLOT),length)
colnames(dens)<-c("SITE_ID","BASIN","PLOT","predens")

#merge density with other plot data
plotd2<-full_join(plotd,dens)

#set expected effect sizes
#start by assuming that post density depends only on pre-density and age
dens.b<-1#effect of pre-density on post-density
age.b<- -.5#older stands have lower post-density
#want to add in treatment effect and account for variation by location as well at some point...
#trt.eff<- -.5#trt.eff should be numeric- like an amount of wood removed. should get this from Kyle-

#use the effect sizes and predictors plus error to generate the y variable
betas<-c(dens.b,age.b)
x<-subset(plotd2, select=c(predens, AGE_BH_2006))#AGE_BH_2006 is related to AGE but is numeric, with some NAS
          
ypred<- as.matrix(x)%*%betas + rnorm(nrow(x),0,1)
plot(x$predens,y)
alldatmod<-lm(ypred~x$predens+x$AGE_BH_2006)
coef(alldatmod)

#now lets write a for loop that uses different sample sizes to figure out how many plots are needed to correctly recover the effects
fulldat<-cbind(ypred,x)
ns<-rep(c(5,25,50,75,100,125), times=10)

allns<-c()
for(i in 1:length(ns)){
  subsdat<-sample_n(fulldat, ns[i])
  fit<-lm(ypred~predens+AGE_BH_2006,data=subsdat)
  fit.sum<-c(ns[i],coef(fit),confint(fit)[1,],confint(fit)[2,],confint(fit)[3,])
  allns<-rbind(allns,fit.sum)
}
allns<-as.data.frame(allns)
colnames(allns)<-c("n","int","predens.b","AGE.b","int.lc","int.uc","predens.b.lc","predens.b.uc","AGE.b.lc","AGE.b.uc")

windows(height=5,width=8)
par(mfrow=c(1,2))
plot(allns$n,allns$predens.b,main="pre-density",ylim=c(0,2))
for(i in 1:dim(allns)[1]){
  arrows(allns$n[i],allns$predens.b.lc[i],allns$n[i],allns$predens.b.uc[i],length=0.1,code = 0, angle = 90, lwd=3,col= alpha("gray",0.1))
}
abline(h=dens.b, lwd=2, col="red")
plot(allns$n,allns$AGE.b,main="age",ylim=c(-1,0))
for(i in 1:dim(allns)[1]){
  arrows(allns$n[i],allns$AGE.b.lc[i],allns$n[i],allns$AGE.b.uc[i],length=0.1,code = 0, angle = 90, lwd=3,col= alpha("gray",0.1))
}
abline(h=age.b, lwd=2, col="red")
