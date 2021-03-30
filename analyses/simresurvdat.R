#Simulating plot resurvey data to figure out how many plots we need to resurvey 
#Started November 8, 2019 by ailene ettinger
#ailene.ettinger@tnc.org
# housekeeping
#tree size (dbh, height)
# vertical structure: crown class (4,5,6 might be a good measure of this)
# CWD- abundance
#could choose an understory indicator speices or two- like rattle species
#assume that THIN removed 30% of trees, no change in control and road; 
#for response variable look at dbh, height, and densitiy (if time look at vertical structure)

#do this by the tuesday before thanksgiving

#for next time look at age, density, 
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# libraries
library(dplyr)
library(RColorBrewer)
library(scales)
library(lme4)
#set working directory
setwd("~/GitHub/ellsworth")

#read in tree data and plot data
treed<-read.csv("data/LIVETREES_CLEAN_04282008.csv", header=TRUE)
treed<-treed[,1:21]
plotd<-read.csv("data/PLOT_CLEAN_11062008.csv", header=TRUE)

treed2$CROWN<-as.numeric(treed2$CROWN)
plotd2$TRT<-as.factor(plotd2$TRT)
plotd2$BLOCK<-as.factor(plotd2$BLOCK)
plotd2$predens<-as.numeric(plotd2$predens)
treed2$HT<-as.numeric(treed2$HT)
treed2$DBH<-as.numeric(treed2$DBH)
treed2$CROWN<-as.numeric(treed2$CROWN)

#Summarize number of trees in each plot to get pre-treatment tree density
dens<-aggregate(treed$TAG,by=list(treed$SITE_ID,treed$BASIN,treed$PLOT),length)
colnames(dens)<-c("SITE_ID","BASIN","PLOT","predens")

dbh.mn<-aggregate(treed$DBH,by=list(treed$SITE_ID,treed$BASIN,treed$PLOT),mean, na.rm=TRUE)
colnames(dbh.mn)<-c("SITE_ID","BASIN","PLOT","dbh.mn")

ht.mn<-aggregate(treed$HT,by=list(treed$SITE_ID,treed$BASIN,treed$PLOT),mean, na.rm=TRUE)
colnames(ht.mn)<-c("SITE_ID","BASIN","PLOT","dbh.mn")
crown.mn<-aggregate(treed$CROWN,by=list(treed$SITE_ID,treed$BASIN,treed$PLOT),mean, na.rm=TRUE)
colnames(crown.mn)<-c("SITE_ID","BASIN","PLOT","crown.mn")

#merge density with other plot data
plotd2<-full_join(plotd,dens)#plot-level data
plotd2<-full_join(plotd2,dbh.mn)#plot-level data
plotd2<-left_join(plotd2,ht.mn)#plot-level data
plotd2<-full_join(plotd2,crown.mn)#plot-level data

treed2<-left_join(treed,plotd2)#individal tree data
dim(treed2)

#first just see how much variation there is by basin and treatment
densmod<-lmer(predens~AGE_BH_2006 + (1|BLOCK), data=plotd2)
densvar<-##how do i extraxt block level variance? want this for my data simulations!
htmod<-lmer(HT~ AGE_BH_2006+(1|BLOCK), data=treed2)

dbhmod<-lmer(DBH~ AGE_BH_2006+(1|BLOCK), data=treed2)
crownmod<-lmer(CROWN~ AGE_BH_2006+(1|BLOCK), data=treed2)

summary(densmod)#positive effect of age on density, before treatment, there is much higher variance by block than by TRT!
summary(htmod)#positive effect of age on ht, higher variance by TRT...
summary(dbhmod)#positive effect of age on dbh, similar variance by TRT and Block
summary(crownmod)#positive effect of age on crown, lsightly higher variance in TRt than block
#

colors<-c("darkblue","lightgreen","goldenrod")
symbs<-c(21,22,23)
blocks<-unique(plotd2$BLOCK)
treats<-unique(plotd2$TRT)
pdf(paste("analyses/figures/pretrt_agevsdens.pdf",sep=""),height=6,width=6)

#windows()
plot(plotd2$AGE_BH_2006,plotd2$predens, pch=symbs[as.numeric(as.factor(plotd2$TRT))], bg=colors[as.numeric(as.factor(plotd2$BLOCK))], xlab="Age, 2006 (years)", ylab= "Density, 2006 (trees/plot)")
legend("topright", legend=c("C","N","S","THIN","CON","ROAD"),pch=c(21,21,21,symbs),pt.bg=c(colors,"lightgreen","lightgreen","lightgreen") )
dev.off()

pdf(paste("analyses/figures/pretrt_agevsdbh.pdf",sep=""),height=6,width=6)
#windows()
plot(plotd2$AGE_BH_2006,plotd2$dbh.mn, pch=symbs[as.numeric(as.factor(plotd2$TRT))], bg=colors[as.numeric(as.factor(plotd2$BLOCK))], xlab="Age, 2006 (years)", ylab= "Mean DBH")
legend("topright", legend=c("C","N","S","THIN","CON","ROAD"),pch=c(21,21,21,symbs),pt.bg=c(colors,"lightgreen","lightgreen","lightgreen") )
dev.off()

pdf(paste("analyses/figures/pretrt_agevsht.pdf",sep=""),height=6,width=6)
#windows()
plot(plotd2$AGE_BH_2006,plotd2$HT, pch=symbs[as.numeric(as.factor(plotd2$TRT))], bg=colors[as.numeric(as.factor(plotd2$BLOCK))], xlab="Age, 2006 (years)", ylab= "Mean DBH")
legend("topright", legend=c("C","N","S","THIN","CON","ROAD"),pch=c(21,21,21,symbs),pt.bg=c(colors,"lightgreen","lightgreen","lightgreen") )
dev.off()

pdf(paste("analyses/figures/pretrt_agevscrown.pdf",sep=""),height=6,width=6)
#windows()
plot(plotd2$AGE_BH_2006,plotd2$CROWN, pch=symbs[as.numeric(as.factor(plotd2$TRT))], bg=colors[as.numeric(as.factor(plotd2$BLOCK))], xlab="Age, 2006 (years)", ylab= "Mean DBH")
legend("topright", legend=c("C","N","S","THIN","CON","ROAD"),pch=c(21,21,21,symbs),pt.bg=c(colors,"lightgreen","lightgreen","lightgreen") )
dev.off()

#set expected effect sizes
#start by assuming that post density depends only on pre-density and age
dens.b<-1#effect of pre-density on post-density
age.b<- -.5#older stands have lower post-density
sigma<-10
#want to add in treatment effect and account for variation by location as well at some point...
#trt.eff<- -.5#trt.eff should be numeric- like an amount of wood removed. should get this from Kyle-

#use the effect sizes and predictors plus error to generate the y variable
betas<-c(dens.b,age.b)
x<-subset(plotd2, select=c(predens, AGE_BH_2006))#AGE_BH_2006 is related to AGE but is numeric, with some NAS
          
ypred<- as.matrix(x)%*%betas + rnorm(nrow(x),0,sigma)
plot(x$predens,ypred)

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

pdf(paste("analyses/figures/plotnums_var",sigma,".pdf",sep=""),height=5,width=8)
#windows()
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
dev.off()



#to code this for new data
lmer(BA~BASIN + AGE_BH_2006+ TRT+ AGE_BH:TRT + (1|SITE_ID), data=plotd2)


