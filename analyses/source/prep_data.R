treed<-treed[,1:21]
#Summarize number of trees in each plot to get pre-treatment tree density
dens<-aggregate(treed$TAG,by=list(treed$SITE_ID,treed$BASIN,treed$PLOT),length)
colnames(dens)<-c("SITE_ID","BASIN","PLOT","predens")

#merge density with other plot data
plotd2<-left_join(plotd,dens)

#merge treatment data with other plot data
colnames(trtd)<-c("BASIN","STAND_ID", "STAND_TYPE","TREATED","COM.THIN","PCT","YR_TREATED","AGE_2019")
plotd3<-left_join(plotd2,trtd)
#hmm...the treatment plot data is missing some stands and having trouble merging. 
# looks like basins N2, S3, and C1 were treated
#For now, let's assume that the treatment lead to removal of 30% of the density
plotd2$trt<-0
plotd2$trt[plotd2$BASIN=="C1"|plotd2$BASIN=="N2"|plotd2$BASIN=="S3"]<-0.3*plotd2$predens[plotd2$BASIN=="C1"|plotd2$BASIN=="N2"|plotd2$BASIN=="S3"]

#add other possble explanatory variables (cwd, dbh, height, crown)



#standardize predictors
plotd2$predens.z<-(plotd2$predens-mean(plotd2$predens))/sd(plotd2$predens)
plotd2$age2006.z<-(plotd2$AGE_BH_2006-mean(plotd2$AGE_BH_2006, na.rm=TRUE))/sd(plotd2$AGE_BH_2006, na.rm=TRUE)
plotd2$trt.z<-(plotd2$trt-mean(plotd2$trt))/sd(plotd2$trt)


#set up data:
x<-subset(plotd2, select=c(BLOCK,predens.z, age2006.z, trt.z))#AGE_BH_2006 is related to AGE but is numeric, with some NAS
colnames(x)[1]<-c("block")         

#remove NAs
x<-x[-which(is.na(x$age2006.z)),]
