treed<-treed[,1:21]
#Summarize number of trees in each plot to get pre-treatment tree density
dens<-aggregate(treed$TAG,by=list(treed$SITE_ID,treed$BASIN,treed$PLOT),length)
colnames(dens)<-c("SITE_ID","BASIN","PLOT","predens")

#Summarize number of trees in each plot to get pre-treatment tree density
dens<-aggregate(treed$TAG,by=list(treed$SITE_ID,treed$BASIN,treed$PLOT),length)
colnames(dens)<-c("SITE_ID","BASIN","PLOT","predens")

dbh.mn<-aggregate(treed$DBH,by=list(treed$SITE_ID,treed$BASIN,treed$PLOT),mean, na.rm=TRUE)
colnames(dbh.mn)<-c("SITE_ID","BASIN","PLOT","dbh.mn")

ht.mn<-aggregate(treed$HT,by=list(treed$SITE_ID,treed$BASIN,treed$PLOT),mean, na.rm=TRUE)
colnames(ht.mn)<-c("SITE_ID","BASIN","PLOT","ht.mn")
crown.mn<-aggregate(treed$CROWN,by=list(treed$SITE_ID,treed$BASIN,treed$PLOT),mean, na.rm=TRUE)
colnames(crown.mn)<-c("SITE_ID","BASIN","PLOT","crown.mn")


#merge density with other plot data
plotd2<-full_join(plotd,dens)#plot-level data

plotd2<-full_join(plotd2,dbh.mn)#plot-level data
plotd2<-left_join(plotd2,ht.mn)#plot-level data
plotd2<-full_join(plotd2,crown.mn)#plot-level data
plotd2$stand.code<-substr(plotd2$STAND.TYPE,1,2)

treed2<-left_join(treed,plotd2, by=c("BASIN","PLOT"))#individal tree data
#merge treatment data with other plot data
colnames(trtd)<-c("BASIN","STAND_ID", "STAND_TYPE","TREATED","COM.THIN","PCT","YR_TREATED","AGE_2019")
plotd3<-left_join(plotd2,trtd)
#hmm...the treatment plot data is missing some stands and having trouble merging. keep it as a separate file for now
# looks like basins N2, S3, and C1 were treated
#For now, let's assume that the treatment lead to removal of 30% of the density
plotd2$trt<-0
plotd2$trt[plotd2$BASIN=="C1"|plotd2$BASIN=="N2"|plotd2$BASIN=="S3"]<-0.3

#add other possble explanatory variables (cwd, dbh, height, crown)
treed2$CROWN<-as.numeric(treed2$CROWN)
plotd2$TRT<-as.factor(plotd2$TRT)
plotd2$BLOCK<-as.factor(plotd2$BLOCK)
plotd2$predens<-as.numeric(plotd2$predens)
treed2$HT<-as.numeric(treed2$HT)
treed2$DBH<-as.numeric(treed2$DBH)
treed2$CROWN<-as.numeric(treed2$CROWN)

#standardize predictors
plotd2$predens.z<-(plotd2$predens-mean(plotd2$predens))/sd(plotd2$predens)
plotd2$age2006.z<-(plotd2$AGE_BH_2006-mean(plotd2$AGE_BH_2006, na.rm=TRUE))/sd(plotd2$AGE_BH_2006, na.rm=TRUE)

#separate data by stand type
xwh<-plotd2[plotd2$stand.code=="WH",]
xdf<-plotd2[plotd2$stand.code=="DF",]
x2wh<-treed2[treed2$stand.code=="WH",]
x2df<-treed2[treed2$stand.code=="DF",]


