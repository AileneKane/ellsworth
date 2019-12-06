#first just see how much variation there is by basin and treatment
densmod<-lmer(predens~age2006.z + (1|BLOCK)  + (1|STAND.TYPE), data=plotd2)
summary(densmod)
densd<-VarCorr(densmod,comp="Variance")[1]

##how do i extraxt block level variance? want this for my data simulations!
htmod<-lmer(HT~ AGE_BH_2006+(1|BLOCK), data=treed2)
htsd<-VarCorr(htmod,comp="Variance")

dbhmod<-lmer(DBH~ AGE_BH_2006+(1|BLOCK), data=treed2)
dbhsd<-VarCorr(dbhmod,comp="Variance")

crownmod<-lmer(CROWN~ AGE_BH_2006+(1|BLOCK), data=treed2)
crownsd<-VarCorr(crownmod,comp="Variance")

summary(densmod)#positive effect of age on density, before treatment, there is much higher variance by block than by TRT!
summary(htmod)#positive effect of age on ht, higher variance by TRT...
summary(dbhmod)#positive effect of age on dbh, similar variance by TRT and Block
summary(crownmod)#positive effect of age on crown, lsightly higher variance in TRt than block
#

