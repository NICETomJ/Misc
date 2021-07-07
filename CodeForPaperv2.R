# Clean code for paper

## Load Required Packages

library("data.table")
library("ggplot2")
library("plotly")
library("dplyr")

# This File has been created by GR using previous data. Due to GDPR issues te 2018 values wil need to be changed
AllYearIQRs<-fread("Q:/Guidelines/In development/Diabetes/6. HE working/TJ Working/Misc/Reference Costs/Completeschedule.csv")

##SSO will contain spreadsheet output which removes issues around GDPR censored values
#this is ripped from https://www.england.nhs.uk/national-cost-collection/#ncc1819

ScheduleSpreadsheetOutput<-fread("Q:/Guidelines/In development/Diabetes/6. HE working/TJ Working/Misc/Reference Costs/2018ScheduleCOMPLETE.csv")

SSO<-ScheduleSpreadsheetOutput[,.(`Department Code`,`Currency Code`,`FCE Vol`,`Average Unit Cost`,`No. Data Submissions`)] # select required columns for merge
SSO[,Year:=2018]
AllYearIQRs<-merge(AllYearIQRs,SSO,by=c("Year","Department Code","Currency Code"),all.x=T)

# now if average unit cost (from spreadsheet) exists then transplant it into mean column
AllYearIQRs[!(is.na(`Average Unit Cost`)),mean:=`Average Unit Cost`]
# same for FCEs
AllYearIQRs[!(is.na(`FCE Vol`)),FCEs:=`FCE Vol`]
# and for submissions (if not >8 FCEs these were previously removed.)
AllYearIQRs[!(is.na(`No. Data Submissions`)),CSubmissions:=`No. Data Submissions`]




## 2018 schedule of costs does not include XS bed days, therefore add these onto previous years to gie a consistent picture
XS<-AllYearIQRs[grepl("XS",`Department Code`),.(Year,`Department Code`,`Currency Code`,`Service code`,FCEs,mean)]
setnames(XS,c("Year","Department Code","Currency Code","Service code","FCEsXS","meanXS"))
XS[,`Department Code`:=gsub("_XS","",`Department Code`)]

AllYearIQRs<-merge(AllYearIQRs,XS,by=c("Year","Department Code","Currency Code","Service code"),all.x=T)
AllYearIQRs[is.na(meanXS),meanXS:=0]
AllYearIQRs[is.na(FCEsXS),FCEsXS:=0]
AllYearIQRs[,meanBACKUP:=mean] # this will store previous years' means pre adding XS
AllYearIQRs[,mean:=mean+meanXS*FCEsXS/FCEs]
AllYearIQRs[,UQ:=UQ+meanXS*FCEsXS/FCEs]
AllYearIQRs[,LQ:=LQ+meanXS*FCEsXS/FCEs]

##An important note re the above is that to be truly confident of consistency it may be necessary to update the RAW values in the data generation process
# these raw values no longer seem to be available
## to add on the XS bed days and THEN fit a gamma distribution. Here I have simply added on the average XS bed day cost to the pre-calculated gamma distributions


AllYearIQRs[,SE:=((UQ-LQ)/2/qnorm(0.75))/sqrt(CSubmissions)]
AllYearIQRs[,GammaAlpha:=mean^2/SE^2]
AllYearIQRs[,GammaBeta:=SE^2/mean]
AllYearIQRs[,ConfIntLow:=qgamma(0.025,GammaAlpha, 1/GammaBeta)]
AllYearIQRs[,ConfIntHigh:=qgamma(0.975,GammaAlpha, 1/GammaBeta)]
AllYearIQRs[,MeanGamma:=GammaAlpha*GammaBeta]

## gamma distributions now created

# Next section of code creates the fixed 10%se CIs (0.196) and generates some summary stats

# Prepare data for input ####
YearToPredict<-2018
# Firstly Filter the data by only selecting Currency Codes/ Department Code combinations where there were over 100 FCEs in 2017
UseableCodes<-AllYearIQRs[FCEs>100 & CSubmissions>10 & Year ==YearToPredict-1 & `Department Code`%in% c("EL","NEL","DC","NES") ,.(`Currency Code`,`Department Code`)]
# also remove 2012 data due to different structure
# In the paper only look at EL,NEL,DE and NES


AnalysisInputFM<-AllYearIQRs[`Department Code`%in% c("EL","NEL","DC","NES") & Year>2012]
AnalysisInputFM[,FCELogFloor:=floor(log(`FCE Vol`))]

AnalysisInput<-merge(UseableCodes,AnalysisInputFM,all.x=T,all.y=F,by=c("Currency Code","Department Code"))

### simplest method is to set SE to a fixed % of the previous year's mean then set up confidence interval as 1.96*SE
AnalysisInput[,Simple10pcCIupper:=mean*1.196]
AnalysisInput[,Simple10pcCIlower:=mean*0.804]

# now need to to test coverage and accuracy

Get2017<-AnalysisInput[Year==YearToPredict-1,.(`Department Code`,`Currency Code`,Simple10pcCIlower,Simple10pcCIupper,mean,FCEs,CSubmissions)] # select the year before
setnames(Get2017,c("Department Code","Currency Code","ConfIntLow2017","ConfIntHigh2017","MeanSimplePrediction","PYFCEs","PYSubs"))


SimpleData<-merge(AnalysisInput[Year==YearToPredict],Get2017,by=c("Department Code","Currency Code"),all.x=F)

SimpleData[,InSimpleInterval:=0]
SimpleData[mean<ConfIntHigh2017 & mean>ConfIntLow2017,InSimpleInterval:=1]
SimpleSum<-SimpleData[,.(Av=mean(InSimpleInterval)),by=.(`Department Code`)]
SimpleData[,Dispersion:=abs(1-(MeanSimplePrediction/mean))]
SimpleData[,DispSQUARED:=Dispersion^2]
SimpleData[,PropCIWidth:=(ConfIntLow2017-ConfIntHigh2017)/MeanSimplePrediction]
SimpleData[,StandardisedActual:=mean/MeanSimplePrediction]
SimpleSum<-SimpleData[Dispersion<1 & submissions>10,.(AvIn=mean(InSimpleInterval),AvDist=mean(Dispersion),MSE=mean(DispSQUARED,na.rm=T)),by=.(`Department Code`)]
SimpleSum2<-SimpleData[,.(AvIn=mean(InSimpleInterval),AvDist=mean(Dispersion),MSE=mean(DispSQUARED,na.rm=T),Vol=.N),by=.(`Department Code`,FCELogFloor)]


ggplot(SimpleData,aes(StandardisedActual,log(FCEs)))+geom_point()+geom_vline(xintercept=0.804)+geom_vline(xintercept=1.196)
ggplot(SimpleData,aes(StandardisedActual,log(PYFCEs)))+geom_point()+geom_vline(xintercept=0.804)+geom_vline(xintercept=1.196)

ggplot(SimpleData,aes(CSubmissions,PYSubs))+geom_point()


ggplot(SimpleData[StandardisedActual<2],aes(StandardisedActual,submissions))+geom_point()+geom_vline(xintercept=0.804)+geom_vline(xintercept=1.196)
ggplot(SimpleData[StandardisedActual<2],aes(StandardisedActual,PYSubs))+geom_point()+geom_vline(xintercept=0.804)+geom_vline(xintercept=1.196)
ggplot(SimpleData[StandardisedActual<2],aes(StandardisedActual,log(PYFCEs)))+geom_point()+geom_vline(xintercept=0.804)+geom_vline(xintercept=1.196)


ggplot(SimpleData[StandardisedActual<2],aes(StandardisedActual,log(mean)))+geom_point()+geom_vline(xintercept=0.804)+geom_vline(xintercept=1.196)

## see how accuracy improves based on previous year FCEs

SimpleOrder<-SimpleData[order(`Department Code`,-PYFCEs)]
SimpleOrder[,VolRank:=rowid(`Department Code`)]
SimpleOrder[,ByCurrencyCodeAccuracy:=cummean(InSimpleInterval),by=.(`Department Code`)]
SimpleOrder[,ByCurrencyCodeAccuracy:=cummean(InSimpleInterval),by=.(`Department Code`)]


ggplot(SimpleOrder,aes(log(PYFCEs),ByCurrencyCodeAccuracy,colour=`Department Code`))+geom_line()
ggplot(SimpleOrder,aes(-VolRank,ByCurrencyCodeAccuracy,colour=`Department Code`))+geom_line()+ggtitle(paste0("Predicting ",YearToPredict))+ylim(0,1)

ggplot(SimpleOrder,aes(log(FCEs),log(PYFCEs),colour=InSimpleInterval))+geom_point()
ggplot(SimpleOrder,aes(log(CSubmissions),log(PYSubs),colour=InSimpleInterval))+geom_point()+xlim(0,6)+ylim(0,6)

##### it shoudl now be possible to do something similar for the gamma dist.


Get2017<-AnalysisInput[Year==YearToPredict-1,.(`Department Code`,`Currency Code`,ConfIntLow,ConfIntHigh,MeanGamma,FCEs,CSubmissions)]
setnames(Get2017,c("Department Code","Currency Code","ConfIntLow2017","ConfIntHigh2017","MeanPrediction2017","PYFCEs","PYSubs"))

GammaData<-merge(AnalysisInput[Year==YearToPredict],Get2017,by=c("Department Code","Currency Code"),all.x=F)


GammaData[,InGammaInterval:=0]
GammaData[mean<ConfIntHigh2017 & mean>ConfIntLow2017,InGammaInterval:=1]
GammaSum<-GammaData[,.(Av=mean(InGammaInterval)),by=.(`Department Code`)]
GammaData[,Dispersion:=abs(1-(MeanPrediction2017/mean))]
GammaData[,DispSQUARED:=Dispersion^2]
GammaData[,PropCIWidth:=(-(ConfIntLow2017-ConfIntHigh2017))/MeanPrediction2017]
GammaSum<-GammaData[Dispersion<1 & submissions>10,.(AvIn=mean(InGammaInterval),AvDist=mean(Dispersion,na.rm=T),MSE=mean(DispSQUARED,na.rm=T)),by=.(`Department Code`)]



## now need to calculate accuracy measure - values are not quite normally distributed - so Accuracy will be determined by how many upper confidence intervals away from the predicted mean,
# the true values are
GammaData[mean>MeanPrediction2017,Accuracy:=(mean-MeanPrediction2017)/(ConfIntHigh2017-MeanPrediction2017)]
GammaData[mean<MeanPrediction2017,Accuracy:=-(mean-MeanPrediction2017)/(ConfIntLow2017-MeanPrediction2017)]
GammaData[,StandardisedActual:=mean/MeanPrediction2017]
GammaData[,InInt:="No"]
GammaData[InGammaInterval==1,InInt:="Yes"]


GammaOrder<-GammaData[order(`Department Code`,-PYFCEs)]
GammaOrder[,VolRank:=rowid(`Department Code`)]
GammaOrder[,ByCurrencyCodeAccuracy:=cummean(InGammaInterval),by=.(`Department Code`)]
GammaOrder[,ByCurrencyCodeAccuracy:=cummean(InGammaInterval),by=.(`Department Code`)]


#ggplot(GammaOrder,aes(log(PYFCEs),ByCurrencyCodeAccuracy,colour=`Department Code`))+geom_line()
ggplot(GammaOrder,aes(-VolRank,ByCurrencyCodeAccuracy,colour=`Department Code`))+geom_line()+ggtitle(paste0("Predicting ",YearToPredict))+ylim(0,1)


ggplot(GammaData,aes(CSubmissions,PYSubs))+geom_point()

ggplot(GammaData[StandardisedActual<2],aes(StandardisedActual,log(mean),colour=InGammaInterval))+geom_point()+geom_vline(xintercept=0.804)+geom_vline(xintercept=1.196)

ggplot(GammaData[StandardisedActual<2],aes(StandardisedActual,PYSubs,colour=InInt))+geom_point()+geom_vline(xintercept=0.804)+geom_vline(xintercept=1.196)+
  guides(colour=guide_legend("Actual Value in Gamma 95% CI"), alpha = FALSE)+theme_classic()+ggtitle(paste0("Predicting ",YearToPredict))+ylab("Previous Year Submissions")

ggplot(GammaData[StandardisedActual<2 & `Department Code`=="NES"],aes(StandardisedActual,PYSubs,colour=InInt))+geom_point()+geom_vline(xintercept=0.804)+geom_vline(xintercept=1.196)+
  guides(colour=guide_legend("Actual Value in Gamma 95% CI"), alpha = FALSE)+theme_classic()+ggtitle(paste0("Predicting ",YearToPredict))+ylab("Previous Year Submissions")


### Now try and generate the linear model 



ForLoop<-UseableCodes[!duplicated(`Currency Code`,`Department Code`)]

TableSetup<-data.frame(`Currency Code`=character(),`Department Code`=character(),fit=numeric(),lwr=numeric(),upr=numeric(),PVal=numeric(),Years=numeric())
for(i in 1:nrow(Get2017))
{#print(loopval)
  ThisLoop<-data.frame(`Currency.Code`=Get2017$`Currency Code`[i],`Department.Code`=Get2017$`Department Code`[i])
  test<-AnalysisInput[ `Currency Code`==ThisLoop$`Currency.Code`[1] & `Department Code`==ThisLoop$`Department.Code`[1]]
  
  if(nrow(test)>3) # can only predict if more than 2 years to predict from 
  {
    CPredict<-lm(log(mean)~Year,test[Year<YearToPredict])  ## should it be weighted? 
    
    test2<-data.table(cbind(ThisLoop,exp(predict(CPredict,data.frame(Year=c(YearToPredict)), interval = "confidence",level=0.95))))
    test2[,PVal:=summary(CPredict)$coefficients[2,4]]
    test2[,InflationForce:=signif(summary(CPredict)$coefficients[2,1],3)]
    test2[,Years:=nrow(test)]
    TableSetup<-rbind(TableSetup,test2)
    
  }
}

View(TableSetup)
setnames(TableSetup,c("Currency.Code","Department.Code"),c("Currency Code","Department Code"))

## now see how able this is to predict actual values

LMtable<-merge(GammaData,TableSetup,all.x=T,by=c("Currency Code","Department Code"))
LMtable[mean>fit,AccuracyLM:=(log(mean)-log(fit))/(log(upr)-log(fit))] ## accuracy on a log scale here
LMtable[mean<fit,AccuracyLM:=-(log(mean)-log(fit))/(log(lwr)-log(fit))] ## accuracy on a log scale here
LMtable[,StandardisedActualLM:=mean/fit]


#####


LMtable[,InLMInterval:=0]
LMtable[mean<upr & mean>lwr,InLMInterval:=1]
LMSum<-LMData[,.(Av=mean(InLMInterval)),by=.(`Department Code`)]
LMtable[,Dispersion:=abs(1-(fit/mean))]
LMtable[,DispSQUARED:=Dispersion^2]
LMtable[,PropCIWidth:=(upr-lwr)/fit]
LMtable[,PropCIWidthLOG:=(log(upr)-log(lwr))/log(fit)]

LMSum<-LMtable[!(is.na(fit)) & Dispersion<1 & submissions>10,.(AvIn=mean(InLMInterval),AvDist=mean(Dispersion,na.rm=T),MSE=mean(DispSQUARED,na.rm=T)),by=.(`Department Code`)] # the LM method predicts fewer due to needing 3 years

pp<-ggplot(LMtable[StandardisedActualLM<2],aes(StandardisedActualLM,PYSubs,colour=as.character(InLMInterval)))+geom_point()+geom_vline(xintercept=0.804)+geom_vline(xintercept=1.196)
ggplotly(pp)

LMtable2<-LMtable[!(is.na(fit))]
LMOrder<-LMtable2[order(`Department Code`,-PYFCEs)]
LMOrder[,VolRank:=rowid(`Department Code`)]
LMOrder[,ByCurrencyCodeAccuracy:=cummean(InLMInterval),by=.(`Department Code`)]
LMOrder[,ByCurrencyCodeAccuracy:=cummean(InLMInterval),by=.(`Department Code`)]


#ggplot(GammaOrder,aes(log(PYFCEs),ByCurrencyCodeAccuracy,colour=`Department Code`))+geom_line()
ggplot(LMOrder,aes(-VolRank,ByCurrencyCodeAccuracy,colour=`Department Code`))+geom_line()+ggtitle(paste0("Predicting ",YearToPredict))+ylim(0,1)


ggplot(LMtable[abs(InflationForce)<0.3],aes(InflationForce,fill=`Department Code`))+geom_histogram(alpha=0.3,position="identity")
ggplot(LMtable[!(is.na(fit))],aes(log(PYFCEs),PVal,colour=InLMInterval))+geom_point()

#less accurate the smaller p is? (narrower confidence intervals?)
LMtable2<-LMtable[!(is.na(fit))]
LMtable2[,PValCat:=cut(PVal,c(0:20)/20,labels=(0:19)/20)]

Lmtab2sum<-LMtable2[,.(Av=mean(InLMInterval),Width=mean(PropCIWidthLOG)),by=.(PValCat)]
Lmtab3sum<-LMtable2[CSubmissions>10,.(Av=mean(InLMInterval),Width=mean(PropCIWidthLOG)),by=.(PValCat)]

ggplot(Lmtab3sum,aes(PValCat,Av))+geom_point()
ggplot(Lmtab2sum,aes(PValCat,Av))+geom_point()

ggplot(Lmtab2sum,aes(PValCat,Width))+geom_point()




############ Effect of pooling CC scores

New<-AllYearIQRs[,NoCC:=substr(`Currency Code`,1,4)]
New2<-New[,.(FCETotal=sum(FCEs),CostTotal=sum(FCEs*mean),Subs=sum(CSubmissions)),by=.(Year,NoCC,`Department Code`)]
New2[,mean:=CostTotal/FCETotal]

YearToPredict<-2018
# Firstly Filter the data by only selecting Currency Codes/ Department Code combinations where there were over 100 FCEs in 2017
UseableCodes<-New2[FCETotal>100 & Subs>10 & Year ==YearToPredict-1 & `Department Code`%in% c("EL","NEL","DC","NES") ,.(NoCC,`Department Code`)]

###


AnalysisInputFM<-New2[`Department Code`%in% c("EL","NEL","DC","NES") & Year>2012]
#AnalysisInputFM[,FCELogFloor:=floor(log(`FCE Vol`))]

AnalysisInput<-merge(UseableCodes,AnalysisInputFM,all.x=T,all.y=F,by=c("NoCC","Department Code"))
setnames(AnalysisInput,"NoCC","Currency Code")

Get2017<-AnalysisInput[Year==YearToPredict-1,.(`Department Code`,`Currency Code`)] # select the year before


ForLoop<-UseableCodes[!duplicated(NoCC,`Department Code`)]

TableSetup<-data.frame(`Currency Code`=character(),`Department Code`=character(),fit=numeric(),lwr=numeric(),upr=numeric(),PVal=numeric(),Years=numeric())
for(i in 1:nrow(Get2017))
{#print(loopval)
  ThisLoop<-data.frame(`Currency.Code`=Get2017$`Currency Code`[i],`Department.Code`=Get2017$`Department Code`[i])
  test<-AnalysisInput[ `Currency Code`==ThisLoop$`Currency.Code`[1] & `Department Code`==ThisLoop$`Department.Code`[1]]
  
  if(nrow(test)>3) # can only predict if more than 2 years to predict from 
  {
    CPredict<-lm(log(mean)~Year,test[Year<YearToPredict])  ## should it be weighted? 
    
    test2<-data.table(cbind(ThisLoop,exp(predict(CPredict,data.frame(Year=c(YearToPredict)), interval = "confidence",level=0.95))))
    test2[,PVal:=summary(CPredict)$coefficients[2,4]]
    test2[,InflationForce:=signif(summary(CPredict)$coefficients[2,1],3)]
    test2[,Years:=nrow(test)]
    TableSetup<-rbind(TableSetup,test2)
    
  }
}

View(TableSetup)
setnames(TableSetup,c("Currency.Code","Department.Code"),c("Currency Code","Department Code"))

## now see how able this is to predict actual values

LMtableTOTAL<-merge(AnalysisInput[Year==YearToPredict],TableSetup,all.x=T,by=c("Currency Code","Department Code"))
LMtableTOTAL[mean>fit,AccuracyLM:=(log(mean)-log(fit))/(log(upr)-log(fit))] ## accuracy on a log scale here
LMtableTOTAL[mean<fit,AccuracyLM:=-(log(mean)-log(fit))/(log(lwr)-log(fit))] ## accuracy on a log scale here
LMtableTOTAL[,StandardisedActualLM:=mean/fit]
LMtableTOTAL[,InLMInterval:=0]
LMtableTOTAL[mean<upr & mean>lwr,InLMInterval:=1]
mean(LMtableTOTAL[!(is.na(fit))]$InLMInterval
  )

LMtabsummary<-LMtableTOTAL[!(is.na(fit)),.(Av=mean(InLMInterval)),by=.(`Department Code`)]
