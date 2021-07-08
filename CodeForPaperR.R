### Official Code for Paper

## Load Required Packages

library("data.table")
library("ggplot2")
library("plotly")

# This File has been created by GR using previous data. Due to GDPR issues te 2018 values wil need to be changed
AllYearIQRs<-fread("Q:/Guidelines/In development/Diabetes/6. HE working/TJ Working/Misc/Reference Costs/Completeschedule.csv")

##SSO will contain spreadsheet output which removes issues around GDPR censored values
#this is ripped from https://www.england.nhs.uk/national-cost-collection/#ncc1819

ScheduleSpreadsheetOutput<-fread("Q:/Guidelines/In development/Diabetes/6. HE working/TJ Working/Misc/Reference Costs/2018ScheduleCOMPLETE.csv")

SSO<-ScheduleSpreadsheetOutput[,.(`Department Code`,`Currency Code`,`FCE Vol`,`Average Unit Cost`)] # select required columns for merge
SSO[,Year:=2018]
AllYearIQRs<-merge(AllYearIQRs,SSO,by=c("Year","Department Code","Currency Code"),all.x=T)

# now if average unit cost (from spreadsheet) exists then transplant it into mean column
AllYearIQRs[!(is.na(`Average Unit Cost`)),mean:=`Average Unit Cost`]
# same for FCEs
AllYearIQRs[!(is.na(`FCE Vol`)),FCEs:=`FCE Vol`]


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


# Prepare data for input ####
YearToPredict<-2017
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

Get2017<-AnalysisInput[Year==YearToPredict-1,.(`Department Code`,`Currency Code`,Simple10pcCIlower,Simple10pcCIupper,mean,FCELogFloor)] # select the year before
setnames(Get2017,c("Department Code","Currency Code","ConfIntLow2017","ConfIntHigh2017","MeanPrediction2017","FCELogFloor"))


SimpleData<-merge(AnalysisInput[Year==YearToPredict],Get2017,by=c("Department Code","Currency Code"),all.x=F)

SimpleData[,InSimpleInterval:=0]
SimpleData[mean<ConfIntHigh2017 & mean>ConfIntLow2017,InSimpleInterval:=1]
SimpleSum<-SimpleData[,.(Av=mean(InSimpleInterval)),by=.(`Department Code`)]
SimpleData[,Dispersion:=abs(1-(MeanPrediction2017/mean))]
SimpleData[,DispSQUARED:=Dispersion^2]
SimpleData[,PropCIWidth:=(Simple10pcCIupper-Simple10pcCIlower)/MeanPrediction]

SimpleSum<-SimpleData[Dispersion<1 & submissions>10,.(AvIn=mean(InSimpleInterval),AvDist=mean(Dispersion),MSE=mean(DispSQUARED,na.rm=T)),by=.(`Department Code`)]
SimpleSum2<-SimpleData[,.(AvIn=mean(InSimpleInterval),AvDist=mean(Dispersion),MSE=mean(DispSQUARED,na.rm=T),Vol=.N),by=.(`Department Code`,FCELogFloor.x)]



# Explore previous method of estimation (fitting Gamma distribution) ####

# Firstly simply use the 2017 values and transplant onto 2018 and check how many lie within 95% confidence interval

Get2017<-AnalysisInput[Year==YearToPredict-1,.(`Department Code`,`Currency Code`,ConfIntLow,ConfIntHigh,MeanPrediction)]
setnames(Get2017,c("Department Code","Currency Code","ConfIntLow2017","ConfIntHigh2017","MeanPrediction2017"))

GammaData[,InGammaInterval:=0]
GammaData[mean<ConfIntHigh2017 & mean>ConfIntLow2017,InGammaInterval:=1]
GammaSum<-GammaData[,.(Av=mean(InGammaInterval)),by=.(`Department Code`)]
GammaData[,Dispersion:=abs(1-(MeanPrediction2017/mean))]
GammaData[,DispSQUARED:=Dispersion^2]
GammaData[,PropCIWidth:=(-(ConfIntLow2017-ConfIntHigh2017))/MeanPrediction2017]
GammaSum<-GammaData[Dispersion<1 & submissions>10,.(AvIn=mean(InGammaInterval),AvDist=mean(Dispersion,na.rm=T),MSE=mean(DispSQUARED,na.rm=T)),by=.(`Department Code`)]



GammaData<-merge(AnalysisInput[Year==YearToPredict],Get2017,by=c("Department Code","Currency Code"),all.x=F)

## now need to calculate accuracy measure - values are not quite normally distributed - so Accuracy will be determined by how many upper confidence intervals away from the predicted mean,
# the true values are
GammaData[mean>MeanPrediction2017,Accuracy:=(mean-MeanPrediction2017)/(ConfIntHigh2017-MeanPrediction2017)]
GammaData[mean<MeanPrediction2017,Accuracy:=-(mean-MeanPrediction2017)/(ConfIntLow2017-MeanPrediction2017)]

hist(GammaData[abs(Accuracy)<5 & `Department Code`=="EL"]$Accuracy,50)
hist(GammaData[abs(Accuracy)<5 & `Department Code`=="NEL"]$Accuracy,50)
hist(GammaData[abs(Accuracy)<5 & `Department Code`=="NES"]$Accuracy,50)
hist(GammaData[abs(Accuracy)<5 & `Department Code`=="DC"]$Accuracy,50)

ggplot(GammaData[abs(Accuracy)<5],aes(Accuracy,fill=`Department Code`))+geom_histogram(position="identity",alpha=0.2,bins=50)

ggplot(GammaData[abs(Accuracy)<5],aes(Accuracy,fill=`Department Code`))+geom_density(alpha=0.2)+ggtitle(paste0(YearToPredict," Accuracy "))

table(abs(GammaData$Accuracy)<1,GammaData$`Department Code`)

## sometimes the 2017 value is inflated when estimating inputs for economic models. Does this look feasible?

GammaData[mean>MeanPrediction2017*1.02,AccuracyInflated:=(mean-MeanPrediction2017*1.02)/(ConfIntHigh2017*1.02-MeanPrediction2017*1.02)]
GammaData[mean<MeanPrediction2017*1.02,AccuracyInflated:=-(mean-MeanPrediction2017*1.02)/(ConfIntLow2017*1.02-MeanPrediction2017*1.02)]

table(abs(GammaData$AccuracyInflated)<1,GammaData$`Department Code`)

## it isn't completely clear that this is preferabe - roughly 60% of next year's costs lie in the requested confidence interval


# Alternate method - simple linear model ####
# potential issues regarding lack of predictive power for new(ish codes...)


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


#####


LMtable[,InLMInterval:=0]
LMtable[mean<upr & mean>lwr,InLMInterval:=1]
LMSum<-LMData[,.(Av=mean(InLMInterval)),by=.(`Department Code`)]
LMtable[,Dispersion:=abs(1-(fit/mean))]
LMtable[,DispSQUARED:=Dispersion^2]
LMtable[,PropCIWidth:=(upr-lwr)/fit]
LMSum<-LMtable[!(is.na(fit)) & Dispersion<1 & submissions>10,.(AvIn=mean(InLMInterval),AvDist=mean(Dispersion,na.rm=T),MSE=mean(DispSQUARED,na.rm=T)),by=.(`Department Code`)] # the LM method predicts fewer due to needing 3 years

f<-ggplotly(ggplot(LMtable,aes(log(PropCIWidth),PVal,colour=as.character(InLMInterval)))+geom_point(mapping=aes(shape=as.character(Years))))

# ... of prior data

LMSum$Type<-"LM"
SimpleSum$Type<-"Simple"
GammaSum$Type<-"Gamma"

outtotal<-rbind(LMSum,SimpleSum)
outtotal<-rbind(outtotal,GammaSum)


ggplot(LMtable[abs(AccuracyLM)<5],aes(AccuracyLM,fill=`Department Code`))+geom_density(alpha=0.2)+ggtitle(paste0(YearToPredict," Accuracy LM "))
#generally speaking this is quite significantly better 

LMtable[,AccuracyCAT:=ifelse(abs(Accuracy)<1,1,0)]
LMtable[is.na(Accuracy),AccuracyCAT:=2]

LMtable[,AccuracyCATLM:=ifelse(abs(AccuracyLM)<1,1,0)]
LMtable[is.na(AccuracyLM),AccuracyCATLM:=2]

table(LMtable$AccuracyCATLM,GammaData$`Department Code`)
table(LMtable$AccuracyCAT,GammaData$`Department Code`)


# Comparison Histograms

Comp<-data.table(melt(LMtable[,.(`Currency Code`,`Department Code`,Accuracy,AccuracyLM)],id.vars=c("Currency Code","Department Code"))) # melted comparison

ggplot(Comp[`Department Code`=="EL"],aes(value,fill=variable))+geom_density(alpha=0.5)+xlim(-5,5)
ggplot(Comp[`Department Code`=="EL"],aes(value,fill=variable))+geom_histogram(alpha=0.5,bins=50)+xlim(-5,5) # doesn't account for missingness

ggplot(Comp[`Department Code`=="EL"],aes(value,fill=variable))+geom_density(alpha=0.5)+xlab("Accuracy")+scale_x_continuous(breaks=-5:5,limits=c(-5,5))+ggtitle("EL Prediction Accuracy")
ggplot(Comp[`Department Code`=="NEL"],aes(value,fill=variable))+geom_density(alpha=0.5)+xlab("Accuracy")+scale_x_continuous(breaks=-5:5,limits=c(-5,5))+ggtitle("NEL Prediction Accuracy")
ggplot(Comp[`Department Code`=="DC"],aes(value,fill=variable))+geom_density(alpha=0.5)+xlab("Accuracy")+scale_x_continuous(breaks=-5:5,limits=c(-5,5))+ggtitle("DC Prediction Accuracy")
ggplot(Comp[`Department Code`=="NES"],aes(value,fill=variable))+geom_density(alpha=0.5)+xlab("Accuracy")+scale_x_continuous(breaks=-5:5,limits=c(-5,5))+ggtitle("NES Prediction Accuracy")

Comp

