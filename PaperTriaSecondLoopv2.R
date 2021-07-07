
library("data.table")
library("ggplot2")


AllYearIQRs<-fread("Q:/Guidelines/In development/Diabetes/6. HE working/TJ Working/Misc/Reference Costs/scheduleTJ.csv")
AllYearIQRs<-fread("Q:/Guidelines/In development/Diabetes/6. HE working/TJ Working/Misc/Reference Costs/Completeschedule.csv")

##SSO will contain spreadsheet output which removes issues around GDPR censored values
ScheduleSpreadsheetOutput<-fread("Q:/Guidelines/In development/Diabetes/6. HE working/TJ Working/Misc/Reference Costs/2018ScheduleCOMPLETE.csv")

SSO<-ScheduleSpreadsheetOutput[,.(`Department Code`,`Currency Code`,`FCE Vol`,`Average Unit Cost`)]
SSO[,Year:=2018]
AllYearIQRs<-merge(AllYearIQRs,SSO,by=c("Year","Department Code","Currency Code"),all.x=T)

# now if average unit cost (from spreadsheet) exists then transplant it into mean column
AllYearIQRs[!(is.na(`Average Unit Cost`)),mean:=`Average Unit Cost`]
AllYearIQRs[!(is.na(`FCE Vol`)),FCEs:=`Average Unit Cost`]


## optional code to crudely add on XS bed days to give a consistent picture across years
XS<-AllYearIQRs[grepl("XS",`Department Code`),.(Year,`Department Code`,`Currency Code`,`Service code`,FCEs,mean)]
setnames(XS,c("Year","Department Code","Currency Code","Service code","FCEsXS","meanXS"))
XS[,`Department Code`:=gsub("_XS","",`Department Code`)]

AllYearIQRs<-merge(AllYearIQRs,XS,by=c("Year","Department Code","Currency Code","Service code"),all.x=T)
AllYearIQRs[is.na(meanXS),meanXS:=0]
AllYearIQRs[is.na(FCEsXS),FCEsXS:=0]
AllYearIQRs[,meanBACKUP:=mean]
AllYearIQRs[,mean:=mean+meanXS*FCEsXS/FCEs]
AllYearIQRs[,UQ:=UQ+meanXS*FCEsXS/FCEs]
AllYearIQRs[,LQ:=LQ+meanXS*FCEsXS/FCEs]

# important to note that it would be better to arrange the (e.g EL) total BEFORE calculating IQR (in the GR schedule file - to give a more accurate total FCE cost spread...)



AllYearIQRs[,SE:=((UQ-LQ)/2/qnorm(0.75))/sqrt(CSubmissions)]
AllYearIQRs[,GammaAlpha:=mean^2/SE^2]
AllYearIQRs[,GammaBeta:=SE^2/mean]
AllYearIQRs[,ConfIntLow:=qgamma(0.025,GammaAlpha, 1/GammaBeta)]
AllYearIQRs[,ConfIntHigh:=qgamma(0.975,GammaAlpha, 1/GammaBeta)]

### Now begin loop through Department Codes
table(AllYearIQRs$`Department Code`,AllYearIQRs$`Service code`=="ALL")
table(AllYearIQRs[`Department Code`=="EL"]$Year,AllYearIQRs[`Department Code`=="EL"]$`Service code`=="ALL")
table(AllYearIQRs[`Department Code`=="NEL"]$Year,AllYearIQRs[`Department Code`=="NEL"]$`Service code`=="ALL")

## 2012 was structured differenty so ignorefor now

LoopInput<-AllYearIQRs[`Department Code`%in% c("EL","NEL","DC","NES") & Year>2012]

outputtable<-data.frame(DepCode=character(),PredictionType=character(),InConfInt=numeric(),OutsideConfInt=numeric(),NotAvailabe=numeric())

PredictYear<-2018
for (Type in c("EL","NEL","DC","NES"))
{## firsty filter by Type
  print(Type)
  ForCalc<-LoopInput[`Department Code`==Type]
  UseableCodes<-ForCalc[submissions>10 & FCEs>100 & Year ==PredictYear-1]$`Currency Code` # to be modelled must have 100 FCEs and over 10 submissions in 2017
  # the above line could be changed in the future, but low volumes lead to much worse predictions
  ForCalc2<-ForCalc[`Currency Code`%in% UseableCodes]
  
  # firstly calculate how often the 2018 value lies inside the 95% conf interval of 2017 
  Get2017<-ForCalc2[Year==PredictYear-1,.(`Department Code`,`Currency Code`,ConfIntLow,ConfIntHigh)]
  setnames(Get2017,c("Department Code","Currency Code","ConfIntLow2017","ConfIntHigh2017"))
  
  NM<-merge(ForCalc2[Year==PredictYear],Get2017,by=c("Department Code","Currency Code"),all.x=T)
  NM18<-NM[Year==PredictYear]
  NM18[,Accuracy:=((mean-ConfIntLow2017)/(ConfIntHigh2017-ConfIntLow2017)-0.5)*2]
  # accuracy of -1 is on the lower point of the 95% conf interval, 0 is at mean, 1 is upper limit
  NM18[,MeanPrediction:=((ConfIntHigh2017+ConfIntLow2017)/2-mean)/mean+1]
  print(ggplot(NM18[submissions>10 & FCEs>100],aes(MeanPrediction,submissions,colour=log(FCEs)))+geom_point()+xlim(0.5,1.5)+ggtitle(Type))
  NM18[,MeanCategory:=cut(MeanPrediction,c(-Inf,0.75,0.85,0.95,1,1.05,1.15,1.25,Inf))]
  print(summary(as.factor(NM18$MeanCategory)))
  
  outputvec<-summary(as.factor(abs(NM18$Accuracy)<1)) # NAs driven by issues with IQR caculations, see ForCalc2[`Currency Code`=="AA71B"] (EL) for an example
  
  toadd<-data.frame(DepCode=Type,PredictionType="Gamma",InConfInt=as.numeric(outputvec[2]),OutsideConfInt=as.numeric(outputvec[1]),NotAvailable=as.numeric(outputvec[3]))
  outputtable<<-rbind(outputtable,toadd)
  
  ##### Now inflate values by 2%
  
  
  NM18v2<-copy(NM18)
  NM18v2[,ConfIntLow2017:=ConfIntLow2017*1.02]
  NM18v2[,ConfIntHigh2017:=ConfIntHigh2017*1.02]
  NM18v2[,Accuracy:=((mean-ConfIntLow2017)/(ConfIntHigh2017-ConfIntLow2017)-0.5)*2]
  
  outputvec<-summary(as.factor(abs(NM18v2$Accuracy)<1)) # NAs driven by issues with IQR caculations, see ForCalc2[`Currency Code`=="AA71B"] (EL) for an example
  
  toadd<-data.frame(DepCode=Type,PredictionType="Gamma2pcInf",InConfInt=as.numeric(outputvec[2]),OutsideConfInt=as.numeric(outputvec[1]),NotAvailable=as.numeric(outputvec[3]))
  outputtable<<-rbind(outputtable,toadd)
  
  
  ## Now run simple linear model (with associated conf intervals)
  
  
  
  TableSetup<-data.frame(`Currency Code`=character(),`Department Code`=character(),fit=numeric(),lwr=numeric(),upr=numeric(),PVal=numeric())
  for(loopval in UseableCodes)
  {#print(loopval)
    ThisLoop<-data.frame(`Currency Code`=loopval,`Department Code`=Type)
    test<-ForCalc2[ `Currency Code`==loopval]
    if(nrow(test)>3) # can only predict if more than 2 years to predict from 
    {
      CPredict<-lm(log(mean)~Year,test[Year<PredictYear])  ## should it be weighted? 
      
      test2<-data.table(cbind(ThisLoop,exp(predict(CPredict,data.frame(Year=c(PredictYear)), interval = "confidence",level=0.95))))
      test2[,PVal:=summary(CPredict)$coefficients[2,4]]
      test2[,InflationForce:=signif(summary(CPredict)$coefficients[2,1],3)]
      TableSetup<-rbind(TableSetup,test2)
      
    }
  }
  
  View(TableSetup)
  setnames(TableSetup,c("Currency.Code","Department.Code"),c("Currency Code","Department Code"))
  
  ## now see how able this is to predict actual values
  
  LMtable<-merge(NM18,TableSetup,all.x=T,by=c("Currency Code","Department Code"))
  LMtable[,AccuracyLM:=((mean-lwr)/(upr-lwr)-0.5)*2]
  
  outputvec<-summary(as.factor(abs(LMtable$AccuracyLM)<1)) # NAs driven by issues with IQR caculations, see ForCalc2[`Currency Code`=="AA71B"] (EL) for an example
  
  toadd<-data.frame(DepCode=Type,PredictionType="LinearModel",InConfInt=as.numeric(outputvec[2]),OutsideConfInt=as.numeric(outputvec[1]),NotAvailable=as.numeric(outputvec[3]))
  outputtable<<-rbind(outputtable,toadd)
  
  
  
}

#### 23 09 2020. Following conversation with GR suggest I use summary from schedule of costs 2018 (which does not suffer from GDPR deletion of variables less than 9 observations)

ScheduleSpreadsheetOutput<-fread("Q:/Guidelines/In development/Diabetes/6. HE working/TJ Working/Misc/Reference Costs/2018ScheduleCOMPLETE.csv")

SSO<-ScheduleSpreadsheetOutput[,.(`Department Code`,`Currency Code`,`FCE Vol`,`Average Unit Cost`)]
SSO[,`FCE Vol`:=as.numeric(`FCE Vol`)]


FromBase2018<-AllYearIQRs[Year==2018 & `Service code`=="ALL"]


Comparison<-merge(FromBase2018,SSO,by=c("Department Code","Currency Code"),all.y=T)

# fairly clear that these numbers don't always match!


ggplot(Comparison[`Department Code`=="EL"],aes(`Average Unit Cost`,mean,colour=log(FCEs)))+geom_point()+geom_abline(slope=0.9)+geom_abline(slope=1.1)

Comparison[,CalculatedProportion:=mean/`Average Unit Cost`]
Comparison[,LogCat:=cut(log(`FCE Vol`),c(0:11),labels=c(1:11))]

ggplot(Comparison[`Department Code`=="EL"],aes(log(as.numeric(`FCE Vol`)),CalculatedProportion))+geom_point()

CompSum<-Comparison[!(is.na(`FCE Vol`)),.(AvProp=mean(CalculatedProportion,na.rm=T)),by=.(`Department Code`,LogCat)]

ggplot(CompSum,aes(LogCat,AvProp,colour=`Department Code`,group=`Department Code`))+geom_line()

###### comparison now contains actual 2018 values (without GDPR skew)
# now bring in ForCalc (incl linear modelling conf intervals)
# get mean for merging
Tomergemean<-FromBase2018[,.(`Currency Code`,`Department Code`,mean)]



ConfInt2018<-ForCalc2[Year==2018]

ConfInt2018NEW<-merge(TableSetup,SSO,by=c("Department Code","Currency Code"),all.x=T)
ConfInt2018NEW<-merge(ConfInt2018NEW,Tomergemean,by=c("Department Code","Currency Code"),all.x=T,all.y=F)

ConfInt2018NEW[,AccuracyFROMXL:=((`Average Unit Cost`-lwr)/(upr-lwr)-0.5)*2]
ConfInt2018NEW[,AccuracyFlagXL:=ifelse(abs(AccuracyFROMXL)<1,1,0)]
ConfInt2018NEW[,Accuracy:=((mean-lwr)/(upr-lwr)-0.5)*2]
ConfInt2018NEW[,AccuracyFlag:=ifelse(abs(Accuracy)<1,1,0)]

mean(ConfInt2018NEW$AccuracyFlag,na.rm=T)
mean(ConfInt2018NEW$AccuracyFlagXL,na.rm=T)


head(ConfInt2018NEW)