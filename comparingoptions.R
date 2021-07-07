# Get All IQRs - 
#PSSRU costs available here:https://www.pssru.ac.uk/pub/uc/uc2019/sources-of-information.pdf

library("data.table")
library("ggplot2")


AllYearIQRs<-fread("Q:/Guidelines/In development/Diabetes/6. HE working/TJ Working/Misc/Reference Costs/scheduleTJ.csv")
AllYearIQRs<-fread("Q:/Guidelines/In development/Diabetes/6. HE working/TJ Working/Misc/Reference Costs/Completeschedule.csv")

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

AllYearIQRs<-AllYearIQRs[`Department Code`=="EL"]

ggplot(AllYearIQRs[`Currency Code`=="WA24B" & `Service code`==100],aes(Year,mean))+geom_point()+geom_line()
ggplot(AllYearIQRs[`Currency Code`=="WA24B" & `Service code`==101],aes(Year,mean))+geom_point()+geom_line(aes(Year,ConfIntHigh))+geom_line(aes(Year,ConfIntLow))

###
Filt<-AllYearIQRs[`Service code`=="ALL"]
Get2017<-Filt[Year==2017,.(`Department Code`,`Currency Code`,ConfIntLow,ConfIntHigh)]
setnames(Get2017,c("Department Code","Currency Code","ConfIntLow2017","ConfIntHigh2017"))

NM<-merge(Filt,Get2017,by=c("Department Code","Currency Code"),all.x=T)
NM18<-NM[Year==2018]
NM18[,Accuracy:=((mean-ConfIntLow2017)/(ConfIntHigh2017-ConfIntLow2017)-0.5)*2]
NM18[,MeanError:=((ConfIntHigh2017+ConfIntLow2017)/2-mean)/mean+1]
NM18[,MeanCategory:=cut(MeanError,c(-Inf,0.75,0.85,0.95,1,1.05,1.15,1.25,Inf))]


Useable<-NM18[!(is.na(Accuracy))]
table(Useable$MeanCategory,floor(Useable$submissions/10))
table(Useable$MeanCategory,abs(Useable$Accuracy)<1)
table(Useable[submissions>100]$MeanCategory,abs(Useable[submissions>100]$Accuracy)<1)
r<-table(floor(Useable[submissions>10 & FCEs>100]$submissions/10),abs(Useable[submissions>10 & FCEs>100]$Accuracy)<1)
prop.table(r,1)

## around 50% of 95% confidence intervals contain the true value


ggplot(Useable[submissions>10 & FCEs>100],aes(Accuracy,submissions,colour=log(FCEs)))+geom_point()+xlim(-10,10)+geom_vline(xintercept=1)+geom_vline(xintercept=-1)

ggplot(Useable[submissions>10 & FCEs>100],aes(Accuracy,submissions,colour=))+geom_point()+xlim(-10,10)+geom_vline(xintercept=1)+geom_vline(xintercept=-1)


ggplot(Useable[submissions>10 & FCEs>100],aes(MeanError,submissions,colour=log(FCEs)))+geom_point()+xlim(0.5,1.5)

# PSSRU inflation factor for 2017 is 1.16 for 2018 is 2.31 https://doi.org/10.22024/UniKent%2F01.02.79286 ()
# assuming this is published beforehand... 

# Another simple solution may be to inflate costs by 2%

NM18v2<-copy(NM18)
NM18v2[,ConfIntLow2017:=ConfIntLow2017*1.02]
NM18v2[,ConfIntHigh2017:=ConfIntHigh2017*1.02]
NM18v2[,Accuracy:=((mean-ConfIntLow2017)/(ConfIntHigh2017-ConfIntLow2017)-0.5)*2]
NM18v2[,MeanError:=((ConfIntHigh2017+ConfIntLow2017)/2-mean)/mean+1]
NM18v2[,MeanCategory:=cut(MeanError,c(-Inf,0.75,0.85,0.95,1,1.05,1.15,1.25,Inf))]


Useable2<-NM18v2[!(is.na(Accuracy))]
table(abs(Useable2[submissions>10 & FCEs>100]$Accuracy)<1)
table(abs(Useable[submissions>10 & FCEs>100]$Accuracy)<1) # inflating by 2% improves the picture but only marginally
table(abs(Useable2[submissions>10 & FCEs>100]$Accuracy)<2) # doubing confidence intervals does help!


table(Useable2$MeanCategory,floor(Useable2$submissions/10))
table(Useable2$MeanCategory,abs(Useable2$Accuracy)<1)

## now try and generate a simple linear model for all currency codes which appear in Useabe (or useable 2)

ELVals<-unique(Useable[`Department Code`=="EL"]$`Currency Code`)

TableSetup<-data.frame(`Currency Code`=character(),`Department Code`=character(),fit=numeric(),lwr=numeric(),upr=numeric(),PVal=numeric())
for(loopval in ELVals)
{#print(loopval)
  ThisLoop<-data.frame(`Currency Code`=loopval,`Department Code`="EL")
test<-Filt[`Department Code`=="EL" & `Currency Code`==loopval]
if(nrow(test)>3) # can only predict if more than 2 years to predict from 
{
CPredict<-lm(log(mean)~Year,test[Year!=2018])  ## should it be weighted? 

test2<-data.table(cbind(ThisLoop,exp(predict(CPredict,test[Year==2018], interval = "confidence",level=0.95))))
test2[,PVal:=summary(CPredict)$coefficients[2,4]]
test2[,InflationForce:=signif(summary(CPredict)$coefficients[2,1],3)]
  TableSetup<-rbind(TableSetup,test2)
  
}
}

View(TableSetup)
setnames(TableSetup,c("Currency.Code","Department.Code"),c("Currency Code","Department Code"))

## now see how able this is to predict actual values

ELTest<-merge(Useable[`Department Code`=="EL" & submissions>10 & FCEs>100],TableSetup,all.x=T)
ELTest[,AccuracyLM:=((mean-lwr)/(upr-lwr)-0.5)*2]

# how big is confidence interval relative to mean?
ELTest[,GammaStandardisedCI:=(UQ-LQ)/mean]
ELTest[,LMStandardisedCI:=(upr-lwr)/mean]
table(abs(ELTest$GammaStandardisedCI)<1)
table(abs(ELTest$LMStandardisedCI)<1)


table(abs(ELTest$Accuracy)<1)
table(abs(ELTest$AccuracyLM)<1)



head(NM18v2)
test<-Filt[`Department Code`=="EL" & `Currency Code`=="HN23C"]
CPredict<-lm(log(mean)~Year,test[Year!=2018])  ## should it be weighted? 

test2<-cbind(test,exp(predict(CPredict,test, interval = "confidence",level=0.95)))
test2[,PVal:=summary(CPredict)$coefficients[2,4]]

h<-ggplot(test2,aes(Year,mean))+geom_line()+geom_line(aes(Year,ConfIntHigh))+geom_line(aes(Year,ConfIntLow))


print(h+geom_ribbon(aes(ymin=lwr, ymax=upr, x=Year), fill="grey",alpha=0.3,linetype=0,colour="green")+theme_bw()+ggtitle(paste0("Average Costs")
))

# find where LM is better (some examples)
LMPreferred<-ELTest[abs(ELTest$Accuracy)>1 & abs(ELTest$AccuracyLM)<1 ]

head(LMPreferred)

LMError<-ELTest[is.na(AccuracyLM)]
