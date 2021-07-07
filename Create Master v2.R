
library("data.table")

#### 8 


RawData<-fread("//nice/Data/Users/Private/TJONES/NHSReference/0809Data/Data 1.csv")


Part1A<-RawData[!is.na(`FCE`)]
setnames(Part1A,"Currency code","Currency Code")
setnames(Part1A,"Department code","Department Code")

##
#`Supplier type`
OutputTable08<-Part1A[,.(Submissions=.N,FCE=sum(FCE),BedDays=sum(`Bed days`),Mean=weighted.mean(`Unit cost`,FCE)),by=.(`Currency Code`,`Department Code`)] ## added supplier type == OWN, not available for 17/18
OutputTable08[,AverageStayLength:=BedDays/FCE]
OutputTable08[,UQ:=-1]
OutputTable08[,LQ:=-1]


for(i in 1:nrow(OutputTable08))
{
  GetVals<-Part1A[`Currency Code`==OutputTable12$`Currency Code`[i] & `Department Code`==OutputTable08$`Department Code`[i]]
  QVec<-quantile(rep(GetVals$`Unit cost`,GetVals$FCE))
  OutputTable12$UQ[i]<-QVec[4]
  OutputTable12$LQ[i]<-QVec[2]
}

OutputTable08[,Year:=2008]

## not included in raw data as format very different


#### 9 


RawData<-fread("//nice/Data/Users/Private/TJONES/NHSReference/0910Data/1 Data.csv")


Part1A<-RawData
setnames(Part1A,"Currency code","Currency Code")
setnames(Part1A,"Department code","Department Code")
Part1A$Year<-2009
RawTab<-Part1A

##
#`Supplier type`
OutputTable09<-Part1A[,.(Submissions=.N,FCE=sum(Activity),BedDays=sum(`Bed days`),Mean=weighted.mean(`Unit cost`,Activity)),by=.(`Currency Code`,`Department Code`)] ## added supplier type == OWN, not available for 17/18
OutputTable09[,AverageStayLength:=BedDays/FCE]
OutputTable09[,UQ:=-1]
OutputTable09[,LQ:=-1]


for(i in 1:nrow(OutputTable09))
{
  GetVals<-Part1A[`Currency Code`==OutputTable12$`Currency Code`[i] & `Department Code`==OutputTable09$`Department Code`[i]]
  QVec<-quantile(rep(GetVals$`Unit cost`,GetVals$Activity))
  OutputTable12$UQ[i]<-QVec[4]
  OutputTable12$LQ[i]<-QVec[2]
}

OutputTable09[,Year:=2009]



####

#### 10 


RawData<-fread("//nice/Data/Users/Private/TJONES/NHSReference/1011Data/1 Data.csv")


Part1A<-RawData
setnames(Part1A,"Currency code","Currency Code")
setnames(Part1A,"Department code","Department Code")

Part1A$Year<-2010
RawTab<-rbind(RawTab,Part1A)


##
#`Supplier type`
OutputTable10<-Part1A[,.(Submissions=.N,FCE=sum(Activity),BedDays=sum(`Bed days`),Mean=weighted.mean(`Unit cost`,Activity)),by=.(`Currency Code`,`Department Code`)] ## added supplier type == OWN, not available for 17/18
OutputTable10[,AverageStayLength:=BedDays/FCE]
OutputTable10[,UQ:=-1]
OutputTable10[,LQ:=-1]


for(i in 1:nrow(OutputTable10))
{
  GetVals<-Part1A[`Currency Code`==OutputTable12$`Currency Code`[i] & `Department Code`==OutputTable10$`Department Code`[i]]
  QVec<-quantile(rep(GetVals$`Unit cost`,GetVals$Activity))
  OutputTable12$UQ[i]<-QVec[4]
  OutputTable12$LQ[i]<-QVec[2]
}

OutputTable10[,Year:=2010]




#### 11

RawData<-fread("//nice/Data/Users/Private/TJONES/NHSReference/1112Data/1 Data.csv")


Part1A<-RawData
setnames(Part1A,"Currency code","Currency Code")
setnames(Part1A,"Department code","Department Code")

Part1A$Year<-2011
RawTab<-rbind(RawTab,Part1A,fill=T)


##
#`Supplier type`
OutputTable11<-Part1A[,.(Submissions=.N,FCE=sum(Activity),BedDays=sum(`Bed days`),Mean=weighted.mean(`Unit cost`,Activity)),by=.(`Currency Code`,`Department Code`)] ## added supplier type == OWN, not available for 17/18
OutputTable11[,AverageStayLength:=BedDays/FCE]
OutputTable11[,UQ:=-1]
OutputTable11[,LQ:=-1]


for(i in 1:nrow(OutputTable11))
{
  GetVals<-Part1A[`Currency Code`==OutputTable12$`Currency Code`[i] & `Department Code`==OutputTable11$`Department Code`[i]]
  QVec<-quantile(rep(GetVals$`Unit cost`,GetVals$Activity))
  OutputTable11$UQ[i]<-QVec[4]
  OutputTable11$LQ[i]<-QVec[2]
}

OutputTable11[,Year:=2011]




############## 12 

RawData<-fread("//nice/Data/Users/Private/TJONES/NHSReference/1213Data/1 Data.csv")


Part1A<-RawData
setnames(Part1A,"Currency code","Currency Code")
setnames(Part1A,"Department code","Department Code")
setnames(Part1A,"Actual_Cost","Actual_cost")
setnames(Part1A,"Expected_Cost","Expected_cost")
setnames(Part1A,"Mapping_Pot","Mapping_pot")



Part1A$Year<-2012
RawTab<-rbind(RawTab,Part1A,fill=T)


##
#`Supplier type`
OutputTable12<-Part1A[,.(Submissions=.N,FCE=sum(Activity),BedDays=sum(`Bed days`),Mean=weighted.mean(`Unit cost`,Activity)),by=.(`Currency Code`,`Department Code`)] ## added supplier type == OWN, not available for 17/18
OutputTable12[,AverageStayLength:=BedDays/FCE]
OutputTable12[,UQ:=-1]
OutputTable12[,LQ:=-1]


for(i in 1:nrow(OutputTable12))
{
  GetVals<-Part1A[`Currency Code`==OutputTable12$`Currency Code`[i] & `Department Code`==OutputTable12$`Department Code`[i]]
  QVec<-quantile(rep(GetVals$`Unit cost`,GetVals$Activity))
  OutputTable12$UQ[i]<-QVec[4]
  OutputTable12$LQ[i]<-QVec[2]
}

OutputTable12[,Year:=2012]


############# 13

RawData<-fread("//nice/Data/Users/Private/TJONES/NHSReference/1314Data/1 Data.csv")


Part1A<-RawData
setnames(Part1A,"Currency code","Currency Code")
setnames(Part1A,"Department code","Department Code")
Part1A$Year<-2013
RawTab<-rbind(RawTab,Part1A,fill=T)


##
#`Supplier type`
OutputTable13<-Part1A[,.(Submissions=.N,FCE=sum(Activity),BedDays=sum(`Bed days`),Mean=weighted.mean(`Unit cost`,Activity)),by=.(`Currency Code`,`Department Code`)] ## added supplier type == OWN, not available for 17/18
OutputTable13[,AverageStayLength:=BedDays/FCE]
OutputTable13[,UQ:=-1]
OutputTable13[,LQ:=-1]


#`Supplier type`
OutputTable13<-Part1A[,.(Submissions=.N,FCE=sum(Activity),BedDays=sum(`Bed days`),Mean=weighted.mean(`Unit cost`,Activity)),by=.(`Currency Code`,`Department Code`)] ## added supplier type == OWN, not available for 17/18
OutputTable13[,AverageStayLength:=BedDays/FCE]
OutputTable13[,UQ:=-1]
OutputTable13[,LQ:=-1]


for(i in 1:nrow(OutputTable13))
{
  GetVals<-Part1A[`Currency Code`==OutputTable13$`Currency Code`[i] & `Department Code`==OutputTable13$`Department Code`[i]]
  QVec<-quantile(rep(GetVals$`Unit cost`,GetVals$Activity))
  OutputTable13$UQ[i]<-QVec[4]
  OutputTable13$LQ[i]<-QVec[2]
}

OutputTable13[,Year:=2013]

############# 14

RawData<-fread("//nice/Data/Users/Private/TJONES/NHSReference/1415Data/1 Data.csv")


Part1A<-RawData
setnames(Part1A,"Currency code","Currency Code")
setnames(Part1A,"Department code","Department Code")
setnames(Part1A,"Actual cost","Actual_cost")
setnames(Part1A,"Expected cost","Expected_cost")
setnames(Part1A,"Mapping pot","Mapping_pot")
Part1A$Year<-2014
RawTab<-rbind(RawTab,Part1A,fill=T)


##
#`Supplier type`
OutputTable14<-Part1A[,.(Submissions=.N,FCE=sum(Activity),BedDays=sum(`Bed days`),Mean=weighted.mean(`Unit cost`,Activity)),by=.(`Currency Code`,`Department Code`)] ## added supplier type == OWN, not available for 17/18
OutputTable14[,AverageStayLength:=BedDays/FCE]
OutputTable14[,UQ:=-1]
OutputTable14[,LQ:=-1]


for(i in 1:nrow(OutputTable14))
{
  GetVals<-Part1A[`Currency Code`==OutputTable14$`Currency Code`[i] & `Department Code`==OutputTable14$`Department Code`[i]]
  QVec<-quantile(rep(GetVals$`Unit cost`,GetVals$Activity))
  OutputTable14$UQ[i]<-QVec[4]
  OutputTable14$LQ[i]<-QVec[2]
}

OutputTable14[,Year:=2014]

############## 15 

RawData<-fread("//nice/Data/Users/Private/TJONES/NHSReference/1516Data/1a Data.csv")


Part1A<-RawData
setnames(Part1A,"Org Code","Org code")
setnames(Part1A,"Service Code","Service code")
setnames(Part1A,"Actual Cost","Actual_cost")
setnames(Part1A,"Expected Cost","Expected_cost")
setnames(Part1A,"Mapping pot","Mapping_pot")

Part1A$Year<-2015
RawTab<-rbind(RawTab,Part1A,fill=T)


##
#`Supplier type`
OutputTable15<-Part1A[,.(Submissions=.N,FCE=sum(Activity),BedDays=sum(`Bed days`),Mean=weighted.mean(`Unit cost`,Activity)),by=.(`Currency Code`,`Department Code`)] ## added supplier type == OWN, not available for 17/18
OutputTable15[,AverageStayLength:=BedDays/FCE]
OutputTable15[,UQ:=-1]
OutputTable15[,LQ:=-1]


for(i in 1:nrow(OutputTable15))
{
  GetVals<-Part1A[`Currency Code`==OutputTable15$`Currency Code`[i] & `Department Code`==OutputTable15$`Department Code`[i]]
  QVec<-quantile(rep(GetVals$`Unit cost`,GetVals$Activity))
  OutputTable15$UQ[i]<-QVec[4]
  OutputTable15$LQ[i]<-QVec[2]
}
OutputTable15[,Year:=2015]

############# 16 

Part1<-fread("//nice/Data/Users/Private/TJONES/NHSReference/201617_ReferenceCostData/8 - Organisation level source data part 1/csv061117NoMff (1).csv")


Part1A<-Part1
setnames(Part1A,"Org Code","Org code")
setnames(Part1A,"Service Code","Service code")
setnames(Part1A,"Actual Cost","Actual_cost")
setnames(Part1A,"Expected Cost","Expected_cost")
setnames(Part1A,"Mapping pot","Mapping_pot")


Part1A$Year<-2016
RawTab<-rbind(RawTab,Part1A,fill=T)



FirstTest<-Part1[`Currency Code`=="HE32C" & !is.na(`Bed days`)]

##

OutputTable16<-Part1A[`Supplier type`=="OWN",.(Submissions=.N,FCE=sum(Activity),BedDays=sum(`Bed days`),Mean=weighted.mean(`Unit cost`,Activity)),by=.(`Currency Code`,`Department Code`)] ## added supplier type == OWN
OutputTable16[,AverageStayLength:=BedDays/FCE]
OutputTable16[,UQ:=-1]
OutputTable16[,LQ:=-1]


for(i in 1:nrow(OutputTable16))
{
  GetVals<-Part1A[`Currency Code`==OutputTable16$`Currency Code`[i] & `Department Code`==OutputTable16$`Department Code`[i]]
  QVec<-quantile(rep(GetVals$`Unit cost`,GetVals$Activity))
  OutputTable16$UQ[i]<-QVec[4]
  OutputTable16$LQ[i]<-QVec[2]
}

OutputTable16[,Year:=2016]

###### 17  

RawData<-fread("//nice/Data/Users/Private/TJONES/NHSReference/ReferenceData1718/1 - Data v2.csv")


Part1A<-RawData
setnames(Part1A,"Dept Code","Department Code")
setnames(Part1A,"Org Code","Org code")
setnames(Part1A,"Service Code","Service code")
setnames(Part1A,"Actual Cost","Actual_cost")
setnames(Part1A,"Expected Cost","Expected_cost")
setnames(Part1A,"Mapping pot","Mapping_pot")


Part1A$Year<-2017
RawTab<-rbind(RawTab,Part1A,fill=T)



##
#`Supplier type`
OutputTable17<-Part1A[,.(Submissions=.N,FCE=sum(Activity),BedDays=sum(`Bed days`),Mean=weighted.mean(`Unit cost`,Activity)),by=.(`Currency Code`,`Department Code`)] ## added supplier type == OWN, not available for 17/18
OutputTable17[,AverageStayLength:=BedDays/FCE]
OutputTable17[,UQ:=-1]
OutputTable17[,LQ:=-1]

### remove any cases with NA in activities

OutputTable17

for(i in 1:nrow(OutputTable17))
{
  GetVals<-Part1A[`Currency Code`==OutputTable17$`Currency Code`[i] & `Department Code`==OutputTable17$`Department Code`[i]]
  QVec<-quantile(rep(GetVals$`Unit cost`,GetVals$Activity))
  OutputTable17$UQ[i]<-QVec[4]
  OutputTable17$LQ[i]<-QVec[2]
}
OutputTable17[,Year:=2017]


RawData<-fread("//nice/Data/Users/Private/TJONES/NHSReference/1819Data/1 Data.csv")


Part1A<-RawData
setnames(Part1A,"Dept Code","Department Code")
setnames(Part1A,"Org Code","Org code")
setnames(Part1A,"Service Code","Service code")
setnames(Part1A,"Actual Cost","Actual_cost")
setnames(Part1A,"Expected Cost","Expected_cost")
setnames(Part1A,"Mapping pot","Mapping_pot")

Part1A$Activity<-as.numeric(Part1A$Activity)
Part1A$Mean<-as.numeric(Part1A$Mean)


Part1A$Year<-2018
RawTab<-rbind(RawTab,Part1A,fill=T)



##
#`Supplier type`
OutputTable18<-Part1A[,.(Submissions=.N,FCE=sum(Activity),BedDays=sum(`Bed days`),Mean=weighted.mean(`Unit cost`,Activity)),by=.(`Currency Code`,`Department Code`)] ## added supplier type == OWN, not available for 17/18
OutputTable18[,AverageStayLength:=BedDays/FCE]
OutputTable18[,UQ:=-1]
OutputTable18[,LQ:=-1]

### remove any cases with NA in activities

#OutputTable18 dont run outputtable for 18 as bed days no longer reported

# for(i in 1:nrow(OutputTable18))
'{
  GetVals<-Part1A[`Currency Code`==OutputTable17$`Currency Code`[i] & `Department Code`==OutputTable17$`Department Code`[i]]
  QVec<-quantile(rep(GetVals$`Unit cost`,GetVals$Activity))
  OutputTable17$UQ[i]<-QVec[4]
  OutputTable17$LQ[i]<-QVec[2]
}'
OutputTable18[,Year:=2018]




bindlist<-list(OutputTable08,OutputTable09,OutputTable10,OutputTable11,OutputTable12,OutputTable13,OutputTable14,OutputTable15,OutputTable16,OutputTable17) # no 2018

OutputTotal<-do.call("rbind",bindlist)

## Now load in Mapping Data
RootMap<-fread("//nice/Data/Users/Private/TJONES/NHSReference/Cost Type mapping.csv") # unable to find a similar document for 2018 so 2018 codes will have no root
# https://digital.nhs.uk/services/national-casemix-office/downloads-groupers-and-tools/payment---hrg4-2018-19-local-payment-grouper


RootMerge<-melt(RootMap[,c("HRG4 Root",(names(RootMap)[grepl("HRG RC",names(RootMap))])),with=F],id.vars = "HRG4 Root")
RootMerge<-RootMerge[nchar(value)>0]

RootMerge[,Year:=gsub("HRG RC","",variable)]
RootMerge[,Year:=gsub("\\d\\d$","",Year)]
RootMerge[,Year:=as.numeric(paste0("20",Year))]

LabelMerge<-melt(RootMap[,c("HRG4 Root",(names(RootMap)[grepl("HRG Label",names(RootMap))])),with=F],id.vars = "HRG4 Root")
LabelMerge<-LabelMerge[nchar(value)>0]

LabelMerge[,Year:=gsub("HRG Label RC","",variable)]
LabelMerge[,Year:=gsub("\\d\\d$","",Year)]
LabelMerge[,Year:=as.numeric(paste0("20",Year))]



setnames(RootMerge,c("RootCode","Discard","Currency Code","Year"))
setnames(LabelMerge,c("Label","Discard","Currency Code","Year"))

TotMerge<-merge(RootMerge[,.(RootCode,`Currency Code`,Year)],LabelMerge[,.(Label,`Currency Code`,Year)],by=c("Currency Code","Year"))


ncol(RootMap)

NewFormat<-RootMap[,c(1:3),with=F]
setnames(NewFormat,c("Root","Currency Code","Label"))
NewFormat$Year<-2006



for (i in 2:13)
{Grab<-RootMap[,c(1,(i*2):(i*2+1)),with=F]
setnames(Grab,c("Root","Currency Code","Label"))
Grab$Year<-2005+i
NewFormat<-rbind(NewFormat,Grab)}

NewFormat<-NewFormat[nchar(`Currency Code`)>0]

OT2<-merge(OutputTotal,NewFormat,by=c("Currency Code","Year"),all.x=T)

write.csv(OT2, "//nice/Data/Users/Private/TJONES/NHSReference/MasterDataFormatted.csv")
write.table(OT2, file = "//nice/Data/Users/Private/TJONES/NHSReference/TEXTtest.txt", sep = "\t",
            row.names = FALSE)

RawTab$Activity<-as.numeric(RawTab$Activity)
RawOutput<-merge(RawTab,NewFormat,by=c("Currency Code","Year"),all.x=T)
write.csv(RawOutput[Year>2011], "//nice/Data/Users/Private/TJONES/NHSReference/RAWDataMerged12plus18.csv")
