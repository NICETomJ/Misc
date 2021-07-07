library(tidyverse)
library(dtplyr)
library("data.table")

#### Generate File for GR

RAW<-fread("Q:/Guidelines/In development/Diabetes/6. HE working/TJ Working/Misc/Reference Costs/RAWDataMerged12plus18.csv")

## in 2018 values <9 are replaced with a * (NA) change this to 0
RAW[is.na(Activity),Activity:=0]


head(RAW)


RAW[,`Department Code`:=gsub("^EI","EL",`Department Code`)]
RAW[,`Department Code`:=gsub("^NEI_L","NEL",`Department Code`)]
RAW[,`Department Code`:=gsub("NEI_S","NES",`Department Code`)]

RAW[,UnitCost2:=ifelse(is.na(`Unit cost`), ifelse(Activity==0, NA, `Actual_cost`/Activity), `Unit cost`)]

# Remove codes with 0 activity (will cause some issues but unavoidable as all submissions with <9 have been set to 0 - GDPR?)

GRSummary<-RAW[Year>201 & Activity>0,.(Submissions=.N,FCE=sum(Activity),SD=sd(UnitCost2),LQ=quantile(UnitCost2,na.rm=T)[2],UQ=quantile(UnitCost2,na.rm=T)[4],TotalCost=sum(Actual_cost),TotalBedDays=sum(`Bed days`)),by=.(`Currency Code`,Year,`Department Code`)]#,`Service code`)]
GRSummary[,Mean:=TotalCost/FCE]
GRSummary[,AvBedDays:=TotalBedDays/FCE]

write.csv(GRSummary,"Q:/Guidelines/In development/Diabetes/6. HE working/TJ Working/Misc/Reference Costs/GRFile.csv")

tst <- RAW[`Currency Code`=="NZ50A" & `Department Code`=="EL" & `Service code`==501 & Year==2018 & Activity>0,.(Submissions=.N,FCE=sum(Activity),SD=sd(`Unit cost`),LQ=quantile(`Unit cost`,na.rm=T)[2],UQ=quantile(`Unit cost`,na.rm=T)[4],TotalCost=sum(Actual_cost),TotalBedDays=sum(`Bed days`)),by=.(`Currency Code`,Year,`Department Code`,`Service code`)]
tst <- RAW[`Currency Code`=="NZ50A" & `Department Code`=="EL" & `Service code`==501 & Year==2018]
tst <- RAW[is.na(`Unit cost`) | round(`Unit cost`, 2)!=round(UnitCost2, 2)]

GRSummary[`Currency Code`=="NZ50A" & `Department Code`=="EL" & `Service code`==501 & Year==2018]



## 2012
# Separate summary stats provided for services within organisations for all department codes
# Curr1, Org1, ServA, N=1, Mean=100
# Curr1, Org1, ServB, N=2, Mean=250
# Curr1, Org2, ServA, N=2, Mean=500
# --->
# Curr1, ServA: IQR = [100, 500]
# Curr1, ServB: IQR = [250, 250]

SCdeps <- c("CL", "NCL", "OPROC", "EM", "CHEM", "CC", "HCD", "RAD", "REHAB", "SPC", "RENAL", "MH", "CHS")

subD <- RAW %>% 
  as_tibble() %>% 
  filter(`Department Code` %in% SCdeps | Year==2012,
         Year == 2012,
         is.na(`Supplier type`) | `Supplier type`!="OUT",
         `Currency Code`!="UZ01Z") %>% 
  arrange(`Currency Code`, `Org code`, `Service code`)
ss <- subD %>% 
  select(Year, `Department Code`, `Service code`, `Currency Code`, `Org code`, Activity, UnitCost2) %>% 
  group_by(`Currency Code`, `Service code`) %>% 
  uncount(Activity) %>% 
  summarise(submissions = n_distinct(`Org code`),
            FCEs = n(),
            mean = mean(UnitCost2),
            LQ = quantile(UnitCost2, na.rm=T, type=6)[2],
            UQ = quantile(UnitCost2, na.rm=T, type=6)[4])
ss
write.table(ss, "clipboard-9999", sep="\t", row.names = FALSE)

## 2013-15
# Service codes dropped from output of all inpatient codes,
# but still used in calculation of quantiles
# Curr1, Org1, ServA, N=1, Mean=100
# Curr1, Org1, ServB, N=2, Mean=250
# Curr1, Org2, ServA, N=2, Mean=500
# --->
# Curr1: IQR = [175, 500]

subD <- RAW %>% 
  as_tibble() %>% 
  filter(`Department Code`=="EL",
         Year==2015,
         is.na(`Supplier type`) | `Supplier type`!="OUT",
         `Currency Code`!="UZ01Z") %>% 
  arrange(`Currency Code`, `Org code`)
ss <- subD %>% 
  select(`Currency Code`, `Org code`, Activity, UnitCost2) %>% 
  group_by(`Currency Code`) %>% 
  uncount(Activity) %>% 
  summarise(submissions = n_distinct(`Org code`),
            FCEs = n(),
            mean = mean(UnitCost2),
            LQ = quantile(UnitCost2, na.rm=T, type=6)[2],
            UQ = quantile(UnitCost2, na.rm=T, type=6)[4])
ss
write.table(ss, "clipboard-9999", sep="\t")

# 2 things changed in 2016:
# (1) they started pooling services within organisations to find an
#     organisation-level mean which was then repeated to calculate quantiles
# (2) they started using a different quantile algorithm (moves from SPSS to S [6 to 7 in quantile type])
#
# Curr1, Org1, ServA, N=1, Mean=100
# Curr1, Org1, ServB, N=2, Mean=250
# Curr1, Org2, ServA, N=2, Mean=500
# --->
# Curr1: IQR = [200, 500]

subD <- tblRAW %>% 
  as_tibble() %>% 
  filter(`Department Code`=="NEL",
         Year==2016,
         is.na(`Supplier type`) | `Supplier type`!="OUT",
         `Currency Code`=="AA23G") %>% 
  arrange(`Currency Code`, `Org code`)
ss <- subD %>% 
  select(`Currency Code`, `Org code`, `Service code`, Activity, UnitCost2) %>% 
  group_by(`Currency Code`, `Org code`) %>% 
  summarise(submissions1 = n(),
            tmp = sum(UnitCost2*Activity)/sum(Activity),
            Activity = sum(Activity)) %>% 
  rename(UnitCost2=tmp) %>% 
  ungroup() %>% 
  group_by(`Currency Code`) %>% 
  uncount(Activity) %>% 
  summarise(submissions = n_distinct(`Org code`),
            FCEs = n(),
            mean = mean(UnitCost2),
            LQ = quantile(UnitCost2, na.rm=T, type=7)[2],
            UQ = quantile(UnitCost2, na.rm=T, type=7)[4])
ss

vals <- subD %>% pull(UnitCost2)
wghts <- subD %>% pull(Activity)
wtd.quantile(vals, wghts, type="quantile")
wtd.quant(vals, wghts, t=7)
wtd.mean(vals, wghts)
