library(tidyverse)
library(dtplyr)
library(data.table)
library(Hmisc)

wtd.quant <- function(vals, wghts, t=7) {
  xx <- numeric(sum(wghts))
  for (i in 1:NROW(vals)) {
    xx[ifelse(i==1, 1, sum(wghts[1:(i-1)])+1):sum(wghts[1:i])] <- vals[i]
    }
  return(quantile(xx, type=t))
  }

RAW<-fread("Q:/Guidelines/In development/Diabetes/6. HE working/TJ Working/Misc/Reference Costs/RAWDataMerged12plus18.csv")
RAW[is.na(Activity),Activity:=0]
RAW[,`Department Code`:=gsub("^EI","EL",`Department Code`)]
RAW[,`Department Code`:=gsub("^NEI_L","NEL",`Department Code`)]
RAW[,`Department Code`:=gsub("NEI_S","NES",`Department Code`)]
RAW[,UnitCost2:=ifelse(is.na(`Unit cost`), ifelse(Activity==0, NA, `Actual_cost`/Activity), `Unit cost`)]
RAW[,V1:=NULL]

tblRAW <- RAW %>%
  as_tibble() %>% 
  filter(Activity > 0,
         is.na(`Supplier type`) | `Supplier type`!="OUT",
         `Currency Code`!="UZ01Z")
rm(RAW)

deps <- tblRAW %>%
  distinct(`Department Code`) %>%
  pull()
SCdeps <- c("CL", "NCL", "OPROC", "EM", "CHEM", "CC", "HCD", "RAD", "REHAB", "SPC", "RENAL", "MH", "CHS")


## TJ input
deps<-c("EL","NEL")

res <- tibble(Year=numeric(),
              `Department Code`=character(),
              `Currency Code`=character(),
              `Service code`=character(),
              submissions=numeric(),
              FCEs=numeric(),
              mean=numeric(),
              LQ=numeric(),
              UQ=numeric(),
              BedDays=numeric(),
              AvgBedDays=numeric())

for (yy in 2012:2018) {
  for (dd in deps) {
    print(paste(c(yy, dd)))
    subD <- tblRAW %>% 
      filter(Year == yy,
             `Department Code` == dd)
    if (NROW(subD)>0) {
      ss <- subD %>% 
        select(Year, `Org code`, `Department Code`, `Service code`, `Currency Code`, Activity, Actual_cost, `Bed days`) %>% 
        mutate(UnitCost = Actual_cost / Activity) %>% 
        {if (yy==2016 & !dd %in% SCdeps) {
          group_by(., Year, `Department Code`, `Currency Code`, `Org code`) %>% 
            summarise(Actual_cost = sum(Actual_cost),
                      Activity = sum(Activity),
                      UnitCost = Actual_cost/Activity,
                      `Bed days` = sum(`Bed days`),
                      NServs = n_distinct(`Service code`)) %>% 
            ungroup()}
          else .} %>% 
        group_by_at(c("Year", "Department Code", "Currency Code",
                      if(dd %in% SCdeps | yy==2012)"Service code")) %>% 
        summarise(submissions = ifelse(yy==2016 & !dd %in% SCdeps, sum(NServs), n_distinct(`Org code`)),
                  CSubmissions = n_distinct(`Org code`),
                  FCEs = sum(Activity),
                  mean = wtd.mean(UnitCost, Activity),
                  # LQ = wtd.quantile(UnitCost, Activity, type = "quantile")[2],
                  # UQ = wtd.quantile(UnitCost, Activity, type = "quantile")[4],
                  LQ = wtd.quant(UnitCost, Activity, t = ifelse(yy<2016, 6, 7))[2],
                  UQ = wtd.quant(UnitCost, Activity, t = ifelse(yy<2016, 6, 7))[4],
                  BedDays = sum(`Bed days`),
                  AvgBedDays = BedDays/FCEs) %>% 
        {if (!dd %in% SCdeps & yy!=2012) {mutate(., `Service code`="ALL")} else .} %>%
        arrange(Year, `Department Code`, `Currency Code`, `Service code`) 
      res <- res %>%
        bind_rows(ss)
    }
    
  }
  
  
}

write.csv(res, "Q:/Guidelines/In development/Diabetes/6. HE working/TJ Working/Misc/Reference Costs/scheduleTJ.csv", row.names = F)

## END


write.table(ss, "clipboard-9999", sep="\t")

yy = 2016
dd = "NEL"
tmp <- subD %>% filter(`Currency Code`=="AA22C")
vals <- tmp %>% pull(UnitCost2)
wghts <- tmp %>% pull(Activity)

vals <- 1:5
wghts <- c(1)

wtd.quantile(vals, wghts, type="quantile")
wtd.quant(vals, wghts)

