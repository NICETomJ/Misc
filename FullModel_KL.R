### An implementation of the UKPDS OM2 risk equations

# Load All Required Packages ####

install.packages("data.table")
install.packages("reshape2")
install.packages("ggplot2")
install.packages("rstudioapi")

library("data.table") 
library("reshape2") # for summary charts
library("ggplot2") # for summary charts
library("rstudioapi")
# Load all required functions ####

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #  set wd to current location
  

source("RequiredFunctions.R") # script with required functions

# download bootstrap parameters and 1000 simulations of treatment effects, timepath parameters
GetBSVals() # for risk equation bootstraps
GetCentralTimePathParameters() # load in the parameters for risk factor time-paths


# Set up Model input tables and define Years modelled and number of patients ####

IntensificationLevel<-"Second Intensification"  # which intensification level is being modelled?  possible inputs are: "Initial Therapy" First Intensification", "Second Intensification"
ADDorREPLACE<-"Add"  # should be = "Add  or "Replace" depending on which intensification path is being used

InitialTherapyHbA1cEffect<-fread(paste0("./TreatmentEffect/",IntensificationLevel,"/HbA1c1000.csv")) #also called InitialTherapy... if Intensification set at higher levels 
InitialTherapyWeightEffect<-fread(paste0("./TreatmentEffect/",IntensificationLevel,"/Weight1000.csv"))#also called InitialTherapy... if Intensification set at higher levels 

if(ADDorREPLACE=="Add")
{
IntensificationPath<-fread("./TreatmentEffect/Initial Therapy/IntensificationNoHyposMetSUMetSUNPH.csv")}else
  {IntensificationPath<-fread("./TreatmentEffect/Initial Therapy/IntensificationNoHyposMetMetNPH.csv")}

FirstIntensification<<-as.matrix(IntensificationPath[1,]  )  ## currently met, SU +met, Met+SU+NPH
SecondIntensification<<-as.matrix(IntensificationPath[2,] ) ## currently met, SU +met, Met+SU+NPH


ModelRunYears<-40
NSim<-1000 # this is number of 'inner loops' how many times are you running the same patient with same parameters through. 
BootStrapVol<-2 # how many bootstraps will you run? 0 if using means
BootStrapRepeats<-10
ChosenTreatment<-"Metformin-NPH insulin"    ######"Metformin-NPH insulin-sulfonylurea"
GetTreatmentMatrix(ChosenTreatment,1) # treatment in character format (e.g. "metformin", followed by the sample number (2:1001 (or 1 for mean)))
set.seed(1) # to ensure reproducible random numbers
if(BootStrapVol>0){
  BootStraps<-BootStrapList(BootStrapVol)
  
}else{
  BootStraps<-1
} # Select bootstraps (see function BootStrapList for more details)
BootStraps<-rep(BootStraps,each=BootStrapRepeats)


###Need to choose starting population - dependent on IntensificationLEvel chosen
set.seed(42)
PopChoose<-if(IntensificationLevel=="Initial Therapy"){1}else if(IntensificationLevel=="First Intensification"){2}else{3}
PopulateModel(paste0("P",PopChoose,"_100000")) # get population, quicker to do this outside the bootstrap loops. In the future 

GetTimePaths() # placeholder applying treatment effect then modelling risk factor time paths

# it is now necessary to identify the subgroups
PrevEvent<-(IHDHist[,1]+CHFHist[,1]+StrokeHist[,1]+MIHist[,1])>0 # included renal failure as CV history, anyone wiht a history of CV events is high risk
AgeQualifies<-(Age>55 & Female==0)|(Age>60 & Female==1) # high risk CV requires age + at least one 
RiskFactor<-SBP[,1]>16|LDL[,1]>35|eGFR[,1]<4.5|MICALB[,1]==1|(BMI[,1]>35 & Afro==0 &Indian==0)|(BMI[,1]>32 & Afro+Indian>0)|Smoker[,1]==1 # one of these risk factors also required

CVSubgroup<-PrevEvent|(AgeQualifies&RiskFactor)
BMISubgroup<-BMI[,1]>30
PrimaryHighRiskSubgroup<-(AgeQualifies&RiskFactor)&!PrevEvent # exclude secondary prevention from here
SecondarySubgroup<-PrevEvent


#Run Model ####
StartTheClock<-Sys.time()
for ( BootS in 1:length(BootStraps))
  
{ AgeDiag<-matrix(AgeDiag) # must return ncol AgeDiag==1 (previously NULL)
  # remove histories
  if(BootS==1) # need to store Y1 values if looping bootstraps, because otherwise when someone has an event in first year it will be added to hist
    #then when the next bootstrap is started the event history % won't be right
{MIHistSave<-MIHist[,1]
IHDHistSave<-IHDHist[,1]
CHFHistSave<-CHFHist[,1]
StrokeHistSave<-StrokeHist[,1]
BlindHistSave<-BlindHist[,1]
RenalHistSave<-RenalHist[,1]
UlcerHistSave<-UlcerHist[,1]
AmpHistSave<-AmpHist[,1]
AgeStore<-Age[,1]  
DiabYearsStore<-DiabYears

t1<-Sys.time() # time benchmark (not essential)

}else
  {ClearLastRunEvents() # reset correct baseline values for next bootstrap
    MIHist[which(MIHistSave==1),]<-1
IHDHist[which(IHDHistSave==1),]<-1
CHFHist[which(CHFHistSave==1),]<-1
StrokeHist[which(StrokeHistSave==1),]<-1
BlindHist[which(BlindHistSave==1),]<-1
RenalHist[which(RenalHistSave==1),]<-1
UlcerHist[which(UlcerHistSave==1),]<-1
AmpHist[which(AmpHistSave==1),]<-1
  Age[,1]<-AgeStore
  AgeNY<-Age+1
  DiabYears<-DiabYearsStore
}
  
  
  
  GetBS(BootStraps[BootS])
  
  
  
  # Model ####
  
  for (i in 1:ModelRunYears)
    
  {
    #### In order to detemine which death equation should be used important to find out what the histories prior to this year were (did patients have a hsitory of any event?)
    Hist<-IHDHist[,i]+CHFHist[,i]+StrokeHist[,i]+MIHist[,i]+BlindHist[,i]+RenalHist[,i]+AmpHist[,i]+UlcerHist[,i]
    DeathRows<-Death[,i]==0 # technically this is actually alive rows
    
    ## now run equations in random order
    # This is a crucial place where this model diverges from the official implementation as the equations are run in the same 
    #(random for each year) order for every patient in any given year - this vastly improves runtimes as it reduces the # of function calls
    # testing against the official implementation confirms that this does not affect the results if each cohort is run through 
    # a sufficient # of times
  
    Equations<-c("CHFfun","IHDfun","MI1fun","MI2fun","Stroke1fun","Stroke2fun","Blindfun","Ulcerfun","Amp1fun","Amp2fun","Renalfun")
    
    for (Eq in sample(Equations))
    {get(Eq)()}
    
    # for event equations combine MI1, MI2 and Stroke1 and Stroke2
    
    MIDeathEq[,i]<-MI1[,i]+MI2[,i]
    StrokeDeathEq[,i]<-Stroke1[,i]+Stroke2[,i]
    
    # now determine if an event has happened
    
    EventCurrent<-IHD[,i]+CHF[,i]+StrokeDeathEq[,i]+MIDeathEq[,i]+Renal[,i]+Amp1[,i]+Amp2[,i] # No Ulcer or blindness forincreased risk of death in year (see suppl. material)
    
    
    
    ################ Now death equations
    
    ## Death Equation 1 if no events and no history of events
    
    ### update DeathHist
    Death1rows<-which(Hist==0 & EventCurrent==0 & Death[,i]==0)
    Death2rows<-which(Hist==0 & EventCurrent>0 & Death[,i]==0)
    Death3rows<-which(Hist>0 & EventCurrent==0 & Death[,i]==0)
    Death4rows<-which(Hist>0 & EventCurrent>0 & Death[,i]==0)
    
    # Death equation 1
    Death[Death1rows,i]<-rbinom(length(Death1rows),1,
                                1-exp(((exp(Death1lambda+DeathFunction(Death1para,Death1rows,i)))*(exp(Death1phi*Age[Death1rows,1])-1))/Death1phi-((exp(Death1lambda+DeathFunction(Death1para,Death1rows,i)))*(exp(Death1phi*AgeNY[Death1rows,1])-1))/Death1phi)) # Eq1
    
    
    Death[Death2rows,i]<-rbinom(length(Death2rows),1,1-logistic(exp(-(Death2lambda+DeathFunction(Death2para,Death2rows,i)))))
    
    # Death Equation 2 
    
   
    
    Death[Death3rows,i]<-rbinom(length(Death3rows),1,1-exp(((exp(Death3lambda+DeathFunction(Death3para,Death3rows,i)))*(exp(Death3phi*Age[Death3rows,1])-1))/Death3phi-((exp(Death3lambda+DeathFunction(Death3para,Death3rows,i)))*(exp(Death3phi*AgeNY[Death3rows,1])-1))/Death3phi)
    ) 
    # Death Equation 3
    
    
    
    Death[Death4rows,i]<-rbinom(length(Death4rows),1,1-logistic(exp(-(Death4lambda+DeathFunction(Death4para,Death4rows,i))))
    ) 
    # Death Equation 4
    
    DeathHist[which(Death[,i]==1),1]<-1
    # Now also make person dead next year
    if(i<ModelRunYears){
      Death[which(Death[,i]==1),i+1]<-1}
    
    
    
    
    ### Now add one year to Age (and AgeNY) and DiabYears
    Age[,1]<-Age[,1]+1
    AgeNY<-Age+1
    DiabYears<-DiabYears+1
    
    #Now begin following loop
  }
  
  
 
  
  ## Could calculate costs and QALYs at this stage - leave for now
  #CQ<-CalculateCostQALYbasic()
  #CQ$Bootstrap<-BootStraps[BootS]
  
  
  # Now create the Multi-state models with 
  ## get transition rates for MSM
  #firstly the starting state (year 0)
  
  #firstly the Overall MSM
  
  InitialVals<-2^(MIHistSave>0)*5^(IHDHistSave>0)*7^(CHFHistSave>0)*11^(StrokeHistSave>0)*
    17^(BlindHistSave>0)*19^(UlcerHistSave>0)*23^(AmpHistSave>0)*31^(RenalHistSave>0)
  ## now the rest of the years
  
  t<-2^(MIHist>0)*3^(MIHist>1)*5^(IHDHist>0)*7^(CHFHist>0)*11^(StrokeHist>0)*13^(StrokeHist>1)*
    17^(BlindHist>0)*19^(UlcerHist>0)*23^(AmpHist>0)*29^(AmpHist>1)*31^(RenalHist>0)*
    (-1)^Death
  
   t<-cbind(as.matrix(InitialVals),t)
  
  
  for (i in 1:((ModelRunYears))) # generate the MSM state transitions, intensification levels and average BMI over 25 
  { #MSM state transitions
    tsub<-data.table(t[,c(i,i+1)])[,.(Vol=.N),by=.(V1,V2)]
    tsub[,Prop:=Vol/sum(Vol),by=.(V1)]
    tsub[,BaseYear:=(i-1)]
    if(i==1)
    {outputstates<-tsub}else{outputstates<-rbind(outputstates,tsub)}
    
    #What proportion of people are at each intensification stage?
    { if(i<ModelRunYears  & sum(Death[,i]==0)>1) # can't run this for final year as i+1 = MOdelRunYears+1 = out of bounds, needs at least 1 patient alive
      {LastYearAlive<-if(i==1){1:NSim}else{which(Death[,(i-1)]==0)}
      
      Stage<-data.table(TreatmentStage[LastYearAlive,c(i,i+1)])[,.(Vol=.N),by=.(StartStage=V1,EndStage=V2)]
      Stage[,Year:=i]
      if(i==1)
      {OutputStage<-Stage}else{OutputStage<-rbind(OutputStage,Stage)}
    }}
    
    ## for people who are alive, what is the average BMI>25
    {
      LastYearAlive<-if(i==1){1:NSim}else{which(Death[,(i-1)]==0)}
      Over25<-data.table(Year=i,TotalOver=sum(pmax(BMI[which(Death[,i]==0),i]-25,0)))
      if(i==1)
      {OutputOver25<-Over25}else{OutputOver25<-rbind(OutputOver25,Over25)}
    }
    
    
    
  }
 
   
   
   
   
   
   
  outputstates[,ModelNumber:=BootS]
  OutputStage[,ModelNumber:=BootS]
  OutputOver25[,ModelNumber:=BootS]
  
  # Now store the values that have been created
  
  if(BootS%%BootStrapRepeats==1|BootStrapRepeats==1)# first loop of bootstrap? create output tables
  {
  MSM<-outputstates
  AllStates<-OutputStage
  AllOver25<-OutputOver25
  }else{ #for all subsequent loops
   
    
    MSM<-rbind(MSM,outputstates)
    AllStates<-rbind(AllStates,OutputStage)
  AllOver25<-rbind(AllOver25,OutputOver25)
  }
  if(BootS==BootStrapRepeats) # if all bootstrap repeats completed then : first time create master bs store
  {
    MSM2<-MSM[,.(Total=sum(Vol)/(BootStrapRepeats)),by=.(V1,V2,BaseYear)]
    MSM2[,BootStrap:=BootStraps[BootS]]
    
    AllStatesOutput<-AllStates[,.(Total=sum(Vol)/(BootStrapRepeats),BootStrap=BootStraps[BootS]),by=.(StartStage,EndStage,Year)]

    AllBMIOutput<-AllOver25[,.(Total=sum(TotalOver)/(BootStrapRepeats),BootStrap=BootStraps[BootS]),by=.(Year)]
    
    
  }else if(BootS%%BootStrapRepeats==0) # for subsequent bootstraps bind te results
  {
    MSM2<-rbind(MSM2,MSM[,.(Total=sum(Vol)/(BootStrapRepeats),BootStrap=BootStraps[BootS]),by=.(V1,V2,BaseYear)])
    rm(MSM) # remove individual multi state models
    AllStatesOutput<-rbind(AllStatesOutput,AllStates[,.(Total=sum(Vol)/(BootStrapRepeats),BootStrap=BootStraps[BootS]),by=.(StartStage,EndStage,Year)])
    rm(AllStates)
    AllBMIOutput<-rbind(AllBMIOutput,AllOver25[,.(Total=sum(TotalOver)/(BootStrapRepeats),BootStrap=BootStraps[BootS]),by=.(Year)])
    rm(AllOver25)
    gc()
    t2<-Sys.time()
    print(t2-t1) 
    t1<-Sys.time()# how long did the bootstrap take to run
  }
  
  
  ### Now repeat the MSM process but for the BMI subgroup only
  InitialVals<-2^(MIHistSave[BMISubgroup]>0)*5^(IHDHistSave[BMISubgroup]>0)*7^(CHFHistSave[BMISubgroup]>0)*11^(StrokeHistSave[BMISubgroup]>0)*
    17^(BlindHistSave[BMISubgroup]>0)*19^(UlcerHistSave[BMISubgroup]>0)*23^(AmpHistSave[BMISubgroup]>0)*31^(RenalHistSave[BMISubgroup]>0)
  ## now the rest of the years
  
  t<-2^(MIHist[BMISubgroup,]>0)*3^(MIHist[BMISubgroup,]>1)*5^(IHDHist[BMISubgroup,]>0)*7^(CHFHist[BMISubgroup,]>0)*11^(StrokeHist[BMISubgroup,]>0)*13^(StrokeHist[BMISubgroup,]>1)*
    17^(BlindHist[BMISubgroup,]>0)*19^(UlcerHist[BMISubgroup,]>0)*23^(AmpHist[BMISubgroup,]>0)*29^(AmpHist[BMISubgroup,]>1)*31^(RenalHist[BMISubgroup,]>0)*
    (-1)^Death[BMISubgroup,]
  
  t<-cbind(as.matrix(InitialVals),t)
  
  
  for (i in 1:((ModelRunYears))) # generate the MSM state transitions, intensification levels and average BMI over 25 
  { #MSM state transitions
    tsub<-data.table(t[,c(i,i+1)])[,.(Vol=.N),by=.(V1,V2)]
    tsub[,Prop:=Vol/sum(Vol),by=.(V1)]
    tsub[,BaseYear:=(i-1)]
    if(i==1)
    {outputstatesBMI<-tsub}else{outputstatesBMI<-rbind(outputstatesBMI,tsub)}
    
    #What proportion of people are at each intensification stage?
    { if(i<ModelRunYears & sum(Death[BMISubgroup,i]==0)>1) # can't run this for final year as i+1 = MOdelRunYears+1 = out of bounds
    {LastYearAlive<-if(i==1){which(BMISubgroup)}else{which(Death[BMISubgroup,(i-1)]==0)}
    
      
      StageBMI<-data.table(TreatmentStage[LastYearAlive,c(i,i+1)])[,.(Vol=.N),by=.(StartStage=V1,EndStage=V2)]
    StageBMI[,Year:=i]
    if(i==1)
    {OutputStageBMI<-StageBMI}else{OutputStageBMI<-rbind(OutputStageBMI,StageBMI)}
    }}
    
    ## for people who are alive, what is the average BMI>25
    {LastYearAlive<-if(i==1){which(BMISubgroup)}else{which(Death[BMISubgroup,(i-1)]==0)}
      Over25BMI<-data.table(Year=i,TotalOver=sum(pmax(BMI[LastYearAlive,i]-25,0)))
      if(i==1)
      {OutputOver25BMI<-Over25BMI}else{OutputOver25BMI<-rbind(OutputOver25BMI,Over25BMI)}
    }
    
    
    
  }
  
  
  
  
  
  outputstatesBMI[,ModelNumber:=BootS]
  OutputStageBMI[,ModelNumber:=BootS]
  OutputOver25BMI[,ModelNumber:=BootS]
  
  # Now store the values that have been created
  
  if(BootS%%BootStrapRepeats==1|BootStrapRepeats==1)# first loop of bootstrap? create output tables
  { 
    #then BMI
   
    MSMBMI<-outputstatesBMI
    AllStatesBMI<-OutputStageBMI
    AllOver25BMI<-OutputOver25BMI
    
    
    
    
  }else{ #for all subsequent loops
   
    #then BMI
    MSMBMI<-rbind(MSMBMI,outputstatesBMI)
    AllStatesBMI<-rbind(AllStatesBMI,OutputStageBMI)
    AllOver25BMI<-rbind(AllOver25BMI,OutputOver25BMI)
    
    
  }
  
  if(BootS==BootStrapRepeats) # if all bootstrap repeats completed then : first time create master bs store
  {
    MSM2BMI<-MSMBMI[,.(Total=sum(Vol)/(BootStrapRepeats)),by=.(V1,V2,BaseYear)]
    MSM2BMI[,BootStrap:=BootStraps[BootS]]
    
    AllStatesOutputBMI<-AllStatesBMI[,.(Total=sum(Vol)/(BootStrapRepeats),BootStrap=BootStraps[BootS]),by=.(StartStage,EndStage,Year)]
    
    AllBMIOutputBMI<-AllOver25BMI[,.(Total=sum(TotalOver)/(BootStrapRepeats),BootStrap=BootStraps[BootS]),by=.(Year)]
    
    
  }else if(BootS%%BootStrapRepeats==0) # for subsequent bootstraps bind te results
  {
    MSM2BMI<-rbind(MSM2BMI,MSMBMI[,.(Total=sum(Vol)/(BootStrapRepeats),BootStrap=BootStraps[BootS]),by=.(V1,V2,BaseYear)])
    rm(MSMBMI) # remove individual multi state models
    AllStatesOutputBMI<-rbind(AllStatesOutputBMI,AllStatesBMI[,.(Total=sum(Vol)/(BootStrapRepeats),BootStrap=BootStraps[BootS]),by=.(StartStage,EndStage,Year)])
    rm(AllStatesBMI)
    AllBMIOutputBMI<-rbind(AllBMIOutputBMI,AllOver25BMI[,.(Total=sum(TotalOver)/(BootStrapRepeats),BootStrap=BootStraps[BootS]),by=.(Year)])
    rm(AllOver25BMI)
    gc()
    t2<-Sys.time()
    print(t2-t1) 
    t1<-Sys.time()# how long did the bootstrap take to run
  }
  
  
  ### Now the same for the High CV  risk subgroup (Primary + Secondary)
  InitialVals<-2^(MIHistSave[CVSubgroup]>0)*5^(IHDHistSave[CVSubgroup]>0)*7^(CHFHistSave[CVSubgroup]>0)*11^(StrokeHistSave[CVSubgroup]>0)*
    17^(BlindHistSave[CVSubgroup]>0)*19^(UlcerHistSave[CVSubgroup]>0)*23^(AmpHistSave[CVSubgroup]>0)*31^(RenalHistSave[CVSubgroup]>0)
  ## now the rest of the years
  
  t<-2^(MIHist[CVSubgroup,]>0)*3^(MIHist[CVSubgroup,]>1)*5^(IHDHist[CVSubgroup,]>0)*7^(CHFHist[CVSubgroup,]>0)*11^(StrokeHist[CVSubgroup,]>0)*13^(StrokeHist[CVSubgroup,]>1)*
    17^(BlindHist[CVSubgroup,]>0)*19^(UlcerHist[CVSubgroup,]>0)*23^(AmpHist[CVSubgroup,]>0)*29^(AmpHist[CVSubgroup,]>1)*31^(RenalHist[CVSubgroup,]>0)*
    (-1)^Death[CVSubgroup,]
  
  t<-cbind(as.matrix(InitialVals),t)
  
  
  for (i in 1:((ModelRunYears))) # generate the MSM state transitions, intensification levels and average CV over 25 
  { #MSM state transitions
    tsub<-data.table(t[,c(i,i+1)])[,.(Vol=.N),by=.(V1,V2)]
    tsub[,Prop:=Vol/sum(Vol),by=.(V1)]
    tsub[,BaseYear:=(i-1)]
    if(i==1)
    {outputstatesCV<-tsub}else{outputstatesCV<-rbind(outputstatesCV,tsub)}
    
    #What proportion of people are at each intensification stage?
    { if(i<ModelRunYears & sum(Death[CVSubgroup,i]==0)>1) # can't run this for final year as i+1 = MOdelRunYears+1 = out of bounds
    {LastYearAlive<-if(i==1){which(CVSubgroup)}else{which(Death[CVSubgroup,(i-1)]==0)}
      
      StageCV<-data.table(TreatmentStage[LastYearAlive,c(i,i+1)])[,.(Vol=.N),by=.(StartStage=V1,EndStage=V2)]
    StageCV[,Year:=i]
    if(i==1)
    {OutputStageCV<-StageCV}else{OutputStageCV<-rbind(OutputStageCV,StageCV)}
    }}
    
    ## for people who are alive, what is the average CV>25
    {LastYearAlive<-if(i==1){which(CVSubgroup)}else{which(Death[CVSubgroup,(i-1)]==0)}
      
      
      Over25CV<-data.table(Year=i,TotalOver=sum(pmax(BMI[LastYearAlive,i]-25,0)))
      if(i==1)
      {OutputOver25CV<-Over25CV}else{OutputOver25CV<-rbind(OutputOver25CV,Over25CV)}
    }
    
    
    
  }
  
  
  
  
  
  outputstatesCV[,ModelNumber:=BootS]
  OutputStageCV[,ModelNumber:=BootS]
  OutputOver25CV[,ModelNumber:=BootS]
  
  # Now store the values that have been created
 
  
  if(BootS%%BootStrapRepeats==1|BootStrapRepeats==1)# first loop of bootstrap? create output tables
  { 
    #then CV
    
    MSMCV<-outputstatesCV
    AllStatesCV<-OutputStageCV
    AllOver25CV<-OutputOver25CV
    
    
    
    
  }else{ #for all subsequent loops
    
    #then CV
    MSMCV<-rbind(MSMCV,outputstatesCV)
    AllStatesCV<-rbind(AllStatesCV,OutputStageCV)
    AllOver25CV<-rbind(AllOver25CV,OutputOver25CV)
    
    
  }
  
  if(BootS==BootStrapRepeats) # if all bootstrap repeats completed then : first time create master bs store
  {
    MSM2CV<-MSMCV[,.(Total=sum(Vol)/(BootStrapRepeats)),by=.(V1,V2,BaseYear)]
    MSM2CV[,BootStrap:=BootStraps[BootS]]
    
    AllStatesOutputCV<-AllStatesCV[,.(Total=sum(Vol)/(BootStrapRepeats),BootStrap=BootStraps[BootS]),by=.(StartStage,EndStage,Year)]
    
    AllBMIOutputCV<-AllOver25CV[,.(Total=sum(TotalOver)/(BootStrapRepeats),BootStrap=BootStraps[BootS]),by=.(Year)]
    
    
  }else if(BootS%%BootStrapRepeats==0) # for subsequent bootstraps bind te results
  {
    MSM2CV<-rbind(MSM2CV,MSMCV[,.(Total=sum(Vol)/(BootStrapRepeats),BootStrap=BootStraps[BootS]),by=.(V1,V2,BaseYear)])
    rm(MSMCV) # remove individual multi state models
    AllStatesOutputCV<-rbind(AllStatesOutputCV,AllStatesCV[,.(Total=sum(Vol)/(BootStrapRepeats),BootStrap=BootStraps[BootS]),by=.(StartStage,EndStage,Year)])
    rm(AllStatesCV)
    AllBMIOutputCV<-rbind(AllBMIOutputCV,AllOver25CV[,.(Total=sum(TotalOver)/(BootStrapRepeats),BootStrap=BootStraps[BootS]),by=.(Year)])
    rm(AllOver25CV)
    gc()
    t2<-Sys.time()
    print(t2-t1) 
    t1<-Sys.time()# how long did the bootstrap take to run
  }
  
  ## Now output primary prevention, high CV risk
  
  
  
  ### Now the same for the High PrimaryHighRisk  risk subgroup (Primary + Secondary)
  InitialVals<-2^(MIHistSave[PrimaryHighRiskSubgroup]>0)*5^(IHDHistSave[PrimaryHighRiskSubgroup]>0)*7^(CHFHistSave[PrimaryHighRiskSubgroup]>0)*11^(StrokeHistSave[PrimaryHighRiskSubgroup]>0)*
    17^(BlindHistSave[PrimaryHighRiskSubgroup]>0)*19^(UlcerHistSave[PrimaryHighRiskSubgroup]>0)*23^(AmpHistSave[PrimaryHighRiskSubgroup]>0)*31^(RenalHistSave[PrimaryHighRiskSubgroup]>0)
  ## now the rest of the years
  
  t<-2^(MIHist[PrimaryHighRiskSubgroup,]>0)*3^(MIHist[PrimaryHighRiskSubgroup,]>1)*5^(IHDHist[PrimaryHighRiskSubgroup,]>0)*7^(CHFHist[PrimaryHighRiskSubgroup,]>0)*11^(StrokeHist[PrimaryHighRiskSubgroup,]>0)*13^(StrokeHist[PrimaryHighRiskSubgroup,]>1)*
    17^(BlindHist[PrimaryHighRiskSubgroup,]>0)*19^(UlcerHist[PrimaryHighRiskSubgroup,]>0)*23^(AmpHist[PrimaryHighRiskSubgroup,]>0)*29^(AmpHist[PrimaryHighRiskSubgroup,]>1)*31^(RenalHist[PrimaryHighRiskSubgroup,]>0)*
    (-1)^Death[PrimaryHighRiskSubgroup,]
  
  t<-cbind(as.matrix(InitialVals),t)
  
  
  for (i in 1:((ModelRunYears))) # generate the MSM state transitions, intensification levels and average PrimaryHighRisk over 25 
  { #MSM state transitions
    tsub<-data.table(t[,c(i,i+1)])[,.(Vol=.N),by=.(V1,V2)]
    tsub[,Prop:=Vol/sum(Vol),by=.(V1)]
    tsub[,BaseYear:=(i-1)]
    if(i==1)
    {outputstatesPrimaryHighRisk<-tsub}else{outputstatesPrimaryHighRisk<-rbind(outputstatesPrimaryHighRisk,tsub)}
    
    #What proportion of people are at each intensification stage?
    { if(i<ModelRunYears & sum(Death[PrimaryHighRiskSubgroup,i]==0)>1) # can't run this for final year as i+1 = MOdelRunYears+1 = out of bounds
    {LastYearAlive<-if(i==1){which(PrimaryHighRiskSubgroup)}else{which(Death[PrimaryHighRiskSubgroup,(i-1)]==0)}
    
    StagePrimaryHighRisk<-data.table(TreatmentStage[LastYearAlive,c(i,i+1)])[,.(Vol=.N),by=.(StartStage=V1,EndStage=V2)]
    StagePrimaryHighRisk[,Year:=i]
    if(i==1)
    {OutputStagePrimaryHighRisk<-StagePrimaryHighRisk}else{OutputStagePrimaryHighRisk<-rbind(OutputStagePrimaryHighRisk,StagePrimaryHighRisk)}
    }}
    
    ## for people who are alive, what is the average PrimaryHighRisk>25
    {LastYearAlive<-if(i==1){which(PrimaryHighRiskSubgroup)}else{which(Death[PrimaryHighRiskSubgroup,(i-1)]==0)}
      
      
      Over25PrimaryHighRisk<-data.table(Year=i,TotalOver=sum(pmax(BMI[LastYearAlive,i]-25,0)))
      if(i==1)
      {OutputOver25PrimaryHighRisk<-Over25PrimaryHighRisk}else{OutputOver25PrimaryHighRisk<-rbind(OutputOver25PrimaryHighRisk,Over25PrimaryHighRisk)}
    }
    
    
    
  }
  
  
  
  
  
  outputstatesPrimaryHighRisk[,ModelNumber:=BootS]
  OutputStagePrimaryHighRisk[,ModelNumber:=BootS]
  OutputOver25PrimaryHighRisk[,ModelNumber:=BootS]
  
  # Now store the values that have been created
  
  
  if(BootS%%BootStrapRepeats==1|BootStrapRepeats==1)# first loop of bootstrap? create output tables
  { 
    #then PrimaryHighRisk
    
    MSMPrimaryHighRisk<-outputstatesPrimaryHighRisk
    AllStatesPrimaryHighRisk<-OutputStagePrimaryHighRisk
    AllOver25PrimaryHighRisk<-OutputOver25PrimaryHighRisk
    
    
    
    
  }else{ #for all subsequent loops
    
    #then PrimaryHighRisk
    MSMPrimaryHighRisk<-rbind(MSMPrimaryHighRisk,outputstatesPrimaryHighRisk)
    AllStatesPrimaryHighRisk<-rbind(AllStatesPrimaryHighRisk,OutputStagePrimaryHighRisk)
    AllOver25PrimaryHighRisk<-rbind(AllOver25PrimaryHighRisk,OutputOver25PrimaryHighRisk)
    
    
  }
  
  if(BootS==BootStrapRepeats) # if all bootstrap repeats completed then : first time create master bs store
  {
    MSM2PrimaryHighRisk<-MSMPrimaryHighRisk[,.(Total=sum(Vol)/(BootStrapRepeats)),by=.(V1,V2,BaseYear)]
    MSM2PrimaryHighRisk[,BootStrap:=BootStraps[BootS]]
    
    AllStatesOutputPrimaryHighRisk<-AllStatesPrimaryHighRisk[,.(Total=sum(Vol)/(BootStrapRepeats),BootStrap=BootStraps[BootS]),by=.(StartStage,EndStage,Year)]
    
    AllBMIOutputPrimaryHighRisk<-AllOver25PrimaryHighRisk[,.(Total=sum(TotalOver)/(BootStrapRepeats),BootStrap=BootStraps[BootS]),by=.(Year)]
    
    
  }else if(BootS%%BootStrapRepeats==0) # for subsequent bootstraps bind te results
  {
    MSM2PrimaryHighRisk<-rbind(MSM2PrimaryHighRisk,MSMPrimaryHighRisk[,.(Total=sum(Vol)/(BootStrapRepeats),BootStrap=BootStraps[BootS]),by=.(V1,V2,BaseYear)])
    rm(MSMPrimaryHighRisk) # remove individual multi state models
    AllStatesOutputPrimaryHighRisk<-rbind(AllStatesOutputPrimaryHighRisk,AllStatesPrimaryHighRisk[,.(Total=sum(Vol)/(BootStrapRepeats),BootStrap=BootStraps[BootS]),by=.(StartStage,EndStage,Year)])
    rm(AllStatesPrimaryHighRisk)
    AllBMIOutputPrimaryHighRisk<-rbind(AllBMIOutputPrimaryHighRisk,AllOver25PrimaryHighRisk[,.(Total=sum(TotalOver)/(BootStrapRepeats),BootStrap=BootStraps[BootS]),by=.(Year)])
    rm(AllOver25PrimaryHighRisk)
    gc()
    t2<-Sys.time()
    print(t2-t1) 
    t1<-Sys.time()# how long did the bootstrap take to run
  }
  
  
  ####Now output secondary  prevention only
  
  
  ### Now the same for the High Secondary  risk subgroup (Primary + Secondary)
  InitialVals<-2^(MIHistSave[SecondarySubgroup]>0)*5^(IHDHistSave[SecondarySubgroup]>0)*7^(CHFHistSave[SecondarySubgroup]>0)*11^(StrokeHistSave[SecondarySubgroup]>0)*
    17^(BlindHistSave[SecondarySubgroup]>0)*19^(UlcerHistSave[SecondarySubgroup]>0)*23^(AmpHistSave[SecondarySubgroup]>0)*31^(RenalHistSave[SecondarySubgroup]>0)
  ## now the rest of the years
  
  t<-2^(MIHist[SecondarySubgroup,]>0)*3^(MIHist[SecondarySubgroup,]>1)*5^(IHDHist[SecondarySubgroup,]>0)*7^(CHFHist[SecondarySubgroup,]>0)*11^(StrokeHist[SecondarySubgroup,]>0)*13^(StrokeHist[SecondarySubgroup,]>1)*
    17^(BlindHist[SecondarySubgroup,]>0)*19^(UlcerHist[SecondarySubgroup,]>0)*23^(AmpHist[SecondarySubgroup,]>0)*29^(AmpHist[SecondarySubgroup,]>1)*31^(RenalHist[SecondarySubgroup,]>0)*
    (-1)^Death[SecondarySubgroup,]
  
  t<-cbind(as.matrix(InitialVals),t)
  
  
  for (i in 1:((ModelRunYears))) # generate the MSM state transitions, intensification levels and average Secondary over 25 
  { #MSM state transitions
    tsub<-data.table(t[,c(i,i+1)])[,.(Vol=.N),by=.(V1,V2)]
    tsub[,Prop:=Vol/sum(Vol),by=.(V1)]
    tsub[,BaseYear:=(i-1)]
    if(i==1)
    {outputstatesSecondary<-tsub}else{outputstatesSecondary<-rbind(outputstatesSecondary,tsub)}
    
    #What proportion of people are at each intensification stage?
    { if(i<ModelRunYears & sum(Death[SecondarySubgroup,i]==0)>1) # can't run this for final year as i+1 = MOdelRunYears+1 = out of bounds
    {LastYearAlive<-if(i==1){which(SecondarySubgroup)}else{which(Death[SecondarySubgroup,(i-1)]==0)}
    
    StageSecondary<-data.table(TreatmentStage[LastYearAlive,c(i,i+1)])[,.(Vol=.N),by=.(StartStage=V1,EndStage=V2)]
    StageSecondary[,Year:=i]
    if(i==1)
    {OutputStageSecondary<-StageSecondary}else{OutputStageSecondary<-rbind(OutputStageSecondary,StageSecondary)}
    }}
    
    ## for people who are alive, what is the average Secondary>25
    {LastYearAlive<-if(i==1){which(SecondarySubgroup)}else{which(Death[SecondarySubgroup,(i-1)]==0)}
      
      
      Over25Secondary<-data.table(Year=i,TotalOver=sum(pmax(BMI[LastYearAlive,i]-25,0)))
      if(i==1)
      {OutputOver25Secondary<-Over25Secondary}else{OutputOver25Secondary<-rbind(OutputOver25Secondary,Over25Secondary)}
    }
    
    
    
  }
  
  
  
  
  
  outputstatesSecondary[,ModelNumber:=BootS]
  OutputStageSecondary[,ModelNumber:=BootS]
  OutputOver25Secondary[,ModelNumber:=BootS]
  
  # Now store the values that have been created
  
  
  if(BootS%%BootStrapRepeats==1|BootStrapRepeats==1)# first loop of bootstrap? create output tables
  { 
    #then Secondary
    
    MSMSecondary<-outputstatesSecondary
    AllStatesSecondary<-OutputStageSecondary
    AllOver25Secondary<-OutputOver25Secondary
    
    
    
    
  }else{ #for all subsequent loops
    
    #then Secondary
    MSMSecondary<-rbind(MSMSecondary,outputstatesSecondary)
    AllStatesSecondary<-rbind(AllStatesSecondary,OutputStageSecondary)
    AllOver25Secondary<-rbind(AllOver25Secondary,OutputOver25Secondary)
    
    
  }
  
  if(BootS==BootStrapRepeats) # if all bootstrap repeats completed then : first time create master bs store
  {
    MSM2Secondary<-MSMSecondary[,.(Total=sum(Vol)/(BootStrapRepeats)),by=.(V1,V2,BaseYear)]
    MSM2Secondary[,BootStrap:=BootStraps[BootS]]
    
    AllStatesOutputSecondary<-AllStatesSecondary[,.(Total=sum(Vol)/(BootStrapRepeats),BootStrap=BootStraps[BootS]),by=.(StartStage,EndStage,Year)]
    
    AllBMIOutputSecondary<-AllOver25Secondary[,.(Total=sum(TotalOver)/(BootStrapRepeats),BootStrap=BootStraps[BootS]),by=.(Year)]
    
    
  }else if(BootS%%BootStrapRepeats==0) # for subsequent bootstraps bind te results
  {
    MSM2Secondary<-rbind(MSM2Secondary,MSMSecondary[,.(Total=sum(Vol)/(BootStrapRepeats),BootStrap=BootStraps[BootS]),by=.(V1,V2,BaseYear)])
    rm(MSMSecondary) # remove individual multi state models
    AllStatesOutputSecondary<-rbind(AllStatesOutputSecondary,AllStatesSecondary[,.(Total=sum(Vol)/(BootStrapRepeats),BootStrap=BootStraps[BootS]),by=.(StartStage,EndStage,Year)])
    rm(AllStatesSecondary)
    AllBMIOutputSecondary<-rbind(AllBMIOutputSecondary,AllOver25Secondary[,.(Total=sum(TotalOver)/(BootStrapRepeats),BootStrap=BootStraps[BootS]),by=.(Year)])
    rm(AllOver25Secondary)
    gc()
    t2<-Sys.time()
    print(t2-t1) 
    t1<-Sys.time()# how long did the bootstrap take to run
  }
  
  
  
  
  
}

StopTheClock<-Sys.time()
TimeTaken<-StopTheClock-StartTheClock


# end of model ####

# now save all results

SaveFileRoot<-paste0(NSim,"Patients",ModelRunYears,"Years",BootStrapVol,"x",BootStrapRepeats,IntensificationLevel,ChosenTreatment)


##firstly save overall results
saveRDS(MSM2,paste("./Results/KusalResults/",SaveFileRoot,"TotalMSM.Rdata"))
saveRDS(AllStatesOutput,paste("./Results/KusalResults/",SaveFileRoot,"TotalTreatmentStages.Rdata"))
#saveRDS(AllBMIOutput,paste("./Results/",SaveFileRoot,"TotalOver25BMI.Rdata"))

##Now save BMI subgroup

saveRDS(MSM2BMI,paste("./Results/KusalResults/",SaveFileRoot,"BMIMSM.Rdata"))
saveRDS(AllStatesOutputBMI,paste("./Results/KusalResults/",SaveFileRoot,"BMITreatmentStages.Rdata"))
#saveRDS(AllBMIOutputBMI,paste("./Results/",SaveFileRoot,"BMIOver25BMI.Rdata"))

## Now Total CV subgroup
saveRDS(MSM2CV,paste("./Results/KusalResults/",SaveFileRoot,"CVMSM.Rdata"))
saveRDS(AllStatesOutputCV,paste("./Results/KusalResults/",SaveFileRoot,"CVTreatmentStages.Rdata"))
#saveRDS(AllBMIOutputCV,paste("./Results/",SaveFileRoot,"CVOver25BMI.Rdata"))

## Now primary subgroup
saveRDS(MSM2PrimaryHighRisk,paste("./Results/KusalResults/",SaveFileRoot,"PrimaryHighRiskMSM.Rdata"))
saveRDS(AllStatesOutputPrimaryHighRisk,paste("./Results/KusalResults/",SaveFileRoot,"PrimaryHighRiskTreatmentStages.Rdata"))
#saveRDS(AllBMIOutputPrimaryHighRisk,paste("./Results/",SaveFileRoot,"PrimaryHighRiskOver25BMI.Rdata"))

## Now secondary Subgroup

saveRDS(MSM2Secondary,paste("./Results/KusalResults/",SaveFileRoot,"SecondaryMSM.Rdata"))
saveRDS(AllStatesOutputSecondary,paste("./Results/KusalResults/",SaveFileRoot,"SecondaryTreatmentStages.Rdata"))
#saveRDS(AllBMIOutputSecondary,paste("./Results/",SaveFileRoot,"SecondaryOver25BMI.Rdata"))

Sys.time()

K<-readRDS(paste("./Results/KusalResults/",SaveFileRoot,"TotalMSM.Rdata"))
