#### Script containing functions for UKPDS R Implementation

# function to load up bootstraps and parametrise equations
GetBSVals<-function()
{# load all required bootstraps ####
  #supp. material Table 4s
  CHFbs<<-fread("./EventEquationBootstraps/CHFbs.csv")
  IHDbs<<-fread("./EventEquationBootstraps/IHDbs.csv")
  MIM1bs<<-fread("./EventEquationBootstraps/MIM1bs.csv")
  MIF1bs<<-fread("./EventEquationBootstraps/MIF1bs.csv")
  MI2bs<<-fread("./EventEquationBootstraps/MI2bs.csv")
  Stroke1bs<<-fread("./EventEquationBootstraps/Stroke1bs.csv")
  Stroke2bs<<-fread("./EventEquationBootstraps/Stroke2bs.csv")
  
  #supp.material table 5
  Blindnessbs<<-fread("./EventEquationBootstraps/Blindnessbs.csv")
  Ulcerbs<<-fread("./EventEquationBootstraps/Ulcerbs.csv")
  Ulcerbs<<-fread("./EventEquationBootstraps/Ulcerbs.csv")
  Amp1NoUlcerbs<<-fread("./EventEquationBootstraps/Amp1NUbs.csv")
  Amp1Ulcerbs<<-fread("./EventEquationBootstraps/Amp1Ubs.csv")
  Amp2bs<<-fread("./EventEquationBootstraps/Amp2bs.csv")
  Renalbs<<-fread("./EventEquationBootstraps/Renalbs.csv")
  
  ## supp. material table 6 (Death)
  
  Death1bs<<-fread("./EventEquationBootstraps/Death1bs.csv")
  Death2bs<<-fread("./EventEquationBootstraps/Death2bs.csv")
  Death3bs<<-fread("./EventEquationBootstraps/Death3bs.csv")
  Death4bs<<-fread("./EventEquationBootstraps/Death4bs.csv")
  
  ## all bootstraps now downloaded
  
  
  
}

#Function to choose bootstraps
BootStrapList<-function(size) # can the mean be used as a bootstrap? assuming not # no replacement
{return(sample(2:5011,size))}

#function to parametise model
GetBS<-function(bsno)
{# This function assigns the correct bootstrapped parameters for the risk equations
  
  
  
  #Table 4 functions
  
  CHFpara<<-as.numeric(CHFbs[bsno,4:25])# col 1 is row number introduced by saving
  IHDpara<<-as.numeric(IHDbs[bsno,4:25])
  
  MIMalepara<<-as.numeric(MIM1bs[bsno,4:25])
  MIFemalepara<<-as.numeric(MIF1bs[bsno,4:25])
  MI2para<<-as.numeric(MI2bs[bsno,4:25])
  
  Stroke1para<<-as.numeric(Stroke1bs[bsno,4:25])
  Stroke2para<<-as.numeric(Stroke2bs[bsno,4:25])
  
  ########## Table 5 use a different set of variables ### :( 
  
  
  Blindpara<<-as.numeric(Blindnessbs[bsno,4:24])
  Ulcerpara<<-as.numeric(Ulcerbs[bsno,4:24])
  
  Amp1NoUlcerpara<<-as.numeric(Amp1NoUlcerbs[bsno,4:24])
  Amp1Ulcerpara<<-as.numeric(Amp1Ulcerbs[bsno,4:24])
  Amp2para<<-as.numeric(Amp2bs[bsno,4:24])
  
  RenalFailpara<<-as.numeric(Renalbs[bsno,4:24])
  
  ## Now also the Death parameters 
  
  Death1para<<-as.numeric(Death1bs[bsno,4:28])
  
  Death2para<<-as.numeric(Death2bs[bsno,4:28])
  
  Death3para<<-as.numeric(Death3bs[bsno,4:28])
  
  Death4para<<-as.numeric(Death4bs[bsno,4:28])
  
  
  # Equation specific parameters ####
  
  
  ## to actually calculate the risk need to bring in equation specific parameters
  ## CHF, WEIBULL
  CHFproblambda<<-as.numeric(CHFbs[bsno,2])
  CHFprobrho<<-as.numeric(CHFbs[bsno,3])
  
  
  #IHD, WEIBULL
  IHDproblambda<<-as.numeric(IHDbs[bsno,2])
  IHDprobrho<<-as.numeric(IHDbs[bsno,3])
  ##1st MI Male, EXPONENTIAL
  # not copied from table one but as exponential survival not time dependent
  
  MIMalelambda<<-as.numeric(MIM1bs[bsno,2])
  
  #MIMaleprob<<-exp(MIMalelambda+Table4Function(MIMalepara)) ## is this right??? looks sensible possible BUG
  
  ## 1st MI Female weibull
  MIFemaleproblambda<<-as.numeric(MIF1bs[bsno,2])
  MIFemaleprobrho<<-as.numeric(MIF1bs[bsno,3])
  
  ### 2nd MI - exponential
  MI2lambda<<-as.numeric(MI2bs[bsno,2])
  
  ### first stroke weibull
  
  Stroke1problambda<<-as.numeric(Stroke1bs[bsno,2])
  Stroke1probrho<<-as.numeric(Stroke1bs[bsno,3])
  
  ## second stroke weibull
  
  Stroke2problambda<<-as.numeric(Stroke2bs[bsno,2])
  Stroke2probrho<<-as.numeric(Stroke2bs[bsno,3])
  
  
  ############ all cases below are for Table 5
  
  ## Blindness exponential
  Blindlambda<<-as.numeric(Blindnessbs[bsno,2])
  
  #### ulcer logistic
  
  Ulcerlambda<<-as.numeric(Ulcerbs[bsno,2])
  #Ulcerprob<<-logistic(exp(Ulcerlambda+Table5Function(Blindpara))) ## see logistic function
  
  ### Amp1 no ulcer, weibull
  
  
  Amp1NoUlcerproblambda<<-as.numeric(Amp1NoUlcerbs[bsno,2])
  Amp1NoUlcerprobrho<<-as.numeric(Amp1NoUlcerbs[bsno,3])
  
  ### Amp1 ulcer, exponential
  Amp1Ulcerlambda<<-as.numeric(Amp1Ulcerbs[bsno,2])
  
  ##### amp2, exponential
  Amp2lambda<<-as.numeric(Amp2bs[bsno,2])
  
  ##Renal failure exponential
  
  RenalFaillambda<<-as.numeric(Renalbs[bsno,2]) ## this seems wrong - gives probabilities way in excess of 1
  #RenalFaillambda<<-(-1)*RenalFaillambda # Possible bug, multiplied this by -1
  
  ## Now all 4 death equations
  
  Death1lambda<<-as.numeric(Death1bs[bsno,2]) # gompertz
  Death1phi<<-as.numeric(Death1bs[bsno,3])
  
  Death2lambda<<-as.numeric(Death2bs[bsno,2]) # logistic
  
  Death3lambda<<-as.numeric(Death3bs[bsno,2]) # gompertz
  Death3phi<<-as.numeric(Death3bs[bsno,3])
  
  Death4lambda<<-as.numeric(Death4bs[bsno,2]) # logistic
  
}

#### Functions required for risk equations
# See the UKPDS supplementary material (Statistical appendix) if not familiar with the maths behind this
# Statistical appendix: https://static-content.springer.com/esm/art%3A10.1007%2Fs00125-013-2940-y/MediaObjects/125_2013_2940_MOESM1_ESM.pdf

# These functions take 3 arguments -
#1) desiredpara - Which risk equation are you running (MI, Heart failure etc.)
#2) keeprows - This enables the function only to be run for eligible patients (not dead or have already had that event) Good for speed
#3) Year - which model year should the function be applied to (1 if first year, 2 if second etc.)

# Table 4 is a reference to the UKPDS supplementary material where this can be found
Table4Function<-function(desiredpara,keeprows,Year){
  MatrixNames<-c("Afro","AgeDiag","Female","Indian","ATFIB","BMI","eGFR","eGFR60minus","Hba1C","HDL","LDL","LDL35","MICALB","PVD","SBP","Smoker","WBC","AmpHist","CHFHist","IHDHist","StrokeHist","UlcerHist") # could pull these out of the funciton (no need to recerate them)
  HazardSum<-matrix(0L,nrow = length(keeprows), ncol = 1)
  for (k in 1:length(desiredpara))
  {#print(k)
    if(desiredpara[k]!=0){
      if( ncol(get(MatrixNames[k]))==1)
      {HazardSum<-HazardSum+get(MatrixNames[k])[keeprows]*desiredpara[k]}
      else {HazardSum<-HazardSum+get(MatrixNames[k])[keeprows,Year]*desiredpara[k]}
    }
  }
  return(HazardSum)
}

# Table 5 is a reference to the UKPDS supplementary material where this can be found
Table5Function<-function(desiredpara,keeprows,Year){
  MatrixNames<-c("Afro","AgeDiag","Female","ATFIB","BMI","eGFR60minus","eGFR60plus","HAEM","Hba1C","HDL","HEARTR","LDL","MICALB","PVD","SBP","WBC","AmpHist","BlindHist","CHFHist","IHDHist","StrokeHist")
  HazardSum<-matrix(0L,nrow = length(keeprows), ncol = 1)
  for (k in 1:length(desiredpara))
  {
    if(desiredpara[k]!=0){
      if( ncol(get(MatrixNames[k]))==1)
      {HazardSum<-HazardSum+get(MatrixNames[k])[keeprows]*desiredpara[k]}
      else {HazardSum<-HazardSum+get(MatrixNames[k])[keeprows,Year]*desiredpara[k]}
    }
  }
  return(HazardSum)
}

#This is table 6 in the supplementary material
DeathFunction<-function(desiredpara,keeprows,Year){
  MatrixNames<-c("Female","Indian","DiabYears","ATFIB","BMICAT1","BMICAT3","Age","HDL","HEARTR","MICALB","PVD","Smoker","WBC","Amp1","AmpHist","Amp2","CHFHist","IHD","IHDHist","MIDeathEq","MIHist","Renal","RenalHist","StrokeDeathEq","StrokeHist")
  HazardSum<-matrix(0L,nrow = length(keeprows), ncol = 1)
  for (j in 1:length(desiredpara))
  {if(desiredpara[j]!=0){
    if( ncol(get(MatrixNames[j]))==1)
    {HazardSum<-HazardSum+get(MatrixNames[j])[keeprows]*desiredpara[j]}
    else {HazardSum<-HazardSum+get(MatrixNames[j])[keeprows,i]*desiredpara[j]}
  }
  }
  return(HazardSum)
}

logistic<-function(x){return(x/(x+1))}

#### Risk Equations v2 (speed)

# See UKPDS OM2 supplementary material for more details of how this works
# These functions will apply the risk equation to every eligible row in a given year

#Eq1  CHF weibull
CHFfun<-function(){
  CHFrows<-which(DeathRows & CHFHist[,i]==0)
  CHF[CHFrows,i]<<-rbinom(length(CHFrows),1,(1-exp(exp(CHFproblambda+Table4Function(CHFpara,CHFrows,i))*
                                                          (DiabYears[CHFrows]^CHFprobrho-(DiabYears[CHFrows]+1)^CHFprobrho
                                                           
                                                          ))))
  CHFHist[which(CHF[,i]==1),i:ModelRunYears]<<-1 # this year and all subsequent years will have CHF History
}


# Eq 2 IHD weibull
IHDfun<-function(){
  IHDrows<-which(DeathRows & IHDHist[,i]==0)
  IHD[IHDrows,i]<<-rbinom(length(IHDrows),1,(1-exp(exp(IHDproblambda+Table4Function(IHDpara,IHDrows,i))*
                                                     (DiabYears[IHDrows]^IHDprobrho-(DiabYears[IHDrows]+1)^IHDprobrho
                                                      
                                                     )))) 
  IHDHist[which(IHD[,i]==1),i:ModelRunYears]<<-1
}
# Eq 3/4 MI Male/Female male exponential/weibull

MI1fun<-function(){
  
  MIMalerows<-which(!FemaleFlag & MIHist[,i]==0 & DeathRows)  
  MIFemalerows<-which(FemaleFlag & MIHist[,i]==0 &DeathRows)
  
  MI1[MIMalerows,i]<<-rbinom(length(MIMalerows),1,(1-exp(-exp(MIMalelambda+Table4Function(MIMalepara,MIMalerows,i)))))  
  
  MI1[MIFemalerows,i]<<-rbinom(length(MIFemalerows),1,(1-exp(exp(MIFemaleproblambda+Table4Function(MIFemalepara,MIFemalerows,i))*
                                                               (DiabYears[MIFemalerows]^MIFemaleprobrho-(DiabYears[MIFemalerows]+1)^MIFemaleprobrho
                                                                
                                                               ))))
  
  MIHist[which(MI1[,i]==1),i:ModelRunYears]<<-1
}
#Eq 5 MI2 leave this in MI1 table exponential
MI2fun<-function(){
  MI2rows<-which(MIHist[,i]==1 & DeathRows)
  MI2[MI2rows,i]<<-rbinom(length(MI2rows),1,1-exp(-exp(MI2lambda+Table4Function(MI2para,MI2rows,i)))) 
  MIHist[which(MI2[,i]==1),i:ModelRunYears]<<-2 # update histories to 2
}

# Eq 6 Stroke 1 weibull

Stroke1fun<-function(){
  Stroke1rows<-which(StrokeHist[,i]==0 & DeathRows)
  Stroke1[Stroke1rows,i]<<-rbinom(length(Stroke1rows),1,(1-exp(exp(Stroke1problambda+Table4Function(Stroke1para,Stroke1rows,i))*
                                                                  (DiabYears[Stroke1rows]^Stroke1probrho-(DiabYears[Stroke1rows]+1)^Stroke1probrho
                                                                   
                                                                  ))))
  StrokeHist[which(Stroke1[,i]==1),i:ModelRunYears]<<-1
}
# Eq 7 Stroke 2 weibull
Stroke2fun<-function(){
  Stroke2rows<-which(DeathRows & StrokeHist[,i]==1)
  Stroke2[Stroke2rows,i]<<-rbinom(length(Stroke2rows),1,(1-exp(exp(Stroke2problambda+Table4Function(Stroke2para,Stroke2rows,i))*
                                                                 (DiabYears[Stroke2rows]^Stroke2probrho-(DiabYears[Stroke2rows]+1)^Stroke2probrho
                                                                  
                                                                 ))))
  StrokeHist[which(Stroke2[,i]==1),i:ModelRunYears]<<-2
}

# Eq 8 Blindness
Blindfun<-function(){
  Blindrows<-which(DeathRows & BlindHist[,i]==0)
  Blind[Blindrows,i]<<-rbinom(length(Blindrows),1,1-exp(-exp(Blindlambda+Table5Function(Blindpara,Blindrows,i))) 
  )
  
  
  BlindHist[which(Blind[,i]==1),i:ModelRunYears]<<-1
}
# Eq 9 logistic
Ulcerfun<-function(){
  Ulcerrows<-which(DeathRows & UlcerHist[,i]==0)
  Ulcer[Ulcerrows,i]<<-rbinom(length(Ulcerrows),1,1-logistic(exp(-(Ulcerlambda+Table5Function(Ulcerpara,Ulcerrows,i)))) ## see logistic function
  )
  
  UlcerHist[which(Ulcer[,i]==1),i:ModelRunYears]<<-1
}

# Eq 10/11 Amputation 1 no prior ulcer / prior ulcer weibull/exponential
Amp1fun<-function(){
  Amp1NoUlcerrows<-which(UlcerHist[,i]==0 & AmpHist[,i]==0 & DeathRows)
  Amp1Ulcerrows<-which(UlcerHist[,i]==1 & AmpHist[,i]==0 & DeathRows)
  
  Amp1[Amp1NoUlcerrows,i]<<-rbinom(length(Amp1NoUlcerrows),1,(1-exp(exp(Amp1NoUlcerproblambda+Table5Function(Amp1NoUlcerpara,Amp1NoUlcerrows,i))*
                                                                  (DiabYears[Amp1NoUlcerrows]^Amp1NoUlcerprobrho-(DiabYears[Amp1NoUlcerrows]+1)^Amp1NoUlcerprobrho
                                                                   
                                                                  ))))
  Amp1[Amp1Ulcerrows,i]<<-rbinom(length(Amp1Ulcerrows),1,1-exp(-exp(Amp1Ulcerlambda+Table5Function(Amp1Ulcerpara,Amp1Ulcerrows,i)))  
  )
  
  AmpHist[which(Amp1[,i]==1),i:ModelRunYears]<<-1
}

## Eq 12 Amp2 exponential
Amp2fun<-function(){
  Amp2rows<-which(AmpHist[,i]==1 & DeathRows)
  Amp2[Amp2rows,i]<<-rbinom(length(Amp2rows),1,1-exp(-exp(Amp2lambda+Table5Function(Amp2para,Amp2rows,i))) ## is this right??? still unsure re exponential
  )
  AmpHist[which(Amp2[,i]==1),i:ModelRunYears]<<-2
}

# Eq 13 Renal Failure exponential

Renalfun<-function(){
  Renalrows<-which(DeathRows & RenalHist[,i]==0)
  Renal[Renalrows,i]<<-rbinom(length(Renalrows),1,1-exp(-exp(RenalFaillambda+Table5Function(RenalFailpara,Renalrows,i) )))
  RenalHist[which(Renal[,i]==1),i:ModelRunYears]<<-1
}

################ Function to populate NG28 Baseline data


PopulateModel<-function(TextFileName) #NSim and ModelRunYears should already be defined, 100000 patients will be repeated for larger samples
  # # Wed Mar 10 16:52:18 2021 -------Note that this will change when we get the new THIN data-----------------------
#TextFileName will be replaced by PI, P2, P3 etc.
  
{
  FiletoRead<-paste0("./Populations/",TextFileName,".csv")
  
  #see Q:\Guidelines\In development\Diabetes\6. HE working\UKPDS Model\OM2\UKPDS Rv2\Correlated baselines for population generation v2
  InitialTherapyPop<-fread(FiletoRead)
  
  SelectRows<-sample(1:nrow(InitialTherapyPop),NSim,replace=T) # this will allow multiple sampling of patients
  Subset<-InitialTherapyPop[SelectRows,]
  
  
  Hba1C<<-matrix(0L,nrow = NSim, ncol = ModelRunYears)
  Hba1C[,1]<<-Subset$HbA1c_LOG
  HbA1c_DIAG<<-Subset$HbA1c_DIAG_LOG
  Age<<-matrix(0L,nrow = NSim, ncol = 1) # Can be updated each model run 
  Age[,1]<<-Subset$AGE_AT_EVENT
  AgeNY<<-Age+1  # this is age next year - useful for establishing likelihood of survival
  AgeDiag<<-Subset$AGE_AT_EVENT-Subset$DiabYears # 1.5 for P1, 4.5 for 2, 8.5 for 3
  Female<<-matrix(0L,nrow = NSim, ncol = 1)  # 1 implies female, 0 implies male
  Female[,1]<<-Subset$Gender # in subset Gender=0 is female
  FemaleFlag<<-Female[,1]==1 # a flag used to identify female patients
  DiabYears<<-matrix(0L,nrow = NSim, ncol = 1)
  DiabYears[,1]<<-Subset$DiabYears ## need to have 1 added to it each year
  Afro<<-matrix(0L,nrow = NSim, ncol = 1) # 1 if afro-caribbean 
  Afro[,1]<<-Subset$Afro
  Indian<<-matrix(0L,nrow = NSim, ncol = 1) # 1 if indian STARTGEN
  Indian[,1]<<-Subset$Indian
  Smoker<<-matrix(0L,nrow = NSim, ncol = ModelRunYears) # 1 if smoker
  Smoker[,1:3]<<-Subset$Smoking
  Smoker_Diag<<-Subset$Smoking_Diag
  MICALB<<-matrix(0L,nrow = NSim, ncol = ModelRunYears)  #Presence of micro- or macro-albuminuria. 1 for urine albumin ???50mg/l; 0 otherwise
  MICALB[,1]<<-Subset$Albuminuria
  ATFIB<<-matrix(0L,nrow = NSim, ncol = ModelRunYears) # atrial fibriliation
  ATFIB[,1:3]<<-Subset$Atrial_Fib
  PVD<<-matrix(0L,nrow = NSim, ncol = ModelRunYears) # 1 for peripheral vascular disease; 0 otherwise. Defined from 
  PVD[,1:3]<<-Subset$PVD # no PVD in original extract???
  
  BMI<<-matrix(0,nrow = NSim, ncol = ModelRunYears) # 
  BMI[,1]<<-Subset$WEIGHT_LOG/((Subset$Height)^2)
  BMI_Diag<<-Subset$WEIGHT_DIAG_LOG/((Subset$Height)^2)
  Height<<-((Subset$Height)^2) # needed for BMI treatment Effect
  HDL<<-matrix(1.5,nrow = NSim, ncol = ModelRunYears) # High density lipoprotein cholesterol (Trans= *10 applied later)
  HDL[,1]<<-Subset$HDL_LOG
  
  HDL_Diag<<-Subset$HDL_DIAG_LOG
  eGFR_Diag<<-Subset$eGFR_Diag
  LDL<<-matrix(0,nrow = NSim, ncol = ModelRunYears)
  LDL[,1]<<-Subset$LDL_LOG
  LDL[which(LDL[,1]>10),1]<-9 # change to 9 if over 10 (UKPDS) max value
  LDL_Diag<<-Subset$LDL_DIAG_LOG
  LDL_Diag[which(LDL[,1]>10)]<-9 # change to 9 if over 10 (UKPDS) max value
  
  HEARTR<<-matrix(0L,nrow = NSim, ncol = ModelRunYears) # heart rate (Trans= /10 applied later)
  HEARTR[,1:3]<<-Subset$HEARTR
  HEARTR_DIAG<<-Subset$HEARTR_DIAG
  SBP<<-matrix(0L,nrow = NSim, ncol = ModelRunYears) # blood pressure (Trans= /10 applied later)
  SBP[,1]<<-Subset$Blood_Pressure_LOG
  SBP_Diag<<-Subset$Blood_Pressure_Diag_LOG
  eGFR<<-matrix(0L,nrow = NSim, ncol = ModelRunYears) # Estimated glomerular filtration rate (Trans= /10 applied later)
  eGFR[,1]<<-Subset$eGFR
  eGFR[which(eGFR[,1]<0),1]<-10 # if eGFR estimated<0 (generated from normal) then replace with 1 (10) VERY!!!! rare
  eGFR_Diag<<-Subset$eGFR_Diag
  eGFR_Diag[which(eGFR[,1]<0)]<-10
  WBC<<-matrix(0L,nrow = NSim, ncol = ModelRunYears) # white blood cell count
  WBC[,1:3]<<-Subset$WBC_LOG
  WBC_Diag<<-Subset$WBC_DIAG_LOG
  
  
  HAEM<<-matrix(0L,nrow = NSim, ncol = ModelRunYears) # haemaglobin
  HAEM[,1:3]<<-Subset$HAEM
  HAEM_Diag<<-Subset$HAEM_DIAG
  ## Now event histories and generating empty equations 
  Death<<-matrix(0L,nrow = NSim, ncol = ModelRunYears)  # can be updated Each model run # 0 correct as starting value
  DeathHist<<-matrix(0L,nrow = NSim, ncol = 1)  # can be updated Each model run # 0 correct as starting value
  
  
  StrokeHist<<-matrix(0L,nrow = NSim, ncol = ModelRunYears)
  StrokeHist[which(Subset$STROKE_FLAG==1),]<-1# can be updated each model run# 
  Stroke1<<-matrix(0L,nrow = NSim, ncol = ModelRunYears) # 0 correct as starting value
  Stroke2<<-matrix(0L,nrow = NSim, ncol = ModelRunYears) # 0 correct as starting value
  StrokeDeathEq<<-matrix(0L,nrow = NSim, ncol = ModelRunYears) # to calculate death prob
  
  
  AmpHist<<-matrix(0L,nrow = NSim, ncol = ModelRunYears)  # can be updated each model run# 0 correct as starting value
  AmpHist[which(Subset$AMP_FLAG==1),]<<-1 # no amputation history data
  Amp1<<-matrix(0L,nrow = NSim, ncol = ModelRunYears) # 0 correct as starting value
  Amp2<<-matrix(0L,nrow = NSim, ncol = ModelRunYears) # 0 correct as starting value
  
  CHFHist<<-matrix(0L,nrow = NSim, ncol = ModelRunYears)  # can be updated each model run# 0 correct as starting value
  CHFHist[which(Subset$CHD_FLAG==1),]<<-1
  CHF<<-matrix(0L,nrow = NSim, ncol = ModelRunYears) # 0 correct as starting value
  
  MIHist<<-matrix(0L,nrow = NSim, ncol = ModelRunYears)  # can be updated each model run# 0 correct as starting value
  MIHist[which(Subset$MI_FLAG==1),]<<-1
  MI1<<-matrix(0L,nrow = NSim, ncol = ModelRunYears) # 0 correct as starting value
  MI2<<-matrix(0L,nrow = NSim, ncol = ModelRunYears) # 0 correct as starting value
  MIDeathEq<<-matrix(0L,nrow = NSim, ncol = ModelRunYears) # to calculate death prob
  
  IHDHist<<-matrix(0L,nrow = NSim, ncol = ModelRunYears)  # can be updated each model run# 0 correct as starting value
  IHDHist[which(Subset$IHD_FLAG==1),]<<-1
  IHD<<-matrix(0L,nrow = NSim, ncol = ModelRunYears) # 0 correct as starting value
  
  UlcerHist<<-matrix(0L,nrow = NSim, ncol = ModelRunYears)  # can be updated each model run# 0 correct as starting value
  UlcerHist[which(Subset$Ulcer_FLAG==1),]<<-1 # assumed 5% for initial therapy
  Ulcer<<-matrix(0L,nrow = NSim, ncol = ModelRunYears) # 0 correct as starting value
  
  BlindHist<<-matrix(0L,nrow = NSim, ncol = ModelRunYears)  # can be updated each model run# 0 correct as starting value
  BlindHist[which(Subset$Blindness_FLAG==1),]<<-1 # assumed 5% for initial therapy
  
  Blind<<-matrix(0L,nrow = NSim, ncol = ModelRunYears) # 0 correct as starting value
  
  
  RenalHist<<-matrix(0L,nrow = NSim, ncol = ModelRunYears)  # can be updated each model run# 0 correct as starting value
  RenalHist[which(Subset$Renal_FLAG==1),]<<-1
  Renal<<-matrix(0L,nrow = NSim, ncol = ModelRunYears) # 0 correct as starting value
  
  
  # create age record
  AgeStore<-matrix(0,nrow = NSim, ncol = ModelRunYears)
  AgeStore[,1]<-Age[,1]
  for(i in 2:ModelRunYears){AgeStore[,i]<-AgeStore[,(i-1)]+1}
  
  
}

##### get parameters for time path equation
GetCentralTimePathParameters<-function() # Base files currently read to NICE drives
{
  ## generate lifetime profiles
  
  TimePaths<<-read.csv("./RiskFactorTimePathEquations/ContinuousRF.csv")
  
  BINARYTimePaths<<-read.csv("./RiskFactorTimePathEquations/BinaryRF.csv")
  
  
  ## now code continuous (only central estimates)
  
  #HbA1c
  ConstantHbA<<-as.numeric(TimePaths$HbA1c[7])
  ParasHbA<<-as.numeric(TimePaths$HbA1c[c(9,11,13,15,17,19)])
  #InitialHba1C<<-Hba1C[,1]
  
  #SBP, this is transformed by /10 usually, be careful with inputs
  ConstantSBP<<-as.numeric(TimePaths$SBP[7])
  ParasSBP<<-as.numeric(TimePaths$SBP[c(9,11,13,15,17,19)])
  #InitialSBP<<-SBP[,1]
  
  #LDL, this is transformed by x10 usually, be careful with inputs
  ConstantLDL<<-as.numeric(TimePaths$LDL[7])
  ParasLDL<<-as.numeric(TimePaths$LDL[c(9,11,13,15,17,19)])
  #InitialLDL<<-LDL[,1]
  
  #HDL, this is transformed by x10 usually, be careful with inputs
  ConstantHDL<<-as.numeric(TimePaths$HDL[7])
  ParasHDL<<-as.numeric(TimePaths$HDL[c(9,11,13,15,17,19)])
  #InitialHDL<<-HDL[,1]
  
  #BMI, this is transformed by x10 usually, be careful with inputs
  ConstantBMI<<-as.numeric(TimePaths$BMI[7])
  ParasBMI<<-as.numeric(TimePaths$BMI[c(9,11,13,15,17,19)])
  #InitialBMI<<-BMI[,1]
  
  #Heart rate (usually divided by 10), careful with inputs
  
  ConstantHEARTR<<-as.numeric(TimePaths$HEART.R[7])
  ParasHEARTR<<-as.numeric(TimePaths$HEART.R[c(9,11,13,15,17,19)])
  #InitialHEARTR<<-HEARTR[,1]
  
  #WBC
  ConstantWBC<<-as.numeric(TimePaths$WBC[7])
  ParasWBC<<-as.numeric(TimePaths$WBC[c(9,11,13,15,17,19)])
  #InitialWBC<<-WBC[,1]
  
  #HAEM
  
  ConstantHAEM<<-as.numeric(TimePaths$HAEM[7])
  ParasHAEM<<-as.numeric(TimePaths$HAEM[c(9,11,13,15,17,19)])
  #InitialHAEM<<-HAEM[,1]
  
  ## now binary RF
  
  #MIC ALBUMINURIA (weib)
  ConstantMICALB<<-as.numeric(BINARYTimePaths$V3[9])
  WeibullMICALB<<-as.numeric(BINARYTimePaths$V3[11])
  ParasMICALB<<-as.numeric(BINARYTimePaths$V3[c(6:19*2+1)])
  
  # PVD (weib)
  
  ConstantPVD<<-as.numeric(BINARYTimePaths$V4[9])
  WeibullPVD<<-as.numeric(BINARYTimePaths$V4[11])
  ParasPVD<<-as.numeric(BINARYTimePaths$V4[c(6:19*2+1)])
  
  # ATFIB (exp)
  
  ConstantATFIB<<-as.numeric(BINARYTimePaths$V5[9])
  ParasATFIB<<-as.numeric(BINARYTimePaths$V5[c(6:19*2+1)])
  
  # Smoker (logistic)
  ConstantSMOKER<<-as.numeric(BINARYTimePaths$V6[9])
  ParasSMOKER<<-as.numeric(BINARYTimePaths$V6[c(6:19*2+1)])
  
  # eGFR in 3 parts
  # 1. is whether or not eGFR will be below 60
  #2. eGFR if < 60
  #3. eGFR if above 60
  
  #eGFRswitch (weibull)
  ConstanteGFRswitch<<-as.numeric(BINARYTimePaths$V7[9])
  WeibulleGFRswitch<<-as.numeric(BINARYTimePaths$V7[11])
  ParaseGFRswitch<<-as.numeric(BINARYTimePaths$V7[c(6:19*2+1)])
  
  # eGFR < 60
  
  ConstanteGFRless60<<-as.numeric(BINARYTimePaths$V8[9])
  TobiteGFRless60<<-as.numeric(BINARYTimePaths$V8[11])
  ParaseGFRless60<<-as.numeric(BINARYTimePaths$V8[c(6:19*2+1)])
  
  #eGFR >=60
  
  ConstanteGFRgrt60<<-as.numeric(BINARYTimePaths$X[9])
  TobiteGFRgrt60<<-as.numeric(BINARYTimePaths$X[11])
  ParaseGFRgrt60<<-as.numeric(BINARYTimePaths$X[c(6:19*2+1)])
  
} 


### calculate costs/qalys based on costs for a 60 year old male ###
# Thu Mar 11 14:01:30 2021 --Not currently used
CalculateCostQALYbasic<-function(BaseCost=881,DiscountRate=3.5)
  
{
  CostQALYdata<-fread("Q:/Guidelines/In development/Diabetes/6. HE working/UKPDS Model/OM2/UKPDS Rv2/ComplicationsSimple.csv") # this function isn't (yet!) so filpath pointing to the wrong place is not relevant
  #These values are  1,InYearUtilityDecrement,2,SubsequentCost,3,SubsequentUtilityDec,
  #4,adjustedfatalcost 5, AdjustedNonfatal 6, AdjustedInYearDec
  #adjustments required as if event happens in a given year (e,g, 3) then the history will be activated from this year. need to avaoid double counting
  
  IHDVals<-as.numeric(CostQALYdata[which(CostQALYdata$Complication=="IHD"),6:11])
  StrokeVals<-as.numeric(CostQALYdata[which(CostQALYdata$Complication=="Stroke"),6:11])
  MIVals<-as.numeric(CostQALYdata[which(CostQALYdata$Complication=="MI"),6:11])
  BlindVals<-as.numeric(CostQALYdata[which(CostQALYdata$Complication=="Blindness"),6:11])
  UlcerVals<-as.numeric(CostQALYdata[which(CostQALYdata$Complication=="Ulcer"),6:11])
  AmpVals<-as.numeric(CostQALYdata[which(CostQALYdata$Complication=="Amputation"),6:11])
  CHFVals<-as.numeric(CostQALYdata[which(CostQALYdata$Complication=="Heart failure"),6:11])
  RenalFailureVals<-as.numeric(CostQALYdata[which(CostQALYdata$Complication=="Renal Failure"),6:11])
  
  
  # 
  BaseAnnualCost<-BaseCost # average of default UKPDS OM2 values
  BaseAnnualQALY<-0.807 # UKPDS default
  
  QALYMatrix<-matrix(BaseAnnualQALY,nrow=NSim,ncol=ModelRunYears)
  CostMatrix<-matrix(BaseAnnualCost,nrow=NSim,ncol=ModelRunYears)
  
  QALYMatrix<-(QALYMatrix+
                 IHD*IHDVals[6]+IHDHist*IHDVals[3]+ #IHD
                 (Stroke1+Stroke2)*StrokeVals[6]+StrokeHist*StrokeVals[3]+#Stroke (disutility will be doubled if patient has a second stroke)
                 (MI1+MI2)*MIVals[6]+MIHist*MIVals[3]+ # MI (disutility will be doubled if patient has a second stroke)
                 +(Amp1+Amp2)*AmpVals[6]+AmpHist*AmpVals[3]+# amputation(disutility will be doubled if patient has a second stroke)
                 CHF*CHFVals[6]+CHFHist*CHFVals[3]+ #Heart failure
                 Blind*BlindVals[6]+BlindHist*BlindVals[3]+ # blindness
                 Ulcer*UlcerVals[6]+UlcerHist*UlcerVals[3]+ # ulcer
                 Renal*RenalFailureVals[6]+RenalHist*RenalFailureVals[3])*# renal failure
    (1-Death) # 0 if dead!
  
  CostMatrix<-(CostMatrix+
                 IHD*Death*IHDVals[4]+IHD*(1-Death)*IHDVals[5]+IHDHist*IHDVals[2]+
                 (Stroke1+Stroke2)*Death*StrokeVals[4]+(Stroke1+Stroke2)*(1-Death)*StrokeVals[5]+StrokeHist*StrokeVals[2]+
                 (MI1+MI2)*Death*MIVals[4]+(MI1+MI2)*(1-Death)*MIVals[5]+MIHist*MIVals[2]+
                 (Amp1+Amp2)*Death*AmpVals[4]+(Amp1+Amp2)*(1-Death)*AmpVals[5]+AmpHist*AmpVals[2]+
                 IHD*Death*IHDVals[4]+IHD*(1-Death)*IHDVals[5]+IHDHist*IHDVals[2]+ #IHD
                 Ulcer*Death*UlcerVals[4]+Ulcer*(1-Death)*UlcerVals[5]+UlcerHist*UlcerVals[2]+ #Ulcer
                 Blind*Death*BlindVals[4]+Blind*(1-Death)*BlindVals[5]+BlindHist*BlindVals[2]+ #Blind
                 Renal*Death*RenalFailureVals[4]+Renal*(1-Death)*RenalFailureVals[5]+RenalHist*RenalFailureVals[2])*# renal failure
    (1-Death) # 0 if dead!
  
  ###Now generate the discounting vector
  QALYDiscount<-1+DiscountRate/100
  QALYDiscountVector<-QALYDiscount^(0:-(ModelRunYears-1))
  CostDiscount<-1+DiscountRate/100
  CostDiscountVector<-CostDiscount^(0:-(ModelRunYears-1))
  
  TotalCost<-sum(CostMatrix%*%CostDiscountVector)
  TotalQALY<-sum(QALYMatrix%*%QALYDiscountVector)
  
  return(data.frame(Cost=TotalCost,QALY=TotalQALY)) # -1 is a dummy value
}

###This function will populate the initial therapy treatment effect data given a treatment and sample number (1 for means)
GetTreatmentMatrix<-function(Treatment,Sample)
{
  TreatmentMatrix<<-matrix(c(InitialTherapyHbA1cEffect[,..Treatment][[1]][Sample],
                             InitialTherapyHbA1cEffect$beta[Sample],
                             InitialTherapyWeightEffect[,..Treatment][[1]][Sample]),nrow=1,ncol=3,byrow=T)
  
  colnames(TreatmentMatrix)<<-c("HbA1c","Beta","Weight")
}

# First Intensification timepaths will apply initial therapy treatment effect, intensify where appropiate and project risk factors over time

InitialTherapyTimePaths<-function()
{
  
  ## already have a function to populate risk factors but need to add a module, which will apply treatment effects at a certain point
  # first establish values to use
  IntensificationThreshold<<-7.5 # if HbA1c over then apply treatment effect 
  
  
  #### hypos can be dealt with in a separate module
  
  
  ## will need an intensification table
  TreatmentStage<<-matrix(1,NSim,ModelRunYears)
  
  # process will be: determine if intensification threshold reached
  # if reached and intensification< n (depending on intensification number)
  
  
  for (i in 2:ModelRunYears)
    
  { ### who has reached the intensification threshold?
    if(i==2){IntensificationFlag<-rep(1,NSim)# intensify everyone first year, then need to be under threshold and not on max treatment
    # everyone will be treated in Y1 (assuming this is initial therapy population)
    #not intolerant
    Hba1C[,2]<<-Hba1C[,1]+(Hba1C[,1]-7.5)*TreatmentMatrix[1,"Beta"]+TreatmentMatrix[1,"HbA1c"]
    
    BMI[,2]<<-BMI[,1]+TreatmentMatrix[1,"Weight"]/Height # height contains height^2 (see population generation)
    
    
    }else{
      #### Hba1c 
      Intensification1<<-TreatmentStage[,(i-1)]==2&IntensificationFlag
      Hba1C[Intensification1,i]<<-Hba1C[Intensification1,(i-1)]+
        (Hba1C[Intensification1,(i-1)]-7.5)*FirstIntensification[,"Beta"]+FirstIntensification[,"HbA1c"]
      
      Intensification2<<-TreatmentStage[,(i-1)]==3&IntensificationFlag
      Hba1C[Intensification2,i]<<-Hba1C[Intensification2,(i-1)]+
        (Hba1C[Intensification2,(i-1)]-7.5)*SecondIntensification[,"Beta"]+SecondIntensification[,"HbA1c"]
      
      
      ###### BMI  
      
      BMI[Intensification1,i]<<-BMI[Intensification1,(i-1)]+FirstIntensification[,"Weight"]/Height[Intensification1] # treatment effect is for weight
      BMI[Intensification2,i]<<-BMI[Intensification2,(i-1)]+SecondIntensification[,"Weight"]/Height[Intensification2]# treatment effect is for weight
      
    }
    
    
    
    #Now use normal prediction equations if no intensification
    # This is an implementation of the risk factor time path equations sent by Jose
    
    
    Hba1C[!IntensificationFlag,i]<<-ConstantHbA+cbind(Female[!IntensificationFlag],Afro[!IntensificationFlag],Indian[!IntensificationFlag],Hba1C[!IntensificationFlag,(i-1)],log((Age[!IntensificationFlag]-AgeDiag[!IntensificationFlag])+(i-1)),HbA1c_DIAG[!IntensificationFlag])%*%ParasHbA
    BMI[!IntensificationFlag,i]<<-ConstantBMI+cbind(Female[!IntensificationFlag],Afro[!IntensificationFlag],Indian[!IntensificationFlag],BMI[!IntensificationFlag,(i-1)],log((Age[!IntensificationFlag]-AgeDiag[!IntensificationFlag])+(i-1)),BMI_Diag[!IntensificationFlag])%*%ParasBMI
    
    # now establish if patient will intensify next year
    IntensificationFlag<-Hba1C[,(i)]>IntensificationThreshold & TreatmentStage[,i]<3 # if already at max stage, can't help
    
    TreatmentStage[IntensificationFlag,i:ModelRunYears]<<-TreatmentStage[IntensificationFlag,i]+1 # if intensify then goes to next stage of treatment
    
    
    
    
    # continuous risk factors
    SBP[,i]<<-ConstantSBP+cbind(Female,Afro,Indian,SBP[,(i-1)],log((Age-AgeDiag)+(i-1)),SBP_Diag)%*%ParasSBP
    LDL[,i]<<-ConstantLDL+cbind(Female,Afro,Indian,LDL[,(i-1)],log((Age-AgeDiag)+(i-1)),LDL_Diag)%*%ParasLDL
    HDL[,i]<<-ConstantHDL+cbind(Female,Afro,Indian,HDL[,(i-1)],log((Age-AgeDiag)+(i-1)),HDL_Diag)%*%ParasHDL
    
    
    
    
    
    ## every 3 years heart rate, WBC and HAEM are updated
    #update 3 years at a time unless end of sim
    if((i-1)%%3==0)
    {
      WBC[,i:(min(i+2,ModelRunYears))]<<-ConstantWBC+cbind(Female,Afro,Indian,WBC[,(i-3)],log((Age-AgeDiag)+(i-3)),WBC_Diag)%*%ParasWBC
      HEARTR[,i:(min(i+2,ModelRunYears))]<<-ConstantHEARTR+cbind(Female,Afro,Indian,HEARTR[,(i-3)],log((Age-AgeDiag)+(i-3)),HEARTR_DIAG)%*%ParasHEARTR
      HAEM[,i:(min(i+2,ModelRunYears))]<<-ConstantHAEM+cbind(Female,Afro,Indian,HAEM[,(i-3)],log((Age-AgeDiag)+(i-3)),HAEM_Diag)%*%ParasHAEM
      # is it possible to undo MICALB?
      
      
      
      
      
    }
    
    
    
  }
  
  
  ## Now apply transformations for use in binary
  
  SBP<<-SBP/10
  HDL<<-HDL*10
  LDL<<-LDL*10
  HEARTR<<-HEARTR/10
  
  ### binary
  
  
  for (i in 2:ModelRunYears)
    
  { 
    BinaryMainTable<<-cbind(Female[,1],Afro[,1],Indian[,1],AgeDiag,Smoker[,(i-1)],
                            Smoker[,1],SBP[,(i-1)],Hba1C[,(i-1)],BMI[,(i-1)],
                            HDL[,(i-1)],LDL[,(i-1)],eGFR[,(i-1)],eGFR[,1],log((Age-AgeDiag)+(i-1)))
    
    MICALBPoss<<-which(MICALB[,(i-1)]==0) # only 
    MICALB[MICALBPoss,i:ModelRunYears]<<-rbinom(length(MICALBPoss),1,(1-exp(exp(ConstantMICALB+BinaryMainTable%*%ParasMICALB)*
                                                                              ((Age-AgeDiag+i-1)^WeibullMICALB-(Age-AgeDiag+i)^WeibullMICALB)))) # apply MICALB to all future observations
    
    ## for egfr, do eGFRswitch first
    eGFRabove60<<-which(eGFR[,i-1]>=60)
    eGFRswitch<<-rbinom(length(eGFRabove60),1,(1-exp(exp(ConstanteGFRswitch+BinaryMainTable%*%ParaseGFRswitch)*
                                                       ((Age-AgeDiag+i-1)^WeibulleGFRswitch-(Age-AgeDiag+i)^WeibulleGFRswitch)))) 
    ## firstly if eGFR<60
    eGFRless60<<-union(which(eGFR[,i-1]<60),  eGFRabove60[as.logical(eGFRswitch)])
    
    BetaDash<<-ConstanteGFRless60+BinaryMainTable[eGFRless60,]%*%ParaseGFRless60
    eGFR[eGFRless60,i]<<-BetaDash- # this is a + in the write-up?, confirmed #-# by Jose Leal 22/12/2020
      TobiteGFRless60*((dnorm(-BetaDash/TobiteGFRless60)-dnorm((60-BetaDash)/TobiteGFRless60))/
                         (pnorm(-BetaDash/TobiteGFRless60)-pnorm((60-BetaDash)/TobiteGFRless60)))
    
    ### now if eGFR>=60 (#all rows without egfrswitch or less than 60 )
    eGFRgrt60<<-setdiff(1:NSim,eGFRless60) # greater than 60
    
    BetaDash<<-ConstanteGFRgrt60+BinaryMainTable[eGFRgrt60,]%*%ParaseGFRgrt60
    eGFR[eGFRgrt60,i]<<-BetaDash+dnorm((60-BetaDash)/TobiteGFRgrt60)/(1-pnorm((60-BetaDash)/TobiteGFRgrt60))
    
    
    
    #some risk factors are only calculated every 3 years
    
    if((i-1)%%3==0)
    {
      
      PVDPoss<<-which(PVD[,(i-3)]==0) # only apply to patients who don't already have it
      PVD[PVDPoss,i:ModelRunYears]<<-rbinom(length(PVDPoss),1,(1-exp(exp(ConstantPVD+BinaryMainTable%*%ParasPVD)*
                                                                       ((Age-AgeDiag+i-1)^WeibullPVD-(Age-AgeDiag+i)^WeibullPVD)))) 
      
      
      ATFIBPoss<<-which(ATFIB[,(i-3)]==0)
      ATFIB[ATFIBPoss,i:ModelRunYears]<<-rbinom(length(ATFIBPoss),1,1-exp(-exp(ConstantATFIB+BinaryMainTable%*%ParasATFIB))) # need to define vcls
      
      # can apply to any patient
      Smoker[,i:(min(i+2,ModelRunYears))]<<-rbinom(NSim,1,1-logistic(exp(-(ConstantSMOKER+BinaryMainTable%*%ParasSMOKER)))) ## see logistic function
      
      
      
      
    }
    
    
    
  }
  
  ############ now need to output in correct forms (for equations,u)
  
  # eGFR needs transformation applied
  eGFR<<-eGFR/10
  
  BMICAT1<<-matrix(as.numeric(BMI<18.5),nrow = NSim, ncol = ModelRunYears) # 1 if BMI<18.5
  BMICAT3<<-matrix(as.numeric(BMI>=25),nrow = NSim, ncol = ModelRunYears) # 1 if BMI>=25
  LDL35<<-matrix(0,nrow = NSim, ncol = ModelRunYears) # knot at 3.5(Trans= *10)
  LDL35[which(LDL>35)]<<-LDL[which(LDL>35)]-35 # 
  
  eGFR60plus<<-matrix(0,nrow = NSim, ncol = ModelRunYears) # +-60 continuous (Trans= /10)
  eGFR60plus[which(eGFR>6)]<<-eGFR[which(eGFR>6)]-6 # 
  eGFR60minus<<-matrix(6,nrow = NSim, ncol = ModelRunYears) # +-60 continuous (Trans= /10)
  eGFR60minus[which(eGFR<6)]<<-eGFR[which(eGFR<6)] # 
  
  
  
}

### time paths generalised
GetTimePaths<-function() # this has an IntensificationLevel (input at top) dependancy
{
  
  ## already have a function to populate risk factors but need to add a module, which will apply treatment effects at a certain point
  # first establish values to use
  IntensificationThreshold<<-7.5 # if HbA1c over then apply treatment effect 
  
  
  #### hypos can be dealt with in a separate module
  
  
  ## will need an intensification table, the starting point will be dependent on the chose intensification level
  IntStageNo<-if(IntensificationLevel=="Initial Therapy"){1}else if(IntensificationLevel=="First Intensification"){2}else{3}
  TreatmentStage<<-matrix(IntStageNo,NSim,ModelRunYears) # higher levels won't intensify further
  
  # process will be: determine if intensification threshold reached
  # if reached and intensification< n (depending on intensification number)
  
  
  for (i in 2:ModelRunYears)
    
  { ### who has reached the intensification threshold?
    if(i==2){IntensificationFlag<-rep(1,NSim)# intensify everyone first year, then need to be under threshold and not on max treatment
    # everyone will be treated in Y1 (assuming this is initial therapy population)
    #not intolerant
    Hba1C[,2]<<-Hba1C[,1]+(Hba1C[,1]-7.5)*TreatmentMatrix[1,"Beta"]+TreatmentMatrix[1,"HbA1c"]
    
    BMI[,2]<<-BMI[,1]+TreatmentMatrix[1,"Weight"]/Height # height contains height^2 (see population generation)
    
    
    }else{
      #### Hba1c 
      Intensification1<<-TreatmentStage[,(i-1)]==2&IntensificationFlag
      Hba1C[Intensification1,i]<<-Hba1C[Intensification1,(i-1)]+
        (Hba1C[Intensification1,(i-1)]-7.5)*FirstIntensification[,"Beta"]+FirstIntensification[,"HbA1c"]
      
      Intensification2<<-TreatmentStage[,(i-1)]==3&IntensificationFlag
      Hba1C[Intensification2,i]<<-Hba1C[Intensification2,(i-1)]+
        (Hba1C[Intensification2,(i-1)]-7.5)*SecondIntensification[,"Beta"]+SecondIntensification[,"HbA1c"]
      
      
      ###### BMI  
      
      BMI[Intensification1,i]<<-BMI[Intensification1,(i-1)]+FirstIntensification[,"Weight"]/Height[Intensification1] # treatment effect is for weight
      BMI[Intensification2,i]<<-BMI[Intensification2,(i-1)]+SecondIntensification[,"Weight"]/Height[Intensification2]# treatment effect is for weight
      
    }
    
    
    
    #Now use normal prediction equations if no intensification
    # This is an implementation of the risk factor time path equations sent by Jose
    
    
    Hba1C[!IntensificationFlag,i]<<-ConstantHbA+cbind(Female[!IntensificationFlag],Afro[!IntensificationFlag],Indian[!IntensificationFlag],Hba1C[!IntensificationFlag,(i-1)],log((Age[!IntensificationFlag]-AgeDiag[!IntensificationFlag])+(i-1)),HbA1c_DIAG[!IntensificationFlag])%*%ParasHbA
    BMI[!IntensificationFlag,i]<<-ConstantBMI+cbind(Female[!IntensificationFlag],Afro[!IntensificationFlag],Indian[!IntensificationFlag],BMI[!IntensificationFlag,(i-1)],log((Age[!IntensificationFlag]-AgeDiag[!IntensificationFlag])+(i-1)),BMI_Diag[!IntensificationFlag])%*%ParasBMI
    
    # now establish if patient will intensify next year
    IntensificationFlag<-Hba1C[,(i)]>IntensificationThreshold & TreatmentStage[,i]<3 # if already at max stage, can't help
    
    TreatmentStage[IntensificationFlag,i:ModelRunYears]<<-TreatmentStage[IntensificationFlag,i]+1 # if intensify then goes to next stage of treatment
    
    
    
    
    # continuous risk factors
    SBP[,i]<<-ConstantSBP+cbind(Female,Afro,Indian,SBP[,(i-1)],log((Age-AgeDiag)+(i-1)),SBP_Diag)%*%ParasSBP
    LDL[,i]<<-ConstantLDL+cbind(Female,Afro,Indian,LDL[,(i-1)],log((Age-AgeDiag)+(i-1)),LDL_Diag)%*%ParasLDL
    HDL[,i]<<-ConstantHDL+cbind(Female,Afro,Indian,HDL[,(i-1)],log((Age-AgeDiag)+(i-1)),HDL_Diag)%*%ParasHDL
    
    
    
    
    
    ## every 3 years heart rate, WBC and HAEM are updated
    #update 3 years at a time unless end of sim
    if((i-1)%%3==0)
    {
      WBC[,i:(min(i+2,ModelRunYears))]<<-ConstantWBC+cbind(Female,Afro,Indian,WBC[,(i-3)],log((Age-AgeDiag)+(i-3)),WBC_Diag)%*%ParasWBC
      HEARTR[,i:(min(i+2,ModelRunYears))]<<-ConstantHEARTR+cbind(Female,Afro,Indian,HEARTR[,(i-3)],log((Age-AgeDiag)+(i-3)),HEARTR_DIAG)%*%ParasHEARTR
      HAEM[,i:(min(i+2,ModelRunYears))]<<-ConstantHAEM+cbind(Female,Afro,Indian,HAEM[,(i-3)],log((Age-AgeDiag)+(i-3)),HAEM_Diag)%*%ParasHAEM
      # is it possible to undo MICALB?
      
      
      
      
      
    }
    
    
    
  }
  
  
  ## Now apply transformations for use in binary
  
  SBP<<-SBP/10
  HDL<<-HDL*10
  LDL<<-LDL*10
  HEARTR<<-HEARTR/10
  
  ### binary
  
  
  for (i in 2:ModelRunYears)
    
  { 
    BinaryMainTable<<-cbind(Female[,1],Afro[,1],Indian[,1],AgeDiag,Smoker[,(i-1)],
                            Smoker[,1],SBP[,(i-1)],Hba1C[,(i-1)],BMI[,(i-1)],
                            HDL[,(i-1)],LDL[,(i-1)],eGFR[,(i-1)],eGFR[,1],log((Age-AgeDiag)+(i-1)))
    
    MICALBPoss<<-which(MICALB[,(i-1)]==0) # only 
    MICALB[MICALBPoss,i:ModelRunYears]<<-rbinom(length(MICALBPoss),1,(1-exp(exp(ConstantMICALB+BinaryMainTable%*%ParasMICALB)*
                                                                              ((Age-AgeDiag+i-1)^WeibullMICALB-(Age-AgeDiag+i)^WeibullMICALB)))) # apply MICALB to all future observations
    
    ## for egfr, do eGFRswitch first
    eGFRabove60<<-which(eGFR[,i-1]>=60)
    eGFRswitch<<-rbinom(length(eGFRabove60),1,(1-exp(exp(ConstanteGFRswitch+BinaryMainTable%*%ParaseGFRswitch)*
                                                       ((Age-AgeDiag+i-1)^WeibulleGFRswitch-(Age-AgeDiag+i)^WeibulleGFRswitch)))) 
    ## firstly if eGFR<60
    eGFRless60<<-union(which(eGFR[,i-1]<60),  eGFRabove60[as.logical(eGFRswitch)])
    
    BetaDash<<-ConstanteGFRless60+BinaryMainTable[eGFRless60,]%*%ParaseGFRless60
    eGFR[eGFRless60,i]<<-BetaDash- # this is a + in the write-up?, confirmed #-# by Jose Leal 22/12/2020
      TobiteGFRless60*((dnorm(-BetaDash/TobiteGFRless60)-dnorm((60-BetaDash)/TobiteGFRless60))/
                         (pnorm(-BetaDash/TobiteGFRless60)-pnorm((60-BetaDash)/TobiteGFRless60)))
    
    ### now if eGFR>=60 (#all rows without egfrswitch or less than 60 )
    eGFRgrt60<<-setdiff(1:NSim,eGFRless60) # greater than 60
    
    BetaDash<<-ConstanteGFRgrt60+BinaryMainTable[eGFRgrt60,]%*%ParaseGFRgrt60
    eGFR[eGFRgrt60,i]<<-BetaDash+dnorm((60-BetaDash)/TobiteGFRgrt60)/(1-pnorm((60-BetaDash)/TobiteGFRgrt60))
    
    
    
    #some risk factors are only calculated every 3 years
    
    if((i-1)%%3==0)
    {
      
      PVDPoss<<-which(PVD[,(i-3)]==0) # only apply to patients who don't already have it
      PVD[PVDPoss,i:ModelRunYears]<<-rbinom(length(PVDPoss),1,(1-exp(exp(ConstantPVD+BinaryMainTable%*%ParasPVD)*
                                                                       ((Age-AgeDiag+i-1)^WeibullPVD-(Age-AgeDiag+i)^WeibullPVD)))) 
      
      
      ATFIBPoss<<-which(ATFIB[,(i-3)]==0)
      ATFIB[ATFIBPoss,i:ModelRunYears]<<-rbinom(length(ATFIBPoss),1,1-exp(-exp(ConstantATFIB+BinaryMainTable%*%ParasATFIB))) # need to define vcls
      
      # can apply to any patient
      Smoker[,i:(min(i+2,ModelRunYears))]<<-rbinom(NSim,1,1-logistic(exp(-(ConstantSMOKER+BinaryMainTable%*%ParasSMOKER)))) ## see logistic function
      
      
      
      
    }
    
    
    
  }
  
  ############ now need to output in correct forms (for equations,u)
  
  # eGFR needs transformation applied
  eGFR<<-eGFR/10
  
  BMICAT1<<-matrix(as.numeric(BMI<18.5),nrow = NSim, ncol = ModelRunYears) # 1 if BMI<18.5
  BMICAT3<<-matrix(as.numeric(BMI>=25),nrow = NSim, ncol = ModelRunYears) # 1 if BMI>=25
  LDL35<<-matrix(0,nrow = NSim, ncol = ModelRunYears) # knot at 3.5(Trans= *10)
  LDL35[which(LDL>35)]<<-LDL[which(LDL>35)]-35 # 
  
  eGFR60plus<<-matrix(0,nrow = NSim, ncol = ModelRunYears) # +-60 continuous (Trans= /10)
  eGFR60plus[which(eGFR>6)]<<-eGFR[which(eGFR>6)]-6 # 
  eGFR60minus<<-matrix(6,nrow = NSim, ncol = ModelRunYears) # +-60 continuous (Trans= /10)
  eGFR60minus[which(eGFR<6)]<<-eGFR[which(eGFR<6)] # 
  
  
  
}



#### OM1 time paths (excl. total cholesterol also included for completeness) - may not need to use

TimePathsOM1<-function()# this has an IntensificationLevel (input at top) dependancy
{
  FRP<-fread("//nice/Data/Users/Private/TJONES/UKPDS Tables/RiskFactorProgression.csv")
  ## EMPA-REG baseline Data
  HbA1cProgpara<<-as.numeric(gsub(" .*","",FRP$V2[c(8:9,14:15)]))
  HbA1cY2para<<-as.numeric(gsub(" .*","",FRP$V2[c(11)])) # special for Hba1C
  
  SBPProgpara<<-as.numeric(gsub(" .*","",FRP$V3[c(8:9,16:17)]))
  HDLProgpara<<-as.numeric(gsub(" .*","",FRP$V4[c(8,18:19)]))
  SMOKProgpara<-as.numeric(gsub(" .*","",FRP$V5[c(8,10,12:13,20:21)]))##
  
  ## already have a function to populate risk factors but need to add a module, which will apply treatment effects at a certain point
  # first establish values to use
  IntensificationThreshold<<-7.5 # if HbA1c over then apply treatment effect 
  
  
  #### hypos can be dealt with in a separate module
  
  ## will need an intensification table, the starting point will be dependent on the chose intensification level
  IntStageNo<-if(IntensificationLevel=="Initial Therapy"){1}else if(IntensificationLevel=="First Intensification"){2}else{3}
  TreatmentStage<<-matrix(IntStageNo,NSim,ModelRunYears) # higher levels won't intensify further
  
  # process will be: determine if intensification threshold reached
  # if reached and intensification< n (depending on intensification number)
  
  
  for (i in 2:ModelRunYears)
    
  { ### who has reached the intensification threshold?
    if(i==2){IntensificationFlag<-rep(1,NSim)# intensify everyone first year, then need to be under threshold and not on max treatment
    # everyone will be treated in Y1 (assuming this is initial therapy population)
    #not intolerant
    Hba1C[,2]<<-Hba1C[,1]+(Hba1C[,1]-7.5)*TreatmentMatrix[1,"Beta"]+TreatmentMatrix[1,"HbA1c"]
    
    BMI[,2]<<-BMI[,1]+TreatmentMatrix[1,"Weight"]/Height # height contains height^2 (see population generation)
    
    
    }else{
      #### Hba1c 
      Intensification1<<-TreatmentStage[,(i-1)]==2&IntensificationFlag
      
      Hba1C[Intensification1,i]<<-Hba1C[Intensification1,(i-1)]+
        (Hba1C[Intensification1,(i-1)]-7.5)*FirstIntensification[,"Beta"]+FirstIntensification[,"HbA1c"]
      
      Intensification2<<-TreatmentStage[,(i-1)]==3&IntensificationFlag
      
      Hba1C[Intensification2,i]<<-Hba1C[Intensification2,(i-1)]+
        (Hba1C[Intensification2,(i-1)]-7.5)*SecondIntensification[,"Beta"]+SecondIntensification[,"HbA1c"]
      
      
      ###### BMI  
      
      BMI[Intensification1,i]<<-BMI[Intensification1,(i-1)]+FirstIntensification[,"Weight"]/Height[Intensification1] # treatment effect is for weight
      BMI[Intensification2,i]<<-BMI[Intensification2,(i-1)]+SecondIntensification[,"Weight"]/Height[Intensification2]# treatment effect is for weight
      
    }
    
    
    
    #Now use normal prediction equations if no intensification
    # OM1 time path equations for HbA1c (Clarke et. al 2004), note that if in second year since diagnosis there is an extra reduction (never used in our modelling as minimum population is 1.5 years and will receive treatment specccific effect)
    
    #HbA1c first
    
    Hba1C[!IntensificationFlag,i]<<-HbA1cProgpara[1]+(Hba1C[!IntensificationFlag,i-1]-7.09)*HbA1cProgpara[3]+(HbA1c_DIAG[!IntensificationFlag]-7.09)*HbA1cProgpara[4]+log(DiabYears[!IntensificationFlag]+i-1)*HbA1cProgpara[2]+7.09
    Hba1C[!IntensificationFlag & (DiabYears+i-1)>1&(DiabYears+i-1)<=2,i]<<-Hba1C[!IntensificationFlag & floor(DiabYears+i-1)==1,i]+HbA1cY2para #logic accounts for non integer years to subtract if 2nd year of diabetes
    
    BMI[!IntensificationFlag,i]<<-BMI[!IntensificationFlag,(i-1)] # No OM1 equation
    # now establish if patient will intensify next year
    IntensificationFlag<-Hba1C[,(i)]>IntensificationThreshold & TreatmentStage[,i]<3 # if already at max stage, can't help
    
    TreatmentStage[IntensificationFlag,i:ModelRunYears]<<-TreatmentStage[IntensificationFlag,i]+1 # if intensify then goes to next stage of treatment
    
    
    
    
    # continuous risk factors
    SBP[,i]<<-135.09+(SBPProgpara[1]+log(DiabYears+i-1)*SBPProgpara[2]+(((SBP[,i-1])-135.09)/10)*SBPProgpara[3]+((SBP_Diag-135.09)/10)*SBPProgpara[4])*10
    LDL[,i]<<-LDL[,(i-1)] # No OM1 equation
    HDL[,i]<<-HDL[,(i-1)] # No OM1 equation
    
    WBC[,i]<<-WBC[,(i-1)]
    HEARTR[,i]<<-HEARTR[,(i-1)]
    HAEM[,i]<<-HAEM[,i-1]
    
    eGFR[,i]<<-eGFR[,(i-1)]
    
    
    ### binary
    MICALB[,i]<<-MICALB[,(i-1)]
    PVD[,i]<<-PVD[,(i-1)]
    ATFIB[,i]<<-ATFIB[,(i-1)]
    
    if((i-1)%%3==0) # smoking does have 
    {
      
      
      # can apply to any patient
      Smoker[,i:(min(i+2,ModelRunYears))]<<-rbinom(NSim,1,logistic(exp(SMOKProgpara[1]+(DiabYears+i-1)*SMOKProgpara[2]+(AgeDiag-52.59)*SMOKProgpara[3]+FemaleFlag*SMOKProgpara[4]+Smoker[,(i-3)]*SMOKProgpara[5]+Smoker_Diag*SMOKProgpara[6]))) ## see logistic function
      
      
      
      
    }
    
    
  }
  
  ## Now apply transformations 
  
  SBP<<-SBP/10
  HDL<<-HDL*10
  LDL<<-LDL*10
  HEARTR<<-HEARTR/10
  
  ### binary
  MICALB[,i]<<-MICALB[,(i-1)]
  PVD[,i]<<-PVD[,(i-1)]
  ATFIB[,i]<<-ATFIB[,(i-1)]
  
  
  
  
  #some risk factors are only calculated every 3 years
  
  
  
  
  
  
  ############ now need to output in correct forms (for equations,u)
  
  # eGFR needs transformation applied
  eGFR<<-eGFR/10
  
  BMICAT1<<-matrix(as.numeric(BMI<18.5),nrow = NSim, ncol = ModelRunYears) # 1 if BMI<18.5
  BMICAT3<<-matrix(as.numeric(BMI>=25),nrow = NSim, ncol = ModelRunYears) # 1 if BMI>=25
  LDL35<<-matrix(0,nrow = NSim, ncol = ModelRunYears) # knot at 3.5(Trans= *10)
  LDL35[which(LDL>35)]<<-LDL[which(LDL>35)]-35 # 
  
  eGFR60plus<<-matrix(0,nrow = NSim, ncol = ModelRunYears) # +-60 continuous (Trans= /10)
  eGFR60plus[which(eGFR>6)]<<-eGFR[which(eGFR>6)]-6 # 
  eGFR60minus<<-matrix(6,nrow = NSim, ncol = ModelRunYears) # +-60 continuous (Trans= /10)
  eGFR60minus[which(eGFR<6)]<<-eGFR[which(eGFR<6)] # 
  
  
  
}
####### clear event storage tables between runs

ClearLastRunEvents<-function()
  
{
  ## Now event histories and generating empty equations 
  Death[]<<-0
  DeathHist[]<<-0
  
  
  
  StrokeHist[]<<-0# can be updated each model run# 
  Stroke1[]<<-0
  Stroke2[]<<-0
  StrokeDeathEq[]<<-0
  
  
  AmpHist[]<<-0 # no amputation history data
  Amp1[]<<-0
  Amp2[]<<-0
  
  CHFHist[]<<-0
  CHF[]<<-0
  
  MIHist[]<<-0
  MI1[]<<-0
  MI2[]<<-0
  MIDeathEq[]<<-0
  
  IHDHist[]<<-0
  IHD[]<<-0
  
  UlcerHist[]<<-0 # assumed 5% for initail therapy
  Ulcer[]<<-0
  
  BlindHist[]<<-0 # assumed 5% for initial therapy
  
  Blind[]<<-0
  
  RenalHist[]<<-0
  Renal[]<<-0
  
}

###### create a new function that will loop through possible treatment values


