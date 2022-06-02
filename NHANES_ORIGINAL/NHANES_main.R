# Main script
# Prepare data frame NHANES_ALL 
# Combine results of individual scripts


# Make sure right working directory
if(!file.exists("./NHANES_main.R")){
  print(paste0("Skript: NHANES_main.R does not exist in working directory ",getwd()))
  # check if library rstudioapi is available
  pass= require(rstudioapi) 
  if(pass){
    # get directory of script if using R Studio
    wd <- dirname(getActiveDocumentContext()$path)
    # Ask whether okay to change working directory to path of script
    while(!( (chwd<-toupper(readline(paste0("Do you want to change to ",wd," (y/n)?")))) %in% c("Y","N"))){}
    # Change path or stop according to answer
    if(chwd=="Y") setwd(wd)
    else pass <- FALSE
  }
  if(!pass) stop(simpleError("Skript: NHANES_main.R does not exist in working directory. Change working directory first, please."))
}

# Make sure run is initialized 
if(!exists("studies")) source("./NHANES_init.R") 


# NHANES Continuous part

if(!exists("NHANES_Cont")) 
{
  if(file.exists("./data/NHANES_Cont.RData")) 
    load("./data/NHANES_Cont.RData")
  else
    source("./NHANES_Cont.R")
}

# older NHANES III Part

if(!exists("NHANES_III_proc")) 
{
  if(file.exists("./data/NHANES_III_proc.RData")) 
    load(file="./data/NHANES_III_proc.RData") 
  else 
    source("./NHANES_III_proc.R")
}




# Mortality

if(!exists("NHANES_mort")) 
{
  if(file.exists("./data/NHANES_mort.RData")) 
    load(file="./data/NHANES_mort.RData") 
  else 
    source("./NHANES_mort.R")
}



# Activity
if (!exists("NHANES_Activity"))
{
  if(file.exists("./data/NHANES_Activity.RDS")) 
    NHANES_Activity<-readRDS("./data/NHANES_Activity.RDS") 
  else 
    source("./NHANES_Activity.R")
}


# concatenate studies and join mortality (inner join)
NHANES_ALL <- inner_join(bind_rows(NHANES_III_proc, left_join(NHANES_Cont,NHANES_Activity, by="SEQN")),
                            select(NHANES_mort,-Wave),by=c("SEQN","Study")) %>% 
# Alternative ohne NHANES_Activity
#    NHANES_ALL <- inner_join(bind_rows(NHANES_III_proc, NHANES_Cont), NHANES_mort,by=c("wave","SEQN")) %>% 

  # Combine weights, approximately (MEC Weights not measured on same base population NHANES III vs. Cont)
    mutate(SampleWeightMEC=SampleWeightMEC*if_else(Study==3,1,8)/9,
  # Survey Cycle
            Cycle=if_else(Study==3,as.character(Phase),Wave),
            Study=factor(Study,seq(1:4),
              c("NHANES", "NHANES II", "NHANES III", "NHANES Continuous")),
            yearDOS=case_when(Cycle=="1"~1990,
                              Cycle=="2"~1993,
                              Cycle=='A'~2000,
                             Cycle=='B'~2002,
                             Cycle=='C'~2004,
                             Cycle=='D'~2006,
                             Cycle=='E'~2008,
                             Cycle=='F'~2010,
                             Cycle=='G'~2012,
                             Cycle=='H'~2014), 
            yearDOB = yearDOS-AgeDOS,
            Death = ifelse(mortstat=='Assumed Dead',1,0),
            Surv=if_else(is.na(permth_exm),permth_int,permth_exm),
            yearDOE=yearDOS + Surv %/% 12,
            Waist_circumference=na_if(Waist_circumference,88888),
            Family_Poverty_income_ratio=na_if(Family_Poverty_income_ratio,888888),
            Standing_height=na_if(Standing_height,88888),
            Weight_body_metric=na_if(Weight_body_metric,888888),
            Triglyceride=na_if(Triglyceride,88888),
            Pulse=na_if(Pulse, 888),
            Glycohemoglobin=na_if(Glycohemoglobin,8888),
            plasGluMG=na_if(plasGluMG,88888),
            plasGluMMOL=na_if(plasGluMMOL,888888),
            LDL=na_if(LDL,8888),
            TotalCholesterol=na_if(TotalCholesterol,88888),
            BMI=na_if(BMI,8888),
            SBP=na_if(SBP,888),
            HDL=na_if(HDL,8888),
            DBP=na_if(DBP,888)
            
  
    ) %>% 
    filter(is.na(AgeDOS)==F)
  
save(NHANES_ALL,file="./data/NHANES_ALL.RData")
write.csv(NHANES_ALL,"./data/NHANES_ALL.csv", row.names = FALSE)  
    
print(paste("Duplicate SEQN in NHANES_ALL:", sum(duplicated(NHANES_ALL$SEQN))))
print(paste("Duplicate ID in NHANES_ALL:", sum(duplicated(NHANES_ALL$ID))))

