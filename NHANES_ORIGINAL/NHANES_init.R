# Initialize

# helper function to check for required libraries, install if requested and load
librariesAsk <- function(requiredlibs) {
  # Determine missing packages
  reqnotinst <- setdiff(requiredlibs,installed.packages())
  
  if (length(reqnotinst)>0) {
    print(paste("Skript: The following required packages are not installed:",reqnotinst))
    # Ask what to do (until valid answer)
    while(!( (inst<-toupper(readline("Do you want to install (y/n)?"))) %in% c("Y","N"))){}
    # Install or stop
    if(inst=="Y") install.packages(reqnotinst)
    else stop(simpleError(paste("Skript: Please install R-packages:",reqnotinst)))
  }
  
  # Load required packages
  lapply(requiredlibs,library,character.only=TRUE)
}

requiredlibs=c("tidyverse","nhanesA","arsenal","haven")
librariesAsk(requiredlibs)

# Mortality Update year
nhanes_mort_year=2019
nhanes_max_year=nhanes_mort_year-6

# Build Table of Studies
(studies<-data.frame(year=seq(from=1999,to=nhanes_max_year,by=2)) %>% 
    # NHANES Continuous, 1999+
    mutate(name=paste0("NHANES_",year,"_",year+1), wave=LETTERS[(year-1997)/2]) %>% 
    # NHANES III 1988-1994 two cohorts
    bind_rows(list(year=NA, name='NHANES_III', wave=NA))
  
)

# list of tables
nhanes_groups<-list("DEMO","CVX","BMX","BPX","BPQ","ALQ","DIQ","MCQ","HIQ","SMQ","PAQ","PFQ","SLQ","OCQ") 
# shorter list for debug
groups<-list("CVX")

# list of additional variables, if spread accross different "groups"
nhanes_extra_vars<-list("LBXCOT")

# list of variables
nhanes_vars <- c("SEQN", # ID within NHANES III and NHANES_Cont (not across both!)
                 "ID",  # ID in data set, Study+SEQN
                 "Wave", # Wave of NHANES_Cont, A-H
                 "Phase", # Phase of NHANES III, 1 or 2
                 "Cycle", # Study Cycle, Wave resp. Phase
                 "Study", # 3/"NHANES III" or 4/"NHANES Continuous"
                 "SampleWeightMEC", # Sample Weight MEC (Examination Data) from WTPFEX6 (III)/WTMEC4YR(Cont A,B)/MEC2YR(Cont Rest)
                 "YearDOS",
                 "Gender",
                 "AgeDOS",
                 "Tot_Income_family",
                 "Tot_Income_household",
                 "Educational_Level_20plus",
                 "Marital_status",
                 "EverSmoker",
                 "CurrentSmoker",
                 "highBP",
                 "highChol",
                 "Alcohol",
                 "Diabetes",
                 "pastHeartAtt",
                 "pastStroke",
                 "pastHeartDis",
                 "pastHeartFailure",
                 "pastAngina",
                 "pastCancer",
                 "Industry",
                 "Occupation",
                 "Health_insurance_coverage",
                 "Cover_Private_insurance",
                 "Cover_Medicare",
                 "Cover_Medicaid_CHIP",
                 "Cover_other_government_insurance",
                 "Cover_any_single_plan",
                 "compare_activity_same_age",
                 "difficulty_walk_quarter_mile",
                 "Cardiovascular_fitness_level",
                 "Predicted_VO2MAX",
                 "Estimated_VO2MAX",
                 "BMI",
                 "Waist_circumference",
                 "Weight_body_metric",
                 "Standing_height",
                 "total_fat_sat",
                 "total_fat_monounsat",
                 "total_fat_polyunsat",
                 "total_fat",
                 "total_fiber",
                 "TotalCholesterol",
                 "HDL",
                 "LDL",
                 "Triglyceride",
                 "Glycohemoglobin",
                 "plasGluMG",
                 "plasGluMMOL",
                 "Cotinine",
                 "pastSkinCancer",
                 "pastOtherCancer",
                 "Family_Poverty_income_ratio",
                 "general_health_condition",
                 "Pulse",
                 "SBP",
                 "DBP",
                 "Smoker"
                    
)

