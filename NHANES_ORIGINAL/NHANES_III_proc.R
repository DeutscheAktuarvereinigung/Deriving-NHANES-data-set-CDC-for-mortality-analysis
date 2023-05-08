# Process NHANES III tables

# Ensure NHANES_III exists
if(!exists("NHANES_III")) { 
  if(file.exists("./data/NHANES_III.RData")) 
      load(file="./data/NHANES_III.RData") 
    else 
      source("./NHANES_III.R") 
}


# define function to rename variables consistently with NHANES_Cont renaming

nhanes_III_rename <- function(data){
  data %>% rename(
       Gender=HSSEX,
       AgeDOS=HSAGEIR,
       SampleWeightMEC=WTPFEX6,
       BMI=BMPBMI,
       Waist_circumference=BMPWAIST,
       Weight_body_metric=BMPWT,
       Standing_height=BMPHT,
       Alcohol=MAPE7,
       FSTSYSBP=PEP6G1,
       SECSYSBP=PEP6H1,
       THRSYSBP=PEP6I1,
       FSTDIBP=PEP6G3,
       SECDIBP=PEP6H3,
       THRDIBP=PEP6I3,
       total_fat_sat=DRPNSFAT,
       total_fat_monounsat=DRPNMFAT,
       total_fat_polyunsat=DRPNPFAT,
       total_fat=DRPNTFAT,
       total_fiber=DRPNFIBE,
       TotalCholesterol=TCPSI,
       HDL=HDPSI,
       LDL=LCPSI,
       Triglyceride=TGPSI,
       Glycohemoglobin=GHP,
       plasGluMG=G1P,
       plasGluMMOL=G1PSI,
       Cotinine=COP,
       EverSmoker=HAR1,
       CurrentSmoker=HAR3,
       Diabetes=HAD1,
       highBP=HAE2,
       highChol=HAE7,
       pastHeartAtt=HAF10,
       pastHeartFailure=HAC1C,
       pastStroke=HAC1D,
       pastSkinCancer=HAC1N,
       pastOtherCancer=HAC1O,
       Phase=SDPPHASE,
       Family_Poverty_income_ratio=DMPPIR,
       Marital_status=HFA12,
       Educational_Level_20plus=HFA8R,
       Cover_Medicare=HFB1,
       Cover_Medicaid_CHIP=HFB6,
       Cover_Private_insurance=HFB10,
       Tot_Income_family_comp20000=HFF18,
       Tot_Income_family=HFF19R,
       general_health_condition=HAB1,
       difficulty_walk_quarter_mile=HAH1,
       compare_activity_same_age=HAT28,
       Pulse=HAZA5
  )
}

# define function to recode levels consistently with NHANES Cont levels

nhanes_III_recode <- function(data){
  
  data %>% 
    mutate(Gender=factor(Gender,seq(1:2),c("Male" , "Female"))) %>%
    mutate(Alcohol=factor(Alcohol,seq(1:2),c("Yes" , "No"))) %>%
    mutate(EverSmoker=factor(EverSmoker,seq(1:2),c("Yes" , "No"))) %>%
    mutate(CurrentSmoker=factor(CurrentSmoker,seq(1:2),c("Yes" , "No"))) %>%
    mutate(Diabetes=factor(Diabetes,seq(1:2),c("Yes" , "No"))) %>%
    mutate(highBP=factor(highBP,seq(1:2),c("Yes" , "No"))) %>%
    mutate(highChol=factor(highChol,seq(1:2),c("Yes" , "No"))) %>%
    mutate(pastHeartAtt=factor(pastHeartAtt,seq(1:2),c("Yes" , "No"))) %>%
    mutate(pastHeartFailure=factor(pastHeartFailure,seq(1:2),c("Yes" , "No"))) %>%
    mutate(pastStroke=factor(pastStroke,seq(1:2),c("Yes" , "No"))) %>%
    mutate(pastSkinCancer=factor(pastSkinCancer,seq(1:2),c("Yes" , "No"))) %>%
    mutate(pastOtherCancer=factor(pastOtherCancer,seq(1:2),c("Yes" , "No"))) %>%
    #mutate(Phase=factor(Phase,seq(1:1),c())) %>%
    # Marital_Status: Re-aligned with NHANES Cont (Married spouse in household or not --> merged, Living as married->renamed )
    mutate(Marital_status=factor(Marital_status,seq(1:7),c("Married" , "Married", "Living with partner" , "Widowed" , 
      "Divorced" , "Separated" , "Never married"))) %>%
    mutate(Educational_Level_20plus=factor(Educational_Level_20plus+1,seq(1:18),
                                           c("Less than 9th grade"
            ,"Less than 9th grade"
            ,"Less than 9th grade"
            ,"Less than 9th grade"
            ,"Less than 9th grade"
            ,"Less than 9th grade"
            ,"Less than 9th grade"
            ,"Less than 9th grade"
            ,"Less than 9th grade"
            ,"9-11th grade"
            ,"9-11th grade"
            ,"9-11th grade"
            ,"High School Grad/GED or equivalent"
            ,"Some College or AA degree"
            ,"Some College or AA degree"
            ,"College Graduate or above"
            ,"College Graduate or above"
            ,"College Graduate or above"))) %>% 
    mutate(Cover_Medicare=factor(Cover_Medicare,seq(1:2),c("Yes" , "No"))) %>%
    mutate(Cover_Medicaid_CHIP=factor(Cover_Medicaid_CHIP,seq(1:2),c("Yes" , "No"))) %>%
    mutate(Cover_Private_insurance=factor(Cover_Private_insurance,seq(1:2),c("Yes" , "No"))) %>%
    # levels of Tot_Income_family(_comp20000) aligned with NHANES Cont levels
    mutate(Tot_Income_family_comp20000=factor(Tot_Income_family_comp20000,seq(1:2),c("Under $20000" , "Over $20000"))) %>%
    mutate(Tot_Income_family=factor(Tot_Income_family+1,seq(1:28),c("$0 to $4999" # R won't accept 0:27
                                                                  ,"$0 to $4999"
                                                                  ,"$0 to $4999"
                                                                  ,"$0 to $4999"
                                                                  ,"$0 to $4999"
                                                                  ,"$0 to $4999"
                                                                  ,"$5000 to $9999"
                                                                  ,"$5000 to $9999"
                                                                  ,"$5000 to $9999"
                                                                  ,"$5000 to $9999"
                                                                  ,"$5000 to $9999"
                                                                  ,"$10000 to $14999"
                                                                  ,"$10000 to $14999"
                                                                  ,"$10000 to $14999"
                                                                  ,"$10000 to $14999"
                                                                  ,"$10000 to $14999"
                                                                  ,"$15000 to $19999"
                                                                  ,"$15000 to $19999"
                                                                  ,"$15000 to $19999"
                                                                  ,"$15000 to $19999"
                                                                  ,"$15000 to $19999"
                                                                  ,"$20000 to $24999"
                                                                  ,"$25000 to $34999"
                                                                  ,"$25000 to $34999"
                                                                  ,"$35000 to $44999"
                                                                  ,"$35000 to $44999"
                                                                  ,"$45000 to $49999"
                                                                  ,"Over $50000"))) %>%
    mutate(general_health_condition=factor(general_health_condition,seq(1:5),c("Excellent" , "Very good" , "Good" , "Fair" , "Poor"))) %>%
    mutate(difficulty_walk_quarter_mile=factor(difficulty_walk_quarter_mile,seq(1:4),c("No difficulty" , "Some difficulty" , "Much difficulty" , "Unable to do"))) %>%
    mutate(compare_activity_same_age=factor(compare_activity_same_age,seq(1:3),c("More active" , "Less active" , "As Active")))
}


# MAIN PART: Process the NHNANES III raw data ##########################

NHANES_III_proc <- NHANES_III %>% 
  nhanes_III_rename() %>% 
  nhanes_III_recode() %>% 
  mutate(SBP=rowMeans(select(.,FSTSYSBP,
                      SECSYSBP,
                      THRSYSBP),na.rm=TRUE),
         DBP=rowMeans(select(.,
                      FSTDIBP,
                      SECDIBP,
                      THRDIBP),na.rm=TRUE),
         Smoker=if_else(is.na(CurrentSmoker),EverSmoker,CurrentSmoker),
         Tot_Income_family = as.factor( # use Over/Under 20000 info in line with NHANES Cont
           if_else(is.na(Tot_Income_family),
                   as.character(Tot_Income_family_comp20000),
                   as.character(Tot_Income_family))
            ),
         Study=3,
         Cycle=as.character(Phase)
      ) %>% 
  select(
    suppressWarnings(one_of(nhanes_vars))
  )



save(NHANES_III_proc, file="./data/NHANES_III_proc.RData")
