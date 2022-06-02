# Prepare data frame NHANES_mort concatenating all public linked mortality files
# Original source: Sample Code published by CDC

if(!exists("studies")) source("./NHANES_init.R") 



# build data set
NHANES_mort <- data.frame()

for (i in 1:nrow(studies)){
  studyfile=paste0(studies$name[i],"_MORT_",nhanes_mort_year,"_PUBLIC.dat")
  url=paste0("https://ftp.cdc.gov/pub/health_statistics/nchs/datalinkage/linked_mortality/",studyfile)
  path=paste0("./data/",studyfile)
  
  print(paste("Mortality, processing",studyfile))
  
  if(!file.exists(path)) download.file(url,destfile=path)
  
  # read in the fixed-width format ASCII file
  NHANES_mort <- bind_rows(NHANES_mort, 
                read_fwf(file=path,
                    col_types = "ciiiiiiiddii",
                    fwf_cols(publicid = c(1,14),
                         eligstat = c(15,15),
                         mortstat = c(16,16),
                         ucod_leading = c(17,19),
                         diabetes = c(20,20),
                         hyperten = c(21,21),
                         dodqtr = c(22,22),
                         dodyear = c(23,26),
                         wgt_new = c(27,34),
                         sa_wgt_new = c(35,42),
                         permth_int = c(43,45),
                         permth_exm = c(46,48)
                    ),
                    na = "."
                ) %>% mutate(Wave=studies$wave[i],
                          Study=if_else(is.na(Wave),3,4)) 
            )

}

# create the ID (SEQN) for the NHANES surveys
# NOTE:   SEQN is the unique ID for NHANES.
# But only within NHANES_III, and within NHANES_Cont, not accross
NHANES_mort <- NHANES_mort %>% mutate(SEQN = as.integer(substr(publicid,1,5)),
                                      ID = paste0(Study,SEQN)) %>% 


#Drop NHIS variables
  select( -publicid,
    -dodqtr,
    -dodyear,
    -wgt_new,
    -sa_wgt_new) %>% 
  mutate(eligstat=
    factor(eligstat,labels=
             c("Eligible", "Under age 18, not available for public release", "Ineligible"))) %>% 
  mutate(mortstat=
           factor(mortstat,labels=
                    c("Assumed Alive", "Assumed Dead"))) %>% 
  mutate(ucod_leading=
           factor(ucod_leading,labels=
                    c( "Accidents (unintentional injuries)"         
                      , "All other causes (residual)"                
                      , "Alzheimer's disease"                        
                      , "Cerebrovascular diseases"                   
                      , "Chronic lower respiratory diseases"         
                      , "Diabetes mellitus"                          
                      , "Disease of heart"                           
                      , "Influenza and pneumonia"                    
                      , "Malignant neoplasm"                         
                      , "Nephritis, nephrotic syndrome and nephrosis"))) %>% 
  mutate(hyperten=
           factor(hyperten,labels=
                    c("No", "Yes"))) %>% 
  mutate(diabetes=
           factor(diabetes,labels=
                    c("No", "Yes"))) %>% 
  
  
#Filter on eligstat=1
  filter(eligstat=="Eligible")
  


# duplicate SEQN
print(paste("Duplicate SEQN in NHANES_mort:", sum(duplicated(NHANES_mort$SEQN))))



#save
save(NHANES_mort,file="./data/NHANES_mort.RData")
