# Full re-run to recreate everything
# (except downloads)

# Make sure right working directory
if(!file.exists("./NHANES_main.R")){
  stop(simpleError("Skript: NHANES_main.R does not exist in working directory. Change working directory first, please."))
}


# clean all variables and data sets to make sure NHANES_main.R recreates from scratch
# 
rm(list=ls())


# Ask what to do delete
while(!( (del<-toupper(
  readline("Do you want to delete downloads in addition to processed data (y/n)?"))) 
  %in% c("Y","N"))){}

# use unlink() to delete files by wildcard
if (del=="Y") {
  unlink('./data/*') # delete everything
} else {
  # delete data processed by project 
  unlink('./data/*.RDS') 
  unlink('./data/*.RData')
}
# Rerun all
source("NHANES_main.R")