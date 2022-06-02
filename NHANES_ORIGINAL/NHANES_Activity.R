#################
# Get activity data (not available from nhanesA)
#################

library(haven)

# Make sure run is initialized 
if(!exists("studies")) {
  # Make sure right working directory
  if(!file.exists("./NHANES_init.R")){
    stop(simpleError("NHANES_init.R does not exist in working directory. Change working directory first, please."))
  }
  
  source("./NHANES_init.R") 
}


# create function to download, unzip, and aggregate acitivity data per wave
# (Activity data too large to bind first and aggregate later on some computers)
process_paxraw<-function(paxwave) {
  year<-studies %>% filter(wave==paxwave) %>% pull(year)
  table<-paste0("PAXRAW_",paxwave)
  zip<-paste0("./data/",table,".ZIP")
  xpt<-paste0("./data/",tolower(table),".xpt")
  url<-paste0("https://wwwn.cdc.gov/Nchs/Nhanes/",year,"-",year+1,"/",table,".ZIP")
  
  if(!file.exists(zip))
    download.file(url,zip,mode='wb')
  if(!file.exists(xpt))
    unzip(zip,exdir="./data") # ./data/ would not work on some systems
  
  read_xpt(xpt) %>% 
    mutate(PAXSTEP=if (paxwave=="C") NA else PAXSTEP) %>% #PAXSTEP not available in wave C
    group_by(SEQN,PAXDAY) %>%
    summarize(INTENSITY_MINUTES=sum(PAXINTEN),STEPS=sum(PAXSTEP)) %>% #get daily sums
    group_by(SEQN) %>% 
    summarize(INTENSITY_MINUTES=mean(INTENSITY_MINUTES),STEPS=mean(STEPS)) # get means per person
    
}

# Activity data set to be merged with NHANES_Cont on SEQN
NHANES_Activity<-bind_rows(process_paxraw("C"),process_paxraw("D"))

saveRDS(NHANES_Activity,file = "./data/NHANES_Activity.RDS")
