# Make sure right working directory
if(!file.exists("./NHANES_main.R")){
  stop(simpleError("Skript: NHANES_main.R does not exist in working directory. Change working directory first, please."))
}


# Load the original NHANES_ALL under different name
loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}

if(!exists("NHANES_ALL_SCOR")) 
  NHANES_ALL_SCOR <- loadRData("../NHANES_ALL.RData")

if(!exists("NHANES_ALL")) {
  if(file.exists("./data/NHANES_ALL.RData"))
    NHANES_ALL <- loadRData("./data/NHANES_ALL.RData")
  else 
    source("NHANES_main.R")
}

print("===================")
print("Summary comparisons")
print("===================")
print("")
print("variables not in old")
print("===================")
print(setdiff(colnames(NHANES_ALL),colnames(NHANES_ALL_SCOR)))
print("")
print("variables not in new")
print("===================")
print(d<-setdiff(colnames(NHANES_ALL_SCOR),colnames(NHANES_ALL)))
print("variables not in new (other than group/2011")
print("===================")
print(d[!str_detect(d,"2011|group")])


print("Length and Deaths")
print("===================")
print(paste0("nrow SCOR: ",nrow(NHANES_ALL_SCOR)," new: ",nrow(NHANES_ALL)))
 
table(NHANES_ALL_SCOR$mortstat)
table(NHANES_ALL$mortstat)



# incompatible classes (integer, factor)
#class(NHANES_ALL_SCOR$eligstat)
#class(NHANES_ALL$eligstat)
# only one level so drop


NHANES_ALL_COMP <- bind_rows(NHANES_ALL %>% select(-eligstat) %>% mutate(source="new"),
                             NHANES_ALL_SCOR %>% select(-eligstat) %>% mutate(source="old"))

table(select(NHANES_ALL_COMP,source,mortstat),exclude=NULL)


# Vergleich SCOR / NEW mit comparedf aus dem Paket arsenal



library(arsenal)

NHANES_ALL_SCOR <- NHANES_ALL_SCOR %>% 
  mutate(Study=if_else(NHANES_TABLE=='NHANES_III',3,4),
         ID=paste0(Study,SEQN))


results <- comparedf(data.frame(NHANES_ALL), NHANES_ALL_SCOR, by="ID", tol.factor= "labels")

test_diff <- diffs(results, vars=c('Tot_Income_family', 'Marital_status','EverSmoker','Alcohol', 'Educational_Level_20plus',
  'eligstat','mortstat','permth_int','permth_exm','yearDOB','yearDOS','yearDOE',"AgeDOS"))


test_diff %>% group_by(var.x) %>% summarise(count=n())

# Unterschiede in einzelnen Variablen zwischen NHANES_ALL und NHANES_SCOR (alt)
# Tot_income_family: nhanes_III_Daten neu: falsche / verschiedene Codierung der einzelnen Einkommensklassen
# Marital_status: Rechtschreibung / Groß- / Kleinschreibung
# EverSmoker: yes/no <--> ausformuliert
# Educational Level: Groß- / Kleinschreibung
# Alcohol: 100% Übereinstimmung

