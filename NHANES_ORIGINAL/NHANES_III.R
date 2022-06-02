# Read NHANES III tables
#
# We use the official SAS programs to extract information how to read the 3 DAT Files
# for the parts EXAM, LAB, ADULT (no mortality information on YOUTH)

# init project
if(!exists("studies")) source("./NHANES_init.R") 


# folder of data on CDC website for NHANES III
folderurl <- "https://wwwn.cdc.gov/nchs/data/nhanes3/1a/"

# Where to find information in SAS programs for EXAM, ADULT and LAB parts
sasparts = data.frame(inputskip=c(2895,1390,643),
                   inputto=c(5263,2628,999),
                   lengthskip=c(7,7,7),
                   lengthto=c(2375,1245,363),
                   row.names=c("EXAM","ADULT","LAB"));

# loop over the three parts
for(part in rownames(sasparts)) {
  print(paste0("Processing NHANES III part ",part))
  dataname <- paste0("NHANES_III_",part)
  filename <- paste0("./data/",dataname,".RData")
  sasprogname <- paste0(tolower(part),".sas")
  sasdataname <- paste0(tolower(part),".dat")
  sasprogpath <- paste0("./data/",sasprogname)
  sasdatapath <- paste0("./data/",sasdataname)
  inputskip<-sasparts[part,]$inputskip
  inputto<-sasparts[part,]$inputto
  lengthskip<-sasparts[part,]$lengthskip
  lengthto<-sasparts[part,]$lengthto

  #check if data already loaded
  if(!exists(dataname)) 
  {
    #check if data already saved
    if(file.exists(filename)) 
      # read saved data
      load(file=filename) 
    else {
      # load sas program file for data definition - binary mode "wb" seems necessary on some machines
      if(!file.exists(sasprogpath)) 
        download.file(paste0(folderurl,sasprogname),sasprogpath,mode="wb")
      # load data file - binary mode "wb" seems necessary on some machines
      if(!file.exists(sasdatapath)) 
        download.file(paste0(folderurl,sasdataname),sasdatapath,mode="wb")
      
      # Read CDC sas program file for part to get data definition
      # Read the input statement, extract variable name, start, end
      datdef <- read_fwf(sasprogpath,skip=inputskip,n_max=inputto-inputskip,
                                col_positions=fwf_positions(start=c(9,18),end=c(17,NA),col_names = c("Variable","Position"))) %>% 
            mutate(start=as.numeric(str_extract(Position,'\\d+(?=(-\\d+)?)')),end=as.numeric(str_extract(Position,'(?<=-)\\d+'))) %>% 
            mutate(end=if_else(is.na(end),start,end))
      
      # Read the length statement, extract type
      types <- read_fwf(sasprogpath,skip=lengthskip,n_max=lengthto-lengthskip,
                              col_positions=fwf_positions(start=c(18),end=c(18),col_names = c("type")),col_types="c") %>% 
        mutate(type=if_else(type=="$","c","n","n")) %>% pull(type) %>% paste0(.,collapse="")
      
      # create dataset, e.g. NHANES_III_EXAM, etc.
      assign(dataname,read_fwf(sasdatapath,
                                  col_positions=fwf_positions(start=datdef$start,end=datdef$end,col_names=datdef$Variable),
                                  col_types = types))
      #save the dataset
      save(list=dataname,file=filename)
      
    }
  }
}



# left join ADULT on SEQN (uniqueness checked, don't expect mortality stats on YOUTH part)
NHANES_III <- NHANES_III_ADULT %>% 
    left_join(NHANES_III_EXAM, by="SEQN") %>% 
    left_join(NHANES_III_LAB, by="SEQN")

save(NHANES_III,file="./data/NHANES_III.RData")
