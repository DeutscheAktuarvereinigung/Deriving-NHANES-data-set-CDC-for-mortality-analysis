###############################
# Prepare data frame NHANES_Cont 
###############################

# make sure we're initialized
if(!exists("studies")) source("./NHANES_init.R") 


##################
# Define helper functions
##################

# function to strip labels from variable names, values and class
# https://stackoverflow.com/questions/2394902/remove-variable-labels-attached-with-foreign-hmisc-spss-import-functions
clear.labels <- function(x) {
  if(is.list(x)) {
    for(i in seq_along(x)) {
      class(x[[i]]) <- setdiff(class(x[[i]]), "labelled") 
      attr(x[[i]],"label") <- NULL
    } 
  } else {
    class(x) <- setdiff(class(x), "labelled")
    attr(x, "label") <- NULL
  }
  return(x)
}


# create function to consistently rename NHANES Continuous variables over waves
nhanes_rename <- function(data)
{
  
  data <- data %>% 
    
    rename_at(vars(matches('RIAGENDR')), function(x){'Gender'}) %>% # DEMO
    rename_at(vars(matches('RIDAGEYR')), function(x){'AgeDOS'}) %>% # DEMO
    rename_at(vars(matches('INDFMINC|INDFMIN2')), function(x){'Tot_Income_family'}) %>% # DEMO
    rename_at(vars(matches('INDFMPIR')), function(x){'Family_Poverty_income_ratio'}) %>% # DEMO
    rename_at(vars(matches('INDHHINC|INDHHIN2')), function(x){'Tot_Income_household'}) %>% # DEMO
    rename_at(vars(matches('DMDEDUC2')), function(x){'Educational_Level_20plus'}) %>% # DEMO
    rename_at(vars(matches('DMDMARTL')), function(x){'Marital_status'}) %>% # DEMO
    rename_at(vars(matches('BMXBMI')), function(x){'BMI'}) %>% # BMX
    rename_at(vars(matches('BMXWAIST')), function(x){'Waist_circumference'}) %>% # BMX
    rename_at(vars(matches('BMXWT')), function(x){'Weight_body_metric'}) %>% # BMX
    rename_at(vars(matches('BMXHT')), function(x){'Standing_height'}) %>% # BMX
    rename_at(vars(matches('BPXPLS')), function(x){'Heart_Pulse'}) %>% # BMX
    rename_at(vars(matches('SMQ020')), function(x){'EverSmoker'}) %>% # SMQ
    rename_at(vars(matches('SMQ040')), function(x){'CurrentSmoker'}) %>% # SMQ
    rename_at(vars(matches('BPQ020')), function(x){'highBP'}) %>% # BPQ
    rename_at(vars(matches('BPQ080')), function(x){'highChol'}) %>% # BPQ
    rename_at(vars(matches('ALQ150|ALQ151')), function(x){'Alcohol'}) %>% # ALQ
    rename_at(vars(matches('DIQ010')), function(x){'Diabetes'}) %>% # DIQ
    rename_at(vars(matches('MCQ160E')), function(x){'pastHeartAtt'}) %>% # MCQ
    rename_at(vars(matches('MCQ160F')), function(x){'pastStroke'}) %>% # MCQ
    rename_at(vars(matches('MCQ160C')), function(x){'pastHeartDis'}) %>% # MCQ
    rename_at(vars(matches('MCQ160B')), function(x){'pastHeartFailure'}) %>% # MCQr
    rename_at(vars(matches('MCQ160D')), function(x){'pastAngina'}) %>% # MCQ
    rename_at(vars(matches('MCQ220')), function(x){'pastCancer'}) %>% # MCQ
    rename_at(vars(matches('OCD230')), function(x){'Industry'}) %>% #OCQ
    rename_at(vars(matches('OCD240')), function(x){'Occupation'}) %>% #OCQ
    rename_at(vars(matches('HID010|HIQ011')), function(x){'Health_insurance_coverage'}) %>% # HIQ
    rename_at(vars(matches('^(HID030A|HIQ031A)$')), function(x){'Cover_Private_insurance'}) %>% # HIQ ganzer Name, Ausschluss HIQ031AA
    rename_at(vars(matches('HID030B|HIQ031B')), function(x){'Cover_Medicare'}) %>% # HIQ
    rename_at(vars(matches('HID030C|HIQ031D')), function(x){'Cover_Medicaid_CHIP'}) %>% # HIQ
    rename_at(vars(matches('HID030D|HIQ031I')), function(x){'Cover_other_government_insurance'}) %>% # HIQ
    rename_at(vars(matches('HID030E|HIQ031J')), function(x){'Cover_any_single_plan'}) %>% # HIQ
    rename_at(vars(matches('PAD680')), function(x){'minutes_sedentary_activity'}) %>% # PAQ
    rename_at(vars(matches('PAQ520')), function(x){'compare_activity_same_age'}) %>% # PAQ
    rename_at(vars(matches('PFQ061B')), function(x){'difficulty_walk_quarter_mile'}) %>% # PFQ
    rename_at(vars(matches('HSD010')), function(x){'general_health_condition'}) %>% # HSQ
    rename_at(vars(matches('SLD010H')), function(x){'Sleep_hours'}) %>% # # SLQ
    rename_at(vars(matches('CVDVOMAX')), function(x){'Predicted_VO2MAX'}) %>% # CVX
    rename_at(vars(matches('CVDESVO2')), function(x){'Estimated_VO2MAX'}) %>% # CVX
    rename_at(vars(matches('CVDFITLV')), function(x){'Cardiovascular_fitness_level'}) %>% # CVX
    rename_at(vars(matches('LBDTCSI')), function(x){'TotalCholesterol'}) %>% # TCHOL hier und folgende in unterschiedlichen Tabellen über die Jahre abgelegt
    rename_at(vars(matches('LBDHDLSI|LBDHDDSI')), function(x){'HDL'}) %>% # HDL
    rename_at(vars(matches('LBDTRSI')), function(x){'Triglyceride'}) %>% # TRIGLY
    rename_at(vars(matches('LBDLDLSI')), function(x){'LDL'}) %>% # TRIGLY
    rename_at(vars(matches('LBXCOT')), function(x){'Cotinine'}) %>% # COT
    rename_at(vars(matches('LBXGLU')), function(x){'plasGluMG'}) %>% # GLU
    rename_at(vars(matches('LBXGLUSI|LBDGLUSI')), function(x){'plasGluMMol'}) # nur 1999-2002
}


# function to name levels for certain renamed code variables
# Levels 7,8,9 (77,88,99, etc.) mapped to NA (usually 7 /8 = refused, invalid, 9=don't know)
nhanes_recode <- function(data) {
  # income scale used in more than one variable, changes from time to time
  # 75-100k, 100k+ added in wave E
  income_scale <- c("$0 to $4999" , "$5000 to $9999" , "$10000 to $14999" , "$15000 to $19999" , 
                    "$20000 to $24999" , "$25000 to $349999" , "$35000 to $44999" , "$45000 to $54999" , 
                    "$55000 to $64999" , "$65000 to $74999" , "$75000 +" , 
                    "Over $20000" , "Under $20000",
                    "$75000 to $99999" , "$100000 +")
  
  
  data %>% 
  mutate(Gender=factor(Gender,seq(1:2),c("Male" , "Female"))) %>%
  mutate(Tot_Income_family=factor(Tot_Income_family,seq(1:15), income_scale)) %>%
  mutate(Tot_Income_household=factor(Tot_Income_household,seq(1:15),income_scale)) %>%
  mutate(Educational_Level_20plus=factor(Educational_Level_20plus,seq(1:5),c("Less than 9th grade" , "9-11th grade" , "High School Grad/GED or equivalent" , "Some College or AA degree" , "College Graduate or above"))) %>%
  mutate(Marital_status=factor(Marital_status,seq(1:6),c("Married" , "Widowed" , "Divorced" , "Separated" , "Never married" , "Living with partner"))) %>%
  mutate(EverSmoker=factor(EverSmoker,seq(1:2),c("Yes" , "No" ))) %>%
  mutate(CurrentSmoker=factor(CurrentSmoker,seq(1:5),c("Every day" , "Some days" , "Not at all" , "Refused" , "Don't know"))) %>%
  mutate(highBP=factor(highBP,seq(1:2),c("Yes" , "No"))) %>%
  mutate(highChol=factor(highChol,seq(1:2),c("Yes" , "No"))) %>%
  mutate(Alcohol=factor(Alcohol,seq(1:2),c("Yes" , "No"))) %>%
  mutate(Diabetes=factor(Diabetes,seq(1:3),c("Yes" , "No" , "Borderline"))) %>%
  mutate(pastHeartAtt=factor(pastHeartAtt,seq(1:2),c("Yes" , "No"))) %>%
  mutate(pastStroke=factor(pastStroke,seq(1:2),c("Yes" , "No"))) %>%
  mutate(pastHeartDis=factor(pastHeartDis,seq(1:2),c("Yes" , "No"))) %>%
  mutate(pastHeartFailure=factor(pastHeartFailure,seq(1:2),c("Yes" , "No"))) %>%
  mutate(pastAngina=factor(pastAngina,seq(1:2),c("Yes" , "No"))) %>%
  mutate(pastCancer=factor(pastCancer,seq(1:2),c("Yes" , "No"))) %>%
  mutate(Industry=factor(Industry,seq(1:44),c("Agriculture production",
                                                    "Agricultural services, forestry, and fishing", 
                                                    "Construction", 
                                                    "Mfg.-Food and kindred products", 
                                                    "Mfg.-Textile mill products",
                                                    "Mfg.-Apparel and other finished textile products", 
                                                    "Mfg.-Paper products, printing, publishing, and allied industries",
                                                    "Mfg.-Chemicals, petroleum, and coal products",
                                                    "Mfg.-Rubber, plastics, and leather products",
                                                    "Mfg.-Lumber and wood products, including furniture",
                                                    "Mfg.-Metal industries",
                                                    "Mfg.-Machinery, except electrical",
                                                    "Mfg.-Electrical machinery, equipment, and supplies",
                                                    "Mfg.-Transportation equipment",
                                                    "Mfg.-Miscellaneous and not specified manufacturing industries",
                                                    "Trucking service",
                                                    "Transportation, except trucking",
                                                    "Communications",
                                                    "Utilities",
                                                    "Wholesale Trade, Durable goods",
                                                    "Wholesale Trade, Non-durable and not specified goods",
                                                    "Retail-Department stores",
                                                    "Retail-Food stores",
                                                    "Retail-Vehicle dealers, supply and service stores",
                                                    "Retail-Apparel and accessory stores",
                                                    "Retail-Eating and drinking places",
                                                    "Other Retail trade",
                                                    "Banking and other finance",
                                                    "Insurance and real estate",
                                                    "Business services",
                                                    "Repair services",
                                                    "Private households",
                                                    "Lodging places",
                                                    "Personal services, except private households and lodging",
                                                    "Entertainment and recreation services",
                                                    "Offices of health practitioners",
                                                    "Hospitals",
                                                    "Health services, n. e. c.",
                                                    "Educational services",
                                                    "Social services",
                                                    "Other professional and related services",
                                                    "Justice, public order, and safety",
                                                    "Public administration, except justice, public order, safety",
                                                    "Military & national security"))) %>%
    mutate(Occupation=factor(Occupation,seq(1:41),c("Executive, administrators, and managers",
                                                    "Management related occupations",
                                                    "Engineers, architects and scientists",
                                                    "Health diagnosing, assessing and treating occupations",
                                                    "Teachers",
                                                    "Writers, artists, entertainers, and athletes",
                                                    "Other professional specialty occupations",
                                                    "Technicians and related support occupations",
                                                    "Supervisors and proprietors, sales occupations",
                                                    "Sales representatives, finance, business, & commodities ex. retail",
                                                    "Sales workers, retail and personal services",
                                                    "Secretaries, stenographers, and typists",
                                                    "Information clerks",
                                                    "Records processing occupations",
                                                    "Material recording, scheduling, and distributing clerks",
                                                    "Miscellaneous administrative support occupations",
                                                    "Private household occupations",
                                                    "Protective service occupations",
                                                    "Waiters and waitresses",
                                                    "Cooks",
                                                    "Miscellaneous food preparation and service occupations",
                                                    "Health service occupations",
                                                    "Cleaning and building service occupations",
                                                    "Personal service occupations",
                                                    "Farm operators, managers, and supervisors",
                                                    "Farm and nursery workers",
                                                    "Related agricultural, forestry, and fishing occupations",
                                                    "Vehicle and mobile equipment mechanics and repairers",
                                                    "Other mechanics and repairers",
                                                    "Construction trades",
                                                    "Extractive and precision production occupations",
                                                    "Textile, apparel, and furnishings machine operators",
                                                    "Machine operators, assorted materials",
                                                    "Fabricators, assemblers, inspectors, and samplers",
                                                    "Motor vehicle operators",
                                                    "Other transportation and material moving occupations",
                                                    "Construction laborers",
                                                    "Laborers, except construction",
                                                    "Freight, stock, and material movers, hand",
                                                    "Other helpers, equipment cleaners, hand packagers and laborers",
                                                    "Military occupations"))) %>%
   mutate(Health_insurance_coverage=factor(Health_insurance_coverage,seq(1:2),c("Yes" , "No"))) %>%
   mutate(Cover_Private_insurance=factor(Cover_Private_insurance,seq(1:2),c("Yes" , "No"))) %>%
   mutate(Cover_Medicare=factor(Cover_Medicare,seq(1:2),c("Yes" , "No"))) %>%
   mutate(Cover_Medicaid_CHIP=factor(Cover_Medicaid_CHIP,seq(1:2),c("Yes" , "No"))) %>%
   mutate(Cover_other_government_insurance=factor(Cover_other_government_insurance,seq(1:2),c("Yes" , "No"))) %>%
   mutate(Cover_any_single_plan=factor(Cover_any_single_plan,seq(1:2),c("Yes" , "No"))) %>%
   mutate(compare_activity_same_age=factor(compare_activity_same_age,seq(1:3),c("More active" , "Less active" , "As Active"))) %>%
   mutate(difficulty_walk_quarter_mile=factor(difficulty_walk_quarter_mile,seq(1:4),c("No difficulty" , "Some difficulty" , "Much difficulty" , "Unable to do"))) %>% 
   mutate(Cardiovascular_fitness_level=factor(Cardiovascular_fitness_level,seq(1:3),c("Low" , "Moderate" , "High")))
  # Todo: Why is this not here:
  #  mutate(general_health_condition=factor(general_health_condition,seq(1:5),c("Excellent" , "Very good" , "Good" , " Fair" , "Poor"))) %>%
  #  mutate(Sleep_hours=factor(Sleep_hours,seq(1:1),c(values from 1 to 11, and "12+"))) %>%
}


# function to acquire all data for a data file group
nhanes_get_group <- function(group) {
  # start with empty list of tables
  NHANES_group <- list()
  
  # all tables that consist of group name followed by underscore wave (except wave A), 
  # Regex, ^=start, but $=end not accepted, replace by \b - word boundary
  # sort by waves to keep indiviuals sorted by SEQN
  tables <- nhanesSearchTableNames(paste0("^",group,"(_[B-Z])?\\b")) %>% str_sort()

  # iterate over the waves (tables in group)
  for(table in tables) {
    # file name to save
    filename <- paste0("./data/",table,".RDS")
    
    # get wave of table - letter after underscore ("XXX_W"), or "A" if no wave ("XXX")
    tablewave=str_extract(table,"(?<=_)[B-Z]")
    if (is.na(tablewave)) tablewave <- "A"
    
    if (tablewave %in% studies$wave) {
      # check if table exists already
      if(file.exists(filename)) 
         #append if it does
         data <- readRDS(filename)
      else {
        rawfilename <- paste0(filename,".download")
        
        if(file.exists(rawfilename)) 
          data <- readRDS(rawfilename) 
        else {
          data<-nhanes(table)
          saveRDS(data, file=rawfilename)
        }
        
        # get table, change factor to string (later bind!), consistently rename variables accross waves
        data <- data %>% 
            mutate(Wave = tablewave) %>% 
            mutate_if(is.factor,as.character) %>% 
            nhanes_rename() %>% 
            clear.labels()
        
        # save processed table
        saveRDS(data,file = filename )
      } 
    
      #if(length(NHANES_group)==0)
      #  NHANES_group <- data
      #else {
        # add new data (no labels in bind_rows)
        NHANES_group <- bind_rows(NHANES_group, data)
        # todo - copy over labels
        #attributes(NHANES_group)<-attributes(data)
      #}
    } 
  }
  
  # Check if table structure allows for merge on SEQN
  print(paste("Duplicate SEQN in NHANES Continuous group ",group , ":", sum(duplicated(NHANES_group$SEQN))))
  
  return(NHANES_group)
}


# function to run through the list of groups to get
nhanes_get_groups <- function(group_list) 
{
  data_list<-list()
  for(group in group_list)  
  {
    name<-paste0(paste0("NHANES_",group))
    if(!exists(name)) 
    {
      filename<-paste0("./",name,".RData")
      if(file.exists(filename))
        load(filename) 
      else 
      {
        data_list[[group]]<-nhanes_get_group(group)
        #save(data_list[[group]],filename) # not working
      }
    }
  }
  data_list
}



nhanes_get_vars <- function(vars)
{
  print("Adding Extra Variables")
  NHANES_vars<-NULL
  
  for(var in vars){
    print(paste0("Processing variable ",var))
    
    filename <- paste0("./data/",var,".RDS")

    # check if processed data with var exists
    if(file.exists(filename)) {
      print("File with saved data exists")
      NHANES_var <- readRDS(filename)
    }  
    else {
      # create data frame for var
      NHANES_var <- data.frame()
  
      tables <- data.frame(name=nhanesSearchVarName(var)) %>% # get tables that contain var
        mutate(wave=replace_na(str_extract(name,"(?<=_)[B-Z]"),"A")) %>%  # extract wave of table
                      # letter after underscore ("XXX_W"), or "A" if no wave ("XXX")
        filter(wave %in% studies$wave) # filter on relevant waves
      
      # iterate over the waves (tables in group)
      for(i in seq_along(tables$name)) {
        table<-tables$name[i]
        tablewave<-tables$wave[i]
        
        print(paste0("Processing table ",table))
        # file name to save
        
        # check if table exists already
        rawfilename <- paste0("./data/",table,".RDS.download")
          
        if(file.exists(rawfilename)){
          print("file with downloaded data exists")
          data <- readRDS(rawfilename) 
        }
        else {
          print("getting raw data")
          data<-nhanes(as.character(table))
          saveRDS(data, file=rawfilename)
        }
        print("processing raw data")
        # get variable, change factor to string (later bind!), consistently rename variables accross waves
        data <- data %>% 
          select(one_of(c("SEQN",var))) %>% 
          mutate(Wave = tablewave) %>% 
          mutate_if(is.factor,as.character) %>% 
          nhanes_rename() %>% 
          clear.labels()
        
        #if(length(NHANES_group)==0)
        #  NHANES_group <- data
        #else {
        # add new data (no labels in bind_rows)
        NHANES_var <- bind_rows(NHANES_var, data)
        # todo - copy over labels
        #attributes(NHANES_group)<-attributes(data)
        #}
        summary(NHANES_var)  
      } 
     
      # save processed table
      saveRDS(NHANES_var,file = filename )
      
      print(summary(NHANES_var)) 
    }
    
    # join individual variables to result dataframe
    if(!exists("NHANES_vars")||is.null(NHANES_vars)) NHANES_vars <- NHANES_var
    else NHANES_vars<-full_join(NHANES_vars,NHANES_var,by="SEQN")
    summary(NHANES_vars)
  }
  return(NHANES_vars)
}


# Main part of script


# Download all tables groups and extra vars

# merge them by SEQN (unique in NHANES Cont)

NHANES_Cont<-
    Reduce(function(x,y){full_join(x,select(y,-Wave),by="SEQN")},nhanes_get_groups(nhanes_groups)) %>% 
    left_join(select(nhanes_get_vars(nhanes_extra_vars),-Wave),by="SEQN") %>% 
    nhanes_recode() %>% 
    mutate(SBP=rowMeans(select(.,BPXSY1,BPXSY2,BPXSY3,BPXSY4),na.rm=TRUE),
          DBP=rowMeans(select(.,BPXDI1,BPXDI2,BPXDI3,BPXDI4),na.rm=TRUE),
          Smoker=factor(if_else(if_else(is.na(CurrentSmoker),EverSmoker=="Yes",CurrentSmoker %in% c("Every day","Some days")),"Yes","No"),levels=c("Yes","No")),
          SampleWeightMEC=if_else(Wave<"C",WTMEC4YR*2,WTMEC2YR)/8, #https://wwwn.cdc.gov/nchs/nhanes/tutorials/module3.aspx
          Study=4,
          Cycle=Wave) %>% 
    select(suppressWarnings(one_of(nhanes_vars)))

# duplicate SEQN
print(paste("Duplicate SEQN in NHANES_Cont:", sum(duplicated(NHANES_Cont$SEQN))))

save(NHANES_Cont,file="./data/NHANES_Cont.RData")
