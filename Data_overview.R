# This script serves to create a clean overview of my data

# Author: Aleid Sunniva Teeuwen
# Date: 12.11.2021
# Project: FoodSecGovSim
# Publication: Ex-ante assessment of food security governance: a systematic review of simulation models

rm(list = ls()) #start with a clean environment

# libraries
print("loading libraries")
library("readxl") #for reading data
library("dplyr") #for piping
library("tidyr") #for data processing
library("countrycode") #for aggregating countries into regions
library("maps") #to get a list of all countries (for aggregating countries into regions)
library("xlsx") #for saving data to xl
library("rJava") #for optimising java in order to be able to save data

# functions
print("sourcing functions")
source("./Functions/dict_classification.R") # for classifying and aggregating atlas ti codes
source("./functions/level1_count.R") #for creating summary statistics
source("./Functions/check_dictionary.R") # for checking whether dictionaries contain classification info for all atlas ti codes
source("./functions/jgc.R") #optimising java 

# dictionaries
source("./dictionaries/timpl_class.R")
source("./dictionaries/timpl_class2.R")
source("./dictionaries/comm_class.R")
source("./dictionaries/comm2_class.R")
source("./dictionaries/goals_class.R")
source("./dictionaries/FSi_class.R")
source("./Dictionaries/NATO_sub.R")
source("./Dictionaries/NATO.R")

# Load data ###################
print("loading data")
quotes <- read_excel("./Data/all_quotes_simpl.xlsx") 
# Load data ###################

# Pre-process data 
#############################################
print("start pre-processing data")
quotes <- quotes[grepl("!Read for Lit review - eligible", quotes$`Document Groups`),] #ignore codes attached in papers ineligible for literature review

# make quotes into long format (one quote per row, but still in the column "name")
quotes_long <- quotes[1,]
quotes_long$code <- NA
quotes_long[,] <- NA

for(i in 1:nrow(quotes)){
  codes <- quotes$Codes[i]
  codes_vec <- unlist(strsplit(codes, "\r\n"))
  for(j in codes_vec){
    new_row <- quotes[i,]
    new_row$code <- j
    quotes_long <- rbind(quotes_long, new_row)
  }
}

quotes_long <- quotes_long[-1,]

quotes_long$code_group <- ""
quotes_long$name <- ""

for(i in 1:nrow(quotes_long)){
  code <- quotes_long$code[i]
  code_vec <- unlist(strsplit(code, ": "))
  quotes_long$code_group[i] <- code_vec[1]
  quotes_long$name[i] <- code_vec[2]
}

quotes_long <- quotes_long[!is.na(quotes_long$name),]

rm(list = c("quotes", "new_row", "code", "codes_vec", "code_vec", "codes", "i", "j"))

quotes_long$name_id <- 1:nrow(quotes_long)

# Fix and aggregate countries
#############################################
print("  fix and aggregate country codes")
# identify papers that represent all countries
to_fix <- which(quotes_long$name == "all countries fix in R")

#get list of all countries
x <- map("world", plot=FALSE)
countries <- unique(gsub("\\:.*","",x$names))

# add country not in list:
countries <- c(countries, "Dominican republic")

#some of the countries extracted from the function above are not really countries (but e.g. sovereign states), and therefore removed
no_country <- c("Ascension Island", "Azores", "Barbuda", "Bonaire", "Canary Islands", "Chagos Archipelago", "Christmas Island",
                "Cocos Islands", "Falkland Islands", "Guadeloupe", "Heard Island", "Madeira Islands", "Martinique", "Mayotte",
                "Micronesia", "Reunion", "Saba", "Saint Kitts" , "Saint Vincent", "Siachen Glacier", "Sint Eustatius", "Sint Maarten",
                "South Georgia", "South Sandwich Islands", "Trinidad", "Vatican", "Virgin Islands, US")

countries <- countries[-which(countries %in% no_country)]

#add row for each country, with the rest of the information remaining the same
for(i in to_fix){
  rowi <- quotes_long[i,]
  for(j in countries){
    rowj <- rowi
    rowj$name <- j
    quotes_long <- rbind(quotes_long, rowj)
  }
}

#remove rows with "all countries fix in R" identifier
quotes_long <- quotes_long[-to_fix,]

#some countries are not recognized by the map function used later on, therefore fixed here
quotes_long$name[quotes_long$name == "Antigua"] <- "Antigua and Barbuda"
quotes_long$name[quotes_long$name == "Côte d'Ivoire"] <- "Cote d'Ivoire"
quotes_long$name[quotes_long$name == "Grenadines"] <- "Saint Vincent and the Grenadines"
quotes_long$name[quotes_long$name == "Nevis"] <- "Saint Kitts and Nevis"
quotes_long$name[quotes_long$name == "Tobago"] <- "Trinidad and Tobago"
quotes_long$name[quotes_long$name == "Eswatini"] <- "Swaziland"

# Aggregate countries into larger regions (continent and 7 and 23 regions as defined by the World Bank for Development evaluation)
for(i in which(quotes_long$code_group == "spatial & temporal - country")){
  dati <- quotes_long$name[i]
  rowi <- quotes_long[i,]
  
  rowi_cont <- rowi
  rowi_r7 <- rowi
  rowi_r7m <- rowi
  rowi_r23 <- rowi 
  
  rowi_cont$name <- countrycode(sourcevar = dati, origin = "country.name", destination = "continent")
  rowi_r7$name <- countrycode(sourcevar = dati, origin = "country.name", destination = "region")
  
  if(dati %in% c("China", "United States of America")){
    rowi_r7m$name <- dati
  }else{
    rowi_r7m$name <- rowi_r7$name
  }
  
  rowi_r23$name <- countrycode(sourcevar = dati, origin = "country.name", destination = "region23")
  
  rowi_cont$code_group <- "spatial & temporal - continent"
  rowi_r7$code_group <- "spatial & temporal - WBD7 region"
  rowi_r7m$code_group <- "spatial & temporal - WBD7 m CHN & USA region"
  rowi_r23$code_group <- "spatial & temporal - WBD23 region" 
  
  quotes_long <- rbind(quotes_long, rowi_cont)
  quotes_long <- rbind(quotes_long, rowi_r7)
  quotes_long <- rbind(quotes_long, rowi_r7m)
  quotes_long <- rbind(quotes_long, rowi_r23)
  
}
rm(list = c("rowi", "rowi_cont", "rowj", "x", "countries", "dati", "i", "j", "no_country", "rowi_r23", "rowi_r7", "to_fix"))
#############################################

# Aggregate spatial scales
#############################################
print("  aggregate and quantify spatial scale codes")
for(i in which(quotes_long$code_group == "spatial & temporal - ref scale")){
  dati <- quotes_long$name[i]
  rowi <- quotes_long[i,]
  rowi_class <- rowi
  
  if(rowi$name %in% c("village/city district", "village/city district > x")){
    rowi_class$name <- "=< village/city district"
  }else if(rowi$name %in% c("city > x > village/city district", "city", "municipality > x > city", "municipality")){
    rowi_class$name <- "village/city district < X =< municipality"
  }else if(rowi$name %in% c("province/state", "province/state > x > municipality")){
    rowi_class$name <- "municipality < X =< province/state"
  }else if(rowi$name %in% c("country > x > province/state", "country")){
    rowi_class$name <- "province/state < X =< country"
  }else if(rowi$name %in% c("continent > x > country","continent")){
    rowi_class$name <- "country < X =< continent"
  }else{
    rowi_class$name <- "continent < X =< earth"
  }
  
  rowi_class$code_group <- "spatial & temporal - ref scale agg"
  
  quotes_long <- rbind(quotes_long, rowi_class)
}

# Quantify spatial scales
for(i in which(quotes_long$code_group == "spatial & temporal - ref scale")){
  dati <- quotes_long$name[i]
  rowi <- quotes_long[i,]
  rowi_class <- rowi
  
  if(rowi$name %in% c("village/city district", "village/city district > x")){
    rowi_class$name <- 1 #"=< village/city district"
  }else if(rowi$name %in% c("city > x > village/city district", "city", "municipality > x > city", "municipality")){
    rowi_class$name <- 2 #"village/city district < X =< municipality"
  }else if(rowi$name %in% c("province/state", "province/state > x > municipality")){
    rowi_class$name <- 3 #"municipality < X =< province/state"
  }else if(rowi$name %in% c("country > x > province/state", "country")){
    rowi_class$name <- 4 #"province/state < X =< country"
  }else if(rowi$name %in% c("continent > x > country","continent")){
    rowi_class$name <- 5 #"country < X =< continent"
  }else{
    rowi_class$name <- 6 #"continent < X =< earth"
  }
  
  rowi_class$code_group <- "spatial & temporal - ref quan scale"
  
  quotes_long <- rbind(quotes_long, rowi_class)
}

# Aggregate per measure - scale aggregated
for(i in which(quotes_long$code_group == "per measure - scale")){
  dati <- quotes_long$name[i]
  rowi <- quotes_long[i,]
  rowi_class <- rowi
  
  if(rowi$name %in% c("village/city district", "village/city district > x")){
    rowi_class$name <- "=< village/city district"
  }else if(rowi$name %in% c("city > x > village/city district", "city", "municipality > x > city", "municipality")){
    rowi_class$name <- "village/city district < X =< municipality"
  }else if(rowi$name %in% c("province/state", "province/state > x > municipality")){
    rowi_class$name <- "municipality < X =< province/state"
  }else if(rowi$name %in% c("country > x > province/state", "country")){
    rowi_class$name <- "province/state < X =< country"
  }else if(rowi$name %in% c("continent > x > country","continent")){
    rowi_class$name <- "country < X =< continent"
  }else{
    rowi_class$name <- "continent < X =< earth"
  }
  
  rowi_class$code_group <- "per measure - scale aggregated"
  
  quotes_long <- rbind(quotes_long, rowi_class)
}

# Quantify per measure - spatial scale
for(i in which(quotes_long$code_group == "per measure - scale")){
  dati <- quotes_long$name[i]
  rowi <- quotes_long[i,]
  rowi_class <- rowi
  
  if(rowi$name %in% c("village/city district", "village/city district > x")){
    rowi_class$name <- 1 #"=< village/city district"
  }else if(rowi$name %in% c("city > x > village/city district", "city", "municipality > x > city", "municipality")){
    rowi_class$name <- 2 #"village/city district < X =< municipality"
  }else if(rowi$name %in% c("province/state", "province/state > x > municipality")){
    rowi_class$name <- 3 #"municipality < X =< province/state"
  }else if(rowi$name %in% c("country > x > province/state", "country")){
    rowi_class$name <- 4 #"province/state < X =< country"
  }else if(rowi$name %in% c("continent > x > country","continent")){
    rowi_class$name <- 5 #"country < X =< continent"
  }else{
    rowi_class$name <- 6 #"continent < X =< earth"
  }
  
  rowi_class$code_group <- "per measure - scale aggregated"
  
  quotes_long <- rbind(quotes_long, rowi_class)
}
#############################################

# Get type and subtype of governance measures
#############################################
print("  classify and aggregate governance measure codes")
check_dictionary(codegroup = "per measure - measure", codedictionary = NATO_sub, dat_long = quotes_long) # check per measure undirected
check_dictionary(codegroup = "per measure - measure", codedictionary = NATO, dat_long = quotes_long) # check per measure undirected

for(i in which(quotes_long$code_group == "per measure - measure")){
  rowi <- quotes_long[i,]
  rowi_subclass <- rowi; rowi_class <- rowi
  
  rowi$class <- ""
  class_clm = which(colnames(rowi) == "class")
  name_clm = which(colnames(rowi) == "name")
  
  rowi <- dict_classification(sheet = rowi, dct = NATO_sub, clm = name_clm, class_clm = class_clm)
  rowi_subclass$code_group <- "per measure - NATO subclass"
  rowi_subclass$name <- rowi$class
  rowi_subclass$name
  quotes_long <- rbind(quotes_long, rowi_subclass)
  
  rowi <- dict_classification(sheet = rowi, dct = NATO, clm = name_clm, class_clm = class_clm)
  rowi_class$code_group <- "per measure - NATO class"
  rowi_class$name <- rowi$class
  rowi_class$name
  quotes_long <- rbind(quotes_long, rowi_class)
}
#############################################

# Get type of governance objectives
#############################################
print("  classify and aggregate governance objective codes")
check_dictionary(codegroup = "per measure - objective", codedictionary = goals_class, dat_long = quotes_long) # check per measure - objective

for(i in which(quotes_long$code_group == "per measure - objective")){
  dati <- quotes_long$name[i]
  rowi <- quotes_long[i,]
  rowi_class <- rowi
  
  rowi$class <- ""
  class_clm = which(colnames(rowi) == "class")
  name_clm = which(colnames(rowi) == "name")
  
  rowi <- dict_classification(sheet = rowi, dct = goals_class, clm = name_clm, class_clm = class_clm)
  
  rowi_class$code_group <- "per measure - objective class"
  rowi_class$name <- rowi$class
  
  quotes_long <- rbind(quotes_long, rowi_class)
}

# Aggregate type of governance objectives further
check_dictionary(codegroup = "per measure - objective", codedictionary = goals_class, dat_long = quotes_long) # check per measure - objective

for(i in which(quotes_long$code_group == "per measure - objective")){
  dati <- quotes_long$name[i]
  rowi <- quotes_long[i,]
  rowi_class <- rowi
  
  rowi$class <- ""
  class_clm = which(colnames(rowi) == "class")
  name_clm = which(colnames(rowi) == "name")
  
  rowi <- dict_classification(sheet = rowi, dct = goals_class, clm = name_clm, class_clm = class_clm)
  
  rowi_class$code_group <- "per measure - objective class 2"
  rowi_class$name <- rowi$class
  
  rowi_class$name <- gsub(pattern = "access  - general", replacement = "access", x = rowi_class$name)
  rowi_class$name <- gsub(pattern = "access - economic", replacement = "access", x = rowi_class$name)
  rowi_class$name <- gsub(pattern = "access  - physical", replacement = "access", x = rowi_class$name)
  if(rowi_class$name %in% c("macro-logistics", "infrastructure & technology")){
    rowi_class$name <- "infrastructure, logistics & technology"
  }
  
  quotes_long <- rbind(quotes_long, rowi_class)
}
#############################################

# Get type of food security indicators
#############################################
print("  classify and aggregate food security indicator codes")
for(i in which(quotes_long$code_group == "per effect - FS indicator")){
  rowi <- quotes_long[i,]
  rowi_FS <- rowi
  
  rowi$class <- ""
  class_clm = which(colnames(rowi) == "class")
  name_clm = which(colnames(rowi) == "name")
  
  rowi <- dict_classification(sheet = rowi, dct = FSi_class, clm = name_clm, class_clm = class_clm)
  
  rowi_FS$code_group <- "per effect - FS indicator class"
  rowi_FS$name <- rowi$class
  
  rowi_FS$name <- gsub(pattern = "access - general", replacement = "access", x = rowi_FS$name)
  rowi_FS$name <- gsub(pattern = "access - economic", replacement = "access", x = rowi_FS$name)
  rowi_FS$name <- gsub(pattern = "access - physical", replacement = "access", x = rowi_FS$name)
  
  quotes_long <- rbind(quotes_long, rowi_FS)
}

quotes_long$name[which(quotes_long$name %in% c("macro-logistics", "infrastructure & technology"))] <- "infrastructure, logistics & technology"
#############################################

# Get type of food commodities
#############################################
print("  classify and aggregate food commodity codes")
check_dictionary(codegroup = "food system - commodity", codedictionary = comm_class, dat_long = quotes_long) #check commodity class

for(i in which(quotes_long$code_group == "food system - commodity")){
  rowi <- quotes_long[i,]
  rowi_FS <- rowi
  
  rowi$class <- ""
  class_clm = which(colnames(rowi) == "class")
  name_clm = which(colnames(rowi) == "name")
  
  rowi <- dict_classification(sheet = rowi, dct = comm_class, clm = name_clm, class_clm = class_clm)
  
  rowi_FS$code_group <- "food system - commodity class"
  rowi_FS$name <- rowi$class
  
  quotes_long <- rbind(quotes_long, rowi_FS)
}

# Aggregate food commodities even more
check_dictionary(codegroup = "food system - commodity", codedictionary = comm_class2, dat_long = quotes_long) #check commodity class 2

for(i in which(quotes_long$code_group == "food system - commodity")){
  rowi <- quotes_long[i,]
  rowi_FS <- rowi
  
  rowi$class <- ""
  class_clm = which(colnames(rowi) == "class")
  name_clm = which(colnames(rowi) == "name")
  
  rowi <- dict_classification(sheet = rowi, dct = comm_class2, clm = name_clm, class_clm = class_clm)
  
  rowi_FS$code_group <- "food system - commodity class 2"
  rowi_FS$name <- rowi$class
  
  quotes_long <- rbind(quotes_long, rowi_FS)
}
#############################################

# Merge food value chain echelons
#############################################
print("  aggregate food value chain echelon codes")
quotes_long$name[which(quotes_long$name == "hunting/fishing/gathering")] <- "production*" #put footnote in figure text

# merge storage and processing
quotes_long$name[which(quotes_long$name == "storage")] <- "storage and processing"
quotes_long$name[which(quotes_long$name == "processing")]  <- "storage and processing"
#############################################

# Get type of agents
#############################################
print("  classify and aggregate agent codes")
check_dictionary(codegroup = "per agent - agent", codedictionary = timpl_class, dat_long = quotes_long) #check agent class

for(i in which(quotes_long$code_group == "per agent - agent")){
  rowi <- quotes_long[i,]
  rowi_FS <- rowi
  
  rowi$class <- ""
  class_clm = which(colnames(rowi) == "class")
  name_clm = which(colnames(rowi) == "name")
  
  rowi <- dict_classification(sheet = rowi, dct = timpl_class, clm = name_clm, class_clm = class_clm)
  
  rowi_FS$code_group <- "per agent - agent class"
  rowi_FS$name <- rowi$class
  
  quotes_long <- rbind(quotes_long, rowi_FS)
}

# Get type of agents more aggregated
check_dictionary(codegroup = "per agent - agent", codedictionary = timpl_class2, dat_long = quotes_long) #check agent class 2

for(i in which(quotes_long$code_group == "per agent - agent")){
  rowi <- quotes_long[i,]
  rowi_FS <- rowi
  rowi_bin <- rowi
  
  rowi$class <- ""
  class_clm = which(colnames(rowi) == "class")
  name_clm = which(colnames(rowi) == "name")
  
  rowi <- dict_classification(sheet = rowi, dct = timpl_class2, clm = name_clm, class_clm = class_clm)
  
  rowi_FS$code_group <- "per agent - agent class 2"
  rowi_FS$name <- rowi$class
  
  quotes_long <- rbind(quotes_long, rowi_FS)
  
  if(rowi_FS$name == "food producers"){
    rowi_bin$name <- "yes"
    rowi_bin$code_group <- "per agent - food producers"
    quotes_long <- rbind(quotes_long, rowi_bin)
  }else if(rowi_FS$name == "food distributors transporters"){
    rowi_bin$name <- "yes"
    rowi_bin$code_group <- "per agent - food distributors transporters"
    quotes_long <- rbind(quotes_long, rowi_bin)    
  }else if(rowi_FS$name == "food storers and processors"){
    rowi_bin$name <- "yes"
    rowi_bin$code_group <- "per agent - food storers and processors"
    quotes_long <- rbind(quotes_long, rowi_bin)    
  }else if(rowi_FS$name == "food retailers"){
    rowi_bin$name <- "yes"
    rowi_bin$code_group <- "per agent - food retailers"
    quotes_long <- rbind(quotes_long, rowi_bin)    
  }else if(rowi_FS$name == "food consumers"){
    rowi_bin$name <- "yes"
    rowi_bin$code_group <- "per agent - food consumers"
    quotes_long <- rbind(quotes_long, rowi_bin)    
  }else if(rowi_FS$name == "generic agents"){
    rowi_bin$name <- "yes"
    rowi_bin$code_group <- "per agent - generic agents"
    quotes_long <- rbind(quotes_long, rowi_bin)    
  }else if(rowi_FS$name == "political entities"){
    rowi_bin$name <- "yes"
    rowi_bin$code_group <- "per agent - political entities"
    quotes_long <- rbind(quotes_long, rowi_bin)    
  }else if(rowi_FS$name == "other non-food agents"){
    rowi_bin$name <- "yes"
    rowi_bin$code_group <- "per agent - non-food agents other"
    quotes_long <- rbind(quotes_long, rowi_bin) 
  }else if(rowi_FS$name == "food traders"){
    rowi_bin$name <- "yes"
    rowi_bin$code_group <- "per agent - food traders"
    quotes_long <- rbind(quotes_long, rowi_bin)
  }else{
    rowi_bin$name <- "yes"
    rowi_bin$code_group <- "per agent - no agents"
    quotes_long <- rbind(quotes_long, rowi_bin)   
  }
}

# Get type of affected agents
for(i in which(quotes_long$code_group == "per effect - affected agent")){
  rowi <- quotes_long[i,]
  rowi_FS <- rowi
  
  rowi$class <- ""
  class_clm = which(colnames(rowi) == "class")
  name_clm = which(colnames(rowi) == "name")
  
  rowi <- dict_classification(sheet = rowi, dct = timpl_class, clm = name_clm, class_clm = class_clm)
  
  rowi_FS$code_group <- "per effect - affected agent class"
  rowi_FS$name <- rowi$class
  
  
  quotes_long <- rbind(quotes_long, rowi_FS)
}

# Get even more aggregated type of affected agents 
check_dictionary(codegroup = "per effect - affected agent", codedictionary = timpl_class2, dat_long = quotes_long) #check agent class 2

for(i in which(quotes_long$code_group == "per effect - affected agent")){
  rowi <- quotes_long[i,]
  rowi_FS <- rowi
  rowi_bin <- rowi
  
  rowi$class <- ""
  class_clm = which(colnames(rowi) == "class")
  name_clm = which(colnames(rowi) == "name")
  
  rowi <- dict_classification(sheet = rowi, dct = timpl_class2, clm = name_clm, class_clm = class_clm)
  rowi_FS$code_group <- "per effect - aff agent class 2"
  rowi_FS$name <- rowi$class
  quotes_long <- rbind(quotes_long, rowi_FS)
  
  if(rowi_FS$name == "food producers"){
    rowi_bin$name <- "yes"
    rowi_bin$code_group <- "per effect - affected food producers"
    quotes_long <- rbind(quotes_long, rowi_bin)
  }else if(rowi_FS$name == "food distributors transporters"){
    rowi_bin$name <- "yes"
    rowi_bin$code_group <- "per effect - affected food distributors transporters"
    quotes_long <- rbind(quotes_long, rowi_bin)    
  }else if(rowi_FS$name == "food storers and processors"){
    rowi_bin$name <- "yes"
    rowi_bin$code_group <- "per effect - affected food storers and processors"
    quotes_long <- rbind(quotes_long, rowi_bin)    
  }else if(rowi_FS$name == "food retailers"){
    rowi_bin$name <- "yes"
    rowi_bin$code_group <- "per effect - affected food retailers"
    quotes_long <- rbind(quotes_long, rowi_bin)    
  }else if(rowi_FS$name == "food consumers"){
    rowi_bin$name <- "yes"
    rowi_bin$code_group <- "per effect - affected food consumers"
    quotes_long <- rbind(quotes_long, rowi_bin)    
  }else if(rowi_FS$name == "generic agents"){
    rowi_bin$name <- "yes"
    rowi_bin$code_group <- "per effect - affected generic agents"
    quotes_long <- rbind(quotes_long, rowi_bin)    
  }else if(rowi_FS$name == "political entities"){
    rowi_bin$name <- "yes"
    rowi_bin$code_group <- "per effect - affected political entities"
    quotes_long <- rbind(quotes_long, rowi_bin)    
  }else if(rowi_FS$name == "other non-food agents"){
    rowi_bin$name <- "yes"
    rowi_bin$code_group <- "per effect - affected non-food agents other"
    quotes_long <- rbind(quotes_long, rowi_bin) 
  }else if(rowi_FS$name == "food traders"){
    rowi_bin$name <- "yes"
    rowi_bin$code_group <- "per effect - affected food traders"
    quotes_long <- rbind(quotes_long, rowi_bin)
  }else{
    rowi_bin$name <- "yes"
    rowi_bin$code_group <- "per effect - no affected agents"
    quotes_long <- rbind(quotes_long, rowi_bin)   
  }
}

#############################################

# Aggregate model types (merge discrete event with mathematical other)
#############################################
print("  aggregate model type codes")
quotes_long$name[quotes_long$name == "discrete event"] <- "mathematical other"

#Split model types
for(i in which(quotes_long$code_group == "per model - type")){
  rowi <- quotes_long[i,]
  rowi_bin <- rowi
  
  if(rowi$name == "ABM"){
    rowi_bin$name <- "yes"
    rowi_bin$code_group <- "per model - ABM"
    quotes_long <- rbind(quotes_long, rowi_bin)
  }else if(rowi$name == "cellular automata"){
    rowi_bin$name <- "yes"
    rowi_bin$code_group <- "per model - cellular automata"
    quotes_long <- rbind(quotes_long, rowi_bin)    
  }else if(rowi$name == "CGE model"){
    rowi_bin$name <- "yes"
    rowi_bin$code_group <- "per model - CGE model"
    quotes_long <- rbind(quotes_long, rowi_bin)    
  }else if(rowi$name == "mathematical other"){
    rowi_bin$name <- "yes"
    rowi_bin$code_group <- "per model - mathematical other"
    quotes_long <- rbind(quotes_long, rowi_bin)    
  }else if(rowi$name == "microsimulation model"){
    rowi_bin$name <- "yes"
    rowi_bin$code_group <- "per model - microsimulation model"
    quotes_long <- rbind(quotes_long, rowi_bin)    
  }else if(rowi$name == "optimisation"){
    rowi_bin$name <- "yes"
    rowi_bin$code_group <- "per model - optimisation"
    quotes_long <- rbind(quotes_long, rowi_bin)    
  }else if(rowi$name == "PE model"){
    rowi_bin$name <- "yes"
    rowi_bin$code_group <- "per model - PE model"
    quotes_long <- rbind(quotes_long, rowi_bin)    
  }else if(rowi$name == "statistical/econometric"){
    rowi_bin$name <- "yes"
    rowi_bin$code_group <- "per model - statistical econometric"
    quotes_long <- rbind(quotes_long, rowi_bin)    
  }else if(rowi$name == "system dynamics model"){
    rowi_bin$name <- "yes"
    rowi_bin$code_group <- "per model - system dynamics model"
    quotes_long <- rbind(quotes_long, rowi_bin)     
  }
  
}

#############################################

# Fix data (split data type and whether the type is primary or secondary)
#############################################
print("  split, classify and aggregate data type codes")
for(i in which(quotes_long$code_group == "modelling - data")){
  dati <- quotes_long$name[i]
  rowi <- quotes_long[i,]
  dat_primi <- sub("\\).*", "", sub(".*\\(", "", dati))
  dat_spliti <- gsub("\\s*\\([^\\)]+\\)","", dati)
  
  rowi_primi <- rowi
  rowi_spliti <- rowi
  
  rowi_primi$code_group <- "modelling - primary or secondary"
  rowi_spliti$code_group <- "modelling - data split"
  
  rowi_primi$name <- dat_primi
  rowi_spliti$name <- dat_spliti
  
  quotes_long <- rbind(quotes_long, rowi_primi)
  quotes_long <- rbind(quotes_long, rowi_spliti)
  
}

# Then split spatial a spatial data and aggregate into different groups

spatial_cats <- c("spatial zones", "spatial gridded", "spatial points", "spatial networks")
aspatial_cats <- c("aggregated statistics", "census/survey/panel", "social accounting matrix", "stakeholder interviews", "expert interviews", "field experiments", "observational studies", "stakeholder workshops/focus sessions")

for(i in which(quotes_long$code_group == "modelling - data split")){
  
  dati <- quotes_long$name[i]
  rowi <- quotes_long[i,]
  rowi_sp <- rowi
  rowi_asp <- rowi
  rowi_quan <- rowi
  rowi_fbin <- rowi
  rowi_lbin <- rowi
  
  
  if(dati %in% "spatial gridded"){
    rowi_sp$code_group <- "modelling - data spatial"
    rowi_quan$code_group <- "modelling - data quan spatial"
    rowi_lbin$code_group <- "modelling - spatial landscape"
    
    rowi_sp$name <- dati
    rowi_quan$name <- 2
    rowi_lbin$name <- "yes"
    
    quotes_long <- rbind(quotes_long, rowi_sp)
    quotes_long <- rbind(quotes_long, rowi_quan)
    quotes_long <- rbind(quotes_long, rowi_lbin)
    
  }else if(dati %in% c("spatial zones", "spatial points", "spatial networks")){
    rowi_sp$code_group <- "modelling - data spatial"
    rowi_quan$code_group <- "modelling - data quan spatial"
    rowi_fbin$code_group <- "modelling - spatial features"
    
    rowi_sp$name <- dati
    rowi_quan$name <- 1
    rowi_fbin$name <- "yes"
    
    quotes_long <- rbind(quotes_long, rowi_sp)
    quotes_long <- rbind(quotes_long, rowi_quan)
    quotes_long <- rbind(quotes_long, rowi_fbin)
    
  }else if(dati %in% aspatial_cats){
    
    rowi_asp$code_group <- "modelling - data aspatial"
    rowi_quan$code_group <- "modelling - data quan spatial"
    
    rowi_quan$name <- 0
    
    if(dati %in% c("social accounting matrix", "aggregated statistics")){
      rowi_asp$name <- "aggregated quantitative"
    }else if(dati %in% c("census/survey/panel", "field experiments")){
      rowi_asp$name <- "disaggregated quantitative"
    }else if(dati %in% c("stakeholder interviews", "expert interviews", "observational studies", "stakeholder workshops/focus sessions", "expert workshops/focus sessions")){
      rowi_asp$name <- "disaggregated qualitative"
    }else{
      rowi_asp$name <- NA
    }
    
    quotes_long <- rbind(quotes_long, rowi_asp)
    quotes_long <- rbind(quotes_long, rowi_quan)
  }
  
}

if(length(which(quotes_long$code_group == "modelling - data spatial" & is.na(quotes_long$name))) > 0){
  quotes_long <- quotes_long[-which(quotes_long$code_group == "modelling - data spatial" & is.na(quotes_long$name)),] #outliers
}

if(length(which(quotes_long$code_group == "modelling - data aspatial" & is.na(quotes_long$name))) > 0){
  quotes_long <- quotes_long[-which(quotes_long$code_group == "modelling - data aspatial" & is.na(quotes_long$name)),] #outliesr
}

if(length(which(quotes_long$code_group == "spatial & temporal - WBD7 region" & is.na(quotes_long$name))) > 0){
  quotes_long <- quotes_long[-which(quotes_long$code_group == "spatial & temporal - WBD7 region" & is.na(quotes_long$name)),] #outliers
}
#############################################

# Split model domain into four yes/no columns
#############################################
print("  split model domain codes")

for(i in which(quotes_long$code_group == "per model - domain")){
  
  dati <- quotes_long$name[i]
  rowi <- quotes_long[i,]
  
  rowi_split <- rowi
  
  if(dati == "economic"){
    rowi_split$code_group <- "per model - economic domain"
    rowi_split$name <- "yes"
    quotes_long <- rbind(quotes_long, rowi_split)
  }else if(dati == "bio-physical"){
    rowi_split$code_group <- "per model - biophysical domain"
    rowi_split$name <- "yes"
    quotes_long <- rbind(quotes_long, rowi_split)
  }else if(dati == "social"){
    rowi_split$code_group <- "per model - social domain"
    rowi_split$name <- "yes"
    quotes_long <- rbind(quotes_long, rowi_split)
  }else if(dati == "logistic"){
    rowi_split$code_group <- "per model - logistic domain"
    rowi_split$name <- "yes"
    quotes_long <- rbind(quotes_long, rowi_split)
  }
}

#############################################

# (Further) split up spatial & temporal - representation split
#############################################
print("  split up spatial representation codes")
for(i in which(quotes_long$code_group == "spatial & temporal - representation split")){
  
  dati <- quotes_long$name[i]
  rowi <- quotes_long[i,]
  
  rowi_split <- rowi
  
  if(dati == "geographic"){
    rowi_split$code_group <- "spatial & temporal - geographic representation"
    rowi_split$name <- "yes"
    quotes_long <- rbind(quotes_long, rowi_split)
  }else if(dati == "hypothetical"){
    rowi_split$code_group <- "spatial & temporal - hypothetical representation"
    rowi_split$name <- "yes"
    quotes_long <- rbind(quotes_long, rowi_split)
  }
}

#############################################

# Split up per measure - type 2 
#############################################
print("  split up type of governance measures in communal, public or private")
for(i in which(quotes_long$code_group == "per measure - type 2")){
  
  dati <- quotes_long$name[i]
  rowi <- quotes_long[i,]
  
  rowi_split <- rowi
  
  if(dati == "public"){
    rowi_split$code_group <- "per measure - public"
    rowi_split$name <- "yes"
    quotes_long <- rbind(quotes_long, rowi_split)
  }else if(dati == "private"){
    rowi_split$code_group <- "per measure - private"
    rowi_split$name <- "yes"
    quotes_long <- rbind(quotes_long, rowi_split)
  }else if(dati == "communal"){
    rowi_split$code_group <- "per measure - communal"
    rowi_split$name <- "yes"
    quotes_long <- rbind(quotes_long, rowi_split)
  }else if(dati == "unclear"){
    rowi_split$code_group <- "per measure - public"
    rowi_split$name <- "unclear"
    quotes_long <- rbind(quotes_long, rowi_split)
    
    rowi_split$code_group <- "per measure - private"
    rowi_split$name <- "unclear"
    quotes_long <- rbind(quotes_long, rowi_split)
    
    rowi_split$code_group <- "per measure - communal"
    rowi_split$name <- "unclear"
    quotes_long <- rbind(quotes_long, rowi_split)
    
  }
}

#############################################

# Split up per measure - simulation
#############################################
print("  split simulation (process or effect-based) codes")
for(i in which(quotes_long$code_group == "per measure - simulation")){
  
  dati <- quotes_long$name[i]
  rowi <- quotes_long[i,]
  
  rowi_split <- rowi
  
  if(dati == "concrete process-based"){ # continue here!!
    rowi_split$code_group <- "per measure - process-based simulation"
    rowi_split$name <- "concrete"
    quotes_long <- rbind(quotes_long, rowi_split)
  }else if(dati == "abstract process-based"){
    rowi_split$code_group <- "per measure - process-based simulation"
    rowi_split$name <- "abstract"
    quotes_long <- rbind(quotes_long, rowi_split)
  }else if(dati == "assumed effect-based"){
    rowi_split$code_group <- "per measure - effect-based simulation"
    rowi_split$name <- "assumed"
    quotes_long <- rbind(quotes_long, rowi_split)
  }
}
#############################################

# Split up per effect - place
#############################################
print("  split up governance impacts based on where they occur (within/outside jurisdiction or global")
for(i in which(quotes_long$code_group == "per effect - place")){
  
  dati <- quotes_long$name[i]
  rowi <- quotes_long[i,]
  
  rowi_split <- rowi
  
  if(dati == "local"){ # continue here!!
    rowi_split$code_group <- "per effect - impact within jurisdiction"
    rowi_split$name <- "yes"
    quotes_long <- rbind(quotes_long, rowi_split)
  }else if(dati == "far away"){
    rowi_split$code_group <- "per effect - impact outside jurisdiction"
    rowi_split$name <- "yes"
    quotes_long <- rbind(quotes_long, rowi_split)
  }else if(dati == "global"){
    rowi_split$code_group <- "per effect - global impact"
    rowi_split$name <- "yes"
    quotes_long <- rbind(quotes_long, rowi_split)
  }
}

#############################################

quotes_long <- quotes_long %>% distinct()
quotes_long$name_id <- 1:nrow(quotes_long)

quotes_wide <- quotes_long %>% pivot_wider(names_from = code_group, values_from = name)
sort(colnames(quotes_wide))

rm(list = c("rowi", "rowi_class", "rowi_FS", "rowi_primi", "rowi_spliti", "rowi_subclass", "class_clm", "dat_primi", "dat_spliti", "dati", "i", "name_clm"))

print("finished pre-processing data")
# Pre-process data finished

# Processing data
#############################################
print("start processing data")

# Remove codes that are not relevant for further analysis
#############################################
print("  remove codes that are not relevant for further analysis")

leave_out <- c("ID", "Quotation Name", "Document", "Document Groups", "Quotation Content", "Comment", "Codes", "Reference", "Density", "Created by", "Modified by", "Created", 
               "Modified", "code", "name_id", "framing", "location", "definition - crowd-shipping", "decision-making", "agent characteristic", "agent decision", "decision-making method",
               "Design concept", "method", "method - model fitting", "agent ability", "data analysis method", "definition", "new word - ceteris paribus", "new word - ginning", 
               "new word - salient", "patch ability", "spatial", "new word - FEFO", "new word - promulgated", "per effect -unit", "software", "new word - cogent", "new word - precipitously",
               "new word - spurious", "papers", "patch characteristic", "title-tag", "spatial & temporal - trade partnership", "spatial & temporal - special administrative region",
               "spatial & temporal - part of earth", "spatial & temporal - county", "spatial & temporal - constituent country", "spatial & temporal - administrative region", 
               "spatial & temporal - district", "spatial & temporal - overseas department and region", "spatial & temporal - planet", "spatial & temporal - river basin", 
               "spatial & temporal - special municipality", "spatial & temporal - temporal extent unit", "theory-decision making", "theory-yield response", "spatial & temporal - state",
               "spatial & temporal - province", "spatial & temporal - part of continent", "spatial & temporal - municipality", "spatial & temporal - city", "nonfood system commodity",
               "agent", "food system - FS agent", "food system - FS type", "method-sensitivity analysis", "method-validation", "method - model testing", "method - sensitivity analysis",
               "modelling - programming language", "modelling - validation type", "papers - first author", "papers - last author", "per agent - interactions", "per effect - effect", 
               "per effect - FS dimension", "per effect - indicator other", "per effect - unit indicator other", "spatial & temporal - constituent state", "per effect - indicator other",
               "per measure - type", "governance - measures", "modelling - calibration?", "new word - incumbent", "class", "per patch - characteristic"
)
#############################################

# Save processed data
#############################################
print("  save processed data: ./Output/quotes_long.csv and ./Output/quotes_wide.csv")
# Make dataframe without useless variables
quotes_long <- quotes_long[-which(quotes_long$code_group %in% leave_out[16:length(leave_out)]),]
quotes_wide <- quotes_wide[,-which(colnames(quotes_wide) %in% leave_out[16:length(leave_out)])]

columnnames_wide <- colnames(quotes_wide)

# Save data
write.csv(quotes_long, file = "./Output/quotes_long.csv")
write.csv(quotes_wide, file = "./Output/quotes_wide.csv")
save(columnnames_wide, file = "./Output/columnnames_wide.rda")
#############################################

print("finished processing data")


# Make overview of the data and save to excel
#############################################
print("make and overview of the frequency of different codes within each code category and save to ./Output/all_variables.xlsx")

# Change names of some variables to avoid problems with export to excel
colnames(quotes_wide)[which(colnames(quotes_wide) == "spatial & temporal - representation features")] <- "spatial & temporal - repr features"
colnames(quotes_wide)[which(colnames(quotes_wide) == "spatial & temporal - representation split")] <- "spatial & temporal - repr split"
colnames(quotes_wide)[which(colnames(quotes_wide) == "spatial & temporal - spatial extent [cells]")] <- "spatial & temporal - cell spatial ext"
colnames(quotes_wide)[which(colnames(quotes_wide) == "spatial & temporal - spatial extent [m2]")] <- "spatial & temporal - m2 spatial ext"
colnames(quotes_wide)[which(colnames(quotes_wide) == "spatial & temporal - spatial extent")] <- "spatial & temporal - spatial ext"
colnames(quotes_wide)[which(colnames(quotes_wide) == "spatial & temporal - spatial resolution [cells]")] <- "spatial & temporal - spatial cells res"
colnames(quotes_wide)[which(colnames(quotes_wide) == "spatial & temporal - spatial resolution [m2]")] <- "spatial & temporal - spatial m2 res"
colnames(quotes_wide)[which(colnames(quotes_wide) == "spatial & temporal - temporal extent [d]")] <- "spatial & temporal - d temporal ext"
colnames(quotes_wide)[which(colnames(quotes_wide) == "spatial & temporal - temporal extent")] <- "spatial & temporal - temporal ext"
colnames(quotes_wide)[which(colnames(quotes_wide) == "spatial & temporal - temporal resolution [d]")] <- "spatial & temporal - temporal d res"
colnames(quotes_wide)[which(colnames(quotes_wide) == "food system - commodity class 2")] <- "food system - commodity 2 class"
colnames(quotes_wide)[which(colnames(quotes_wide) == "food system - commodity class")] <- "food system - commodity 1 class"

favars <- sort(colnames(quotes_wide)[!(colnames(quotes_wide) %in% leave_out)])

options(java.parameters = "-Xmx8000m") #To avoid error when exporting to excel

write.xlsx(as.data.frame(favars), file="./Output/all_variables.xlsx", sheetName="all variables", row.names=FALSE)

varnr <- 0

for(i in favars){
  varnr <- varnr + 1
  print("  ---------------------------------------------------")
  print(paste(" ", i))
  
  gc(verbose = FALSE) #garbage collection R to avoid memory problems
  jgc() #garbage collection Java to avoid memory problems
  
  clmn_nr <- which(colnames(quotes_wide)==i)
  
  datai <- quotes_wide[!is.na(quotes_wide[,i]),]
  datai$name <- datai[,i]
  datai_sum <- level1_count(sheet = datai)
  
  tablenamei <- paste0("./variable_summary_tables/", i, ".xlsx")
  tablenamei = gsub("\\?", "", tablenamei) #remove question mark
  
  write.xlsx(cbind(i = datai_sum$name, "number" = datai_sum$n), file="./Output/all_variables.xlsx", 
             sheetName=paste0(gsub("\\?", "", i),"_",varnr), row.names=FALSE, append = TRUE)
  
  print(datai_sum)
  print("  ===================================================")
  print("                                                     ")
}
#############################################
