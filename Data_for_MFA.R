# Here I create the dataframe upon which I will run the factor analysis

# Author: Aleid Sunniva Teeuwen
# Date: 12.11.2021
# Project: FoodSecGovSim
# Publication: The impact of governance on food security: a systematic review of simulation models


rm(list = ls()) #start with a clean environment


# libraries
print("loading libraries")
# library(readxl) #for reading data
library(dplyr) #for piping
library(tidyr) #for data processing
# library(countrycode) #for aggregating countries into regions
# library(maps) #to get a list of all countries

# functions
print("sourcing functions")
source("./Functions/split_and_add_by_ID.R")
source("./Functions/split_and_add_by_doc.R")
source("./Functions/split_and_merge_by_doc.R")
# source("./Functions/na_to_no.R")
source("./Functions/na_to_no_nested.R")
source("./Functions/na_to_no_nested_simpl.R")

# dictionaries
# source("./dictionaries/timpl_class.R")
# source("./dictionaries/comm_class.R")
# source("./dictionaries/NOTA_class.R") 
# source("./dictionaries/NOTA_subclass.R")#dictionary linking governance measures to NATO subclasses
# source("./dictionaries/goals_class.R")

# Loading data
print("loading data")
#############################################
quotes_long <- read.csv(file = "./Output/quotes_long.csv")
quotes_wide <- read.csv(file = "./Output/quotes_wide.csv")
colnames(quotes_wide)
load("./Output/columnnames_wide.rda")
colnames(quotes_wide) <- c("rownr", columnnames_wide)
sort(colnames(quotes_wide))
#############################################

# Preparing model variables
#############################################
print("preparing model variables")

modelling <- quotes_wide

# main variables ###
modelling_codes <- c("modelling - aim", "modelling - feedback-loop?", "modelling - sensitivity analysis?", 
                     "modelling - validation?", "modelling - data aspatial", "per model - type", 
                     "modelling - primary or secondary")  

# Split up: modelling - aim, modelling - data aspatial

keep <- which(colnames(modelling) %in% c(modelling_codes, "name_id", "Document", "ID"))
modelling <- modelling[,keep]
empty_rows <- which(rowSums(is.na(modelling[,4:ncol(modelling)]))==length(4:ncol(modelling)))
modelling <- modelling[-empty_rows,]

# nested variables ###

# primary or secondary with spatial quan scale#
nest3 <- quotes_wide[,which(colnames(quotes_wide) %in% c("modelling - data quan spatial", "modelling - primary or secondary", "name_id", "Document", "ID"))]
empty_rows <- which(rowSums(is.na(nest3[,4:ncol(nest3)]))==length(4:ncol(nest3)))
nest3 <- nest3[-empty_rows,]
nest3 <- split_and_add_by_ID(sheet = nest3, clm = which(colnames(nest3)=="modelling - data quan spatial")) #data type + prim/sec
if(length(which(is.na(nest3$`modelling - data quan spatial`))) > 0){nest3 <- nest3[-which(is.na(nest3$`modelling - data quan spatial`)),]}
if(length(which(is.na(nest3$`modelling - primary or secondary`))) > 0){nest3 <- nest3[-which(is.na(nest3$`modelling - primary or secondary`)),]}

nest3 <- nest3[,-which(colnames(nest3) %in% c("name_id","ID"))]

# per model - biophysical domain #
nest2 <- quotes_wide[,which(colnames(quotes_wide) %in% c("per model - biophysical domain", "per model - type", "name_id", "Document", "ID"))]
empty_rows <- which(rowSums(is.na(nest2[,4:ncol(nest2)]))==length(4:ncol(nest2)))
nest2 <- nest2[-empty_rows,]
nest2 <- split_and_add_by_ID(sheet = nest2, clm = which(colnames(nest2)=="per model - biophysical domain")) #domain
nest2 <- nest2[-which(is.na(nest2$`per model - type`)),]
nest2 <- nest2[,-which(colnames(nest2) %in% c("name_id","ID"))]

# per model - economic domain #
nest6 <- quotes_wide[,which(colnames(quotes_wide) %in% c("per model - economic domain", "per model - type", "name_id", "Document", "ID"))]
empty_rows <- which(rowSums(is.na(nest6[,4:ncol(nest6)]))==length(4:ncol(nest6)))
nest6 <- nest6[-empty_rows,]
nest6 <- split_and_add_by_ID(sheet = nest6, clm = which(colnames(nest6)=="per model - economic domain")) #domain
nest6 <- nest6[-which(is.na(nest6$`per model - type`)),]
nest6 <- nest6[,-which(colnames(nest6) %in% c("name_id","ID"))]

# per model - logistic domain #
nest7 <- quotes_wide[,which(colnames(quotes_wide) %in% c("per model - logistic domain", "per model - type", "name_id", "Document", "ID"))]
empty_rows <- which(rowSums(is.na(nest7[,4:ncol(nest7)]))==length(4:ncol(nest7)))
nest7 <- nest7[-empty_rows,]
nest7 <- split_and_add_by_ID(sheet = nest7, clm = which(colnames(nest7)=="per model - logistic domain")) #domain
nest7 <- nest7[-which(is.na(nest7$`per model - type`)),]
nest7 <- nest7[,-which(colnames(nest7) %in% c("name_id","ID"))]

# per model - social domain #
nest8 <- quotes_wide[,which(colnames(quotes_wide) %in% c("per model - social domain", "per model - type", "name_id", "Document", "ID"))]
empty_rows <- which(rowSums(is.na(nest8[,4:ncol(nest8)]))==length(4:ncol(nest8)))
nest8 <- nest8[-empty_rows,]
nest8 <- split_and_add_by_ID(sheet = nest8, clm = which(colnames(nest8)=="per model - social domain")) #domain
nest8 <- nest8[-which(is.na(nest8$`per model - type`)),]
nest8 <- nest8[,-which(colnames(nest8) %in% c("name_id","ID"))]

# primary or secondary with data aspatial#
nest4 <- quotes_wide[,which(colnames(quotes_wide) %in% c("modelling - data aspatial", "modelling - primary or secondary", "name_id", "Document", "ID"))]
empty_rows <- which(rowSums(is.na(nest4[,4:ncol(nest4)]))==length(4:ncol(nest4)))
nest4 <- nest4[-empty_rows,]
nest4 <- split_and_add_by_ID(sheet = nest4, clm = 4) #data type + prim/sec
nest4 <- nest4[-which(is.na(nest4$`modelling - data aspatial`)),]
nest4 <- nest4[,-which(colnames(nest4) %in% c("name_id","ID"))]

# level one non-nested data
for(i in c(4:ncol(modelling))){
  modelling <- split_and_add_by_doc(sheet = modelling, clm = i)
}

modelling <- modelling[,-which(colnames(modelling) %in% c("name_id","ID"))]
modelling <- modelling %>% distinct()

# modelling <- merge(modelling, nest)
modelling <- full_join(modelling, nest2)
modelling <- full_join(modelling, nest3)
modelling <- full_join(modelling, nest4)
modelling <- full_join(modelling, nest6)
modelling <- full_join(modelling, nest7)
modelling <- full_join(modelling, nest8)
colnames(modelling)
modelling <- modelling %>% distinct()

modelling$`per model - biophysical domain` <- as.character(modelling$`per model - biophysical domain`)
modelling$`per model - economic domain` <- as.character(modelling$`per model - economic domain`)
modelling$`per model - logistic domain` <- as.character(modelling$`per model - logistic domain`)
modelling$`per model - social domain` <- as.character(modelling$`per model - social domain`)

modelling$`per model - biophysical domain` <- na_to_no_nested(dat = modelling, na_col = "per model - biophysical domain", matchterms = "yes", nest_col = "per model - type")
modelling$`per model - economic domain` <- na_to_no_nested(dat = modelling, na_col = "per model - economic domain", matchterms = "yes", nest_col = "per model - type")
modelling$`per model - logistic domain` <- na_to_no_nested(dat = modelling, na_col = "per model - logistic domain", matchterms = "yes", nest_col = "per model - type")
modelling$`per model - social domain` <- na_to_no_nested(dat = modelling, na_col = "per model - social domain", matchterms = "yes", nest_col = "per model - type")

modelling$`modelling - data aspatial` <- na_to_no_nested_simpl(dat = modelling, na_col = "modelling - data aspatial", matchterms = c("disaggregated quantitative", "aggregated quantitative", "disaggregated qualitative"), nest_col = "Document")

# Split up model type
modelling$rownr <- 1:nrow(modelling)
modelling$yes <- "yes"
modelling$`per model - type` <- paste("per model -", modelling$`per model - type`)
modelling <- modelling %>% spread(key = "per model - type", value = "yes")
modelling <- modelling[,-which(colnames(modelling)=="rownr")]
to_no <- is.na(modelling[,which(colnames(modelling)=="per model - ABM"):ncol(modelling)])
modelling[,which(colnames(modelling)=="per model - ABM"):ncol(modelling)][to_no] <- "no"

modelling <- modelling %>% distinct()

# Split up data aspatial
modelling$rownr <- 1:nrow(modelling)
modelling$yes <- "yes"
modelling$`modelling - data aspatial` <- paste("modelling -", modelling$`modelling - data aspatial`)
modelling <- modelling %>% spread(key = "modelling - data aspatial", value = "yes")
modelling <- modelling[,-which(colnames(modelling)=="rownr")]

for(i in colnames(modelling)[which(colnames(modelling)=="modelling - aggregated quantitative"):ncol(modelling)]){
  cnr <- which(colnames(modelling) == i) 
  modelling[,cnr] <- na_to_no(dat = modelling, na_col = i, matchterm = "yes", val = "yes")
}

if(length(which(colnames(modelling)=="modelling - NA"))>0){modelling <- modelling[,-which(colnames(modelling)=="modelling - NA")]}
if(length(which(colnames(modelling)=="per model - NA"))>0){modelling <- modelling[,-which(colnames(modelling)=="per model - NA")]}
modelling <- modelling %>% distinct()

#############################################


# Preparing agent variables
#############################################
print("preparing agent variables")
agent <- quotes_wide
agent_codes <- c("agent - representation", "per agent - food producers", "per agent - food distributors transporters",
                 "per agent - food storers and processors", "per agent - food retailers", "per agent - food consumers",
                 "per agent - generic agents", "per agent - political entities", "per agent - non-food agents other", 
                 "per agent - no agents", "per agent - food traders")

keep <- which(colnames(agent) %in% c(agent_codes, "name_id", "Document"))
agent<- agent[,keep]
empty_rows <- which(rowSums(is.na(agent[,3:ncol(agent)]))==length(3:ncol(agent)))
agent <- agent[-empty_rows,]

agent_agt <- agent[,-which(colnames(agent)=="agent - representation")]
agent_agt <- agent_agt[,-(which(colnames(agent_agt) %in% c("name_id")))]
empty_rows <- which(rowSums(is.na(agent_agt[,2:ncol(agent_agt)]))==length(2:ncol(agent_agt)))
agent_agt <- agent_agt[-empty_rows,]

for(i in colnames(agent_agt)[2:ncol(agent_agt)]){
  cnr <- which(colnames(agent_agt) == i) 
  agent_agt[,cnr] <- as.character(agent_agt[,cnr])
  agent_agt[,cnr] <- na_to_no(dat = agent_agt, na_col = i, matchterm = "yes", val = "yes")
}

agent <- agent_agt
agent <- agent %>% distinct()

# checking whether everything went OK
nada <- agent[rowSums(is.na(agent)) > 4,]
(mydocs <- sort(unique(agent$Document)))
(length(mydocs)) #nope!

#############################################

agmod <- full_join(agent, modelling)

# checking whether everything went OK
nada <- agmod[rowSums(is.na(agmod)) > 4,]
(mydocs <- sort(unique(agmod$Document)))
(length(mydocs)) #yes!

agmod[rowSums(is.na(agmod[,2:9])) > 4,2:9] <- "no" #models that do not have any agents are given no for each agent (otherwise would get NA)

# Preparing food system variables
#############################################
print("preparing food system variables")
food <- quotes_wide
food_codes <- c("food system - commodity class 2", "food system - echelon")
keep <- which(colnames(food) %in% c(food_codes, "name_id", "Document"))
food <- food[,keep]
empty_rows <- which(rowSums(is.na(food[,3:ncol(food)]))==length(3:ncol(food)))
food <- food[-empty_rows,]

food_com <- food[,-(which(colnames(food)=="food system - echelon"))]
food_com <- food_com[!is.na(food_com$`food system - commodity class`),]
food_com <- food_com[,-(which(colnames(food_com)=="name_id"))]
food_com <- food_com %>% distinct()

food_ech <- food[,-(which(colnames(food)=="food system - commodity class 2"))]
food_ech <- food_ech[!is.na(food_ech$`food system - echelon`),]
food_ech <- food_ech[,-(which(colnames(food_ech)=="name_id"))]
food_ech <- food_ech %>% distinct()

food <- merge(food_com, food_ech)
food <- food %>% distinct()

# Split up food system - echelon

unique(food$`food system - echelon`)

food$rownr <- 1:nrow(food)
food$yes <- "yes"
food$`food system - echelon` <- paste("food -", food$`food system - echelon`)
food <- food %>% spread(key = "food system - echelon", value = "yes")
food <- food[,-which(colnames(food)=="rownr")]

colnames(food)

for(i in colnames(food)[3:ncol(food)]){
  cnr <- which(colnames(food) == i) 
  food[,cnr] <- na_to_no(dat = food, na_col = i, matchterm = "yes", val = "yes")
}


food <- food %>% distinct()

# Split up food system - commodity
unique(food$`food system - commodity class 2`)

food$rownr <- 1:nrow(food)
food$yes <- "yes"
food$`food system - commodity class 2` <- paste("food -", food$`food system - commodity class 2`)
food <- food %>% spread(key = "food system - commodity class 2", value = "yes")
food <- food[,-which(colnames(food)=="rownr")]

colnames(food)

for(i in colnames(food)[8:ncol(food)]){
  cnr <- which(colnames(food) == i) 
  food[,cnr] <- na_to_no(dat = food, na_col = i, matchterm = "yes", val = "yes")
}


food <- food %>% distinct()

# checking whether everything went OK
nada <- food[rowSums(is.na(food)) > 4,]
(mydocs <- sort(unique(food$Document)))
(length(mydocs)) #yes!

#############################################

foagmod <- merge(agmod, food)

# checking whether everything went OK
nada <- foagmod[rowSums(is.na(foagmod)) > 4,]
(mydocs <- sort(unique(foagmod$Document)))
(length(mydocs)) #nope!

# Preparing spatial and temporal variables
#############################################
print("preparing spatial and temporal variables")
spat <- quotes_wide
sort(colnames(quotes_wide))
spat_codes <- c("spatial & temporal - geographic representation", "spatial & temporal - hypothetical representation", 
                "spatial & temporal - WBD7 region", "spatial & temporal - ref quan scale",
                "spatial & temporal - spatial extent [m2]", "spatial & temporal - spatial resolution [m2]",
                "spatial & temporal - temporal extent [d]", "spatial & temporal - temporal resolution [d]")

keep <- which(colnames(spat) %in% c(spat_codes, "name_id", "Document"))
spat <- spat[,keep]
empty_rows <- which(rowSums(is.na(spat[,3:ncol(spat)]))==length(3:ncol(spat)))
spat <- spat[-empty_rows,]

# level one non-nested data
spat <- split_and_merge_by_doc(clms = 3:ncol(spat), sheet = spat, na.rm = FALSE)
spat$`spatial & temporal - geographic representation` <- as.character(spat$`spatial & temporal - geographic representation`)
spat$`spatial & temporal - geographic representation` <- na_to_no(dat = spat, 
                                                                  na_col = "spatial & temporal - geographic representation",
                                                                  matchterm = "yes", val = "yes")
spat$`spatial & temporal - hypothetical representation` <- as.character(spat$`spatial & temporal - hypothetical representation`)
spat$`spatial & temporal - hypothetical representation` <- na_to_no(dat = spat, 
                                                                    na_col = "spatial & temporal - hypothetical representation",
                                                                    matchterm = "yes", val = "yes")

# Split up WBD7 region
spat$rownr <- 1:nrow(spat)
spat$yes <- "yes"
spat$`spatial & temporal - WBD7 region` <- paste("spatial -", spat$`spatial & temporal - WBD7 region`)
spat <- spat %>% spread(key = "spatial & temporal - WBD7 region", value = "yes")
spat <- spat[,-which(colnames(spat)=="rownr")]

for(i in colnames(spat)[9:ncol(spat)]){
  cnr <- which(colnames(spat) == i) 
  spat[,cnr] <- na_to_no(dat = spat, na_col = i, matchterm = "yes", val = "yes")
}


spat <- spat %>% distinct()

# checking whether everything went OK
nada <- spat[rowSums(is.na(spat)) > 4,]
(mydocs <- sort(unique(spat$Document)))
(length(mydocs)) #yes!

#############################################

spfoagmod <- merge(foagmod, spat) 

# checking whether everything went OK
nada <- spfoagmod[rowSums(is.na(spfoagmod)) > 4,]
(mydocs <- sort(unique(spfoagmod$Document)))
(length(mydocs)) #nope!

# Preparing governance variables
#############################################
print("preparing governance variables")
gov <- quotes_wide
gov_codes <- c("governance - combined measures?", "per measure - NATO subclass", "per measure - communal", "per measure - private", 
               "per measure - public", "per measure - objective class 2", "per measure - process-based simulation", 
               "per measure - effect-based simulation", "per measure - spatially targeted?")

keep <- which(colnames(gov) %in% c(gov_codes, "name_id", "Document", "ID"))
gov <- gov[,keep]
empty_rows <- which(rowSums(is.na(gov[,4:ncol(gov)]))==length(4:ncol(gov)))
gov <- gov[-empty_rows,]

# nested data

# Nato subclass
nest <- gov[,which(colnames(gov) %in% c("governance - combined measures?", "per measure - NATO subclass", "name_id", "Document", "ID"))]
nest <- split_and_add_by_ID(sheet = nest, clm = which(colnames(nest)=="per measure - NATO subclass")) #NATO subclass
nest <- nest[,-which(colnames(nest) %in% c("name_id","ID"))]
if(length(which(is.na(nest$`per measure - NATO subclass`))>0)){nest <- nest[-which(is.na(nest$`per measure - NATO subclass`)),]}
nest$`governance - combined measures?` <- na_to_no_nested(dat = nest, na_col = "governance - combined measures?", matchterms = c("yes"), nest_col = "per measure - NATO subclass")
nest <- nest %>% distinct

# process-based simulation
nest4 <- gov[,which(colnames(gov) %in% c("per measure - process-based simulation", "per measure - NATO subclass", "name_id", "Document", "ID"))]
nest4 <- split_and_add_by_ID(sheet = nest4, clm = which(colnames(nest4)=="per measure - process-based simulation")) #process-based simulation
nest4 <- nest4[,-which(colnames(nest4) %in% c("name_id","ID"))]
nest4 <- nest4[-which(is.na(nest4$`per measure - NATO subclass`)),]

# effect-based simulation
nest9 <- gov[,which(colnames(gov) %in% c("per measure - effect-based simulation", "per measure - NATO subclass", "name_id", "Document", "ID"))]
nest9 <- split_and_add_by_ID(sheet = nest9, clm = which(colnames(nest9)=="per measure - effect-based simulation")) #effect-based simulation
nest9 <- nest9[,-which(colnames(nest9) %in% c("name_id","ID"))]
nest9 <- nest9[-which(is.na(nest9$`per measure - NATO subclass`)),]

# objective class 2
nest3 <- gov[,which(colnames(gov) %in% c("per measure - objective class 2", "per measure - NATO subclass", "name_id", "Document", "ID"))]
empty_rows <- which(rowSums(is.na(nest3[,4:ncol(nest3)]))==length(4:ncol(nest3)))
if(sum(empty_rows) != 0){
  nest3 <- nest3[-empty_rows,]
}

nest3 <- split_and_add_by_ID(sheet = nest3, clm = which(colnames(nest3)== "per measure - objective class 2")) #objective class
nest3 <- nest3[,-which(colnames(nest3) %in% c("name_id","ID"))]
nest3 <- nest3[-which(is.na(nest3$`per measure - NATO subclass`)),]

# spatially targeted?
nest6 <- gov[,which(colnames(gov) %in% c("per measure - spatially targeted?", "per measure - NATO subclass", "name_id", "Document", "ID"))]
nest6 <- split_and_add_by_ID(sheet = nest6, clm = 4) #spatially targeted
nest6 <- nest6[-which(is.na(nest6$`per measure - NATO subclass`)),]
nest6 <- nest6[,-which(colnames(nest6) %in% c("name_id","ID"))]

# communal
nest2 <- gov[,which(colnames(gov) %in% c("per measure - communal", "per measure - NATO subclass", "name_id", "Document", "ID"))]
nest2 <- split_and_add_by_ID(sheet = nest2, clm = which(colnames(nest2)=="per measure - communal")) #type 2
nest2 <- nest2[-which(is.na(nest2$`per measure - NATO subclass`)),] #type 2
nest2 <- nest2[,-which(colnames(nest2) %in% c("name_id","ID"))]

# private
nest7 <- gov[,which(colnames(gov) %in% c("per measure - private", "per measure - NATO subclass", "name_id", "Document", "ID"))]
nest7 <- split_and_add_by_ID(sheet = nest7, clm = which(colnames(nest7)=="per measure - private")) #private
nest7 <- nest7[-which(is.na(nest7$`per measure - NATO subclass`)),] #private
nest7 <- nest7[,-which(colnames(nest7) %in% c("name_id","ID"))]

# public
nest8 <- gov[,which(colnames(gov) %in% c("per measure - public", "per measure - NATO subclass", "name_id", "Document", "ID"))]
nest8 <- split_and_add_by_ID(sheet = nest8, clm = which(colnames(nest8)=="per measure - public")) #public
nest8 <- nest8[-which(is.na(nest8$`per measure - NATO subclass`)),] #public
nest8 <- nest8[,-which(colnames(nest8) %in% c("name_id","ID"))]

# merge nested
nest <- full_join(nest, nest2)
nest <- full_join(nest, nest3)
nest <- full_join(nest, nest4)
nest <- full_join(nest, nest6)
nest <- full_join(nest, nest7)
nest <- full_join(nest, nest8)
nest <- full_join(nest, nest9)

gov <- nest %>% distinct()
gov$`per measure - communal` <- na_to_no_nested(dat = gov, na_col = "per measure - communal", matchterms = c("yes", "unclear"), nest_col = "per measure - NATO subclass")
gov$`per measure - private` <- na_to_no_nested(dat = gov, na_col = "per measure - private", matchterms = c("yes", "unclear"), nest_col = "per measure - NATO subclass")
gov$`per measure - public` <- na_to_no_nested(dat = gov, na_col = "per measure - public", matchterms = c("yes", "unclear"), nest_col = "per measure - NATO subclass")
gov$`per measure - effect-based simulation` <- na_to_no_nested(dat = gov, na_col = "per measure - effect-based simulation", matchterms = "assumed", nest_col = "per measure - NATO subclass")
gov$`per measure - process-based simulation` <- na_to_no_nested(dat = gov, na_col = "per measure - process-based simulation", matchterms = c("concrete", "abstract"), nest_col = "per measure - NATO subclass")
gov <- gov %>% distinct()

# Split up per measure - objective class 2
gov$yes <- "yes"
gov <- unite(gov, col = "nest", c(1:4,6:ncol(gov)), sep = "_", remove = FALSE)
gov$`per measure - objective class 2` <- paste("gov - obj", gov$`per measure - objective class 2`)
gov <- gov %>% spread(key = "per measure - objective class 2", value = "yes")

for(i in colnames(gov)[11:ncol(gov)]){
  cnr <- which(colnames(gov) == i)
  gov[,cnr] <- na_to_no_nested(dat = gov, na_col = i, matchterms = "yes", nest_col = "nest")
}

gov <- gov[,-which(colnames(gov)=="nest")]
gov <- gov %>% distinct()

par(mfrow = c(3,3))
for(i in 1:ncol(gov)){print(barplot(table(gov[,i]), main = as.character(colnames(gov[i]))))}

# checking whether everything went OK
nada <- gov[rowSums(is.na(nest)) > 4,]
(mydocs <- sort(unique(gov$Document)))
(length(mydocs))

#############################################

govspfoagmod <- merge(spfoagmod, gov) # <- something goes wrong here, check spfoagmod

# checking whether everything went OK
nada <- govspfoagmod[rowSums(is.na(nest)) > 4,]
(mydocs <- sort(unique(govspfoagmod$Document)))
(length(mydocs))

rm(list = c("spfoagmod", "gov", paste0("nest", c("", 2:4,6)), "gov_codes", "empty_rows", "keep"))

# Preparing governance impact variables
#############################################
print("preparing governance impactvariables")

# main variable
gov <- quotes_wide
gov_codes <- c("per measure - NATO subclass")
keep <- which(colnames(gov) %in% c(gov_codes, "name_id", "Document", "ID"))
gov <- gov[,keep]
empty_rows <- which((is.na(gov[,4])))
gov <- gov[-empty_rows,]

# nested variable

# FS indicator class
nest <- quotes_wide[,which(colnames(quotes_wide) %in% c("per measure - NATO subclass", "per effect - FS indicator class", "name_id", "Document", "ID"))]
empty_rows <- which(rowSums(is.na(nest[,4:ncol(nest)]))==length(4:ncol(nest)))
nest <- nest[-empty_rows,]

nest <- split_and_add_by_ID(sheet = nest, clm = which(colnames(nest) == "per effect - FS indicator class")) #FS indicator class
nest <- nest[-which(is.na(nest$`per measure - NATO subclass`)),]
nest <- nest[,-which(colnames(nest) %in% c("name_id"))]

# variables nested within the nested variable
impact_vars <- c( "per effect - aff agent class 2", #"per effect - affected food producers", "per effect - affected food distributors transporters",
                  #"per effect - affected food storers and processors", "per effect - affected food retailers", "per effect - affected food consumers",
                  #"per effect - affected agents", "per effect - affected political entities", "per effect - affected non-food agents other", 
                  "per effect - direct?", "per effect - intended?", "per effect - place", "per effect - type other")

#affected agent class
nesta <- quotes_wide[,which(colnames(quotes_wide) %in% c("per effect - aff agent class 2", "per effect - FS indicator class", "name_id", "Document", "ID"))]
empty_rows <- which(rowSums(is.na(nesta[,4:ncol(nesta)]))==length(4:ncol(nesta)))
if(sum(empty_rows) != 0){
  nesta <- nesta[-empty_rows,]
}
nesta <- split_and_add_by_ID(sheet = nesta, clm = which(colnames(nesta) == "per effect - aff agent class 2")) #affected agent class
nesta <- nesta[-which(is.na(nesta$`per effect - FS indicator class`)),]
nesta <- nesta[,-which(colnames(nesta) %in% c("name_id"))]

nesta <- merge(nest, nesta)

#per effect - direct?
nestb <- quotes_wide[,which(colnames(quotes_wide) %in% c("per effect - direct?", "per effect - FS indicator class", "name_id", "Document", "ID"))]
empty_rows <- which(rowSums(is.na(nestb[,4:ncol(nestb)]))==length(4:ncol(nestb)))
nestb <- nestb[-empty_rows,]

nestb <- split_and_add_by_ID(sheet = nestb, clm = which(colnames(nestb) == "per effect - direct?")) #per effect - direct?
nestb <- nestb[-which(is.na(nestb$`per effect - FS indicator class`)),]
nestb <- nestb[,-which(colnames(nestb) %in% c("name_id"))]

nestb <- merge(nest, nestb)

#per effect - intended?
nestc <- quotes_wide[,which(colnames(quotes_wide) %in% c("per effect - intended?", "per effect - FS indicator class", "name_id", "Document", "ID"))]
empty_rows <- which(rowSums(is.na(nestc[,4:ncol(nestc)]))==length(4:ncol(nestc)))
nestc <- nestc[-empty_rows,]

nestc <- split_and_add_by_ID(sheet = nestc, clm = which(colnames(nestc) == "per effect - intended?")) #per effect - intended?
nestc <- nestc[-which(is.na(nestc$`per effect - FS indicator class`)),]
nestc <- nestc[,-which(colnames(nestc) %in% c("name_id"))]

nestc <- merge(nest, nestc)

#per effect - global impact
nestd <- quotes_wide[,which(colnames(quotes_wide) %in% c("per effect - global impact", "per effect - FS indicator class", "name_id", "Document", "ID"))]
nestd <- split_and_add_by_ID(sheet = nestd, clm = which(colnames(nestd) == "per effect - global impact")) #per effect - place
nestd <- nestd[-which(is.na(nestd$`per effect - FS indicator class`)),]
nestd <- nestd[,-which(colnames(nestd) %in% c("name_id"))]
nestd <- merge(nest, nestd)

#per effect - impact within jurisdiction
neste <- quotes_wide[,which(colnames(quotes_wide) %in% c("per effect - impact within jurisdiction", "per effect - FS indicator class", "name_id", "Document", "ID"))]
neste <- split_and_add_by_ID(sheet = neste, clm = which(colnames(neste) == "per effect - impact within jurisdiction")) #per effect - place
neste <- neste[-which(is.na(neste$`per effect - FS indicator class`)),]
neste <- neste[,-which(colnames(neste) %in% c("name_id"))]
neste <- merge(nest, neste)

#per effect - impact outside jurisdiction
nestf <- quotes_wide[,which(colnames(quotes_wide) %in% c("per effect - impact outside jurisdiction", "per effect - FS indicator class", "name_id", "Document", "ID"))]
nestf <- split_and_add_by_ID(sheet = nestf, clm = which(colnames(nestf) == "per effect - impact outside jurisdiction")) #per effect - place
nestf <- nestf[-which(is.na(nestf$`per effect - FS indicator class`)),]
nestf <- nestf[,-which(colnames(nestf) %in% c("name_id"))]
nestf <- merge(nest, nestf)

# merge
nest <- full_join(nesta, nestb)
nest <- full_join(nest, nestc)
nest <- full_join(nest, nestd)
nest <- full_join(nest, neste)
nest <- full_join(nest, nestf)

nest <- nest[,-which(colnames(nest) == "ID")]
nest <- nest %>% distinct()

nest$nestcol <- paste(nest$`per effect - FS indicator class`, nest$`per measure - NATO subclass`)
nest$`per effect - global impact` <- na_to_no_nested(dat = nest, na_col = "per effect - global impact", matchterms = "yes", nest_col = "nestcol")
nest$`per effect - impact within jurisdiction` <- na_to_no_nested(dat = nest, na_col = "per effect - impact within jurisdiction", matchterms = "yes", nest_col = "nestcol")
nest$`per effect - impact outside jurisdiction` <- na_to_no_nested(dat = nest, na_col = "per effect - impact outside jurisdiction", matchterms = "yes", nest_col = "nestcol")

nest <- nest[,-which(colnames(nest) == "nestcol")]
nest <- nest %>% distinct()

nest$`per effect - aff agent class 2` <- paste("aff", nest$`per effect - aff agent class 2`)

nest$rownr <- 1:nrow(nest)
nest$yes <- "yes"
nest <- nest %>% spread(key = "per effect - aff agent class 2", value = "yes")
nest <- nest[,-which(colnames(nest)=="rownr")]
to_no <- is.na(nest[,which(colnames(nest)=="aff food consumers"):ncol(nest)])
nest[,which(colnames(nest)=="aff food consumers"):ncol(nest)][to_no] <- "no"

#nest <- nest[,-which(colnames(nest) == 'aff NA')]
nest <- nest %>% distinct()

# Split up per effect - FS indicator class
nest$yes <- "yes"
colnames(nest)
nest <- unite(nest, col = "nest", c(1,2:ncol(nest)), sep = "_", remove = FALSE)
nest$`per effect - FS indicator class` <- paste("FS -", nest$`per effect - FS indicator class`)
nest <- nest %>% spread(key = "per effect - FS indicator class", value = "yes")

colnames(nest)
for(i in colnames(nest)[which(colnames(nest)== "FS - access"):ncol(nest)]){
  cnr <- which(colnames(nest) == i)
  nest[,cnr] <- na_to_no_nested(dat = nest, na_col = i, matchterms = "yes", nest_col = "nest")
}

nest <- nest[,-which(colnames(nest)=="nest")]
nest <- nest %>% distinct()

# checking whether everything went OK
nada <- govspfoagmod[rowSums(is.na(nest)) > 4,]
(mydocs <- sort(unique(nest$Document)))
(length(mydocs))

#############################################
govspfoagmod <- merge(govspfoagmod, nest)
govspfoagmod <- govspfoagmod %>% distinct()
test <- govspfoagmod
govspfoagmod <- test

# Split up per measure - NATO subclass
govspfoagmod$yes <- "yes"
colnames(govspfoagmod)
govspfoagmod <- unite(govspfoagmod, col = "nest", c(1,which(colnames(govspfoagmod)=="governance - combined measures?"):which(colnames(govspfoagmod)=="gov - obj utilisation")), sep = "_", remove = FALSE)
govspfoagmod$`per measure - NATO subclass` <- paste("gov -", govspfoagmod$`per measure - NATO subclass`)
govspfoagmod <- govspfoagmod %>% spread(key = "per measure - NATO subclass", value = "yes")
nada <- govspfoagmod[rowSums(is.na(govspfoagmod)) > 4,]
(mydocs <- sort(unique(govspfoagmod$Document)))
(length(mydocs))

for(i in colnames(govspfoagmod)[which(colnames(govspfoagmod)=="gov - At-large processing"):ncol(govspfoagmod)]){
  cnr <- which(colnames(govspfoagmod) == i)
  govspfoagmod[,cnr] <- na_to_no_nested(dat = govspfoagmod, na_col = i, matchterms = "yes", nest_col = "nest")
}

govspfoagmod <- govspfoagmod[,-which(colnames(govspfoagmod)=="nest")]
govspfoagmod <- govspfoagmod %>% distinct()


rm(list = c(paste0("nest", c("", "a", "b", "c", "d")), "empty_rows", "impact_vars", "gov", "keep", "gov_codes"))

nada <- govspfoagmod[rowSums(is.na(govspfoagmod)) > 4,]
(mydocs <- sort(unique(govspfoagmod$Document)))
(length(mydocs))

govspfoagmod <- govspfoagmod[-which(colnames(govspfoagmod)=="FS - ")]
govspfoagmod <- govspfoagmod[-which(colnames(govspfoagmod)=="gov - obj NA")]

print("save data")
write.csv(govspfoagmod, file = "./Output/factor_analysis_data.csv")
columnnames_wide <- colnames(govspfoagmod)
save(columnnames_wide, file = "./Output/factor_analysis_names.rda")



