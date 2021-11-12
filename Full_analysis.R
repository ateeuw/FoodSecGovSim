# Main script sourcing all other scripts

# Author: Aleid Sunniva Teeuwen
# Date: 12.11.2021
# Project: FoodSecGovSim
# Publication: Ex-ante assessment of food security governance: a systematic review of simulation models

rm(list = ls()) #start with a clean environment

# Install dependencies
list.of.packages <- c("ggplot2", "readxl", "dplyr", "tidyr", "countrycode", "maps", "xlsx", "rJava")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

source("./Data_overview.R") 

# This script pre-processes the raw data exported from atlas ti so that, for instance, countries are grouped into regions and governance measures are grouped into governance tools
# Input: 
# -- ./Data/all_quotes_simpl.xlsx: The raw data exported from atlas ti
# Output: 
# -- ./Output/quotes_wide.csv & ./Output/quotes_long.csv: Two tables (long format and wide format) connecting variables and variable categories to Documents 
# -- ./Output/all_variables.xlsx: An overview with a sheet of summary statistics for each variable

rm(list = ls()) #start with a clean environment

# Install dependencies
list.of.packages <- c("ggplot2", "readxl", "dplyr", "tidyr", "countrycode", "maps", "xlsx", "rJava")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

source("./Data_for_MFA.R")

# This script selects and organises the data so that they are ready for cluster analysis
# Input: 
# -- ./Output/quotes_wide.csv
# Output: 
# -- ./Output/factor_analysis_data.csv: Data ready for cluster analysis

source("./MFA_and_HCA.R")

# This script performs the multiple factor analysis (MFA) and hierarchical cluster analysis (HCA) from which we draw four clusters representing typical approaches to food security governance simulation modelling
# Input:
# -- ./Output/factor_analysis_data.csv
# Output:
# -- ./Output/HCPCesult.csv: This is a dataframe that contains all the individual approaches (see publication for methods) and the clusters they have been attributed to, as well as their coordinates along the principal components
# Figures:
# -- ./Figures/levels_component_maps.pdf: This is a pdf document with the outcome of the factor analysis

source("./Visualisation.R")

# This script is used to visualise the results from the cluster analysis
# Input: 
# -- ./Output/HCPCesult.csv
# Figures:
# -- ./Figures/clusterfigs_n_weighted.pdf
# -- ./Figures/clusterfigs_f_weighted.pdf
# 




