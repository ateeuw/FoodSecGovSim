# libraries
print("loading libraries")
library(dplyr) #for piping
library(tidyr) #for data processing
library(FactoMineR) #*
library(PCAmixdata)  #*
library(ggplot2) #*
library(stringi)
library(missMDA)

# load data
print("loading data")
fadata <- read.csv(file = "./Output/factor_analysis_data_all.csv", sep = ";") #made using script "data_for_factor_analysis.R"
nafa <- fadata[rowSums(is.na(fadata)) > 4,]
load("./Output/factor_analysis_names_all.rda")
colnames(fadata) <- c(columnnames_wide)

# these variables were not important and therefore removed from the dataset
fadata <- fadata[,-which(colnames(fadata) %in% c("modelling - aim", "modelling - feedback-loop?", "modelling - sensitivity analysis?", "modelling - validation?", "governance - combined measures?", "per effect - direct?"))] 
fadata <- fadata %>% distinct()

write.csv(fadata, file = "./Output/factor_analysis_data.csv")
cnames <- colnames(fadata)
save(cnames, file = "./Output/factor_analysis_data.rda")
rm(cnames)

# in order to run the FA the columns per group must be in order
print("ordering variables in the for factor analysis")
#############################################
# sort(colnames(fadata))
agent_vars <- colnames(fadata)[grep("per agent -*", colnames(fadata))]

which(colnames(fadata) %in% agent_vars)
n_agent <- length(which(colnames(fadata) %in% agent_vars))

model_vars <- c(colnames(fadata)[grep("per model -*", colnames(fadata))],
                colnames(fadata)[grep("modelling -*", colnames(fadata))])

which(colnames(fadata) %in% model_vars)
n_model <- length(which(colnames(fadata) %in% model_vars))

food_vars <- c(colnames(fadata)[grep("^food -*", colnames(fadata))])
which(colnames(fadata) %in% food_vars)
n_food <- length(which(colnames(fadata) %in% food_vars))

spat_vars <- colnames(fadata)[grep("^spatial ", colnames(fadata))] 
which(colnames(fadata) %in% spat_vars)
n_spat <- length(which(colnames(fadata) %in% spat_vars))

gov_vars <- c(colnames(fadata)[grep("^per measure ", colnames(fadata))],
              colnames(fadata)[grep("^gov ", colnames(fadata))])#c("governance - combined measures?", 

which(colnames(fadata) %in% gov_vars)
fadata <- fadata[,c(1:78, 96:118, 79:95)]
which(colnames(fadata) %in% gov_vars)
n_gov <- length(which(colnames(fadata) %in% gov_vars))

imp_vars <- c(colnames(fadata)[grep("per effect -*", colnames(fadata))],
              colnames(fadata)[grep("aff *", colnames(fadata))],
              colnames(fadata)[grep("^FS -", colnames(fadata))])

which(colnames(fadata) %in% imp_vars)
n_imp <- length(which(colnames(fadata) %in% imp_vars))

# we also need weights per paper to avoid bias towards papers with many codes:
# make a vector that weighs each Document according to the number of rows each document has
nr_rows <- fadata %>% group_by(Document) %>% count(Document)
nr_rows$weight <- 1/nr_rows$n
# head(nr_rows)
rowweights <- rep(nr_rows$weight, nr_rows$n)

# running analysis
index <- c(rep(1,n_agent), rep(2,n_model),rep(3,n_food),rep(4,n_spat), rep(5,n_gov), rep(6,n_imp)) 
docs <- fadata$Document
fadata <- fadata[,-1]
nafa <- fadata[rowSums(is.na(fadata)) > 4,]
# print(cbind(colnames(fadata), index))
names <- c("agent","model","food", "spat", "gov", "FS") 
#############################################

# check variables
par(mfrow = c(3,3))
for(i in 1:ncol(fadata)){print(barplot(table(fadata[,i]), main = as.character(colnames(fadata[i]))))}


# Multiple factor analysis
#############################################
print("running MFA")
par(mfrow = c(1,1))
res.mfamix<-MFAmix(data=fadata,groups=index,
                   name.groups=names,ndim=3,
                   rename.level=TRUE,graph=TRUE)
#############################################

# Visualising MFA outcome
#############################################
print("saving levels component maps to ./Figures/levels_comp_maps.pdf")
figdir <- "./Figures"
png(filename = "./Figures/levels_comp_maps.png", width = 1300, height = 1300)
plot(res.mfamix, choice = "levels", coloring.var = "groups")
dev.off()

levelsmfa <- as.data.frame(res.mfamix$levels[["coord"]])
rownames(levelsmfa)
levelsmfa$category <- rownames(levelsmfa)
levelsmfa$factor_group <- rep("", nrow(levelsmfa))

agent_factors <- which(stri_startswith_fixed(levelsmfa$category, "per.agent"))
food_factors <- which(stri_startswith_fixed(levelsmfa$category, "food.system"))
model_factors <- c(which(stri_startswith_fixed(levelsmfa$category, "modelling")),
                   which(stri_startswith_fixed(levelsmfa$category, "per.model")))
gov_factors <- which(stri_startswith_fixed(levelsmfa$category, "per.measure"))
eff_factors <- which(stri_startswith_fixed(levelsmfa$category, "per.effect"))
fs_factors <- which(stri_startswith_fixed(levelsmfa$category, "FS..."))
aff_factors <- which(stri_startswith_fixed(levelsmfa$category, "aff."))
spat_factors <- which(stri_startswith_fixed(levelsmfa$category, "spatial"))
nato_factors <- which(stri_startswith_fixed(levelsmfa$category, "gov"))
mtype_factors <- which(stri_startswith_fixed(levelsmfa$category, "per.model...type"))
comm_factors <- which(stri_startswith_fixed(levelsmfa$category, "food.system...commodity.class."))

levelsmfa$factor_group[agent_factors] <- "agent"
levelsmfa$factor_group[food_factors] <- "food system echelon"
levelsmfa$factor_group[model_factors] <- "model (model type separate)"
levelsmfa$factor_group[gov_factors] <- "governance (NATO seperate)"
levelsmfa$factor_group[fs_factors] <- "Food security impact"
levelsmfa$factor_group[eff_factors] <- "within jurisdiction?"
levelsmfa$factor_group[aff_factors] <- "affected agents"
levelsmfa$factor_group[spat_factors] <- "spatial (qualitative)"
levelsmfa$factor_group[nato_factors] <- "NATO subclasses"
levelsmfa$factor_group[mtype_factors] <- "modeltype"
levelsmfa$factor_group[comm_factors] <- "food commodities"

figname <- "./Figures/levels_components_maps.pdf"
pdf(file=figname, paper = "a4r")

plot(res.mfamix, choice = "ind")

ylower <- min(levelsmfa$`dim 2`) - 0.5
yupper <- max(levelsmfa$`dim 2`) + 4
xlower <- min(levelsmfa$`dim 1`) - 0.5
xupper <- max(levelsmfa$`dim 1`) + 12

ggplot(data = levelsmfa, aes(x = `dim 1`, y = `dim 2`, label = category, colour = factor_group)) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  geom_point() +
  geom_text(hjust = 0, angle=30) +
  xlim(xlower, xupper) +
  ylim(ylower, yupper) +
  theme_classic()

for(i in unique(levelsmfa$factor_group)){
  datai <- levelsmfa[levelsmfa$factor_group==i,]
  g <- ggplot(data = datai, aes(x = `dim 1`, y = `dim 2`, label = category)) +
    geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
    geom_point() +
    geom_text(hjust = 0, angle=30) +
    xlim(xlower, xupper) +
    ylim(ylower, yupper) +
    labs(title = i) +
    theme_classic()
  print(g)
}

plot(res.mfamix, choice = "groups", coloring.var = "groups")
plot(res.mfamix, choice = "cor", coloring.var = "groups")
plot(res.mfamix, choice = "sqload", coloring.var = "groups")
plot(res.mfamix, choice = "axes", coloring.var = "groups")

dev.off()
#############################################

# Running HCA
#############################################
print("Running HCA")
tab <- cbind.data.frame(res.mfamix$ind$coord, fadata) # check that the output is $ind$coord and consider the number of components you want
natab <- tab[rowSums(is.na(tab)) > 4,]

resPCA <- imputePCA(tab, 
                    scale.unit=FALSE, 
                    quali.sup= c(4:ncol(tab)),
                    row.w = rowweights
)


resHCPC <- HCPC(resPCA, nb.clust = 4) #http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/117-hcpc-hierarchical-clustering-on-principal-components-essentials/#case-3-clustering-on-mixed-data
#############################################

#Visualising HCA outcome
#############################################
print("Visualising HCA outcome, saving to: ./Figures/HCPCtree.png")
png(filename = "./Figures/HCPCtree.png", width = 1300, height = 1300)
plot(resHCPC, choice = "tree")
dev.off()
#############################################

#Saving HCA outcome
#############################################
print("Saving HCA outcome to: ./Output/HCPCresult.csv")
tab <- cbind.data.frame(tab, docs)
natab <- tab[rowSums(is.na(tab)) > 4,]

colnames(tab)[colnames(tab) == "docs"] <- "Document"

output <- resHCPC$data.clust
outputm <- cbind.data.frame(output, tab)
outputm <- outputm[,-1:-3]

write.csv(outputm, file = "./Output/HCPCresult.csv")
cnames <- colnames(outputm)
save(cnames,  file = "./Output/HCPCcnames.rda")
rm(cnames)
#############################################
