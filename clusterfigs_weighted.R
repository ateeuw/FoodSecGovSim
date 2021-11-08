library("dplyr")
library("tidyr")
library("ggplot2")
library("Rfast")
library("formattable") #for making ncie tables #see https://haozhu233.github.io/kableExtra/awesome_table_in_html.html & http://cran.nexr.com/web/packages/kableExtra/vignettes/use_kableExtra_with_formattable.html 
library("kableExtra") #for making nice tables #see https://haozhu233.github.io/kableExtra/awesome_table_in_html.html & http://cran.nexr.com/web/packages/kableExtra/vignettes/use_kableExtra_with_formattable.html 
library("webshot")


rm(list = ls()) #start with a clean environment

# Functions
source("./Functions/na_to_no.R")


#HCPC analysis

fullkk <- read.csv("./Output/HCPCresult.csv")
load("./Output/HCPCcnames.rda")

fullkk <- fullkk[,-1]
colnames(fullkk) <- c(cnames)

fadata <- read.csv("./Output/factor_analysis_data_reduced_split.csv")
load("./Output/factor_analysis_data_reduced_split.rda")

fadata <- fadata[,-1]
colnames(fadata) <- cnames

data <- fullkk

# Check if data are OK
nada <- data[rowSums(is.na(data)) > 5,]
(mydocs <- sort(unique(data$Document)))
alldocs <- mydocs
(length(mydocs)) #yes!

# After making some changes in ATLAS ti, the order of the cluster got changed. 
# To avoid being confused I change them back to the order I was used to.

data$clust[data$clust == 4] <- "was 1"
data$clust[data$clust == 3] <- "was 2"
data$clust[data$clust == 2] <- 3
data$clust[data$clust == 1] <- 4
data$clust[data$clust == "was 1"] <- 1
data$clust[data$clust == "was 2"] <- 2

ndocs <- data %>% group_by(clust, Document) %>% summarise(n = n()) 

clustcols <- c("grey", "orange", "deepskyblue", "red")

ndocs <- ndocs[order(ndocs$Document, -ndocs$n),]

doctab <- ndocs %>% 
  kable("html", escape = F, caption = paste("")) %>%
  kable_styling(font_size = 20) %>%
  kable_classic(full_width = F, html_font = "Cambria", position = "center") %>%
  row_spec(which(ndocs$clust==1), background = clustcols[1]) %>%
  row_spec(which(ndocs$clust==2), background = clustcols[2]) %>%
  row_spec(which(ndocs$clust==3), background = clustcols[3]) %>%
  row_spec(which(ndocs$clust==4), background = clustcols[4])

doctab %>% as_image(width = 22, file = paste0("./Figures/docs_cluster_table2.png"))

#ndocs$rownr <- 1:nrow(ndocs)
ndocs$clust <- paste0("cluster_", ndocs$clust)
ndocs <- ndocs %>% spread(clust, n)
ndocs[is.na(ndocs)] <- 0

ndocs$dominant_cluster <- rowMaxs(cbind(ndocs$cluster_1, ndocs$cluster_2, ndocs$cluster_3, ndocs$cluster_4))
ndocs <- ndocs[order(ndocs$dominant_cluster),]

doctab <- ndocs %>% 
  kable("html", escape = F, caption = paste("")) %>%
  kable_styling(font_size = 20) %>%
  kable_classic(full_width = F, html_font = "Cambria", position = "center") %>%
  row_spec(which(ndocs$dominant_cluster==1), background = clustcols[1]) %>%
  row_spec(which(ndocs$dominant_cluster==2), background = clustcols[2]) %>%
  row_spec(which(ndocs$dominant_cluster==3), background = clustcols[3]) %>%
  row_spec(which(ndocs$dominant_cluster==4), background = clustcols[4])

doctab %>% as_image(width = 22, file = paste0("./Figures/docs_cluster_table.png"))

#
# Co-occurence between model types

mdat <- data[,c(1,21:29,ncol(data))]

mdat[mdat=="no"] <- NA

for(i in 2:10){
  mdat[,i] <- na_to_no(dat = mdat, na_col = colnames(mdat)[i], matchterm = "yes", val = "yes")
}

mdat <- mdat %>% distinct()
mdat <- gather(data = mdat, model_type, present, `per model - ABM`:`per model - system dynamics model`)
mdat <- mdat[mdat$present=="yes",] 

mdat1 <- mdat[mdat$clust==1,]
V1 <- crossprod(table(mdat1[c(2:3)]))
diag(V1) <- 0
V1 <- as.data.frame(V1)
V1$cluster <- 1
V1$modeltype2 <- row.names(V1)

mdat2 <- mdat[mdat$clust==2,]
V2 <- crossprod(table(mdat2[c(2:3)]))
diag(V2) <- 0
V2 <- as.data.frame(V2)
V2$cluster <- 2
V2$modeltype2 <- row.names(V2)

mdat3 <- mdat[mdat$clust==3,]
V3 <- crossprod(table(mdat3[c(2:3)]))
diag(V3) <- 0
V3 <- as.data.frame(V3)
V3$cluster <- 3
V3$modeltype2 <- row.names(V3)

mdat4 <- mdat[mdat$clust==4,]
V4 <- crossprod(table(mdat4[c(2:3)]))
diag(V4) <- 0
V4 <- as.data.frame(V4)
V4$cluster <- 4
V4$modeltype2 <- row.names(V4)

V <- full_join(V1, V2)
V <- full_join(V, V3)
V <- full_join(V, V4)

V[is.na(V)] <- 0

dat_cooc <- gather(data = V, model_type, cooccurence, 
                   c(`per model - cellular automata`:`per model - system dynamics model`, `per model - ABM`:`per model - statistical/econometric`))

x <- 2
colours4mtyp <- c('#88CCEE', '#44AA99', '#117733', '#332288', '#DDCC77', '#999933', '#CC6677', '#882255', "darkgrey")

dat_cooc$cooccurence <- as.numeric(as.character(dat_cooc$cooccurence))

png(filename = paste0("./Figures/per-model_type_domain_co-occurence.png"), width = 1800, height = 800)
ggplot(dat_cooc, aes(y = modeltype2, x = model_type, col = model_type, label = cooccurence)) +
  geom_point(size = dat_cooc$cooccurence*3+1, alpha = 0.7) +
  geom_text(col = "black", fontface = "bold", size = dat_cooc$cooccurence*3+1) +
  #ggtitle(paste("Total number of studies =", n_studies, ". Studies with combined governances measures =", n_combined)) +
  xlab("Model type") +
  ylab("Co-occuring model type") +
  theme_classic() +
  theme(axis.text.x = element_text(vjust = 0.5, hjust=1, size = rel(1.2*x), angle = 90), 
        axis.text.y = element_text(size = rel(1.2*x)), 
        axis.title = element_text(size=rel(1.2*x), face="bold"),
        legend.title = element_text(size = rel(1.3*x), face = "bold"), 
        legend.position = "none",
        legend.text = element_text(size = rel(1.4*x)),
        title = element_text(size = rel(1.4*x))) + 
  scale_colour_manual(name = "", values = colours4mtyp) +
  #scale_color_gradient(low = "white", high = "red") +
  coord_flip() + facet_grid(. ~ cluster)
dev.off()

rm(list = c("dat", "dat_sum", "dat_cooc", "x"))
#

# echelon co-occurence
edat <- data[,c(1,33:38,ncol(data))]

edat <- edat %>% distinct()
edat <- gather(data = edat, echelon, present, `food - consumption`:`food - trade & wholesale`)
edat <- edat[edat$present=="yes",] 

edat <- data[,c(1,33:38,ncol(data))]

edat$VC <- ""
edat <- edat[,c(1,4,3,6,7,5,2,8,9)]

for(i in 1:nrow(edat)){
  vals <- colnames(edat)[which(edat[i,]=="yes")]
  vals <- gsub(pattern = "food - ", replacement = "", x = vals)
  edat$VC[i] <- paste(vals, collapse = "--")
}

edatsumm <- edat %>% group_by(clust, VC, Document) %>% summarise(n = n()) 
edatsumm$radius <- sqrt(edatsumm$n/3.14)
edatsumm$square <- sqrt(edatsumm$n)
edatsumm$n <- 1
edatsumm <- edatsumm %>% group_by(clust, VC) %>% summarise(n = n()) 
edatsumm$radius <- sqrt(edatsumm$n/3.14)
edatsumm$square <- sqrt(edatsumm$n)

edatsummm <- edat %>% group_by(VC) %>% summarise(n = n()) 
edatsummm$radius <- sqrt(edatsummm$n/3.14)
edatsummm$square <- sqrt(edatsummm$n)

#

# combine agents
colnames(data)
data <- data %>% gather(agent, applicable, `per agent - food consumers`:`per agent - food storers and processors`)
data$agent <- gsub(pattern = "per agent - ", replacement = "", x = data$agent)
data <- data[data$applicable=="yes",]
unique(data$agent)

# Check if data are OK
nada <- data[rowSums(is.na(data)) > 5,]
(mydocs <- sort(unique(data$Document)))
(length(mydocs)) #yes!

# combine domains
data <- data %>% gather(domain, applicable, c(`per model - biophysical domain`, 
                                              `per model - economic domain`:`per model - social domain`))

data$domain <- gsub(pattern = "per model - ", replacement = "", x = data$domain)
data <- data[data$applicable=="yes",]
unique(data$domain)

# Check if data are OK
nada <- data[rowSums(is.na(data)) > 5,]
(mydocs <- sort(unique(data$Document)))
alldocs[-which(alldocs %in% mydocs)]
(length(mydocs)) #nope!

# combine model types
data <- data %>% gather(model_type, applicable, c(`per model - ABM`:`per model - system dynamics model`))

data$model_type <- gsub(pattern = "per model - ", replacement = "", x = data$model_type)
data <- data[data$applicable=="yes",]
unique(data$model_type)

# Check if data are OK
nada <- data[rowSums(is.na(data)) > 5,]
(mydocs <- sort(unique(data$Document)))
(length(mydocs)) #yes!

# combine value chain echelons
data <- data %>% gather(echelon, applicable, c(`food - consumption`:`food - trade & wholesale`))

data$echelon <- gsub(pattern = "food - ", replacement = "", x = data$echelon)
data <- data[data$applicable=="yes",]
unique(data$echelon)

# Check if data are OK
nada <- data[rowSums(is.na(data)) > 5,]
(mydocs <- sort(unique(data$Document)))
(length(mydocs)) #yes!

# combine commodities
data <- data %>% gather(commodity, applicable, c(`food - cereals & cereal products`:`food - vegetables and derived products`))

data$commodity <- gsub(pattern = "food - ", replacement = "", x = data$commodity)
data <- data[data$applicable=="yes",]
unique(data$commodity)

# Check if data are OK
nada <- data[rowSums(is.na(data)) > 5,]
(mydocs <- sort(unique(data$Document)))
(length(mydocs)) #yes!

# combine regions
data <- data %>% gather(region, applicable, c(`spatial - East Asia & Pacific`:`spatial - Sub-Saharan Africa`))

data$region <- gsub(pattern = "spatial - ", replacement = "", x = data$region)
data <- data[data$applicable=="yes",]
unique(data$region)

# Check if data are OK
nada <- data[rowSums(is.na(data)) > 5,]
(mydocs <- sort(unique(data$Document)))
(length(mydocs)) #yes!
alldocs[-which(alldocs %in% mydocs)]

# combine governance objectives
data <- data %>% gather(objective, applicable, c(`gov - obj access`:`gov - obj other`, 
                                                 `gov - obj stability`, `gov - obj utilisation`))

data$objective <- gsub(pattern = "gov - obj ", replacement = "", x = data$objective)
data <- data[data$applicable=="yes",]
unique(data$objective)
data$objective[data$objective=="infrastructure, logistics & technology"] <- "infrastructure, logistics & tech"

# Check if data are OK
nada <- data[rowSums(is.na(data)) > 5,]
(mydocs <- sort(unique(data$Document)))
(length(mydocs)) #yes!
alldocs[-which(alldocs %in% mydocs)]

# combine NATO subclasses
data <- data %>% gather(NATO, applicable, c(`gov - At-large processing`:`gov - Transport and distribution`))

data$NATO <- gsub(pattern = "gov - ", replacement = "", x = data$NATO)
data <- data[data$applicable=="yes",]
unique(data$NATO)

# Check if data are OK
nada <- data[rowSums(is.na(data)) > 5,]
(mydocs <- sort(unique(data$Document)))
(length(mydocs)) #yes!
alldocs[-which(alldocs %in% mydocs)]

# combine affected agents
data <- data %>% gather(aff_agent, applicable, c(`aff food consumers`:`aff political entities`))

data <- data[data$applicable=="yes",]
unique(data$aff_agent)

# Check if data are OK
nada <- data[rowSums(is.na(data)) > 5,]
(mydocs <- sort(unique(data$Document)))
(length(mydocs)) #yes!
alldocs[-which(alldocs %in% mydocs)]

# combine FS indicators
data <- data %>% gather(FS_ind, applicable, c(`FS - access`:`FS - utilisation`))

data$FS_ind <- gsub(pattern = "FS - ", replacement = "", x = data$FS_ind)
data <- data[data$applicable=="yes",]
unique(data$FS_ind)

# Check if data are OK
nada <- data[rowSums(is.na(data)) > 4,]
(mydocs <- sort(unique(data$Document)))
(length(mydocs)) #yes!
alldocs[-which(alldocs %in% mydocs)]

(colnames(data))

data$NATO <- factor(data$NATO , levels = c("Packaged self-serve messages", "Propaganda", "Group-targeted messages", "Bespoke messages", #nodality
                                           "Open compacts", "Open permits", "Standard constraints", "Group-targeted constraints", "Conditional tokens", "Enablements", #authority
                                           "Bounties", "Bearer-directed payments", "Conduits", "Contracts", "Transfers", #treasure
                                           "At-large treatment", "At-large processing", "At-large storage and custody", "Group treatnent", "Group-targeted transportation and distribution", "Processing", "Storage and custody", "Transport and distribution")) #organisation

data$agent <- factor(data$agent,
                     levels = c("food producers","food distributors transporters", "food storers and processors", "food traders", "food retailers", "food consumers", #5
                                "generic agents", "political entities", "other non-food agents", "no agents")) #4

data$aff_agent <- factor(data$aff_agent,
                         levels = c("aff food producers","aff food distributors transporters", "aff food storers and processors", "aff food traders",
                                    "aff food retailers", "aff food consumers", #5
                                    "aff generic agents", "aff political entities", "aff other non-food agents", "aff no agents")) #4


data$echelon <- factor(data$echelon,
                       levels = c("production*", "distribution & transport", "storage and processing", "trade & wholesale", "retail", "consumption"))

unique(data$objective)
data$objective <- factor(data$objective,
                         levels = c("availability", "access", "utilisation", "stability",
                                    "macro-economic", "infrastructure, logistics & tech", "environmental/climate", "other"))

unique(data$region)
data$region <- factor(data$region,
                      levels = c("East Asia & Pacific", "Europe & Central Asia", "Latin America & Caribbean", 
                                 "Middle East & North Africa", "North America", "South Asia", "Sub-Saharan Africa",
                                 "NA"))

unique(data$model_type)
data$model_type <- factor(data$model_type,
                          levels = c("ABM", "cellular automata", "system dynamics model", "optimisation",
                                     "PE model", "CGE model", "microsimulation model", "statistical/econometric",
                                     "mathematical other"))

data$FS_ind <- factor(data$FS_ind,
                      levels = c("availability", "access", "utilisation", "stability"))

cnames <- colnames(data)

colors_distinct <- colorRampPalette(c('#88CCEE', '#44AA99', '#117733', '#332288', '#DDCC77', '#999933','#CC6677', '#882255', '#AA4499'))

ncols <- length(c('#88CCEE', '#44AA99', '#117733', '#332288', '#DDCC77', '#999933','#CC6677', '#882255', '#AA4499')) 

plot(rep(1:ncols), rep(1, ncols), pch = 15, cex = 4,
     col = c('#88CCEE', '#44AA99', '#117733', '#332288', '#DDCC77', '#999933','#CC6677', '#882255', '#AA4499'))

colors_purp <- colorRampPalette(c("#770077", "#9400D3", "#AB82FF"))
colors_red <- colorRampPalette(c("#8B0000", "#FF6A6A"))
colors_yel <- colorRampPalette(c("#8B5A00", "#FFB90F", "#FFFF00", "#FFEC8B"))
colors_pnk <- colorRampPalette(c("#8B0A50", "#EE6AA7", "#FFB6C1"))
colors_pnkgr <- colorRampPalette(c("#9400D3", "#FFB6C1", "#008B00"))
colors_two <- colors_distinct(2)

colours4nato <- c(colors_purp(4), colors_red(6), colors_yel(5), colors_pnk(8))
colours4agent <- c(colors_distinct(6), gray.colors(3), "black")
colours4obj <- c(colors_yel(4), colors_purp(4))
colours4reg <- c(colors_distinct(7),"grey")

figdir <- "./Figures/clusterfigs_n_weighted.pdf"
pdf(file=figdir)

for(i in colnames(data)[c(5:26,28:ncol(data))]){
  
  col_nr <- which(colnames(data)==i)
  
  colnames(data)[col_nr] <- "icol"
  
  summtab <- data %>% group_by(Document) %>%
    summarise(n = n())
  
  summtab$weight <- 1/summtab$n 
  summtab <- summtab[,-which(colnames(summtab)=="n")]
  
  tabsumm <- data %>% group_by(clust, icol, Document) %>% summarise(n = n()) 
  
  ndocs <- tabsumm %>% group_by(clust, Document) %>% summarise(n = n())
  ndocs$n <- 1
  ndocs <- ndocs %>% group_by(clust) %>% summarise(ndocs = sum(n))
  
  summtab <- full_join(tabsumm, summtab)
  summtab$n_weighted <- summtab$weight*summtab$n
  
  tabsumm <- summtab %>% group_by(clust, icol) %>% summarise(n_weighted = sum(n_weighted)) 
  
  totals <- tabsumm %>% group_by(clust) %>% summarise(total = sum(n_weighted))
  totals$total <- round(totals$total)
  
  totals <- merge(totals, ndocs)
  totals$bartext <- paste0(totals$total, " (", totals$ndocs, ")")
  
  summtab <- tabsumm
  
  g <- ggplot(data = summtab, aes(x=clust, y=n_weighted, fill = icol)) +
    geom_bar(position="stack", stat = "identity", colour = "black") +
    xlab("Cluster") + ylab("Number of studies") + 
    scale_y_continuous(breaks = seq(0,80,10)) +
    theme_classic() +
    theme(text = element_text(size = 40), legend.position = c(0.25, 0.7), legend.key.size = unit(1.5, 'cm'))
  
  if(i == "NATO"){
    g <- ggplot(data = summtab, aes(x=clust, y=n_weighted, fill = icol)) +
      geom_bar(position="stack", stat = "identity", colour = "black") +
      xlab("Cluster") + ylab("Number of studies") + 
      scale_y_continuous(breaks = seq(0,80,10)) +
      theme_classic() +
      theme(text = element_text(size = 20), legend.key.size = unit(1, 'cm'))
    
    g <- g + scale_fill_manual(name = "", values = colours4nato)
    govfig <- g
    
  }else if(i == "agent" | i == "aff_agent"){
    g <- g + scale_fill_manual(name = "", values = colours4agent)
    
  }else if(i == "modelling − data quan spatial"){
    summtab$icol <- as.factor(summtab$icol)
    g <- ggplot(data = summtab, aes(x=clust, y=n_weighted, fill = icol)) +
      geom_bar(position="stack", stat = "identity") +
      xlab("Cluster") + ylab("Number of studies") + 
      scale_y_continuous(breaks = seq(0,80,10)) +
      theme_classic() +
      theme(text = element_text(size = 40), legend.position = c(0.25, 0.7), legend.key.size = unit(1.5, 'cm'))
    
    g <- g + scale_fill_discrete(name = "", type = colors_pnkgr(3),
                                 labels = c("no spatial data", "spatial features", "spatial landscapes"))
    
  }else if(i == "spatial & temporal − ref quan scale"){
    summtab$icol <- as.factor(summtab$icol)
    g <- ggplot(data = summtab, aes(x=clust, y=n_weighted, fill = icol)) +
      geom_bar(position="stack", stat = "identity", colour = "black") +
      xlab("Cluster") + ylab("Number of studies") + 
      scale_y_continuous(breaks = seq(0,80,10)) +
      theme_classic() +
      theme(text = element_text(size = 40), legend.position = c(0.32, 0.7), legend.key.size = unit(1.5, 'cm'),
            legend.background = element_rect(fill='transparent'))
    
    g <- g + scale_fill_discrete(name = "", type = colors_pnkgr(6),
                                 labels = c("=< village/city district", "village/city district < X =< municipality",
                                            "municipality < X =< province/state", "province/state < X =< country",
                                            "country < X =< continent", "continent < X =< earth"))
    scalefig <- g
    
  }else if (i == "objective"){
    g <- g + scale_fill_manual(name = "", values = colours4obj)
    
  }else if(i == "Document"){
    n <- length(unique(summtab$icol))
    g <- g + scale_fill_manual(name = "", values = sample(colors_distinct(n), size = n)) +
      geom_text(aes(label = icol), size = 3, position = position_stack(vjust = 0.5)) +
      geom_line(colour = "grey50", linetype = "dotted") +
      theme(legend.position = "none")
    
  }else if(i == "region"){
    g <- g + scale_fill_manual(name = "", values = colours4reg)
    
  }else if(i == "commodity"){
    library(stringr)
    g <- g + scale_fill_manual(name = "", values = sample(colors_distinct(length(unique(summtab$icol)))), 
                                                          labels = str_trunc(summtab$icol, 13))
    
  }else if(i == "model_type"){
    g <- g + scale_fill_manual(name = "", values = colours4mtyp)
    modelfig <- g
    
  }else if(i == "domain"){
    g <- g + scale_fill_manual(name = "", values = grey.colors(4))
    domainfig <- g
    
  }else if (i == "FS_ind"){
    g <- g + scale_fill_manual(name = "", values = colors_yel(4))
    
  }else if(i == "echelon"){
    g <- g + scale_fill_manual(name = "", values = colors_distinct(6))
    
  }else if(is.numeric(summtab$icol)){
    g <- g + scale_fill_gradient2(name = "", low = "#9400D3", mid = "#FFB6C1", high = "#008B00")
    
  }else if(length(unique((summtab$icol)))==2){
    g <- g + scale_fill_manual(name = "", values = colors_two) +
      scale_color_manual("", values = "black")
    
  }else{
    g <- g + scale_fill_manual(name = "", values = sample(colors_distinct(length(unique(summtab$icol))), size = length(unique(summtab$icol)))) +
      scale_color_manual("", values = "black")
  }
  
  print(g)
  
  colnames(data)[col_nr] <- i
  
}

dev.off()

colnames(data) <- cnames

png(filename = paste0("./Figures/modeltype_cluster_bar.png"), width = 1100, height = 1300)
modelfig 
dev.off()

png(filename = paste0("./Figures/modeldomain_cluster_bar.png"), width = 1100, height = 1300)
domainfig 
dev.off()

png(filename = paste0("./Figures/modelscale_cluster_bar.png"), width = 1100, height = 1300)
scalefig 
dev.off()

png(filename = paste0("./Figures/gov_cluster_bar.png"), width = 1100, height = 1300)
govfig + guides(fill=guide_legend(nrow=23,byrow=FALSE))
dev.off()

figdir <- "../server_figures/clusterfigs_f_weighted_20211011.pdf"
pdf(file=figdir)

for(i in colnames(data)[c(5:26,28:ncol(data))]){
  
  col_nr <- which(colnames(data)==i)
  
  colnames(data)[col_nr] <- "icol"
  
  summtab <- data %>% group_by(Document) %>%
    summarise(n = n())
  
  summtab$weight <- 1/summtab$n 
  summtab <- summtab[,-which(colnames(summtab)=="n")]
  
  tabsumm <- data %>% group_by(clust, icol, Document) %>% summarise(n = n()) %>%
    mutate(freq = n / sum(n))
  
  ndocs <- tabsumm %>% group_by(clust, Document) %>% summarise(n = n())
  ndocs$n <- 1
  ndocs <- ndocs %>% group_by(clust) %>% summarise(ndocs = sum(n))
  
  summtab <- full_join(tabsumm, summtab)
  summtab$n_weighted <- summtab$weight*summtab$n
  
  tabsumm <- summtab %>% group_by(clust, icol) %>% summarise(n_weighted = sum(n_weighted)) %>%
    mutate(freq_weighted = n_weighted / sum(n_weighted))
  
  totals <- tabsumm %>% group_by(clust) %>% summarise(total = sum(freq_weighted))
  totals$total <- round(totals$total)
  
  totals <- merge(totals, ndocs)
  totals$bartext <- paste0("(", totals$ndocs, ")")
  
  summtab <- tabsumm
  
  g <- ggplot(data = summtab, aes(x=clust, y=freq_weighted, fill = icol)) +
    geom_bar(position="stack", stat = "identity") +
    xlab("cluster") + ylab("frequency") + 
    labs(title = i, caption = paste0("Rows: ", sum(totals$total), ", docs: ", sum(totals$ndocs))) +
    geom_text(data=totals ,aes(x=clust, y=total,label=bartext,fill=NULL),
              nudge_y = 0.05) +
    theme_classic()
  
  if(i == "NATO"){
    g <- g + scale_fill_manual(name = "", values = colours4nato)
    
  }else if(i == "agent" | i == "aff_agent"){
    g <- g + scale_fill_manual(name = "", values = colours4agent)
    
  }else if(i == "modelling − data quan spatial"){
    summtab$icol <- as.factor(summtab$icol)
    g <- ggplot(data = summtab, aes(x=clust, y=freq_weighted, fill = icol)) +
      geom_bar(position="stack", stat = "identity") +
      xlab("cluster") + ylab("frequency") + labs(title = i) +
      theme_classic()
    g <- g + scale_fill_discrete(name = "", type = colors_pnkgr(3),
                                 labels = c("no spatial data", "spatial features", "spatial landscapes"))
    
  }else if(i == "spatial & temporal − ref quan scale"){
    summtab$icol <- as.factor(summtab$icol)
    g <- ggplot(data = summtab, aes(x=clust, y=freq_weighted, fill = icol)) +
      geom_bar(position="stack", stat = "identity") +
      xlab("cluster") + ylab("frequency") + labs(title = i) +
      theme_classic()
    g <- g + scale_fill_discrete(name = "", type = colors_pnkgr(6),
                                 labels = c("=< village/city district", "village/city district < X =< municipality",
                                            "municipality < X =< province/state", "province/state < X =< country",
                                            "country < X =< continent", "continent < X =< earth"))
  }else if(i == "commodity"){
    library(stringr)
    g <- g + scale_fill_manual(name = "", values = sample(colors_distinct(length(unique(summtab$icol)))), 
                               labels = str_trunc(summtab$icol, 13))
    
  }else if (i == "objective"){
    g <- g + scale_fill_manual(name = "", values = colours4obj)
    
  }else if (i == "FS_ind"){
    g <- g + scale_fill_manual(name = "", values = colors_yel(4))
    
  }else if(i == "echelon"){
    g <- g + scale_fill_manual(name = "", values = colors_distinct(6))
    
  }else if(i == "Document"){
    n <- length(unique(summtab$icol))
    g <- g + scale_fill_manual(name = "", values = sample(colors_distinct(n), size = n)) +
      geom_text(aes(label = icol), size = 3, position = position_stack(vjust = 0.5)) +
      geom_line(colour = "grey50", linetype = "dotted") +
      theme(legend.position = "none")
    
  }else if(i == "region"){
    g <- g + scale_fill_manual(name = "", values = colours4reg)
    
  }else if(i == "model_type"){
    g <- g + scale_fill_manual(name = "", values = colours4mtyp)
    
  }else if(is.numeric(summtab$icol)){
    g <- g + scale_fill_gradient2(name = "", low = "#9400D3", mid = "#FFB6C1", high = "#008B00")
    
  }else if(length(unique((summtab$icol)))==2){
    g <- g + scale_fill_manual(name = "", values = colors_two) +
      scale_color_manual("", values = "black")
    
  }else{
    g <- g + scale_fill_manual(name = "", values = sample(colors_distinct(length(unique(summtab$icol))), size = length(unique(summtab$icol)))) +
      scale_color_manual("", values = "black")
  }
  
  print(g)
  
  colnames(data)[col_nr] <- i
  
}


dev.off()

colnames(data) <- cnames

figdir <- "../server_figures/papercoordinates_20211007.pdf"
pdf(file=figdir)

xupper <- max(data$`dim 1`) + 0.5
yupper <- max(data$`dim 2`) + 0.5

xlower <- min(data$`dim 1`) - 0.5
ylower <- min(data$`dim 2`) - 0.5

for(i in unique(data$Document)){

  datai <- data[data$Document == i,]
  g <- ggplot(datai, aes(x=`dim 1`, y=`dim 2`) ) +
    geom_bin2d(bins = 30) + labs(title = i) +
    xlim(xlower,xupper) + ylim(ylower,yupper) +
    scale_fill_continuous(type = "viridis") +
    theme_bw()

  print(g)

}

dev.off()

###################### per model - type x agent co-occurence ########################

dat_cooc <- data %>% 
  group_by(model_type, Document, agent, clust) %>%
  count(model_type, agent)

dat_cooc$n <- 1


dat_cooc <- dat_cooc %>% 
  group_by(model_type, agent, clust) %>%
  count(model_type, agent)

dat_cooc <- dat_cooc[!is.na(dat_cooc$agent),]

x <- 2

png(filename = paste0("./Figures/per-model_type_agent_co-occurence.png"), width = 2000, height = 1200)
ggplot(dat_cooc, aes(y = agent, x = model_type, col = model_type, label = n)) +
  geom_point(size = dat_cooc$n*2, alpha = 0.5) +
  geom_text(col = "black", fontface = "bold", size = 10) +
  #ggtitle(paste("Total number of studies =", n_studies, ". Studies with combined governances measures =", n_combined)) +
  ylab("Model type") +
  xlab("Model domain") +
  theme_classic() +
  theme(axis.text.x = element_text(vjust = 0.5, hjust=1, size = rel(1.2*x), angle = 90), 
        axis.text.y = element_text(size = rel(1.2*x)), 
        axis.title = element_text(size=rel(1.2*x), face="bold"),
        legend.title = element_text(size = rel(1.3*x), face = "bold"), 
        legend.position = "none",
        legend.text = element_text(size = rel(1.4*x)),
        title = element_text(size = rel(1.4*x))) + 
  scale_colour_manual(name = "", values = colours4mtyp) +
  #scale_color_gradient(low = "white", high = "red") +
  coord_flip() + facet_grid(. ~ clust)
dev.off()

rm(list = c("dat", "dat_sum", "dat_cooc", "x"))

# Spatial table
spatdat <- data[which(colnames(data)%in%
                        c("clust", "Document", "spatial & temporal - spatial extent [m2]",
                          "spatial & temporal - spatial resolution [m2]",
                          "spatial & temporal - geographic representation",
                          "spatial & temporal - hypothetical representation",
                          "per measure - spatially targeted?",
                          "modelling - data quan spatial"))]
spatdat <- spatdat %>% distinct()
spatdat$representation <- NA

# keep highest numbers in data quan spatial only
str(spatdat)
spatdat$`spatial & temporal - geographic representation` <- as.character(spatdat$`spatial & temporal - geographic representation`)
spatdat$`spatial & temporal - hypothetical representation` <- as.character(spatdat$`spatial & temporal - hypothetical representation`)
spatdat$`per measure - spatially targeted?` <- as.character(spatdat$`per measure - spatially targeted?`)

for(i in unique(spatdat$Document)){
  spatdat[spatdat$Document==i,]$`modelling - data quan spatial` <- max(spatdat[spatdat$Document==i,]$`modelling - data quan spatial`)
  spatdat[spatdat$Document==i,]$`spatial & temporal - spatial extent [m2]` <- !is.na(spatdat[spatdat$Document==i,]$`spatial & temporal - spatial extent [m2]`)
  spatdat[spatdat$Document==i,]$`spatial & temporal - spatial resolution [m2]` <- !is.na(spatdat[spatdat$Document==i,]$`spatial & temporal - spatial resolution [m2]`)
  if("yes" %in% spatdat[spatdat$Document==i,]$`spatial & temporal - geographic representation`){spatdat[spatdat$Document==i,]$representation = 2
  }else if("yes" %in% spatdat[spatdat$Document==i,]$`spatial & temporal - hypothetical representation`){spatdat[spatdat$Document==i,]$representation = 1
  }else{spatdat[spatdat$Document==i,]$representation = 0}
  spatdat[spatdat$Document==i,]$`per measure - spatially targeted?` <- ifelse(spatdat[spatdat$Document==i,]$`per measure - spatially targeted?`=="yes", 1, 0)
}

spatdat <- spatdat[,-which(colnames(spatdat)%in%c("spatial & temporal - geographic representation",
                                                  "spatial & temporal - hypothetical representation"))]
spatdat <- spatdat %>% distinct()

# Checked atlas TI 
spatdat[spatdat$Document=="Boulanger 2020",]$`modelling - data quan spatial` <- 1
spatdat[spatdat$Document=="Tanaka 2011",]$`modelling - data quan spatial` <- 1
spatdat[spatdat$Document=="Mukarati 2020",]$`modelling - data quan spatial` <- 0
spatdat[spatdat$Document=="Wossen 2015",]$`modelling - data quan spatial` <- 1
spatdat[spatdat$Document=="Kapuya 2013",]$`modelling - data quan spatial` <- 0
spatdat[spatdat$Document=="Jin 2019",]$`modelling - data quan spatial` <- 0

spatdat <- spatdat %>% distinct()

spatdatsumm <- spatdat %>% group_by(`modelling - data quan spatial`, `spatial & temporal - spatial extent [m2]`,
                                   `spatial & temporal - spatial resolution [m2]`, `per measure - spatially targeted?`,
                                   `representation`, Document) %>%
  count()

spatdatsumm$combined <- paste(spatdatsumm$`modelling - data quan spatial`, spatdatsumm$`spatial & temporal - spatial extent [m2]`,
                              spatdatsumm$`spatial & temporal - spatial resolution [m2]`, spatdatsumm$`per measure - spatially targeted?`,
                              spatdatsumm$representation, sep = " ")
spatdatsumm$n <- 1
spatdatsumm$`per measure - spatially targeted?` <- as.numeric(spatdatsumm$`per measure - spatially targeted?`)

spatfix <- spatdatsumm[1,]

for(i in unique(spatdatsumm$Document)){
  myrows <- which(spatdatsumm[,6] == i)
  mysums <- rowSums((spatdatsumm[myrows,1:5]))
  keep <- myrows[which(mysums == max(mysums))]
  spatfix <- rbind(spatfix, spatdatsumm[keep,])
}

spatfix <- spatfix[-1,]

spatdatsum <- spatfix %>% group_by(`modelling - data quan spatial`, `spatial & temporal - spatial extent [m2]`,
                                   `spatial & temporal - spatial resolution [m2]`, `per measure - spatially targeted?`,
                                   `representation`) %>%
  count()

write.csv(spatdatsum, file = "../variable_summary_tables/spatial_clust.csv")

spatdatsumm <- spatdat %>% group_by(`modelling - data quan spatial`, `spatial & temporal - spatial extent [m2]`,
                                   `spatial & temporal - spatial resolution [m2]`, `per measure - spatially targeted?`,
                                   `representation`) %>%
  count()

write.csv(spatdatsumm, file = "../variable_summary_tables/spatial.csv")

# Governance figure
gov <- data[,colnames(data) %in% c("echelon", "NATO", "FS_ind", "Document", "clust"),]
gov <- gov %>% distinct()
gov <- na.omit(gov)

gov$echelon <- str_to_title(gov$echelon)
gov$FS_ind <- str_to_title(gov$FS_ind)

# Simplify governance by splitting into two columns and grouping into NATO and at-large - individual
gov$NATO4 <- as.character(gov$NATO)
gov$NATO4[gov$NATO %in% c("Packaged self-serve messages", "Propaganda", "Group-targeted messages", "Bespoke messages")] <- "Nodality"
gov$NATO4[gov$NATO %in% c("Open compacts", "Open permits", "Standard constraints", "Group-targeted constraints", "Conditional tokens", "Enablements")] <- "Authority"
gov$NATO4[gov$NATO %in% c("Bounties", "Bearer-directed payments", "Conduits", "Contracts", "Transfers")] <- "Treasure"
gov$NATO4[gov$NATO %in% c("At-large treatment", "At-large processing", "At-large storage and custody", "Group treatnent", "Group-targeted transportation and distribution", "Processing", "Storage and custody", "Transport and distribution")] <- "Organisation"

gov$AGI <- ""
gov$AGI[gov$NATO %in% c("Packaged self-serve messages", "Propaganda", "Open compacts", "Open permits", "Standard constraints", "Bounties", "Bearer-directed payments", "At-large treatment", "At-large processing", "At-large storage and custody")] <- "At-large"
gov$AGI[gov$NATO %in% c("Group-targeted messages", "Group-targeted constraints", "Conduits", "Group treatnent", "Group-targeted transportation and distribution")] <- "Group"
gov$AGI[gov$NATO %in% c("Bespoke messages", "Conditional tokens", "Enablements", "Contracts", "Transfers", "Processing", "Storage and custody", "Transport and distribution")] <- "Individual"

gov <- gov[,-which(colnames(gov)=="NATO")]
gov <- gov %>% distinct()

gov_s <- gov %>% group_by(echelon, NATO4, Document, clust) %>% count()

gov_t <- gov %>% group_by(NATO4, AGI, Document, clust) %>% count()

gov_u <- gov %>% group_by(AGI, FS_ind, Document, clust) %>% count()

#intract_target$target <- paste("receiver:", intract_target$target)
colnames(gov_s) <- c("source",  "target", "ID", "group",  "value")
colnames(gov_t) <- c("source",  "target", "ID", "group",  "value")
colnames(gov_u) <- c("source",  "target", "ID", "group",  "value")

links <- rbind(gov_s, gov_t, gov_u)

links$group <- as.factor(as.character(links$group))
links <- links[order(links$group),]

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name=c(as.character(links$source), 
         as.character(links$target)) %>% unique()
)

nodes$group <- as.factor(c("clusters"))

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1

library(networkD3)

# Give a color for each group:
my_color <- 'd3.scaleOrdinal() .domain(["1", "2", "3", "4", "clusters"]) .range(["grey", "#FF9900", "deepskyblue", "red", "lightgrey"])'

# Make the Network
s_gov <- sankeyNetwork(Links = links, Nodes = nodes,
                       Source = "IDsource", Target = "IDtarget",
                       Value = "value", NodeID = "name", 
                       sinksRight=FALSE, fontSize = 25,
                       colourScale=my_color, LinkGroup="group", NodeGroup="group")
s_gov #!! colour by NATO

saveNetwork(s_gov, "./Figures/governance_sankey.html")
webshot("./Figures/governance_sankey.html","./Figures/governance_sankey.html", vwidth = 1200, vheight = 900)

# Cluster figure
library(ggrepel)

df$perc <- round(df$n*100/sum(df$n))

df2 <- df %>% 
  mutate(csum = rev(cumsum(rev(perc))), 
         pos = perc/2 + lead(csum, 1),
         pos = if_else(is.na(pos), perc/2, pos))

# Make a pie chart of model scales
df <- data %>% group_by(Document, `spatial & temporal - ref quan scale`) %>% count()
df$n <- 1
df <- df %>% group_by(`spatial & temporal - ref quan scale`) %>% count()
df$`spatial & temporal - ref quan scale` <- as.factor(df$`spatial & temporal - ref quan scale`)
ggplot(df, aes(x = "", y = n, fill = `spatial & temporal - ref quan scale`), colour = "black") +
  geom_col(width = 1, color = 1, show.legend = FALSE) + 
  xlab("") + ylab("") + 
  scale_fill_discrete(name = "", type = colors_pnkgr(6),
                                     labels = c("=< village/city district", "village/city district < X =< municipality",
                                                "municipality < X =< province/state", "province/state < X =< country",
                                                "country < X =< continent", "continent < X =< earth")) +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5), size = 7) +
  coord_polar(theta = "y") + 
  theme_classic() + theme(text = element_text(size = 20)) 

# Make a pie chart of model scales
df <- data %>% group_by(Document, `model_type`) %>% count()
df$n <- 1
df <- df %>% group_by(`model_type`) %>% count()
df$`spatial & temporal - ref quan scale` <- as.factor(df$`model_type`)
ggplot(df, aes(x = "", y = n, fill = `model_type`), colour = "black") +
  geom_col(width = 1, color = 1, show.legend = FALSE) + 
  xlab("") + ylab("") + 
  scale_fill_manual(name = "", values = colours4mtyp) +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5), size = 7) +
  coord_polar(theta = "y") + 
  theme_classic() + theme(text = element_text(size = 20)) 


# Make a pie chart of model scales
df <- data %>% group_by(Document, `domain`) %>% count()
df$n <- 1
df <- df %>% group_by(`domain`) %>% count()
df$`spatial & temporal - ref quan scale` <- as.factor(df$`domain`)
ggplot(df, aes(x = "", y = n, fill = `domain`), colour = "black") +
  geom_col(width = 1, color = 1, show.legend = FALSE) + 
  xlab("") + ylab("") + 
  scale_fill_manual(name = "", values = sample(colors_distinct(4), size = 4)) +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5), size = 7) +
  coord_polar(theta = "y") + 
  theme_classic() + theme(text = element_text(size = 20)) 
