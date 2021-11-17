library(ggplot2)
library(dplyr)
library(ggalt)
library(ggforce)
library(concaveman)

fullkk <- read.csv("./Output/HCPCresult.csv")
load("./Output/HCPCcnames.rda")

fullkk <- fullkk[,-1]
colnames(fullkk) <- c(cnames)

outputm <- fullkk

outputm$clust[outputm$clust == 4] <- "was 1"
outputm$clust[outputm$clust == 3] <- "was 2"
outputm$clust[outputm$clust == 2] <- 3
outputm$clust[outputm$clust == 1] <- 4
outputm$clust[outputm$clust == "was 1"] <- 1
outputm$clust[outputm$clust == "was 2"] <- 2

outputm$clust <- as.factor(outputm$clust)

ref_nrs <- read.csv("./Data/References_numbered.csv", sep = ";")
colnames(ref_nrs)[1] <- "number"
outputm$labelnr <- NA

for(i in unique(outputm$Document)){
  
  if(i == "Balié 2020"){
    refnr <- 2
  }else if(i == "Jin 2019"){
    refnr <- 8
  }else if(i == "Müller 2005"){
    refnr <- 57
  }else if(i == "Argüello 2015"){
    refnr <- 62
  }else if(i == "Allcott 2020"){
    refnr <- 71
  }else if(i == "Chakraborty 2020"){
    refnr <- 106
  }else{
    refrow <- which(ref_nrs$Document==i)
    refnr <- ref_nrs$number[refrow]
  }
  summrow <- which(outputm$Document == i)
  outputm$labelnr[summrow] <- refnr
}

p <- ggplot(outputm, aes(`dim 1`, `dim 2`))
p <- p + #geom_point(aes(fill = clust), pch = 21, colour = "black", size = 2, alpha = 0.5) + 
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0) + 
  geom_text(aes(label = labelnr, colour = clust)) +
  scale_fill_manual("cluster", values = alpha(c("grey", "darkgoldenrod1", "deepskyblue", "firebrick1", "green"),0.1)) +
  scale_colour_manual("cluster", values = c("grey", "darkgoldenrod1", "deepskyblue", "firebrick1", "green"))

p <- p + geom_mark_hull(concavity = 8,expand=0,radius=0, 
                   aes(colour=clust, linetype = clust, fill = clust), #https://luisdva.github.io/rstats/Grouping-points/
                   size = 3, alpha = 0.5) +
  ylab("Dimension 2") +
  xlab("Dimension 1") + 
  #geom_text(aes(label=Document), colour = "black", alpha = 0.9, hjust=0, vjust=0) +
  scale_linetype_manual("type", values = c("solid", "dashed", "solid", "dashed")) +
  scale_colour_manual("cluster", values = c("grey", "darkgoldenrod1", "deepskyblue", "firebrick1", "green")) +
  scale_fill_manual("cluster", values = alpha(c("grey", "darkgoldenrod1", "deepskyblue", "firebrick1", "green"))) +
  scale_y_continuous("Dimension 2 (6.9%)", breaks = seq(-4,8,1)) +
  scale_x_continuous("Dimension 1 (7.8%)", breaks = seq(-4,4,1)) +
  theme_classic() + 
  theme(text = element_text(size = 25)) +
  guides(colour = guide_legend(), 
         fill = "none", 
         linetype = "none") 
p


summcluster <- outputm %>% group_by(clust, labelnr) %>% count(Document) 
cluster_av <- summcluster[1,]
for(i in unique(summcluster$Document)){
  rows <- summcluster[summcluster$Document==i,]
  if(nrow(rows) == 1){
    cluster_av <- rbind(cluster_av, rows)
  }else{
    to_delete <- which(rows$n!=max(rows$n))
    cluster_av <- rbind(cluster_av, rows[-to_delete,])
  }
  
}
cluster_av <- cluster_av[-1,]
summcluster2 <- outputm %>% group_by(Document) %>% summarise(dim1av = mean(`dim 1`, na.rm=TRUE),
                                                             dim2av = mean(`dim 2`, na.rm=TRUE),
                                                             dim3av = mean(`dim 3`, na.rm=TRUE)) 
#cbind(sort(unique(summcluster2$Document)), rep(NA, 106))

summcluster3 <- full_join(cluster_av, summcluster2)
skew_y <- summcluster3$dim1av - min(summcluster3$dim1av)
skew_y <- skew_y + skew_y*0.5
skew_x <- summcluster3$dim2av - min(summcluster3$dim2av)
skew_x <- -(skew_x + skew_x*0.5) 

library(ggrepel)
options(ggrepel.max.overlaps = Inf)
p1 <- p + geom_point(data = summcluster3, aes(dim1av, dim2av, fill = clust), pch = 21, colour = "black", size = 12, alpha = 1) +
  geom_label_repel(data = summcluster3, aes(dim1av, dim2av,label=labelnr, fill = clust), size = 8) +#,
                  # hjust=0, vjust=0, size=5, box.padding = 0.2,
                  # segment.inflect = TRUE, segment.square = FALSE,
                  # arrow = arrow(length = unit(0.015, "npc")),
                  # colour = "black",
                  # segment.curvature = -0.1,
                  # alpha = 0.5,
                  # #direction = "x",
                  # segment.ncp = 3,
                  # force = 0.9,
                  # force_pull = 0.1,
                  # # nudge_y=skew_y,
                  # # nudge_x=skew_x,
                  # segment.alpha = 1,
                  # max.iter = 1000000000) +
  coord_cartesian(clip = "off") +
  expand_limits(y = c(-3, 7), x = c(-4,4))

p1

p2 <- p + geom_point(data = summcluster3, aes(dim1av, dim2av, fill = clust), pch = 21, colour = "black", size = 6, alpha = 1) + 
  geom_text(data = summcluster3, aes(dim1av, dim2av, label=labelnr, hjust=0, vjust=0, size=5))

p2

png(filename = paste0("./Figures/clusters_combined.png"), width = 1300, height = 1100)
p1
dev.off()
