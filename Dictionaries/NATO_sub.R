NATO_sub <- list()

GovClass <- read_excel("./Data/GovClass.xlsx")

for(i in unique(GovClass$NATO_subclass)){
  
  class_vec <- GovClass$Atlas_code[which(GovClass$NATO_subclass==i)]
  NATO_sub[[i]] <- class_vec
  
}

rm("GovClass")
rm("class_vec")